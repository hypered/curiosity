{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
-- brittany-disable-next-binding
module Curiosity.Runtime
  ( emptyReplThreads
  , emptyHttpThreads
  , spawnEmailThread
  , spawnUnixThread
  , runWithRuntime
  , RunM(..)
  , reset
  , state
  , setTime
  , handleCommand
  , submitQuotationSuccess
  , runRunM
  , withRuntimeAtomically
  -- * High-level user operations
  , setUserEmailAddrAsVerifiedFull
  , selectUserByIdResolved
  , selectUserByUsername
  , selectUserByUsernameResolved
  , filterUsers
  , filterUsers'
  , signup
  -- * High-level entity operations
  , selectEntityBySlug
  , selectEntityBySlugResolved
  , selectEntitiesWhereUserId
  , readLegalEntities
  -- * High-level unit operations
  , selectUnitBySlug
  -- * Form edition
  -- ** Quotation
  , formNewQuotation'
  , readCreateQuotationForm'
  , readCreateQuotationForms'
  , readCreateQuotationFormResolved'
  , writeCreateQuotationForm
  , submitCreateQuotationForm
  , setQuotationAsSignedFull
  , filterQuotations
  , filterQuotations'
  , selectQuotationById
  -- ** Orders
  , filterOrders
  , filterOrders'
  -- ** Contract
  , newCreateContractForm
  , readCreateContractForm
  , writeCreateContractForm
  , addExpenseToContractForm
  , writeExpenseToContractForm
  , removeExpenseFromContractForm
  -- ** Simple Contract
  , newCreateSimpleContractForm
  , readCreateSimpleContractForm
  , writeCreateSimpleContractForm
  , addRoleToSimpleContractForm
  , addDateToSimpleContractForm
  , writeDateToSimpleContractForm
  , removeDateFromSimpleContractForm
  , addVATToSimpleContractForm
  , addExpenseToSimpleContractForm
  , writeExpenseToSimpleContractForm
  , removeExpenseFromSimpleContractForm
  -- * Emails
  , filterEmails
  , filterEmails'
  , selectEmailById
  -- * Servant compat
  , appMHandlerNatTrans
  -- * Re-exports for backwards compat: must be removed in the future.
  , module RType
  , module RErr
  , module RIO
  , module AppM
  ) where

import qualified Commence.Multilogging         as ML
import qualified Commence.Runtime.Errors       as Errs
import qualified Commence.Runtime.Storage      as S
import qualified Control.Concurrent.STM        as STM
import           Control.Lens                  as Lens
import "exceptions" Control.Monad.Catch         ( MonadCatch
                                                , MonadMask
                                                , MonadThrow
                                                )
import qualified Curiosity.Command             as Command
import qualified Curiosity.Core                as Core
import qualified Curiosity.Data                as Data
import qualified Curiosity.Data.Business       as Business
import qualified Curiosity.Data.Email          as Email
import qualified Curiosity.Data.Employment     as Employment
import qualified Curiosity.Data.Invoice        as Invoice
import qualified Curiosity.Data.Legal          as Legal
import qualified Curiosity.Data.Order          as Order
import qualified Curiosity.Data.Quotation      as Quotation
import qualified Curiosity.Data.RemittanceAdv  as RemittanceAdv
import qualified Curiosity.Data.SimpleContract as SimpleContract
import qualified Curiosity.Data.User           as User
import           Curiosity.Runtime.Error       as RErr
import           Curiosity.Runtime.IO          as RIO
import           Curiosity.Runtime.IO.AppM     as AppM
import           Curiosity.Runtime.Type        as RType
import           Curiosity.STM.Helpers          ( atomicallyM )
import qualified Data.Aeson.Text               as Aeson
import qualified Data.ByteString.Char8         as B
import           Data.List                      ( lookup
                                                , nub
                                                )
import qualified Data.Map                      as M
import qualified Data.Text.Encoding            as T
import qualified Data.Text.Lazy                as LT
import           Data.UnixTime                  ( formatUnixTime
                                                , fromEpochTime
                                                , getUnixTime
                                                , toEpochTime
                                                )
import           Network.Socket
import           Network.Socket.ByteString      ( recv
                                                , sendAll
                                                )
import qualified Options.Applicative           as A
import           Prelude                 hiding ( state )
import qualified Servant
import           System.PosixCompat.Types       ( EpochTime )

--------------------------------------------------------------------------------
showThreads :: Threads -> IO [Text]
showThreads ts = case ts of
  NoThreads        -> pure ["Threads are disabled."]
  ReplThreads mvarEmails ->
    showThreads' "Email" mvarEmails
  HttpThreads mvarEmails mvarUnix -> do
    t1 <- showThreads' "Email" mvarEmails
    t2 <- showThreads' "UNIX-domain socket" mvarUnix
    pure $ t1 <> t2

showThreads' :: Text -> MVar ThreadId -> IO [Text]
showThreads' name mvar = do
  b <- isEmptyMVar mvar
  if b then pure [name <> " thread: stopped."] else pure [name <> " thread: running."]


--------------------------------------------------------------------------------
-- | `RunM` is basically STM operations (from `Curiosity.Core`) together with
-- the ability to do IO for logging.
newtype RunM a = RunM { unRunM :: ReaderT Runtime IO a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadReader Runtime
           , MonadError IOException
           , MonadThrow
           , MonadCatch
           , MonadMask
           )

runRunM :: forall a m . MonadIO m => Runtime -> RunM a -> m a
runRunM runtime RunM {..} = liftIO $ runReaderT unRunM runtime

-- | Same as `runRunM` but advance the simulated time.
stepRunM :: forall a m . MonadIO m => Runtime -> RunM a -> m a
stepRunM runtime RunM {..} = liftIO $ do
  -- We're injecting (into the STM-readable state) the current time once per
  -- `runRunM` call.
  -- Maybe we should inject the time once per `atomically` instead.
  now <- toEpochTime <$> getUnixTime
  let RunM setNow = do
        mode <- readSteppingMode
        case mode of
          Data.Normal  -> setTime now
            -- TODO This should be False on automated actions.
          Data.Stepped -> void $ stepTime True
            -- TODO Mixed is not yet implemented.
          Data.Mixed   -> void $ stepTime True
  runReaderT (setNow >> unRunM) runtime

-- | Support for logging for the application
instance ML.MonadAppNameLogMulti RunM where
  askLoggers = asks _rLoggers
  localLoggers modLogger =
    local (over rLoggers . over ML.appNameLoggers $ fmap modLogger)


--------------------------------------------------------------------------------
-- | Reset the database to the empty state.
reset :: RunM ()
reset = ML.localEnv (<> "Command" <> "Reset") $ do
  ML.info "Resetting to the empty state."
  db   <- asks _rDb
  mode <- readSteppingMode
  now  <- case mode of
    Data.Normal  -> liftIO $ toEpochTime <$> getUnixTime
    Data.Stepped -> pure 0
    Data.Mixed   -> liftIO $ toEpochTime <$> getUnixTime
  atomicallyM $ Core.reset db now
  ML.info "State is now empty."

-- | Retrieve the whole state as a pure value.
state :: RunM Data.HaskDb
state = do
  db <- asks _rDb
  atomicallyM $ Core.readFullStmDbInHask' db

-- | Retrieve the current simulated time.
time :: RunM EpochTime
time = do
  db <- asks _rDb
  atomicallyM $ Core.readTime db

-- | Set the current simulated time.
setTime :: EpochTime -> RunM ()
setTime x = do
  db <- asks _rDb
  atomicallyM $ Core.writeTime db x

-- | Advance the time by a second or to the next minute.
stepTime :: Bool -> RunM EpochTime
stepTime minute = do
  db <- asks _rDb
  atomicallyM $ Core.stepTime db minute

-- | Retrieve the stepping mode.
readSteppingMode :: RunM Data.SteppingMode
readSteppingMode = do
  db <- asks _rDb
  atomicallyM $ Core.readSteppingMode db

-- | Retrieve the threads state.
threads :: RunM [Text]
threads = do
  ts <- asks _rThreads
  liftIO $ showThreads ts

emptyReplThreads :: IO Threads
emptyReplThreads = do
  mvarEmails <- newEmptyMVar
  pure $ ReplThreads mvarEmails

emptyHttpThreads :: IO Threads
emptyHttpThreads = do
  mvarEmails <- newEmptyMVar
  mvarUnix <- newEmptyMVar
  pure $ HttpThreads mvarEmails mvarUnix

spawnEmailThread :: RunM Text
spawnEmailThread = do
  runtime <- ask
  ts      <- asks _rThreads
  case ts of
    NoThreads        -> pure "Threads are disabled."
    ReplThreads mvarEmails -> spawnEmailThread' runtime mvarEmails
    HttpThreads mvarEmails _ -> spawnEmailThread' runtime mvarEmails

spawnEmailThread' :: Runtime -> MVar ThreadId -> RunM Text
spawnEmailThread' runtime mvar = do
  mthread <- liftIO $ tryTakeMVar mvar
  case mthread of
    Nothing -> do
      ML.localEnv (<> "Threads" <> "Email") $ do
        ML.info "Starting email thread."
      liftIO $ do
        t <- forkIO $ runRunM runtime emailThread
        putMVar mvar t
        pure "Email thread started."
    Just t -> do
      liftIO $ putMVar mvar t
      pure "Email thread alread running."

killEmailThread :: RunM Text
killEmailThread = do
  ts <- asks _rThreads
  case ts of
    NoThreads        -> pure "Threads are disabled."
    ReplThreads mvarEmails -> killEmailThread' mvarEmails
    HttpThreads mvarEmails _ -> killEmailThread' mvarEmails

killEmailThread' :: MVar ThreadId -> RunM Text
killEmailThread' mvar = do
  mthread <- liftIO $ tryTakeMVar mvar
  case mthread of
    Nothing -> do
      pure "Email thread already stopped."
    Just t -> do
      ML.localEnv (<> "Threads" <> "Email") $ do
        ML.info "Stopping email thread."
      liftIO $ killThread t
      pure "Email thread stopped."

emailThread :: RunM ()
emailThread = do
  let loop = do
        liftIO $ threadDelay $ 5 * 1000 * 1000 -- 5 seconds.
        ML.localEnv (<> "Threads") $ do
          emailStep
        loop
  loop

-- | One iteration of the `emailThread` loop.
emailStep :: RunM [Email.Email]
emailStep = do
  db      <- asks _rDb
  records <- emailStepDryRun
  logEmails records
  -- TODO Have a single operation ?
  atomicallyM $ mapM_ (setEmailDone db) records
  pure records
 where
  logEmails records =
    ML.localEnv (<> "Actions" <> "SendEmails")
      .  unless (null records)
      .  ML.info
      $  "Processing "
      <> show (length records)
      <> " emails..."

emailStepDryRun :: RunM [Email.Email]
emailStepDryRun = do
  db <- asks _rDb
  atomicallyM $ filterEmails db Email.EmailsTodo

verifyEmailStep :: RunM [User.UserProfile]
verifyEmailStep = do
  db      <- asks _rDb
  records <- verifyEmailStepDryRun
  logUsers records
  -- TODO Have a single operation ?
  atomicallyM $ mapM_
    ( setUserEmailAddrAsVerified db
    . User._userCredsName
    . User._userProfileCreds
    )
    records
  pure records
 where
  logUsers records =
    ML.localEnv (<> "Actions" <> "VerifyEmails")
      $  unless (null records)
      $  ML.info
      $  "Processing "
      <> show (length records)
      <> " users..."

verifyEmailStepDryRun :: RunM [User.UserProfile]
verifyEmailStepDryRun = do
  db      <- asks _rDb
  records <- atomicallyM $ filterUsers db User.PredicateEmailAddrToVerify
  pure records

spawnUnixThread :: RunM Text
spawnUnixThread = do
  runtime <- ask
  ts      <- asks _rThreads
  case ts of
    NoThreads        -> pure "Threads are disabled."
    ReplThreads _ -> pure "No UNIX-domain socket thread in REPL." -- TODO
    HttpThreads _ mvarUnix -> spawnUnixThread' runtime mvarUnix

spawnUnixThread' :: Runtime -> MVar ThreadId -> RunM Text
spawnUnixThread' runtime mvar = do
  mthread <- liftIO $ tryTakeMVar mvar
  case mthread of
    Nothing -> do
      ML.localEnv (<> "Threads" <> "UNIX") $ do
        ML.info "Starting UNIX-domain socket thread."
      liftIO $ do
        t <- forkIO $ runRunM runtime unixThread
        putMVar mvar t
        pure "UNIX-domain socket thread started."
    Just t -> do
      liftIO $ putMVar mvar t
      pure "UNIX-domain socket thread alread running."

unixThread :: RunM ()
unixThread = do
  runtime <- ask
  liftIO $ runWithRuntime runtime

-- | Natural transformation from some `AppM` in any given mode, to a servant
-- Handler.
appMHandlerNatTrans :: forall a . Runtime -> AppM a -> Servant.Handler a
appMHandlerNatTrans rt appM =
  let
      -- We peel off the AppM + ReaderT layers, exposing our ExceptT
      -- RuntimeErr IO a This is very similar to Servant's Handler:
      -- https://hackage.haskell.org/package/servant-server-0.17/docs/Servant-Server-Internal-Handler.html#t:Handler
      unwrapReaderT          = (`runReaderT` rt) . runAppM $ appM
      -- Map our errors to `ServantError`
      runtimeErrToServantErr = withExceptT Errs.asServantError
  in 
      -- Re-wrap as servant `Handler`
      Servant.Handler $ runtimeErrToServantErr unwrapReaderT


--------------------------------------------------------------------------------
-- | Handle a single command. The return type provides some flexibility, so
-- this function can be used in both `cty` and `cty repl`.
handleCommand
  :: MonadIO m
  => Runtime
  -> User.UserName -- ^ The user performing the command.
  -> Command.Command
  -> m (ExitCode, [Text])
handleCommand runtime@Runtime {..} user command = do
  case command of
    Command.State useHs -> do
      value <- runRunM runtime state
      let value' = if useHs
            then show value
            else LT.toStrict (Aeson.encodeToLazyText value)
      pure (ExitSuccess, [value'])
    Command.Reset -> do
      runRunM runtime reset
      pure (ExitSuccess, ["State is now empty."])
    Command.Threads -> do
      value <- runRunM runtime threads
      pure (ExitSuccess, value)
    Command.StartEmail -> do
      value <- runRunM runtime spawnEmailThread
      pure (ExitSuccess, [value])
    Command.StopEmail -> do
      value <- runRunM runtime killEmailThread
      pure (ExitSuccess, [value])
    Command.StepEmail -> do
      stepRunM runtime emailStep
      pure (ExitSuccess, ["Email queue processed."])
    Command.CreateBusinessUnit input -> do
      output <- runAppMSafe runtime . atomicallyM $ Core.createBusiness _rDb
                                                                        input
      case output of
        Right mid -> do
          case mid of
            Right (Business.UnitId id) -> do
              pure (ExitSuccess, ["Business entity created: " <> id])
            Left err -> pure (ExitFailure 1, [show err])
        Left err -> pure (ExitFailure 1, [show err])
    Command.UpdateBusinessUnit input -> do
      output <- runAppMSafe runtime . atomicallyM $ Core.updateBusiness _rDb
                                                                        input
      case output of
        Right mid -> do
          case mid of
            Right () -> do
              pure
                ( ExitSuccess
                , ["Business unit updated: " <> Business._updateSlug input]
                )
            Left err -> pure (ExitFailure 1, [show err])
        Left err -> pure (ExitFailure 1, [show err])
    Command.LinkBusinessUnitToUser slug uid role -> do
      mid <- runRunM runtime $ linkBusinessUnitToUser' slug uid role
      case mid of
        Right () -> do
          pure (ExitSuccess, ["Business unit updated: " <> slug])
        Left err -> pure (ExitFailure 1, [show err])
    Command.CreateLegalEntity input -> do
      output <- runAppMSafe runtime . atomicallyM $ createLegal _rDb input
      case output of
        Right mid -> do
          case mid of
            Right (Legal.EntityId id) -> do
              pure (ExitSuccess, ["Legal entity created: " <> id])
            Left err -> pure (ExitFailure 1, [show err])
        Left err -> pure (ExitFailure 1, [show err])
    Command.UpdateLegalEntity input -> do
      mid <- runRunM runtime $ updateLegal' input
      case mid of
        Right () -> do
          pure
            (ExitSuccess, ["Legal entity updated: " <> Legal._updateSlug input])
        Left err -> pure (ExitFailure 1, [show err])
    Command.LinkLegalEntityToUser slug uid role -> do
      mid <- runRunM runtime $ linkLegalEntityToUser' slug uid role
      case mid of
        Right () -> do
          pure (ExitSuccess, ["Legal entity updated: " <> slug])
        Left err -> pure (ExitFailure 1, [show err])
    Command.UpdateLegalEntityIsSupervised slug b -> do
      mid <- runRunM runtime $ updateLegalEntityIsSupervised' slug b
      case mid of
        Right () -> do
          pure (ExitSuccess, ["Legal entity updated: " <> slug])
        Left err -> pure (ExitFailure 1, [show err])
    Command.UpdateLegalEntityIsHost slug b -> do
      mid <- runRunM runtime $ updateLegalEntityIsHost' slug b
      case mid of
        Right () -> do
          pure (ExitSuccess, ["Legal entity updated: " <> slug])
        Left err -> pure (ExitFailure 1, [show err])
    Command.Signup input -> do
      muid <- stepRunM runtime $ signup input
      case muid of
        Right (User.UserId uid, Email.EmailId eid) -> pure
          ( ExitSuccess
          , [ "User created: " <> uid
            , "Signup confirmation email enqueued: " <> eid
            ]
          )
        Left err -> case err of
          User.ValidationErrs errs -> pure (ExitFailure 1, map show errs)
          _                        -> pure (ExitFailure 1, [show err])
    Command.Invite input -> do
      muid <- stepRunM runtime $ inviteUser input
      case muid of
        Right (User.UserId uid) ->
          pure (ExitSuccess, ["User created: " <> uid])
        Left err -> pure (ExitFailure 1, [show err])
    Command.SelectUser useHs uid short -> do
      output <- runAppMSafe runtime . atomicallyM $ Core.selectUserById _rDb uid
      case output of
        Right mprofile -> do
          case mprofile of
            Just value -> do
              let value' = if useHs
                    then show value
                    else LT.toStrict (Aeson.encodeToLazyText value)
              if short
                then pure
                  ( ExitSuccess
                  , [ User.unUserId (User._userProfileId value)
                      <> " "
                      <> User.unUserName
                           (User._userCredsName $ User._userProfileCreds value)
                    ]
                  )
                else pure (ExitSuccess, [value'])
            Nothing -> pure (ExitFailure 1, ["No such user."])
        Left err -> pure (ExitFailure 1, [show err])
    Command.FilterUsers predicate -> do
      profiles <- runRunM runtime $ filterUsers' predicate
      let f User.UserProfile {..} =
            let User.UserId i = _userProfileId
                n             = case _userProfileCreds of
                  User.Credentials {..} ->
                    let User.UserName n' = _userCredsName in n'
                  User.InviteToken _ -> "/"
            in  i <> " " <> n
      pure (ExitSuccess, map f profiles)
    Command.UpdateUser input -> do
      output <-
        runAppMSafe runtime . S.liftTxn @AppM @STM $ S.dbUpdate @AppM @STM
          _rDb
          input
      case output of
        Right mid -> do
          case mid of
            Right storageResult -> do
              case storageResult of
                Right [uid] ->
                  pure (ExitSuccess, ["User updated: " <> User.unUserId uid])
                Right _   -> pure (ExitFailure 1, ["No record updated."])
                Left  err -> pure (ExitFailure 1, [show err])
            Left err -> pure (ExitFailure 1, [show err])
        Left err -> pure (ExitFailure 1, [show err])
    Command.SetUserEmailAddrAsVerified username -> do
      output <-
        runAppMSafe runtime
        .   liftIO
        .   STM.atomically
        $   Core.selectUserByUsername _rDb user
        >>= \case
              Just profile ->
                setUserEmailAddrAsVerifiedFull _rDb (profile, username)
              Nothing -> pure . Left . User.UserNotFound $ User.unUserName user
      case output of
        Right (Right ()) -> do
          pure (ExitSuccess, ["User successfully updated."])
        Right (Left err) -> pure (ExitFailure 1, [show err])
        Left  err        -> pure (ExitFailure 1, [show err])
    Command.CreateEmployment input -> do
      output <-
        runAppMSafe runtime
        .   liftIO
        .   STM.atomically
        $   Core.selectUserByUsername _rDb user
        >>= \case
              Just profile -> do
                merror <- submitCreateContractForm' _rDb (profile, input)
                -- TODO Should we have a type to combine multiple possible errors ?
                case merror of
                  Left (Employment.Err err) ->
                    pure . Left $ User.UserNotFound err
                  Right id -> pure $ Right id
              Nothing -> pure . Left . User.UserNotFound $ User.unUserName user
      case output of
        Right mid -> do
          case mid of
            Right (Employment.ContractId id) -> do
              pure (ExitSuccess, ["Employment contract created: " <> id])
            Left err -> pure (ExitFailure 1, [show err])
        Left err -> pure (ExitFailure 1, [show err])
    Command.CreateInvoice -> do
      output <- runAppMSafe runtime . atomicallyM $ createInvoice _rDb
      case output of
        Right mid -> do
          case mid of
            Right (Invoice.InvoiceId id) -> do
              pure (ExitSuccess, ["Invoice created: " <> id])
            Left err -> pure (ExitFailure 1, [show err])
        Left err -> pure (ExitFailure 1, [show err])
    Command.FormNewQuotation input -> do
      mkey <- runRunM runtime $ formNewQuotation user input
      case mkey of
        Right key -> do
          pure (ExitSuccess, ["Quotation form created: " <> key])
        Left err -> pure (ExitFailure 1, [show err])
    Command.FormValidateQuotation input ->
      atomicallyM $ Core.selectUserByUsername _rDb user >>= \case
        Just profile -> do
          mcontract <- readCreateQuotationFormResolved _rDb profile input
          case mcontract of
            Right (contract, resolvedClient) -> do
              case
                  Quotation.validateCreateQuotation profile
                                                    contract
                                                    resolvedClient
                of
                  Right _    -> pure (ExitSuccess, ["Quotation form is valid."])
                  Left  errs -> pure (ExitFailure 1, map Quotation.unErr errs)
            Left _ -> pure (ExitFailure 1, ["Key not found: " <> input])
        Nothing ->
          pure (ExitFailure 1, ["Username not found: " <> User.unUserName user])
    Command.FormSubmitQuotation input ->
      atomicallyM $ Core.selectUserByUsername _rDb user >>= \case
        Just profile -> do
          mid <- submitCreateQuotationForm _rDb (profile, input)
          case mid of
            Right id  -> pure (ExitSuccess, submitQuotationSuccess id)
            Left  err -> pure (ExitFailure 1, [Quotation.unErr err])
        Nothing ->
          pure (ExitFailure 1, ["Username not found: " <> User.unUserName user])
    Command.SignQuotation input -> do
      moid <- stepRunM runtime $ signQuotation user input
      case moid of
        Right (Order.OrderId oid) ->
          pure (ExitSuccess, ["Order created: " <> oid])
        Left err -> pure (ExitFailure 1, [show err])
    Command.RejectQuotation input mcomment -> do
      moid <- stepRunM runtime $ rejectQuotation user input mcomment
      case moid of
        Right ()  -> pure (ExitSuccess, ["Quotation rejected."])
        Left  err -> pure (ExitFailure 1, [show err])
    Command.EmitInvoice input ->
      atomicallyM $ Core.selectUserByUsername _rDb user >>= \case
        Just profile -> do
          mids <- invoiceOrder _rDb (profile, input)
          case mids of
            Right (id0, id1, id2, id3) -> pure
              ( ExitSuccess
              , [ "Invoice created: " <> Invoice.unInvoiceId id0
                , "Internal (proxy) invoice created: "
                  <> Invoice.unInvoiceId id1
                , "Generating payment for " <> Invoice.unInvoiceId id1 <> "..."
                , "Remittance advice (using proxy bank account) created: "
                  <> RemittanceAdv.unRemittanceAdvId id2
                , "Remittance advice (using business unit bank account) created: "
                  <> RemittanceAdv.unRemittanceAdvId id3
                , "Invoice sent to client: " <> Invoice.unInvoiceId id0
                ]
              )
            Left err -> pure (ExitFailure 1, [Order.unErr err])
        Nothing ->
          pure (ExitFailure 1, ["Username not found: " <> User.unUserName user])
    Command.SendReminder input ->
      -- TODO Check this is the "system" user ?
                                  atomicallyM $ do
      Core.createEmail _rDb
                       Email.InvoiceReminderEmail
                       "TODO sender email addr"
                       "TODO client email addr"
      pure
        ( ExitSuccess
        , ["Reminder for invoice sent: " <> Invoice.unInvoiceId input]
        )
    Command.MatchPayment input ->
      -- TODO Check this is the "system" user ?
                                  atomicallyM $ do
      mids <- matchPayment _rDb input
      case mids of
        Right (id0, id1) -> pure
          ( ExitSuccess
          , [ "Generating payment for " <> Invoice.unInvoiceId input <> "..."
            , "Remittance advice (using client bank account) created: "
              <> RemittanceAdv.unRemittanceAdvId id0
            , "Remittance advice (using business unit bank account) created: "
              <> RemittanceAdv.unRemittanceAdvId id1
            ]
          )
        Left err -> pure (ExitFailure 1, [Invoice.unErr err])
    Command.FormNewSimpleContract input -> do
      output <-
        runAppMSafe runtime
        .   liftIO
        .   STM.atomically
        $   Core.selectUserByUsername _rDb user
        >>= \case
              Just profile -> do
                key <- newCreateSimpleContractForm _rDb (profile, input)
                pure $ Right key
              Nothing -> pure . Left . User.UserNotFound $ User.unUserName user
      case output of
        Right mkey -> do
          case mkey of
            Right key -> do
              pure (ExitSuccess, ["Simple contract form created: " <> key])
            Left err -> pure (ExitFailure 1, [show err])
        Left err -> pure (ExitFailure 1, [show err])
    Command.FormValidateSimpleContract input ->
      atomicallyM $ Core.selectUserByUsername _rDb user >>= \case
        Just profile -> do
          mcontract <- readCreateSimpleContractForm _rDb (profile, input)
          case mcontract of
            Right contract -> do
              case
                  SimpleContract.validateCreateSimpleContract profile contract
                of
                  Right _ ->
                    pure (ExitSuccess, ["Simple contract form is valid."])
                  Left errs ->
                    pure (ExitFailure 1, map SimpleContract.unErr errs)
            Left _ -> pure (ExitFailure 1, ["Key not found: " <> input])
        Nothing ->
          pure (ExitFailure 1, ["Username not found: " <> User.unUserName user])
    Command.FilterEmails predicate -> do
      records <- runRunM runtime $ filterEmails' predicate
      let f Email.Email {..} =
            let Email.EmailId      i         = _emailId
                User.UserEmailAddr recipient = _emailRecipient
            in  i <> " " <> recipient
      pure (ExitSuccess, map f records)
    Command.ViewQueues _ -> do
      -- TODO Check first if the user has the necessary rights to handle this
      -- queue.
      (_, output1) <- handleCommand
        runtime
        user
        (Command.FilterUsers User.PredicateEmailAddrToVerify)
      (_, output2) <- handleCommand runtime
                                    user
                                    (Command.FilterEmails Email.EmailsTodo)
      pure
        ( ExitSuccess
        , ["Email addresses to verify:"]
        <> output1
        <> ["Emails to send:"]
        <> output2
        )
    Command.Step True dryRun -> do
      (users, emails) <- runRunM runtime $ do
        users  <- if dryRun then verifyEmailStepDryRun else verifyEmailStep
        emails <- if dryRun then emailStepDryRun else emailStep
        pure (users, emails)
      let
        displayUser User.UserProfile {..} =
          "Setting user email addr. to verified: "
            <> User.unUserEmailAddr _userProfileEmailAddr
        displayEmail Email.Email {..} =
          "Sending email: " <> Email.unEmailId _emailId
        output1 = map displayUser users
        output2 = map displayEmail emails
      pure (ExitSuccess, output1 <> output2 <> ["All steps done."])
    Command.Time step minute -> do
      let fmt = "%d/%b/%Y:%T %z"
      output <- if step
        then runRunM runtime $ stepTime minute
        else runRunM runtime time
      output' <- liftIO $ formatUnixTime fmt $ fromEpochTime output
      pure (ExitSuccess, [T.decodeUtf8 output'])
    _ -> do
      -- TODO It seems that showing the command causes a stack overflow. For
      -- instance the error happens by passing the Log or the Reset commands.
      -- I think this has to do with the fact that they contain a conf.
      -- pure (ExitFailure 1, ["Unhandled command " <> show command])
      pure (ExitFailure 1, ["Unhandled command."])

submitQuotationSuccess id =
  [ "Quotation form validated."
  , "Quotation created: " <> Quotation.unQuotationId id
  , "Quotation sent to client: " <> Quotation.unQuotationId id
  ]

setUserEmailAddrAsVerifiedFull
  :: Core.StmDb -> (User.UserProfile, User.UserName) -> STM (Either User.Err ())
setUserEmailAddrAsVerifiedFull db (user, input) = transaction
 where
  transaction = do
    b <- Core.canPerform 'User.SetUserEmailAddrAsVerified db user
    if b
      then setUserEmailAddrAsVerified db input
      else pure . Left $ User.MissingRight User.CanVerifyEmailAddr


--------------------------------------------------------------------------------
createLegal
  :: Core.StmDb -> Legal.Create -> STM (Either Legal.Err Legal.EntityId)
createLegal db Legal.Create {..} = do
  STM.catchSTM (Right <$> transaction) (pure . Left)
 where
  transaction = do
    newId <- Core.generateLegalId db
    let new = Legal.Entity
          newId
          _createSlug
          _createName
          _createCbeNumber
          _createVatNumber
          Nothing
          []
                           -- TODO Better logic for initial values.
          [Legal.AuthorizedAsBuyer, Legal.AuthorizedAsSeller]
          False
          False
    createLegalFull db new >>= either STM.throwSTM pure

createLegalFull
  :: Core.StmDb -> Legal.Entity -> STM (Either Legal.Err Legal.EntityId)
createLegalFull db new = do
  modifyLegalEntities db (++ [new])
  pure . Right $ Legal._entityId new

updateLegal :: Core.StmDb -> Legal.Update -> STM (Either User.Err ())
updateLegal db Legal.Update {..} = do
  mentity <- selectEntityBySlug db _updateSlug
  case mentity of
    Just Legal.Entity{} -> do
      let replaceOlder entities =
            [ if Legal._entitySlug e == _updateSlug
                then e { Legal._entityDescription = _updateDescription }
                else e
            | e <- entities
            ]
      modifyLegalEntities db replaceOlder
      pure $ Right ()
    Nothing -> pure . Left $ User.UserNotFound _updateSlug -- TODO

updateLegal' :: Legal.Update -> RunM (Either User.Err ())
updateLegal' update = do
  db <- asks _rDb
  atomicallyM $ updateLegal db update

linkLegalEntityToUser
  :: Core.StmDb
  -> Text
  -> User.UserId
  -> Legal.ActingRole
  -> STM (Either User.Err ())
linkLegalEntityToUser db slug uid role = do
  mentity <- selectEntityBySlug db slug
  case mentity of
    Just Legal.Entity {..} -> do
      let replaceOlder entities =
            [ if Legal._entitySlug e == slug
                then e
                  { Legal._entityUsersAndRoles = nub
                                                 $ Legal.ActingUserId uid role
                                                 : _entityUsersAndRoles
                  }
                else e
            | e <- entities
            ]
      modifyLegalEntities db replaceOlder
      pure $ Right ()
    Nothing -> pure . Left $ User.UserNotFound slug -- TODO

linkLegalEntityToUser'
  :: Text -> User.UserId -> Legal.ActingRole -> RunM (Either User.Err ())
linkLegalEntityToUser' slug uid role = do
  db <- asks _rDb
  atomicallyM $ linkLegalEntityToUser db slug uid role

updateLegalEntityIsSupervised
  :: Core.StmDb -> Text -> Bool -> STM (Either User.Err ())
updateLegalEntityIsSupervised db slug b = do
  mentity <- selectEntityBySlug db slug
  case mentity of
    Just Legal.Entity{} -> do
      let replaceOlder entities =
            [ if Legal._entitySlug e == slug
                then e { Legal._entityIsSupervised = b }
                else e
            | e <- entities
            ]
      modifyLegalEntities db replaceOlder
      pure $ Right ()
    Nothing -> pure . Left $ User.UserNotFound slug -- TODO

updateLegalEntityIsSupervised' :: Text -> Bool -> RunM (Either User.Err ())
updateLegalEntityIsSupervised' slug b = do
  db <- asks _rDb
  atomicallyM $ updateLegalEntityIsSupervised db slug b

updateLegalEntityIsHost
  :: Core.StmDb -> Text -> Bool -> STM (Either User.Err ())
updateLegalEntityIsHost db slug b = do
  mentity <- selectEntityBySlug db slug
  case mentity of
    Just Legal.Entity{} -> do
      let replaceOlder entities =
            [ if Legal._entitySlug e == slug
                then e { Legal._entityIsHost = b }
                else e
            | e <- entities
            ]
      modifyLegalEntities db replaceOlder
      pure $ Right ()
    Nothing -> pure . Left $ User.UserNotFound slug -- TODO

updateLegalEntityIsHost' :: Text -> Bool -> RunM (Either User.Err ())
updateLegalEntityIsHost' slug b = do
  db <- asks _rDb
  atomicallyM $ updateLegalEntityIsHost db slug b

modifyLegalEntities
  :: Core.StmDb -> ([Legal.Entity] -> [Legal.Entity]) -> STM ()
modifyLegalEntities db f =
  let tvar = Data._dbLegalEntities db in STM.modifyTVar tvar f


--------------------------------------------------------------------------------
selectUnitBySlug :: Text -> RunM (Maybe Business.Unit)
selectUnitBySlug slug = do
  db <- asks _rDb
  atomicallyM $ Core.selectUnitBySlug db slug

linkBusinessUnitToUser'
  :: Text -> User.UserId -> Business.ActingRole -> RunM (Either User.Err ())
linkBusinessUnitToUser' slug uid role = do
  db <- asks _rDb
  atomicallyM $ Core.linkBusinessUnitToUser db slug uid role


--------------------------------------------------------------------------------
createQuotation
  :: Core.StmDb
  -> Quotation.Quotation
  -> STM (Either Quotation.Err Quotation.QuotationId)
createQuotation db quotation = do
  STM.catchSTM (Right <$> transaction) (pure . Left)
 where
  transaction = do
    newId <- Core.generateQuotationId db
    let new = quotation { Quotation._quotationId = newId }
    createQuotationFull db new >>= either STM.throwSTM pure

createQuotationFull
  :: Core.StmDb
  -> Quotation.Quotation
  -> STM (Either Quotation.Err Quotation.QuotationId)
createQuotationFull db new = do
  modifyQuotations db (++ [new])
  pure . Right $ Quotation._quotationId new

modifyQuotations
  :: Core.StmDb -> ([Quotation.Quotation] -> [Quotation.Quotation]) -> STM ()
modifyQuotations db f =
  let tvar = Data._dbQuotations db in STM.modifyTVar tvar f

createOrderForQuotation
  :: Core.StmDb
  -> (User.UserProfile, Quotation.QuotationId)
     -- ^ TODO SignQuotation data type, including e.g. the signature data.
  -> STM (Either Quotation.Err Order.OrderId)
createOrderForQuotation db _ = do
  mid <- createOrder db
  case mid of
    Right id              -> pure $ Right id
    Left  (Order.Err err) -> pure $ Left $ Quotation.Err err


--------------------------------------------------------------------------------
createOrder :: Core.StmDb -> STM (Either Order.Err Order.OrderId)
createOrder db = do
  STM.catchSTM (Right <$> transaction) (pure . Left)
 where
  transaction = do
    newId <- Core.generateOrderId db
    let new = Order.Order newId
    createOrderFull db new >>= either STM.throwSTM pure

createOrderFull
  :: Core.StmDb -> Order.Order -> STM (Either Order.Err Order.OrderId)
createOrderFull db new = do
  modifyOrders db (++ [new])
  pure . Right $ Order._orderId new

modifyOrders :: Core.StmDb -> ([Order.Order] -> [Order.Order]) -> STM ()
modifyOrders db f = let tvar = Data._dbOrders db in STM.modifyTVar tvar f

invoiceOrder
  :: Core.StmDb
  -> (User.UserProfile, Order.OrderId)
  -> STM
       ( Either
           Order.Err
           ( Invoice.InvoiceId
           , Invoice.InvoiceId
           , RemittanceAdv.RemittanceAdvId
           , RemittanceAdv.RemittanceAdvId
           )
       )
invoiceOrder db (profile, _) = do
  mids <- STM.catchSTM (Right <$> createTwoInvoices db) (pure . Left)
  case mids of
    Right (id0, id1, id2, id3) -> do
      -- Invoices and remittance advices created, do the rest of the atomic process.
      Core.createEmail db
                       Email.InvoiceEmail
                       (User._userProfileEmailAddr profile)
                       (User._userProfileEmailAddr profile)
      -- TODO The email address should be the one from the client.
      pure $ Right (id0, id1, id2, id3)
    Left (Invoice.Err err) -> pure $ Left $ Order.Err err

createTwoInvoices db = do
  mid0 <- createInvoice db
  mid1 <- createInvoice db
  mid2 <- createRemittanceAdv db
  mid3 <- createRemittanceAdv db
  case (mid0, mid1, mid2, mid3) of
    (Right id0, Right id1, Right id2, Right id3) -> pure (id0, id1, id2, id3)
    _ -> STM.throwSTM $ Invoice.Err "Failed to create invoices."


--------------------------------------------------------------------------------
createRemittanceAdv
  :: Core.StmDb -> STM (Either RemittanceAdv.Err RemittanceAdv.RemittanceAdvId)
createRemittanceAdv db = do
  STM.catchSTM (Right <$> transaction) (pure . Left)
 where
  transaction = do
    newId <- Core.generateRemittanceAdvId db
    let new = RemittanceAdv.RemittanceAdv newId
    createRemittanceAdvFull db new >>= either STM.throwSTM pure

createRemittanceAdvFull
  :: Core.StmDb
  -> RemittanceAdv.RemittanceAdv
  -> STM (Either RemittanceAdv.Err RemittanceAdv.RemittanceAdvId)
createRemittanceAdvFull db new = do
  modifyRemittanceAdvs db (++ [new])
  pure . Right $ RemittanceAdv._remittanceAdvId new

modifyRemittanceAdvs
  :: Core.StmDb
  -> ([RemittanceAdv.RemittanceAdv] -> [RemittanceAdv.RemittanceAdv])
  -> STM ()
modifyRemittanceAdvs db f =
  let tvar = Data._dbRemittanceAdvs db in STM.modifyTVar tvar f


--------------------------------------------------------------------------------
createEmployment
  :: Core.StmDb
  -> Employment.Contract
  -> STM (Either Employment.Err Employment.ContractId)
createEmployment db _ = do
  STM.catchSTM (Right <$> transaction) (pure . Left)
 where
  transaction = do
    newId <- Core.generateEmploymentId db
    let new = Employment.Contract newId
    createEmploymentFull db new >>= either STM.throwSTM pure

createEmploymentFull
  :: Core.StmDb
  -> Employment.Contract
  -> STM (Either Employment.Err Employment.ContractId)
createEmploymentFull db new = do
  modifyEmployments db (++ [new])
  pure . Right $ Employment._contractId new

modifyEmployments
  :: Core.StmDb -> ([Employment.Contract] -> [Employment.Contract]) -> STM ()
modifyEmployments db f =
  let tvar = Data._dbEmployments db in STM.modifyTVar tvar f

newCreateContractForm
  :: Core.StmDb -> (User.UserProfile, Employment.CreateContractAll') -> STM Text
newCreateContractForm db (profile, Employment.CreateContractAll' gi ty ld rs inv)
  = do
    key <- Core.genRandomText db
    STM.modifyTVar (Data._dbFormCreateContractAll db) (add key)
    pure key
 where
  add key =
    M.insert (username, key) (Employment.CreateContractAll gi ty ld rs inv [])
  username = User._userCredsName $ User._userProfileCreds profile

readCreateContractForm
  :: Core.StmDb
  -> (User.UserProfile, Text)
  -> STM (Either () Employment.CreateContractAll)
readCreateContractForm db (profile, key) = do
  m <- STM.readTVar $ Data._dbFormCreateContractAll db
  let mform = M.lookup (username, key) m
  pure $ maybe (Left ()) Right mform
  where username = User._userCredsName $ User._userProfileCreds profile

writeCreateContractForm
  :: Core.StmDb
  -> (User.UserProfile, Text, Employment.CreateContractAll')
  -> STM Text
writeCreateContractForm db (profile, key, Employment.CreateContractAll' gi ty ld rs inv)
  = do
    STM.modifyTVar (Data._dbFormCreateContractAll db) save
    pure key
 where
  -- TODO Return an error when the key is not found.
  save = M.adjust
    (\(Employment.CreateContractAll _ _ _ _ _ es) ->
      Employment.CreateContractAll gi ty ld rs inv es
    )
    (username, key)
  username = User._userCredsName $ User._userProfileCreds profile

addExpenseToContractForm
  :: Core.StmDb -> (User.UserProfile, Text, Employment.AddExpense) -> STM () -- TODO Possible errors
addExpenseToContractForm db (profile, key, expense) = do
  STM.modifyTVar (Data._dbFormCreateContractAll db) save
 where
  save = M.adjust
    (\(Employment.CreateContractAll gi ty ld rs inv es) ->
      Employment.CreateContractAll gi ty ld rs inv $ es ++ [expense]
    )
    (username, key)
  username = User._userCredsName $ User._userProfileCreds profile

writeExpenseToContractForm
  :: Core.StmDb
  -> (User.UserProfile, Text, Int, Employment.AddExpense)
  -> STM () -- TODO Possible errors
writeExpenseToContractForm db (profile, key, idx, expense) = do
  STM.modifyTVar (Data._dbFormCreateContractAll db) save
 where
  save = M.adjust
    (\(Employment.CreateContractAll gi ty ld rs inv es) ->
      let f i e = if i == idx then expense else e
          es' = zipWith f [0 ..] es
      in  Employment.CreateContractAll gi ty ld rs inv es'
    )
    (username, key)
  username = User._userCredsName $ User._userProfileCreds profile

removeExpenseFromContractForm
  :: Core.StmDb -> (User.UserProfile, Text, Int) -> STM () -- TODO Possible errors
removeExpenseFromContractForm db (profile, key, idx) = do
  STM.modifyTVar (Data._dbFormCreateContractAll db) save
 where
  save = M.adjust
    (\(Employment.CreateContractAll gi ty ld rs inv es) ->
      let es' = map snd . filter ((/= idx) . fst) $ zip [0 ..] es
      in  Employment.CreateContractAll gi ty ld rs inv es'
    )
    (username, key)
  username = User._userCredsName $ User._userProfileCreds profile

-- | Fetch the contract form from the staging area, then attempt to validate
-- and create it.
submitCreateContractForm
  :: Core.StmDb
  -> (User.UserProfile, Employment.SubmitContract)
  -> STM (Either Employment.Err Employment.ContractId)
submitCreateContractForm db (profile, Employment.SubmitContract key) = do
  minput <- readCreateContractForm db (profile, key)
  case minput of
    Right input -> submitCreateContractForm' db (profile, input)
    Left  err   -> pure . Left $ Employment.Err (show err)

-- | Attempt to validate a contract form and create it.
submitCreateContractForm'
  :: Core.StmDb
  -> (User.UserProfile, Employment.CreateContractAll)
  -> STM (Either Employment.Err Employment.ContractId)
submitCreateContractForm' db (profile, input) = do
  let mc = Employment.validateCreateContract profile input
  case mc of
    Right c   -> createEmployment db c
    Left  err -> pure . Left $ Employment.Err (show err)


--------------------------------------------------------------------------------
-- | Create a new form instance in the staging area.
formNewQuotation
  :: User.UserName
  -> Quotation.CreateQuotationAll
  -> RunM (Either User.Err Text)
formNewQuotation user input =
  ML.localEnv (<> "Command" <> "FormNewQuotation") $ do
    ML.info "Instantiating new quotation form..."
    db   <- asks _rDb
    mkey <- atomicallyM $ Core.selectUserByUsername db user >>= \case
      Just profile -> do
        key <- newCreateQuotationForm db (profile, input)
        pure $ Right key
      Nothing -> pure . Left . User.UserNotFound $ User.unUserName user
    case mkey of
      Left  err -> ML.info $ "Can't instantiate form: " <> show err
      Right key -> ML.info $ "Quotation form created: " <> key
    pure mkey

-- | Create a new form instance in the staging area.
-- TODO Refacto: this is the same as above, but using a profile, instead of a username.
formNewQuotation'
  :: User.UserProfile -> Quotation.CreateQuotationAll -> RunM Text
formNewQuotation' profile input =
  ML.localEnv (<> "Command" <> "FormNewQuotation") $ do
    ML.info "Instantiating new quotation form..."
    db  <- asks _rDb
    key <- atomicallyM $ do
      newCreateQuotationForm db (profile, input)
    ML.info $ "Quotation form created: " <> key
    pure key

-- | Create a new form instance in the staging area.
newCreateQuotationForm
  :: Core.StmDb -> (User.UserProfile, Quotation.CreateQuotationAll) -> STM Text
newCreateQuotationForm db (profile, form) = do
  key <- Core.genRandomText db
  STM.modifyTVar (Data._dbFormCreateQuotationAll db) (add key)
  pure key
 where
  add key = M.insert (username, key) form
  username = User._userCredsName $ User._userProfileCreds profile

readCreateQuotationFormResolved'
  :: User.UserProfile
  -> Text
  -> RunM
       (Either () (Quotation.CreateQuotationAll, Maybe User.UserProfile))
readCreateQuotationFormResolved' profile key = do
  db <- asks _rDb
  atomicallyM $ readCreateQuotationFormResolved db profile key

-- | A version of `readCreateQuotationForm'` that also tries to lookup related
-- data, e.g. the client, which is used for validation.
readCreateQuotationFormResolved
  :: Core.StmDb
  -> User.UserProfile
  -> Text
  -> STM
       ( Either
           ()
           (Quotation.CreateQuotationAll, Maybe User.UserProfile)
       )
readCreateQuotationFormResolved db profile key = do
  mform <- readCreateQuotationForm db (profile, key)
  case mform of
    Right form -> do
      mclient <- maybe (pure Nothing) (Core.selectUserByUsername db)
        $ Quotation._createQuotationClientUsername form
      pure $ Right (form, mclient)
    Left err -> pure $ Left err

readCreateQuotationForm'
  :: User.UserProfile -> Text -> RunM (Either () Quotation.CreateQuotationAll)
readCreateQuotationForm' profile key = do
  db <- asks _rDb
  atomicallyM $ readCreateQuotationForm db (profile, key)

readCreateQuotationForm
  :: Core.StmDb
  -> (User.UserProfile, Text)
  -> STM (Either () Quotation.CreateQuotationAll)
readCreateQuotationForm = readForm Data._dbFormCreateQuotationAll

readCreateQuotationForms'
  :: User.UserProfile -> RunM (Either () [(Text, Quotation.CreateQuotationAll)])
readCreateQuotationForms' profile = do
  db <- asks _rDb
  atomicallyM $ readCreateQuotationForms db profile

readCreateQuotationForms
  :: Core.StmDb
  -> User.UserProfile
  -> STM (Either () [(Text, Quotation.CreateQuotationAll)])
readCreateQuotationForms = readForms Data._dbFormCreateQuotationAll

writeCreateQuotationForm
  :: Core.StmDb
  -> (User.UserProfile, Text, Quotation.CreateQuotationAll)
  -> STM Text
writeCreateQuotationForm db (profile, key, form) = do
  STM.modifyTVar (Data._dbFormCreateQuotationAll db) save
  pure key
 where
  -- TODO Return an error when the key is not found.
  save     = M.adjust (const form) (username, key)
  username = User._userCredsName $ User._userProfileCreds profile

deleteCreateQuotationForm :: Core.StmDb -> (User.UserProfile, Text) -> STM () -- TODO Error
deleteCreateQuotationForm = deleteForm Data._dbFormCreateQuotationAll

-- | Fetch the quotation form from the staging area, then attempt to validate
-- and create it. If successfull, the form is deleted.
submitCreateQuotationForm
  :: Core.StmDb
  -> (User.UserProfile, Quotation.SubmitQuotation)
  -> STM (Either Quotation.Err Quotation.QuotationId)
submitCreateQuotationForm db (profile, Quotation.SubmitQuotation key) = do
  minput <- readCreateQuotationFormResolved db profile key
  case minput of
    Right (input, mresolvedClient) -> do
      mid <- submitCreateQuotationForm' db (profile, input) mresolvedClient
      case mid of
        Right (id, resolvedClient) -> do
          -- Quotation created, do the rest of the atomic process.
          deleteCreateQuotationForm db (profile, key)
          Core.createEmail db
                           Email.QuotationEmail
                           (User._userProfileEmailAddr profile)
                           (User._userProfileEmailAddr resolvedClient)
          pure $ Right id
        Left err -> pure $ Left err
    Left err -> pure . Left $ Quotation.Err (show err)

-- | Attempt to validate a quotation form and create it.
submitCreateQuotationForm'
  :: Core.StmDb
  -> (User.UserProfile, Quotation.CreateQuotationAll)
  -> Maybe User.UserProfile
  -> STM
       (Either Quotation.Err (Quotation.QuotationId, User.UserProfile))
submitCreateQuotationForm' db (profile, input) mResolvedClient = do
  let mc = Quotation.validateCreateQuotation profile input mResolvedClient
  case mc of
    Right (c, resolvedClient) -> do
      mid <- createQuotation db c
      case mid of
        Right id  -> pure $ Right (id, resolvedClient)
        Left  err -> pure $ Left err
    Left err -> pure . Left $ Quotation.Err (show err)

signQuotation
  :: User.UserName
  -> Quotation.QuotationId
  -> RunM (Either Quotation.Err Order.OrderId)
signQuotation user input = ML.localEnv (<> "Command" <> "SignQuotation") $ do
  ML.info "Signing quotation..."
  db   <- asks _rDb
  moid <- atomicallyM $ signQuotation' db user input
  case moid of
    Right (Order.OrderId oid) -> do
      ML.info $ "Order created: " <> oid
      pure . Right $ Order.OrderId oid
    Left err@(Quotation.Err msg) -> do
      ML.info msg
      pure $ Left err

rejectQuotation
  :: User.UserName
  -> Quotation.QuotationId
  -> Maybe Text
  -> RunM (Either Quotation.Err ())
rejectQuotation user input mcomment =
  ML.localEnv (<> "Command" <> "RejectQuotation") $ do
    ML.info "Rejecting quotation..."
    db   <- asks _rDb
    moid <- atomicallyM $ rejectQuotation' db user input mcomment
    case moid of
      Right () -> do
        ML.info "Quotation rejected."
        pure $ Right ()
      Left err@(Quotation.Err msg) -> do
        ML.info msg
        pure $ Left err

signQuotation' db user input = Core.selectUserByUsername db user >>= \case
  Just profile -> do
    mid <- setQuotationAsSignedFull db (profile, input)
    case mid of
      Right id  -> pure $ Right id
      Left  err -> pure $ Left err
  Nothing ->
    pure $ Left $ Quotation.Err $ "Username not found: " <> User.unUserName user

setQuotationAsSignedFull
  :: Core.StmDb
  -> (User.UserProfile, Quotation.QuotationId)
  -> STM (Either Quotation.Err Order.OrderId)
setQuotationAsSignedFull db (user, input) = STM.catchSTM
  (Right <$> transaction)
  (pure . Left)
 where
  transaction = do
    -- TODO Check the user can sign (e.g. the quotation is her).
    oid <- createOrderForQuotation db (user, input) >>= either STM.throwSTM pure
    setQuotationAsSigned db input oid >>= either STM.throwSTM pure
    pure oid

setQuotationAsSigned
  :: Core.StmDb
  -> Quotation.QuotationId
  -> Order.OrderId
  -> STM (Either Quotation.Err ())
setQuotationAsSigned db id oid = do
  mquotation <- Core.selectQuotationById db id
  case mquotation of
    Just Quotation.Quotation {..} -> case _quotationState of
      Quotation.QuotationSent -> do
        let replaceOlder records =
              [ if Quotation._quotationId r == id
                  then r
                    { Quotation._quotationState = Quotation.QuotationSigned oid
                    }
                  else r
              | r <- records
              ]
        Core.modifyQuotations db replaceOlder
        pure $ Right ()
      _ -> pure . Left $ Quotation.Err "Quotation is not in the Sent state."
    Nothing -> pure . Left $ Quotation.Err "No such quotation."

rejectQuotation' db user input mcomment =
  Core.selectUserByUsername db user >>= \case
    Just profile -> setQuotationAsRejectedFull db profile input mcomment
    Nothing ->
      pure
        $  Left
        $  Quotation.Err
        $  "Username not found: "
        <> User.unUserName user

-- FIXME: Remove the "user" param since not used.
setQuotationAsRejectedFull
  :: Core.StmDb
  -> User.UserProfile
  -> Quotation.QuotationId
  -> Maybe Text
  -> STM (Either Quotation.Err ())
setQuotationAsRejectedFull db user input mcomment = STM.catchSTM
  (Right <$> transaction)
  (pure . Left)
 where
  transaction = do
    -- TODO Check the user can reject (e.g. the quotation is her).
    setQuotationAsRejected db input mcomment >>= either STM.throwSTM pure

setQuotationAsRejected
  :: Core.StmDb
  -> Quotation.QuotationId
  -> Maybe Text
  -> STM (Either Quotation.Err ())
setQuotationAsRejected db id mcomment = do
  mquotation <- Core.selectQuotationById db id
  case mquotation of
    Just Quotation.Quotation {..} -> case _quotationState of
      Quotation.QuotationSent -> do
        let replaceOlder records =
              [ if Quotation._quotationId r == id
                  then r
                    { Quotation._quotationState = Quotation.QuotationRejected
                                                    mcomment
                    }
                  else r
              | r <- records
              ]
        Core.modifyQuotations db replaceOlder
        pure $ Right ()
      _ -> pure . Left $ Quotation.Err "Quotation is not in the Sent state."
    Nothing -> pure . Left $ Quotation.Err "No such quotation."

filterQuotations
  :: Core.StmDb -> Quotation.Predicate -> STM [Quotation.Quotation]
filterQuotations db predicate = do
  let tvar = Data._dbQuotations db
  records <- STM.readTVar tvar
  pure $ filter (Quotation.applyPredicate predicate) records

filterQuotations' :: Quotation.Predicate -> RunM [Quotation.Quotation]
filterQuotations' predicate = do
  db <- asks _rDb
  atomicallyM $ filterQuotations db predicate

selectQuotationById
  :: Core.StmDb -> Quotation.QuotationId -> IO (Maybe Quotation.Quotation)
selectQuotationById db id = STM.atomically $ Core.selectQuotationById db id


--------------------------------------------------------------------------------
filterOrders :: Core.StmDb -> Order.Predicate -> STM [Order.Order]
filterOrders db predicate = do
  let tvar = Data._dbOrders db
  records <- STM.readTVar tvar
  pure $ filter (Order.applyPredicate predicate) records

filterOrders' :: Order.Predicate -> RunM [Order.Order]
filterOrders' predicate = do
  db <- asks _rDb
  atomicallyM $ filterOrders db predicate


--------------------------------------------------------------------------------
-- | Create a new form instance in the staging area.
newCreateSimpleContractForm
  :: Core.StmDb
  -> (User.UserProfile, SimpleContract.CreateContractAll')
  -> STM Text
newCreateSimpleContractForm db (profile, SimpleContract.CreateContractAll' ty rs cl inv)
  = do
    key <- Core.genRandomText db
    STM.modifyTVar (Data._dbFormCreateSimpleContractAll db) (add key)
    pure key
 where
  add key = M.insert (username, key)
                     (SimpleContract.CreateContractAll ty rs cl inv [] [])
  username = User._userCredsName $ User._userProfileCreds profile

readCreateSimpleContractForm
  :: Core.StmDb
  -> (User.UserProfile, Text)
  -> STM (Either () SimpleContract.CreateContractAll)
readCreateSimpleContractForm = readForm Data._dbFormCreateSimpleContractAll

writeCreateSimpleContractForm
  :: Core.StmDb
  -> (User.UserProfile, Text, SimpleContract.CreateContractAll')
  -> STM Text
writeCreateSimpleContractForm db (profile, key, SimpleContract.CreateContractAll' ty rs cl inv)
  = do
    STM.modifyTVar (Data._dbFormCreateSimpleContractAll db) save
    pure key
 where
  -- TODO Return an error when the key is not found.
  save = M.adjust
    (\(SimpleContract.CreateContractAll _ _ _ _ ds es) ->
      SimpleContract.CreateContractAll ty rs cl inv ds es
    )
    (username, key)
  username = User._userCredsName $ User._userProfileCreds profile

addRoleToSimpleContractForm
  :: Core.StmDb -> (User.UserProfile, Text, SimpleContract.SelectRole) -> STM () -- TODO Possible errors
addRoleToSimpleContractForm db (profile, key, SimpleContract.SelectRole role) =
  do
    STM.modifyTVar (Data._dbFormCreateSimpleContractAll db) save
 where
  save = M.adjust
    (\(SimpleContract.CreateContractAll ty rs cl inv ds es) ->
      let ty' = ty { SimpleContract._createContractRole = role }
      in  SimpleContract.CreateContractAll ty' rs cl inv ds es
    )
    (username, key)
  username = User._userCredsName $ User._userProfileCreds profile

addDateToSimpleContractForm
  :: Core.StmDb -> (User.UserProfile, Text, SimpleContract.AddDate) -> STM () -- TODO Possible errors
addDateToSimpleContractForm db (profile, key, date) = do
  STM.modifyTVar (Data._dbFormCreateSimpleContractAll db) save
 where
  save = M.adjust
    (\(SimpleContract.CreateContractAll ty rs cl inv ds es) ->
      SimpleContract.CreateContractAll ty rs cl inv (ds ++ [date]) es
    )
    (username, key)
  username = User._userCredsName $ User._userProfileCreds profile

writeDateToSimpleContractForm
  :: Core.StmDb
  -> (User.UserProfile, Text, Int, SimpleContract.AddDate)
  -> STM () -- TODO Possible errors
writeDateToSimpleContractForm db (profile, key, idx, date) = do
  STM.modifyTVar (Data._dbFormCreateSimpleContractAll db) save
 where
  save = M.adjust
    (\(SimpleContract.CreateContractAll ty rs cl inv ds es) ->
      let f i e = if i == idx then date else e
          ds' = zipWith f [0 ..] ds
      in  SimpleContract.CreateContractAll ty rs cl inv ds' es
    )
    (username, key)
  username = User._userCredsName $ User._userProfileCreds profile

removeDateFromSimpleContractForm
  :: Core.StmDb -> (User.UserProfile, Text, Int) -> STM () -- TODO Possible errors
removeDateFromSimpleContractForm db (profile, key, idx) = do
  STM.modifyTVar (Data._dbFormCreateSimpleContractAll db) save
 where
  save = M.adjust
    (\(SimpleContract.CreateContractAll ty rs cl inv ds es) ->
      let ds' = map snd . filter ((/= idx) . fst) $ zip [0 ..] ds
      in  SimpleContract.CreateContractAll ty rs cl inv ds' es
    )
    (username, key)
  username = User._userCredsName $ User._userProfileCreds profile

addVATToSimpleContractForm
  :: Core.StmDb -> (User.UserProfile, Text, SimpleContract.SelectVAT) -> STM () -- TODO Possible errors
addVATToSimpleContractForm db (profile, key, SimpleContract.SelectVAT rate) =
  do
    STM.modifyTVar (Data._dbFormCreateSimpleContractAll db) save
 where
  save = M.adjust
    (\(SimpleContract.CreateContractAll ty rs cl inv ds es) ->
      let inv' = inv { SimpleContract._createContractVAT = rate }
      in  SimpleContract.CreateContractAll ty rs cl inv' ds es
    )
    (username, key)
  username = User._userCredsName $ User._userProfileCreds profile

addExpenseToSimpleContractForm
  :: Core.StmDb -> (User.UserProfile, Text, SimpleContract.AddExpense) -> STM () -- TODO Possible errors
addExpenseToSimpleContractForm db (profile, key, expense) = do
  STM.modifyTVar (Data._dbFormCreateSimpleContractAll db) save
 where
  save = M.adjust
    (\(SimpleContract.CreateContractAll ty ld cl rs inv es) ->
      SimpleContract.CreateContractAll ty ld cl rs inv $ es ++ [expense]
    )
    (username, key)
  username = User._userCredsName $ User._userProfileCreds profile

writeExpenseToSimpleContractForm
  :: Core.StmDb
  -> (User.UserProfile, Text, Int, SimpleContract.AddExpense)
  -> STM () -- TODO Possible errors
writeExpenseToSimpleContractForm db (profile, key, idx, expense) = do
  STM.modifyTVar (Data._dbFormCreateSimpleContractAll db) save
 where
  save = M.adjust
    (\(SimpleContract.CreateContractAll ty ld rs cl inv es) ->
      let f i e = if i == idx then expense else e
          es' = zipWith f [0 ..] es
      in  SimpleContract.CreateContractAll ty ld rs cl inv es'
    )
    (username, key)
  username = User._userCredsName $ User._userProfileCreds profile

removeExpenseFromSimpleContractForm
  :: Core.StmDb -> (User.UserProfile, Text, Int) -> STM () -- TODO Possible errors
removeExpenseFromSimpleContractForm db (profile, key, idx) = do
  STM.modifyTVar (Data._dbFormCreateSimpleContractAll db) save
 where
  save = M.adjust
    (\(SimpleContract.CreateContractAll ty ld rs cl inv es) ->
      let es' = map snd . filter ((/= idx) . fst) $ zip [0 ..] es
      in  SimpleContract.CreateContractAll ty ld rs cl inv es'
    )
    (username, key)
  username = User._userCredsName $ User._userProfileCreds profile


--------------------------------------------------------------------------------
createInvoice :: Core.StmDb -> STM (Either Invoice.Err Invoice.InvoiceId)
createInvoice db = do
  STM.catchSTM (Right <$> transaction) (pure . Left)
 where
  transaction = do
    newId <- Core.generateInvoiceId db
    let new = Invoice.Invoice newId
    createInvoiceFull db new >>= either STM.throwSTM pure

createInvoiceFull
  :: Core.StmDb -> Invoice.Invoice -> STM (Either Invoice.Err Invoice.InvoiceId)
createInvoiceFull db new = do
  modifyInvoices db (++ [new])
  pure . Right $ Invoice._entityId new

modifyInvoices
  :: Core.StmDb -> ([Invoice.Invoice] -> [Invoice.Invoice]) -> STM ()
modifyInvoices db f = let tvar = Data._dbInvoices db in STM.modifyTVar tvar f

matchPayment
  :: Core.StmDb
  -> Invoice.InvoiceId
  -> STM
       ( Either
           Invoice.Err
           (RemittanceAdv.RemittanceAdvId, RemittanceAdv.RemittanceAdvId)
       )
matchPayment db _ = do
  mids <- STM.catchSTM (Right <$> createTwoRemittanceAdvs db) (pure . Left)
  pure mids

createTwoRemittanceAdvs db = do
  mid0 <- createRemittanceAdv db
  mid1 <- createRemittanceAdv db
  case (mid0, mid1) of
    (Right id0, Right id1) -> pure (id0, id1)
    _ -> STM.throwSTM $ Invoice.Err "Failed to create remittance advices."

selectUserByIdResolved db id = do
  let usersTVar = Data._dbUserProfiles db
  users' <- STM.readTVar usersTVar
  case find ((== id) . S.dbId) users' of
    Just user -> do
      entities <- selectEntitiesWhereUserId db $ User._userProfileId user
      pure $ Just (user, entities)
    Nothing -> pure Nothing

selectUserByUsername
  :: Core.StmDb -> User.UserName -> IO (Maybe User.UserProfile)
selectUserByUsername db username =
  STM.atomically $ Core.selectUserByUsername db username

selectUserByUsernameResolved
  :: Core.StmDb
  -> User.UserName
  -> STM (Maybe (User.UserProfile, [Legal.EntityAndRole]))
selectUserByUsernameResolved db username = do
  let usersTVar = Data._dbUserProfiles db
  users' <- STM.readTVar usersTVar
  case
      find ((== username) . User._userCredsName . User._userProfileCreds) users'
    of
      Just user -> do
        entities <- selectEntitiesWhereUserId db $ User._userProfileId user
        pure $ Just (user, entities)
      Nothing -> pure Nothing

filterUsers :: Core.StmDb -> User.Predicate -> STM [User.UserProfile]
filterUsers db predicate = do
  let tvar = Data._dbUserProfiles db
  records <- STM.readTVar tvar
  pure $ filter (User.applyPredicate predicate) records

filterUsers' :: User.Predicate -> RunM [User.UserProfile]
filterUsers' predicate = do
  db <- asks _rDb
  atomicallyM $ filterUsers db predicate

signup
  :: User.Signup -> RunM (Either User.Err (User.UserId, Email.EmailId))
signup input = ML.localEnv (<> "Command" <> "Signup") $ do
  ML.info "Signing up new user..."
  db   <- asks _rDb
  muid <- atomicallyM $ Core.signup db input
  case muid of
    Right (User.UserId uid, Email.EmailId eid) -> do
      ML.info $ "User created: " <> uid
      ML.info $ "Signup confirmation email enqueued: " <> eid
      pure $ Right (User.UserId uid, Email.EmailId eid)
    Left err -> do
      ML.info $ "Failed to create user: " <> show err
      pure $ Left err

inviteUser :: User.Invite -> RunM (Either User.Err User.UserId)
inviteUser input = ML.localEnv (<> "Command" <> "Signup") $ do
  ML.info "Inviting new user..."
  db   <- asks _rDb
  muid <- atomicallyM $ Core.inviteUser db input
  case muid of
    Right (User.UserId uid, Email.EmailId eid) -> do
      ML.info $ "User created: " <> uid
      ML.info $ "Signup confirmation email enqueued: " <> eid
      pure . Right $ User.UserId uid
    Left err -> do
      ML.info $ "Failed to create user: " <> show err
      pure $ Left err

setUserEmailAddrAsVerified
  :: Core.StmDb -> User.UserName -> STM (Either User.Err ())
setUserEmailAddrAsVerified db username = do
  mprofile <- Core.selectUserByUsername db username
  case mprofile of
    Just User.UserProfile {..} -> case _userProfileEmailAddrVerified of
      Nothing -> do
        let replaceOlder users =
              [ if User._userCredsName (User._userProfileCreds u) == username
                  then u { User._userProfileEmailAddrVerified = Just "TODO" }
                  else u
              | u <- users
              ]
        Core.modifyUsers db replaceOlder
        pure $ Right ()
      Just _ -> pure . Left $ User.EmailAddrAlreadyVerified
    Nothing -> pure . Left $ User.UserNotFound $ User.unUserName username

selectEntityBySlug :: Core.StmDb -> Text -> STM (Maybe Legal.Entity)
selectEntityBySlug db name = do
  let tvar = Data._dbLegalEntities db
  records <- STM.readTVar tvar
  pure $ find ((== name) . Legal._entitySlug) records

selectEntityBySlugResolved
  :: Core.StmDb -> Text -> STM (Maybe (Legal.Entity, [Legal.ActingUser]))
selectEntityBySlugResolved db name = do
  let tvar = Data._dbLegalEntities db
  records <- STM.readTVar tvar
  case find ((== name) . Legal._entitySlug) records of
    Just entity -> do
      let select (Legal.ActingUserId uid role) = do
            muser <- Core.selectUserById db uid
            pure (muser, role)
      musers <- mapM select $ Legal._entityUsersAndRoles entity
      if any (isNothing . fst) musers
        then pure Nothing -- TODO Error
        else pure . Just . (entity, ) $ map
          (\(Just u, role) -> Legal.ActingUser u role)
          musers
    Nothing -> pure Nothing

-- | Select legal entities where the given user ID is "acting".
selectEntitiesWhereUserId
  :: Core.StmDb -> User.UserId -> STM [Legal.EntityAndRole]
selectEntitiesWhereUserId db uid = do
  let tvar = Data._dbLegalEntities db
  records <- STM.readTVar tvar
  pure $ mapMaybe getEntityAndRole records
 where
  getRole =
    lookup uid
      . map (\(Legal.ActingUserId uid' role) -> (uid', role))
      . Legal._entityUsersAndRoles
  getEntityAndRole e = Legal.EntityAndRole e <$> getRole e

readLegalEntities :: Core.StmDb -> STM [Legal.Entity]
readLegalEntities db = do
  let tvar = Data._dbLegalEntities db
  records <- STM.readTVar tvar
  pure records

withRuntimeAtomically f a = ask >>= \rt -> atomicallyM $ f rt a


--------------------------------------------------------------------------------
filterEmails :: Core.StmDb -> Email.Predicate -> STM [Email.Email]
filterEmails db predicate = do
  let tvar = Data._dbEmails db
  records <- STM.readTVar tvar
  pure $ filter (Email.applyPredicate predicate) records

filterEmails' :: Email.Predicate -> RunM [Email.Email]
filterEmails' predicate = do
  db <- asks _rDb
  atomicallyM $ filterEmails db predicate

setEmailDone :: Core.StmDb -> Email.Email -> STM (Either Email.Err ())
setEmailDone db Email.Email {..} = do
  mrecord <- Core.selectEmailById db _emailId
  case mrecord of
    Just Email.Email{} -> do
      let replaceOlder records =
            [ if Email._emailId e == _emailId
                then e { Email._emailState = Email.EmailDone }
                else e
            | e <- records
            ]
      Core.modifyEmails db replaceOlder
      pure $ Right ()
    Nothing -> pure . Left $ Email.Err "Email not found" -- TODO

selectEmailById :: Email.EmailId -> RunM (Maybe Email.Email)
selectEmailById eid = do
  db <- asks _rDb
  atomicallyM $ Core.selectEmailById db eid

--------------------------------------------------------------------------------
readForm
  :: forall a
   . (Core.StmDb -> STM.TVar (Map (User.UserName, Text) a))
  -> Core.StmDb
  -> (User.UserProfile, Text)
  -> STM (Either () a)
readForm getTVar db (profile, key) = do
  m <- STM.readTVar $ getTVar db
  let mform = M.lookup (username, key) m
  pure $ maybe (Left ()) Right mform
  where username = User._userCredsName $ User._userProfileCreds profile

readForms
  :: forall a
   . (Core.StmDb -> STM.TVar (Map (User.UserName, Text) a))
  -> Core.StmDb
  -> User.UserProfile
  -> STM (Either () [(Text, a)])
readForms getTVar db profile = do
  m <- STM.readTVar $ getTVar db
  let mform = M.filterWithKey (\(username', _) _ -> username == username') m
  pure $ Right $ map (\((_, key), a) -> (key, a)) $ M.toList mform
  where username = User._userCredsName $ User._userProfileCreds profile

deleteForm
  :: forall a
   . (Core.StmDb -> STM.TVar (Map (User.UserName, Text) a))
  -> Core.StmDb
  -> (User.UserProfile, Text)
  -> STM ()
deleteForm getTVar db (profile, key) =
  let tvar = getTVar db
      f    = M.delete (username, key)
  in  STM.modifyTVar tvar f
  where username = User._userCredsName $ User._userProfileCreds profile


--------------------------------------------------------------------------------
runWithRuntime runtime = do
  putStrLn @Text "Creating curiosity.sock..."
  sock <- socket AF_UNIX Stream 0
  bind sock $ SockAddrUnix "curiosity.sock"
  listen sock maxListenQueue

  putStrLn @Text "Listening on curiosity.sock..."
  server runtime sock -- TODO bracket (or catch) and close
  close sock

server runtime sock = do
  (conn, _) <- accept sock -- TODO bracket (or catch) and close too
  void $ forkFinally
    (handler runtime conn)
    (const $ putStrLn @Text "Closing connection." >> close conn)
  server runtime sock

handler runtime conn = do
  putStrLn @Text "New connection..."
  sendAll conn "Curiosity UNIX-domain socket server.\n"
  repl runtime conn

repl runtime conn = do
  msg <- recv conn 1024
  let command = map B.unpack $ B.words msg -- TODO decodeUtf8
  case command of
    _ | B.null msg -> return () -- Connection lost.
    ["quit"]       -> return ()
    []             -> repl runtime conn
    _              -> do
      let result = A.execParserPure A.defaultPrefs Command.parserInfo command
      case result of
        A.Success command -> do
          case command of
            _ -> do
              (_, output) <- handleCommand runtime "TODO" command
              mapM_ (\x -> sendAll conn (T.encodeUtf8 x <> "\n")) output
        A.Failure err -> case err of
          A.ParserFailure execFailure -> do
            let (msg, _, _) = execFailure "cty"
            sendAll conn (B.pack $ show msg <> "\n")
        A.CompletionInvoked _ -> print @IO @Text "Shouldn't happen"

      repl runtime conn
