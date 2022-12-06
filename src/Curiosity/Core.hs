{-# LANGUAGE TemplateHaskellQuotes #-}
-- | STM operations around `Curiosity.Data`.

-- brittany-disable-next-binding
module Curiosity.Core
  ( StmDb
  -- * Whole database manipulation
  , instantiateEmptyStmDb
  , instantiateStmDb
  , reset
  , readFullStmDbInHask'
  , signup
  , inviteUser
  , createUserFull
  , modifyUsers
  , selectUserById
  , selectUserByUsername
  , selectUserByInviteToken
  , modifyQuotations
  , selectQuotationById
  -- * ID generation
  , generateUserId
  , generateBusinessId
  , generateLegalId
  , generateQuotationId
  , generateOrderId
  , generateRemittanceAdvId
  , generateEmploymentId
  , generateInvoiceId
  , generateEmailId
  -- * Operations on business units
  , createBusiness
  , updateBusiness
  , selectUnitBySlug
  , linkBusinessUnitToUser
  -- * Operations on emails
  , createEmail
  , modifyEmails
  , selectEmailById
  -- * Pseudo-random number generation
  , genRandomText
  , readStdGen
  , writeStdGen
  -- * Simulated time
  , readTime
  , writeTime
  , stepTime
  , readSteppingMode
  -- * User rights
  , canPerform
  ) where

import qualified Control.Concurrent.STM        as STM
import           Curiosity.Data
import qualified Curiosity.Data.Business       as Business
import qualified Curiosity.Data.Counter        as C
import qualified Curiosity.Data.Email          as Email
import qualified Curiosity.Data.Employment     as Employment
import qualified Curiosity.Data.Invoice        as Invoice
import qualified Curiosity.Data.Legal          as Legal
import qualified Curiosity.Data.Order          as Order
import qualified Curiosity.Data.Quotation      as Quotation
import qualified Curiosity.Data.RemittanceAdv  as RemittanceAdv
import qualified Curiosity.Data.User           as User
import           Data.List                      ( nub )
import qualified Data.List                     as L
import qualified Data.Text                     as T
import           Foreign.C.Types                ( CTime(..) )
import qualified Language.Haskell.TH.Syntax    as Syntax
import           System.PosixCompat.Types       ( EpochTime )
import qualified System.Random                 as Rand
import qualified System.Random.Internal        as Rand
import qualified System.Random.SplitMix        as SM


--------------------------------------------------------------------------------
-- | Stm database type, used for live example applications, values reside in @STM@
type StmDb = Db STM.TVar

-- | Generate a new empty database.
instantiateEmptyStmDb :: STM StmDb
instantiateEmptyStmDb = instantiateStmDb emptyHask

-- brittany-disable-next-binding
-- | Generate a new database from a given pure value.
instantiateStmDb :: HaskDb -> STM StmDb
instantiateStmDb Db
  { _dbNextBusinessId = C.CounterValue (Identity seedNextBusinessId)
  , _dbBusinessUnits = Identity seedBusinessUnits
  , _dbNextLegalId = C.CounterValue (Identity seedNextLegalId)
  , _dbLegalEntities = Identity seedLegalEntities
  , _dbNextUserId = C.CounterValue (Identity seedNextUserId)
  , _dbUserProfiles = Identity seedProfiles
  , _dbNextQuotationId = C.CounterValue (Identity seedNextQuotationId)
  , _dbQuotations = Identity seedQuotations
  , _dbNextOrderId = C.CounterValue (Identity seedNextOrderId)
  , _dbOrders = Identity seedOrders
  , _dbNextInvoiceId = C.CounterValue (Identity seedNextInvoiceId)
  , _dbInvoices = Identity seedInvoices
  , _dbNextRemittanceAdvId = C.CounterValue (Identity seedNextRemittanceAdvId)
  , _dbRemittanceAdvs = Identity seedRemittanceAdvs
  , _dbNextEmploymentId = C.CounterValue (Identity seedNextEmploymentId)
  , _dbEmployments = Identity seedEmployments
  , _dbRandomGenState = Identity seedRandomGenState
  , _dbEpochTime = Identity seedEpochTime
  , _dbSteppingMode = Identity seedSteppingMode
  , _dbFormCreateQuotationAll = Identity seedFormCreateQuotationAll
  , _dbFormCreateContractAll = Identity seedFormCreateContractAll
  , _dbFormCreateSimpleContractAll = Identity seedFormCreateSimpleContractAll
  , _dbNextEmailId = C.CounterValue (Identity seedNextEmailId)
  , _dbEmails = Identity seedEmails
  }
  = do
    _dbNextBusinessId              <- C.newCounter seedNextBusinessId
    _dbBusinessUnits               <- STM.newTVar seedBusinessUnits
    _dbNextLegalId                 <- C.newCounter seedNextLegalId
    _dbLegalEntities               <- STM.newTVar seedLegalEntities
    _dbNextUserId                  <- C.newCounter seedNextUserId
    _dbUserProfiles                <- STM.newTVar seedProfiles
    _dbNextQuotationId             <- C.newCounter seedNextQuotationId
    _dbQuotations                  <- STM.newTVar seedQuotations
    _dbNextOrderId                 <- C.newCounter seedNextOrderId
    _dbOrders                      <- STM.newTVar seedOrders
    _dbNextInvoiceId               <- C.newCounter seedNextInvoiceId
    _dbInvoices                    <- STM.newTVar seedInvoices
    _dbNextRemittanceAdvId         <- C.newCounter seedNextRemittanceAdvId
    _dbRemittanceAdvs              <- STM.newTVar seedRemittanceAdvs
    _dbNextEmploymentId            <- C.newCounter seedNextEmploymentId
    _dbEmployments                 <- STM.newTVar seedEmployments

    _dbRandomGenState              <- STM.newTVar seedRandomGenState
    _dbEpochTime                   <- STM.newTVar seedEpochTime
    _dbSteppingMode                <- STM.newTVar seedSteppingMode

    _dbFormCreateQuotationAll      <- STM.newTVar seedFormCreateQuotationAll
    _dbFormCreateContractAll       <- STM.newTVar seedFormCreateContractAll
    _dbFormCreateSimpleContractAll <- STM.newTVar
      seedFormCreateSimpleContractAll

    _dbNextEmailId                 <- C.newCounter seedNextEmailId
    _dbEmails                      <- STM.newTVar seedEmails
    pure Db { .. }

-- brittany-disable-next-binding
-- | Reset the database to the empty state.
reset :: StmDb -> EpochTime -> STM ()
reset stmDb now = do
  C.writeCounter (_dbNextBusinessId stmDb) seedNextBusinessId
  STM.writeTVar (_dbBusinessUnits stmDb) seedBusinessUnits
  C.writeCounter (_dbNextLegalId stmDb) seedNextLegalId
  STM.writeTVar (_dbLegalEntities stmDb) seedLegalEntities
  C.writeCounter (_dbNextUserId stmDb) seedNextUserId
  STM.writeTVar (_dbUserProfiles stmDb) seedProfiles
  C.writeCounter (_dbNextQuotationId stmDb) seedNextQuotationId
  STM.writeTVar (_dbQuotations stmDb) seedQuotations
  C.writeCounter (_dbNextOrderId stmDb) seedNextOrderId
  STM.writeTVar (_dbOrders stmDb) seedOrders
  C.writeCounter (_dbNextInvoiceId stmDb) seedNextInvoiceId
  STM.writeTVar (_dbInvoices stmDb) seedInvoices
  C.writeCounter (_dbNextRemittanceAdvId stmDb) seedNextRemittanceAdvId
  STM.writeTVar (_dbRemittanceAdvs stmDb) seedRemittanceAdvs
  C.writeCounter (_dbNextEmploymentId stmDb) seedNextEmploymentId
  STM.writeTVar (_dbEmployments stmDb) seedEmployments

  STM.writeTVar (_dbRandomGenState stmDb) seedRandomGenState
  STM.writeTVar (_dbEpochTime stmDb) now
  STM.writeTVar (_dbSteppingMode stmDb) seedSteppingMode

  STM.writeTVar (_dbFormCreateQuotationAll stmDb) seedFormCreateQuotationAll
  STM.writeTVar (_dbFormCreateContractAll stmDb) seedFormCreateContractAll
  STM.writeTVar (_dbFormCreateSimpleContractAll stmDb)
                seedFormCreateSimpleContractAll

  C.writeCounter (_dbNextEmailId stmDb) seedNextEmailId
  STM.writeTVar (_dbEmails stmDb) seedEmails
 where
  Db
    { _dbNextBusinessId = C.CounterValue (Identity seedNextBusinessId)
    , _dbBusinessUnits = Identity seedBusinessUnits
    , _dbNextLegalId = C.CounterValue (Identity seedNextLegalId)
    , _dbLegalEntities = Identity seedLegalEntities
    , _dbNextUserId = C.CounterValue (Identity seedNextUserId)
    , _dbUserProfiles = Identity seedProfiles
    , _dbNextQuotationId = C.CounterValue (Identity seedNextQuotationId)
    , _dbQuotations = Identity seedQuotations
    , _dbNextOrderId = C.CounterValue (Identity seedNextOrderId)
    , _dbOrders = Identity seedOrders
    , _dbNextInvoiceId = C.CounterValue (Identity seedNextInvoiceId)
    , _dbInvoices = Identity seedInvoices
    , _dbNextRemittanceAdvId = C.CounterValue (Identity seedNextRemittanceAdvId)
    , _dbRemittanceAdvs = Identity seedRemittanceAdvs
    , _dbNextEmploymentId = C.CounterValue (Identity seedNextEmploymentId)
    , _dbEmployments = Identity seedEmployments
    , _dbRandomGenState = Identity seedRandomGenState
    , _dbSteppingMode = Identity seedSteppingMode
    , _dbFormCreateQuotationAll = Identity seedFormCreateQuotationAll
    , _dbFormCreateContractAll = Identity seedFormCreateContractAll
    , _dbFormCreateSimpleContractAll = Identity seedFormCreateSimpleContractAll
    , _dbNextEmailId = C.CounterValue (Identity seedNextEmailId)
    , _dbEmails = Identity seedEmails
    }
    = emptyHask

-- | Reads all values of the `Db` product type from `STM.STM` to @Hask@.
readFullStmDbInHask' :: StmDb -> STM HaskDb
readFullStmDbInHask' stmDb = do
  _dbNextBusinessId      <- pure <$> C.readCounter (_dbNextBusinessId stmDb)
  _dbBusinessUnits       <- pure <$> STM.readTVar (_dbBusinessUnits stmDb)
  _dbNextLegalId         <- pure <$> C.readCounter (_dbNextLegalId stmDb)
  _dbLegalEntities       <- pure <$> STM.readTVar (_dbLegalEntities stmDb)
  _dbNextUserId          <- pure <$> C.readCounter (_dbNextUserId stmDb)
  _dbUserProfiles        <- pure <$> STM.readTVar (_dbUserProfiles stmDb)
  _dbNextQuotationId     <- pure <$> C.readCounter (_dbNextQuotationId stmDb)
  _dbQuotations          <- pure <$> STM.readTVar (_dbQuotations stmDb)
  _dbNextOrderId         <- pure <$> C.readCounter (_dbNextOrderId stmDb)
  _dbOrders              <- pure <$> STM.readTVar (_dbOrders stmDb)
  _dbNextInvoiceId       <- pure <$> C.readCounter (_dbNextInvoiceId stmDb)
  _dbInvoices            <- pure <$> STM.readTVar (_dbInvoices stmDb)
  _dbNextRemittanceAdvId <- pure
    <$> C.readCounter (_dbNextRemittanceAdvId stmDb)
  _dbRemittanceAdvs         <- pure <$> STM.readTVar (_dbRemittanceAdvs stmDb)
  _dbNextEmploymentId <- pure <$> C.readCounter (_dbNextEmploymentId stmDb)
  _dbEmployments            <- pure <$> STM.readTVar (_dbEmployments stmDb)

  _dbRandomGenState         <- pure <$> STM.readTVar (_dbRandomGenState stmDb)
  _dbEpochTime              <- pure <$> STM.readTVar (_dbEpochTime stmDb)
  _dbSteppingMode           <- pure <$> STM.readTVar (_dbSteppingMode stmDb)

  _dbFormCreateQuotationAll <- pure
    <$> STM.readTVar (_dbFormCreateQuotationAll stmDb)
  _dbFormCreateContractAll <- pure
    <$> STM.readTVar (_dbFormCreateContractAll stmDb)
  _dbFormCreateSimpleContractAll <- pure
    <$> STM.readTVar (_dbFormCreateSimpleContractAll stmDb)

  _dbNextEmailId <- pure <$> C.readCounter (_dbNextEmailId stmDb)
  _dbEmails      <- pure <$> STM.readTVar (_dbEmails stmDb)
  pure Db { .. }


--------------------------------------------------------------------------------
-- | Generate a fresh user ID.
generateUserId :: StmDb -> STM User.UserId
generateUserId Db {..} =
  User.UserId <$> C.bumpCounterPrefix User.userIdPrefix _dbNextUserId

-- | Generate a fresh busines unit ID.
generateBusinessId :: StmDb -> STM Business.UnitId
generateBusinessId Db {..} =
  Business.UnitId
    <$> C.bumpCounterPrefix Business.unitIdPrefix _dbNextBusinessId

-- | Generate a fresh legal entity ID.
generateLegalId :: StmDb -> STM Legal.EntityId
generateLegalId Db {..} =
  Legal.EntityId <$> C.bumpCounterPrefix Legal.entityIdPrefix _dbNextLegalId

-- | Generate a fresh quotation ID.
generateQuotationId :: StmDb -> STM Quotation.QuotationId
generateQuotationId Db {..} =
  Quotation.QuotationId
    <$> C.bumpCounterPrefix Quotation.quotationIdPrefix _dbNextQuotationId

-- | Generate a fresh order ID.
generateOrderId :: StmDb -> STM Order.OrderId
generateOrderId Db {..} =
  Order.OrderId <$> C.bumpCounterPrefix Order.orderIdPrefix _dbNextOrderId

-- | Generate a fresh remittance advice ID.
generateRemittanceAdvId :: StmDb -> STM RemittanceAdv.RemittanceAdvId
generateRemittanceAdvId Db {..} =
  RemittanceAdv.RemittanceAdvId
    <$> C.bumpCounterPrefix RemittanceAdv.remittanceAdvIdPrefix
                            _dbNextRemittanceAdvId

-- | Generate a fresh employment contract ID.
generateEmploymentId :: StmDb -> STM Employment.ContractId
generateEmploymentId Db {..} =
  Employment.ContractId
    <$> C.bumpCounterPrefix Employment.contractIdPrefix _dbNextEmploymentId

-- | Generate a fresh invoice ID.
generateInvoiceId :: StmDb -> STM Invoice.InvoiceId
generateInvoiceId Db {..} =
  Invoice.InvoiceId
    <$> C.bumpCounterPrefix Invoice.invoiceIdPrefix _dbNextInvoiceId

-- | Generate a fresh email ID.
generateEmailId :: StmDb -> STM Email.EmailId
generateEmailId Db {..} =
  Email.EmailId <$> C.bumpCounterPrefix Email.emailIdPrefix _dbNextEmailId


--------------------------------------------------------------------------------
signup
  :: StmDb -> User.Signup -> STM (Either User.Err (User.UserId, Email.EmailId))
signup db input@User.Signup {..} = do
  STM.catchSTM (Right <$> transaction) (pure . Left)
 where
  transaction = do
    now        <- readTime db
    newId      <- generateUserId db
    newProfile <-
      pure (User.validateSignup now newId input)
        >>= either (STM.throwSTM . User.ValidationErrs) pure
    emailId <-
      createEmail db Email.SignupConfirmationEmail Email.systemEmailAddr email
        >>= either STM.throwSTM pure
    -- We fail the transaction if createUserFull returns an error,
    -- so that we don't increment _dbNextUserId.
    userId <- createUserFull db newProfile >>= either STM.throwSTM pure
    pure (userId, emailId)

inviteUser
  :: StmDb -> User.Invite -> STM (Either User.Err (User.UserId, Email.EmailId))
inviteUser db User.Invite {..} = do
  STM.catchSTM (Right <$> transaction) (pure . Left)
 where
  transaction = do
    now   <- readTime db
    newId <- generateUserId db
    let email      = _inviteEmail
        token      = "TODO"
        newProfile = User.UserProfile
          newId
          (User.InviteToken token)
          Nothing
          Nothing
          email
          Nothing
          False
          (User.UserCompletion1 Nothing Nothing Nothing)
          (User.UserCompletion2 Nothing Nothing)
          now
          Nothing
          -- The very first user has plenty of rights:
          (if newId == User.firstUserId then User.firstUserRights else [])
          -- TODO Define some mechanism to get the initial authorizations
          [User.AuthorizedAsEmployee]
          Nothing
    emailId <-
      createEmail db (Email.InviteEmail token) Email.systemEmailAddr email
        >>= either STM.throwSTM pure
    -- We fail the transaction if createUserFull returns an error,
    -- so that we don't increment _dbNextUserId.
    userId <- createUserFull db newProfile >>= either STM.throwSTM pure
    pure (userId, emailId)

createUserFull
  :: StmDb -> User.UserProfile -> STM (Either User.Err User.UserId)
createUserFull db newProfile = case User._userProfileCreds newProfile of
  User.Credentials {..} -> do
    let username     = _userCredsName
        newProfileId = User._userProfileId newProfile
        createNew    = do
          mprofile <- selectUserByUsername db username
          case mprofile of
            Just _  -> existsErr
            Nothing -> do
              modifyUsers db (++ [newProfile])
              pure $ Right newProfileId
        existsErr = pure . Left $ User.UserExists
    if username `elem` User.usernameBlocklist
      then pure . Left $ User.UsernameBlocked
      else do
        mprofile <- selectUserById db newProfileId
        case mprofile of
          Just _  -> existsErr
          Nothing -> createNew
  User.InviteToken _ -> createUserFull' db newProfile

-- | Similar to `createUserFull` but doesn't check usernames.
createUserFull'
  :: StmDb -> User.UserProfile -> STM (Either User.Err User.UserId)
createUserFull' db newProfile = do
  mprofile <- selectUserById db newProfileId
  case mprofile of
    Just _  -> existsErr
    Nothing -> createNew
 where
  newProfileId = User._userProfileId newProfile
  createNew    = do
    modifyUsers db (<> [newProfile])
    pure $ Right newProfileId
  existsErr = pure . Left $ User.UserExists

modifyUsers :: StmDb -> ([User.UserProfile] -> [User.UserProfile]) -> STM ()
modifyUsers db f = let tvar = _dbUserProfiles db in STM.modifyTVar tvar f

selectUserById :: StmDb -> User.UserId -> STM (Maybe User.UserProfile)
selectUserById db id = do
  let tvar = _dbUserProfiles db
  STM.readTVar tvar <&> find ((== id) . User._userProfileId)

selectUserByUsername :: StmDb -> User.UserName -> STM (Maybe User.UserProfile)
selectUserByUsername db username = do
  let tvar = _dbUserProfiles db
  records <- STM.readTVar tvar
  pure $ find ((== Just username) . User.getUsername . User._userProfileCreds)
              records

selectUserByInviteToken :: StmDb -> Text -> STM (Maybe User.UserProfile)
selectUserByInviteToken db token = do
  let tvar = _dbUserProfiles db
  records <- STM.readTVar tvar
  pure $ find ((== Just token) . User.getInviteToken . User._userProfileCreds)
              records


--------------------------------------------------------------------------------
modifyQuotations
  :: StmDb -> ([Quotation.Quotation] -> [Quotation.Quotation]) -> STM ()
modifyQuotations db f = let tvar = _dbQuotations db in STM.modifyTVar tvar f

selectQuotationById
  :: StmDb -> Quotation.QuotationId -> STM (Maybe Quotation.Quotation)
selectQuotationById db id = do
  let tvar = _dbQuotations db
  records <- STM.readTVar tvar
  pure $ find ((== id) . Quotation._quotationId) records


--------------------------------------------------------------------------------
createBusiness
  :: StmDb -> Business.Create -> STM (Either Business.Err Business.UnitId)
createBusiness db Business.Create {..} = do
  STM.catchSTM (Right <$> transaction) (pure . Left)
 where
  transaction = do
    newId <- generateBusinessId db
    let new =
          Business.Unit newId _createSlug _createName Nothing "TODO" [] [] []
    createBusinessFull db new >>= either STM.throwSTM pure

createBusinessFull
  :: StmDb -> Business.Unit -> STM (Either Business.Err Business.UnitId)
createBusinessFull db new = do
  modifyBusinessUnits db (<> [new])
  pure . Right $ Business._entityId new

updateBusiness :: StmDb -> Business.Update -> STM (Either Business.Err ())
updateBusiness db Business.Update {..} = do
  mentity <- selectUnitBySlug db _updateSlug
  case mentity of
    Just Business.Unit{} -> do
      let replaceOlder entities =
            [ if Business._entitySlug e == _updateSlug
                then e { Business._entityDescription = _updateDescription }
                else e
            | e <- entities
            ]
      modifyBusinessUnits db replaceOlder
      pure $ Right ()
    Nothing ->
      pure . Left . Business.Err $ "No such business unit: " <> _updateSlug

linkBusinessUnitToUser
  :: StmDb
  -> Text
  -> User.UserId
  -> Business.ActingRole
  -> STM (Either User.Err ())
linkBusinessUnitToUser db slug uid role = do
  mentity <- selectUnitBySlug db slug
  case mentity of
    Just Business.Unit {..} -> do
      let replaceOlder units =
            [ if Business._entitySlug e == slug
                then case role of
                  Business.Holder ->
                    e { Business._entityHolders = nub $ uid : _entityHolders }
                  _ -> e
                else e
            | e <- units
            ]
      modifyBusinessUnits db replaceOlder
      pure $ Right ()
    Nothing -> pure . Left $ User.UserNotFound slug -- TODO

modifyBusinessUnits :: StmDb -> ([Business.Unit] -> [Business.Unit]) -> STM ()
modifyBusinessUnits db f =
  let tvar = _dbBusinessUnits db in STM.modifyTVar tvar f

selectUnitBySlug :: StmDb -> Text -> STM (Maybe Business.Unit)
selectUnitBySlug db name = do
  let tvar = _dbBusinessUnits db
  records <- STM.readTVar tvar
  pure $ find ((== name) . Business._entitySlug) records


--------------------------------------------------------------------------------
-- | This enqueues an email (i.e. it is in the Todo state).
createEmail
  :: StmDb
  -> Email.EmailTemplate
  -> User.UserEmailAddr
  -> User.UserEmailAddr
  -> STM (Either Email.Err Email.EmailId)
createEmail db template senderAddr recipientAddr = do
  STM.catchSTM (Right <$> transaction) (pure . Left)
 where
  transaction = do
    newId <- generateEmailId db
    let new =
          Email.Email newId template senderAddr recipientAddr Email.EmailTodo
    createEmailFull db new >>= either STM.throwSTM pure

createEmailFull :: StmDb -> Email.Email -> STM (Either Email.Err Email.EmailId)
createEmailFull db new = do
  modifyEmails db (<> [new])
  pure . Right $ Email._emailId new

modifyEmails :: StmDb -> ([Email.Email] -> [Email.Email]) -> STM ()
modifyEmails db f = let tvar = _dbEmails db in STM.modifyTVar tvar f

selectEmailById :: StmDb -> Email.EmailId -> STM (Maybe Email.Email)
selectEmailById db id = do
  let tvar = _dbEmails db
  STM.readTVar tvar <&> find ((== id) . Email._emailId)

--------------------------------------------------------------------------------
canPerform :: Syntax.Name -> StmDb -> User.UserProfile -> STM Bool
canPerform action _ User.UserProfile {..}
  | action == 'User.SetUserEmailAddrAsVerified
  = pure $ User.CanVerifyEmailAddr `elem` _userProfileRights
  | otherwise
  = pure False


--------------------------------------------------------------------------------
readStdGen :: StmDb -> STM Rand.StdGen
readStdGen db = do
  (seed, gamma) <- STM.readTVar $ _dbRandomGenState db
  let g = Rand.StdGen $ SM.seedSMGen' (seed, gamma)
  pure g

writeStdGen :: StmDb -> Rand.StdGen -> STM ()
writeStdGen db g = do
  let (seed, gamma) = SM.unseedSMGen $ Rand.unStdGen g
  STM.writeTVar (_dbRandomGenState db) (seed, gamma)

genRandomText :: StmDb -> STM Text
genRandomText db = do
  g1 <- readStdGen db
  let ags = take 8 $ unfoldr
        (\g -> let (a, g') = Rand.uniformR ('A', 'Z') g in Just ((a, g'), g'))
        g1
      s  = T.pack $ fst <$> ags
      g2 = snd $ L.last ags
  writeStdGen db g2
  pure s


--------------------------------------------------------------------------------
readTime :: StmDb -> STM EpochTime
readTime db = STM.readTVar $ _dbEpochTime db

writeTime :: StmDb -> EpochTime -> STM ()
writeTime db = STM.writeTVar (_dbEpochTime db)

stepTime :: StmDb -> Bool -> STM EpochTime
stepTime db minute = do
  CTime t <- STM.readTVar $ _dbEpochTime db
  let d  = 60 - (t `mod` 60) -- seconds remaining before the next minute
      t' = CTime $ if minute then t + d else t + 1
  STM.writeTVar (_dbEpochTime db) t'
  pure t'

readSteppingMode :: StmDb -> STM SteppingMode
readSteppingMode db = STM.readTVar $ _dbSteppingMode db
