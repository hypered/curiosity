{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
module Prototype.Runtime
  ( Conf(..)
  , IOErr(..)
  , confRepl
  , confServer
  , confLogging
  , confCookie
  , confMkJwtSettings
  , confDbFile
  , ServerConf(..)
  , Runtime(..)
  , rConf
  , rDb
  , rLoggers
  , rJwtSettings
  , ExeAppM(..)
  , boot
  , powerdown
  , readDb
  , readDbSafe
  , saveDb
  , saveDbAs
  , runExeAppMSafe
  -- * Servant compat
  , exampleAppMHandlerNatTrans
  ) where

import qualified Commence.InteractiveState.Repl
                                               as Repl
import qualified Commence.Multilogging         as ML
import qualified Commence.Runtime.Errors       as Errs
import qualified Commence.Runtime.Storage      as S
import           Commence.Types.Secret          ( (=:=) )
import qualified Control.Concurrent.STM        as STM
import           Control.Lens                  as Lens
import "exceptions" Control.Monad.Catch         ( MonadCatch
                                                , MonadMask
                                                , MonadThrow
                                                )
import qualified Crypto.JOSE.JWK               as JWK
import qualified Data.ByteString.Lazy          as BS
import qualified Data.List                     as L
import qualified Data.Text                     as T
import qualified Network.HTTP.Types            as HTTP
import qualified Prototype.Data                as Data
import qualified Prototype.Data.Todo           as Todo
import qualified Prototype.Data.User           as User
import qualified Servant
import qualified Servant.Auth.Server           as Srv
import           System.Directory               ( doesFileExist )

data ServerConf = ServerConf
  { _serverPort      :: Int
  , _serverStaticDir :: FilePath
  , _serverDataDir   :: FilePath
  }
  deriving Show

-- | Application config.
data Conf = Conf
  { _confRepl          :: Repl.ReplConf -- ^ Config. for the REPL.
  , _confServer        :: ServerConf -- ^ Config. for the HTTP server.
  , _confLogging       :: ML.LoggingConf -- ^ Logging configuration.
  , _confCookie        :: Srv.CookieSettings -- ^ Settings for setting cookies as a server (for authentication etc.).
  , _confMkJwtSettings :: JWK.JWK -> Srv.JWTSettings -- ^ JWK settings to use, depending on the key employed.
  , _confDbFile        :: Maybe FilePath
  -- ^ An optional filepath to write the DB to, or read it from.
  -- If the file is absent, it will be created on server exit, with the latest DB state written to it.
  }

makeLenses ''Conf

-- | The runtime, a central product type that should contain all our runtime supporting values. 
data Runtime = Runtime
  { _rConf        :: Conf -- ^ The application configuration.
  , _rDb          :: Data.StmDb Runtime -- ^ The Storage. 
  , _rLoggers     :: ML.AppNameLoggers -- ^ Multiple loggers to log over. 
  , _rJwtSettings :: Srv.JWTSettings -- ^ JWT settings to use.
  }

makeLenses ''Runtime

instance Data.RuntimeHasStmDb Runtime where
  stmDbFromRuntime = _rDb

newtype ExeAppM a = ExeAppM { runExeAppM :: ReaderT Runtime (ExceptT Errs.RuntimeErr IO) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadReader Runtime
           , MonadError Errs.RuntimeErr
           , MonadThrow
           , MonadCatch
           , MonadMask
           )

-- | Run the `ExeAppM` computation catching all possible exceptions. 
runExeAppMSafe
  :: forall a m
   . MonadIO m
  => Runtime
  -> ExeAppM a
  -> m (Either Errs.RuntimeErr a)
runExeAppMSafe rt (ExeAppM op') =
  liftIO
    . fmap (join . first Errs.RuntimeException)
    . try @SomeException
    . runExceptT
    $ runReaderT op' rt

-- | Definition of all operations for the UserProfiles (selects and updates)
instance S.DBStorage ExeAppM User.UserProfile where
  dbUpdate = \case

    User.UserCreate newProfile ->
      ML.localEnv (<> "Storage" <> "UserProfile" <> "UserCreate") $ do
        ML.info
          $  "Creating new user: "
          <> (show . User._userCredsName $ User._userProfileCreds newProfile)
          <> "..."
        onUserIdExists newProfileId createNew existsErr
     where
      newProfileId = S.dbId newProfile
      createNew    = onUserNameExists
        (newProfile ^. User.userProfileCreds . User.userCredsName)
        (do
          result <- withUserStorage
            $ modifyUserProfiles newProfileId (newProfile :)
          ML.info "User created."
          pure result
        )
        existsErr
      existsErr err = do
        ML.info "User already exists."
        Errs.throwError' . User.UserExists $ show err

    User.UserCreateGeneratingUserId username password email -> do
      -- generate a new and random user-id
      newId <- User.genRandomUserId 10
      let newProfile = User.UserProfile newId
                                        (User.Credentials username password)
                                        "TODO"
                                        email
      S.dbUpdate $ User.UserCreate newProfile

    User.UserDelete id -> onUserIdExists id (userNotFound $ show id) deleteUser
     where
      deleteUser _ =
        withUserStorage $ modifyUserProfiles id (filter $ (/= id) . S.dbId)

    User.UserPasswordUpdate id newPass -> onUserIdExists
      id
      (userNotFound $ show id)
      updateUser
     where
      updateUser _ = withUserStorage $ modifyUserProfiles id replaceOlder
      setPassword =
        set (User.userProfileCreds . User.userCredsPassword) newPass
      replaceOlder users =
        [ if S.dbId u == id then setPassword u else u | u <- users ]

   where
    modifyUserProfiles id f userProfiles =
      liftIO $ STM.atomically (STM.modifyTVar userProfiles f) $> [id]

  dbSelect = \case

    User.UserLoginWithUserName userName (User.Password passInput) ->
      -- Try to look up an unambigous user-id using the human friendly name, and then execute UserLogin.
      S.dbSelect (User.SelectUserByUserName userName) >>= \case
        [u] | passwordsMatch -> pure [u]
         where
          passwordsMatch = storedPass =:= passInput
          User.Password storedPass =
            u ^. User.userProfileCreds . User.userCredsPassword
        _ -> userNotFound $ "No user with userName = " <> userName ^. coerced

    User.SelectUserById id ->
      withUserStorage $ liftIO . STM.readTVarIO >=> pure . filter
        ((== id) . S.dbId)

    User.SelectUserByUserName userName ->
      withUserStorage $ liftIO . STM.readTVarIO >=> pure . filter
        ((== userName) . User._userCredsName . User._userProfileCreds)

-- | Support for logging for the example application 
instance ML.MonadAppNameLogMulti ExeAppM where
  askLoggers = asks _rLoggers
  localLoggers modLogger =
    local (over rLoggers . over ML.appNameLoggers $ fmap modLogger)

onUserIdExists id onNone onExisting =
  S.dbSelect (User.SelectUserById id) <&> headMay >>= maybe onNone onExisting
onUserNameExists userName onNone onExisting =
  S.dbSelect (User.SelectUserByUserName userName)
    <&> headMay
    >>= maybe onNone onExisting
userNotFound =
  Errs.throwError' . User.UserNotFound . mappend "User not found: "
withUserStorage f = asks (Data._dbUserProfiles . _rDb) >>= f

instance S.DBStorage ExeAppM Todo.TodoList where

  dbUpdate = \case
    Todo.AddItem id item -> onTodoListExists id
                                             (todoListNotFound id)
                                             modifyList

     where
      modifyList list' =
        let newList =
              list' { Todo._todoListItems = item : Todo._todoListItems list' }
        in  replaceTodoList newList $> [id]

    Todo.DeleteItem id itemName -> onTodoListExists id
                                                    (todoListNotFound id)
                                                    modifyList
     where
      modifyList list' =
        let newList = list'
              { Todo._todoListItems = filter
                                        ((/= itemName) . Todo._todoItemName)
                                        (Todo._todoListItems list')
              }
        in  replaceTodoList newList $> [id]

    Todo.MarkItem id itemName itemState -> onTodoListExists
      id
      (todoListNotFound id)
      modifyList
     where
      modifyList list' =
        let
          newList = list'
            { Todo._todoListItems = fmap replaceItem (Todo._todoListItems list')
            }
          replaceItem item@Todo.TodoListItem {..}
            | _todoItemName == itemName = item { Todo._todoItemState = itemState
                                               }
            | otherwise = item
        in
          replaceTodoList newList $> [id]

    Todo.DeleteList id -> withTodoStorage $ \todoStm -> do
      todos <- liftIO . STM.readTVarIO $ todoStm
      let existing = find ((== id) . S.dbId) todos
      if isNothing existing
        then todoListNotFound id
        else
          liftIO
              ( STM.atomically
              $ STM.modifyTVar' todoStm (filter $ (/= id) . S.dbId)
              )
            $> [id]
    Todo.AddUsersToList id users -> onTodoListExists id
                                                     (todoListNotFound id)
                                                     modifyList
     where
      modifyList list' =
        let newList =
              list' & Todo.todoListUsers %~ L.nub . mappend (toList users)
        in  replaceTodoList newList $> [id]
    Todo.RemoveUsersFromList id users -> onTodoListExists
      id
      (todoListNotFound id)
      modifyList
     where
      modifyList list' =
        let newList = list' & Todo.todoListUsers %~ (L.\\ (toList users))
        in  replaceTodoList newList $> [id]

    Todo.CreateList newList -> withTodoStorage $ \todoStm -> do
      todos <- liftIO . STM.readTVarIO $ todoStm
      let existing = find ((== newId) . S.dbId) todos
          newId    = S.dbId newList
      if isJust existing
        then existsErr newId
        else
          liftIO (STM.atomically $ STM.modifyTVar' todoStm (newList :))
            $> [newId]
      where existsErr = Errs.throwError' . Todo.TodoListExists

  dbSelect = \case
    Todo.SelectTodoListById id -> filtStoredTodos $ (== id) . S.dbId
    Todo.SelectTodoListsByPendingItems ->
      filtStoredTodos
        $ any ((== Todo.TodoListItemPending) . Todo._todoItemState)
        . Todo._todoListItems
    Todo.SelectTodoListsByUser userId ->
      filtStoredTodos $ elem userId . Todo._todoListUsers

withTodoStorage f = asks (Data._dbTodos . _rDb) >>= f
filtStoredTodos f =
  withTodoStorage $ liftIO . STM.readTVarIO >=> pure . filter f

onTodoListExists id onNone onExisting =
  S.dbSelect (Todo.SelectTodoListById id)
    <&> headMay
    >>= maybe onNone onExisting

todoListNotFound = Errs.throwError' . Todo.TodoListNotFound . show

replaceTodoList newList =
  let replaceList list' | S.dbId list' == S.dbId newList = newList
                        | otherwise                      = list'
  in  withTodoStorage $ \stmLists ->
        liftIO . STM.atomically $ STM.modifyTVar' stmLists $ fmap replaceList

-- | Boot up a runtime.
boot
  :: MonadIO m
  => Conf -- ^ configuration to boot with.
  -> JWK.JWK -- ^ A Key for JSON Web Tokens (used to encrypt auth. cookies). 
  -> m (Either Errs.RuntimeErr Runtime)
boot _rConf jwk = do

  _rLoggers <- ML.makeDefaultLoggersWithConf $ _rConf ^. confLogging

  eDb       <- instantiateDb _rConf
  pure $ case eDb of
    Left err -> Left err
    Right _rDb ->
      Right Runtime { _rJwtSettings = (_rConf ^. confMkJwtSettings) jwk, .. }

-- | Power down the application: attempting to save the DB state in given file, if possible, and reporting errors otherwise.
powerdown :: MonadIO m => Runtime -> m (Maybe Errs.RuntimeErr)
powerdown runtime@Runtime {..} = do
  mDbSaveErr <- saveDb runtime
  -- finally, close loggers.
  ML.flushAndCloseLoggers _rLoggers
  pure mDbSaveErr

saveDb :: MonadIO m => Runtime -> m (Maybe Errs.RuntimeErr)
saveDb runtime =
  maybe (pure Nothing) (saveDbAs runtime) $ _rConf runtime ^. confDbFile

saveDbAs :: MonadIO m => Runtime -> FilePath -> m (Maybe Errs.RuntimeErr)
saveDbAs runtime fpath = do
  haskDb <- Data.readFullStmDbInHask $ _rDb runtime
  let bs = Data.serialiseDb haskDb
  liftIO (try @SomeException (BS.writeFile fpath bs))
    <&> either (Just . Errs.RuntimeException) (const Nothing)

{- | Instantiate the db.

1. The state is either the empty db, or if a _confDbFile file is specified, is read from the file.

2. Whenever the application exits, the state is written to disk, if a _confDbFile is specified. 
-}
instantiateDb
  :: forall m
   . MonadIO m
  => Conf
  -> m (Either Errs.RuntimeErr (Data.StmDb Runtime))
instantiateDb Conf {..} = readDbSafe _confDbFile

readDb
  :: forall m
   . MonadIO m
  => Maybe FilePath
  -> m (Either Errs.RuntimeErr (Data.StmDb Runtime))
readDb mpath = case mpath of
  Just fpath -> do
    -- We may want to read the file only when the file exists.
    exists <- liftIO $ doesFileExist fpath
    if (exists) then fromFile fpath else useEmpty
  Nothing -> useEmpty
 where
  fromFile fpath = do
    fdata <- liftIO (BS.readFile fpath)
    -- We may want to deserialise the data only when the data is non-empty.
    if BS.null fdata
      then useEmpty
      else
        Data.deserialiseDb fdata & either (pure . Left . Errs.knownErr) useState
  useEmpty = Right <$> Data.instantiateEmptyStmDb
  useState = fmap Right . Data.instantiateStmDb

-- | A safer version of readDb: this fails if the file doesn't exist or is
-- empty. This helps in catching early mistake, e.g. from user specifying the
-- wrong file name on the command-line.
readDbSafe
  :: forall m
   . MonadIO m
  => Maybe FilePath
  -> m (Either Errs.RuntimeErr (Data.StmDb Runtime))
readDbSafe mpath = case mpath of
  Just fpath -> do
    -- We may want to read the file only when the file exists.
    exists <- liftIO $ doesFileExist fpath
    if exists
      then fromFile fpath
      else pure . Left . Errs.knownErr $ FileDoesntExistErr fpath
  Nothing -> useEmpty
 where
  fromFile fpath = do
    fdata <- liftIO (BS.readFile fpath)
    Data.deserialiseDb fdata & either (pure . Left . Errs.knownErr) useState
  useEmpty = Right <$> Data.instantiateEmptyStmDb
  useState = fmap Right . Data.instantiateStmDb

newtype IOErr = FileDoesntExistErr FilePath
  deriving Show

instance Errs.IsRuntimeErr IOErr where
  errCode FileDoesntExistErr{} = "ERR.FILE_DOENST_EXIST"
  httpStatus FileDoesntExistErr{} = HTTP.notFound404
  userMessage = Just . \case
    FileDoesntExistErr fpath -> T.unwords ["File doesn't exist:", T.pack fpath]

-- | Natural transformation from some `ExeAppM` in any given mode, to a servant Handler. 
exampleAppMHandlerNatTrans
  :: forall a . Runtime -> ExeAppM a -> Servant.Handler a
exampleAppMHandlerNatTrans rt appM =
  let
    -- We peel off the ExeAppM + ReaderT layers, exposing our ExceptT RuntimeErr IO a
    -- This is very similar to Servant's Handler: https://hackage.haskell.org/package/servant-server-0.17/docs/Servant-Server-Internal-Handler.html#t:Handler
      unwrapReaderT          = (`runReaderT` rt) . runExeAppM $ appM
      -- Map our errors to `ServantError` 
      runtimeErrToServantErr = withExceptT Errs.asServantError
  in 
    -- re-wrap as servant `Handler`
      Servant.Handler $ runtimeErrToServantErr unwrapReaderT

