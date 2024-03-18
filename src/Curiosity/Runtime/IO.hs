module Curiosity.Runtime.IO
  ( -- * managing runtimes: boot, shutdown etc.
    bootConf
  , bootDbAndLogFile
  , instantiateDb
  , readDb
  , readDbSafe
  , powerdown
  , saveDb
  , saveDbAs
  , readFullStmDbInHask
  ) where

import Commence.Multilogging qualified as ML
import Commence.Runtime.Errors qualified as Errs
import Control.Concurrent.STM qualified as STM
import Control.Lens
import Curiosity.Core qualified as Core
import Curiosity.Parse qualified as Command
import Curiosity.Runtime.Error qualified as RErr
import Curiosity.Runtime.Type
import Curiosity.Types.Store qualified as Store
import Data.ByteString.Lazy qualified as BS
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as T
import System.Directory (doesFileExist)

--------------------------------------------------------------------------------

-- | Boot up a runtime.
bootConf
  :: MonadIO m
  => Command.Conf
  -- ^ configuration to bootConf with.
  -> Threads
  -> m (Either Errs.RuntimeErr Runtime)
bootConf _rConf _rThreads =
  liftIO
    ( try @SomeException
        . ML.makeDefaultLoggersWithConf
        $ _rConf
          ^. Command.confLogging
    )
    >>= \case
      Left loggerErrs ->
        putStrLn @Text
          ( "Cannot instantiate, logger instantiation failed: "
              <> show loggerErrs
          )
          $> Left (Errs.RuntimeException loggerErrs)
      Right _rLoggers -> do
        eDb <- instantiateDb _rConf
        pure $ case eDb of
          Left err -> Left err
          Right _rDb -> Right Runtime {..}

-- | Create a runtime from a given state.
bootDbAndLogFile :: MonadIO m => Store.HaskDb -> FilePath -> m Runtime
bootDbAndLogFile db logsPath = do
  let loggingConf = Command.mkLoggingConf logsPath
      _rConf = Command.defaultConf {Command._confLogging = loggingConf}
  _rDb <- liftIO . STM.atomically $ Core.instantiateStmDb db
  _rLoggers <- ML.makeDefaultLoggersWithConf loggingConf
  let _rThreads = NoThreads
  pure $ Runtime {..}

-- | Instantiate the db.
--
--1. The state is either the empty db, or if a _confDbFile file is specified, is
--read from the file.
--
--2. Whenever the application exits, the state is written to disk, if a
--_confDbFile is specified.
instantiateDb
  :: forall m
   . MonadIO m
  => Command.Conf
  -> m (Either Errs.RuntimeErr Core.StmDb)
instantiateDb Command.Conf {..} = readDbSafe _confDbFile

readDb
  :: forall m
   . MonadIO m
  => Maybe FilePath
  -> m (Either Errs.RuntimeErr Core.StmDb)
readDb mpath = case mpath of
  Just fpath -> do
    -- We may want to read the file only when the file exists.
    exists <- liftIO $ doesFileExist fpath
    if exists then fromFile fpath else useEmpty
  Nothing -> useEmpty
 where
  fromFile fpath =
    liftIO (try @SomeException $ T.readFile fpath) >>= \case
      Left err ->
        putStrLn @Text ("Unable to read db file: " <> maybe "" T.pack mpath)
          $> Left (Errs.RuntimeException err)
      Right fdata ->
        -- We may want to deserialise the data only when the data is non-empty.
        if T.null fdata
          then useEmpty
          else
            Store.deserialiseDbStrict (TE.encodeUtf8 fdata)
              & either (pure . Left . Errs.knownErr) useState
  useEmpty = Right <$> liftIO (STM.atomically Core.instantiateEmptyStmDb)
  useState = fmap Right . liftIO . STM.atomically . Core.instantiateStmDb

-- | A safer version of readDb: this fails if the file doesn't exist or is
-- empty. This helps in catching early mistake, e.g. from user specifying the
-- wrong file name on the command-line.
readDbSafe
  :: forall m
   . MonadIO m
  => Maybe FilePath
  -> m (Either Errs.RuntimeErr Core.StmDb)
readDbSafe mpath = case mpath of
  Just fpath -> do
    -- We may want to read the file only when the file exists.
    exists <- liftIO $ doesFileExist fpath
    if exists
      then fromFile fpath
      else pure . Left . Errs.knownErr $ RErr.FileDoesntExistErr fpath
  Nothing -> useEmpty
 where
  fromFile fpath = do
    fdata <- liftIO (T.readFile fpath)
    Store.deserialiseDbStrict (TE.encodeUtf8 fdata)
      & either (pure . Left . Errs.knownErr) useState
  useEmpty = Right <$> liftIO (STM.atomically Core.instantiateEmptyStmDb)
  useState = fmap Right . liftIO . STM.atomically . Core.instantiateStmDb

-- | Power down the application: attempting to save the DB state in given file,
-- if possible, and reporting errors otherwise.
powerdown :: MonadIO m => Runtime -> m (Maybe Errs.RuntimeErr)
powerdown runtime@Runtime {..} = do
  mDbSaveErr <- saveDb runtime
  reportDbSaveErr mDbSaveErr
  -- finally, close loggers.
  eLoggingCloseErrs <-
    liftIO . try @SomeException $
      ML.flushAndCloseLoggers
        _rLoggers
  reportLoggingCloseErrs eLoggingCloseErrs
  pure $ mDbSaveErr <|> first Errs.RuntimeException eLoggingCloseErrs ^? _Left
 where
  reportLoggingCloseErrs eLoggingCloseErrs =
    when (isLeft eLoggingCloseErrs)
      . putStrLn @Text
      $ "Errors when closing loggers: "
        <> show eLoggingCloseErrs
  reportDbSaveErr mDbSaveErr =
    when (isJust mDbSaveErr)
      . putStrLn @Text
      $ "DB state cannot be saved: "
        <> show mDbSaveErr

saveDb :: MonadIO m => Runtime -> m (Maybe Errs.RuntimeErr)
saveDb runtime =
  maybe (pure Nothing) (saveDbAs runtime) $ _rConf runtime ^. Command.confDbFile

saveDbAs :: MonadIO m => Runtime -> FilePath -> m (Maybe Errs.RuntimeErr)
saveDbAs runtime fpath = do
  haskDb <- readFullStmDbInHask $ _rDb runtime
  let bs = Store.serialiseDb haskDb
  liftIO
    (try @SomeException (T.writeFile fpath . TE.decodeUtf8 $ BS.toStrict bs))
    <&> either (Just . Errs.RuntimeException) (const Nothing)

-- | Reads all values of the `Db` product type from `STM.STM` to @Hask@.
readFullStmDbInHask
  :: MonadIO m
  => Core.StmDb
  -> m Store.HaskDb
readFullStmDbInHask = liftIO . STM.atomically . Core.readFullStmDbInHask'
