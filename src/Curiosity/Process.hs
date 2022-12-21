{-# LANGUAGE DataKinds #-}
module Curiosity.Process
  ( startServer
  , endServer
  , shutdown
  , startupLogInfo
  , logInfo
  ) where

import qualified Commence.Multilogging         as ML
import qualified Commence.Runtime.Errors       as Errs
import qualified Control.Monad.Log             as L
import qualified Curiosity.Parse               as Command
import qualified Curiosity.Runtime             as Rt
import qualified Curiosity.Server              as Srv


--------------------------------------------------------------------------------
startServer :: Command.ServerConf -> Rt.Runtime -> IO Errs.RuntimeErr
startServer conf runtime@Rt.Runtime {..} = do
  let port = Command._serverPort conf
  startupLogInfo _rLoggers $ "Starting up server on port " <> show port <> "..."
  try @SomeException (Srv.run conf runtime) >>= pure . either
    Errs.RuntimeException
    (const $ Errs.RuntimeException UserInterrupt)
  -- FIXME: improve this, incorrect error reporting here.

-- | Start cleanly shut down the server thread on an error, and re-report the error.
endServer :: ML.AppNameLoggers -> Errs.RuntimeErr -> IO ()
endServer loggers =
  startupLogInfo loggers . mappend "Server process ended: " . Errs.displayErr

{- | Startup logging using standard loggers instead of using the putStrLn blindly.
`putStrLn` may cause issues with the REPL since both rely on STDOUT.

The implementation is simple: if there are no loggers, we don't output anything. However, if there is one, we log on the first logger.

FIXME: check if the logger is not using STDOUT, or, find the first non-STDOUT logger and log on that.
-}
startupLogInfo :: MonadIO m => ML.AppNameLoggers -> Text -> m ()
startupLogInfo loggers = logInfo (<> "Boot") loggers

logInfo
  :: MonadIO m
  => (ML.AppName -> ML.AppName)
  -> ML.AppNameLoggers
  -> Text
  -> m ()
logInfo env (ML.AppNameLoggers loggers) msg = mapM_ logOver loggers
  where logOver l = L.runLogT' l . L.localEnv env $ L.info msg


--------------------------------------------------------------------------------
shutdown :: Rt.Runtime -> Maybe SomeException -> IO ExitCode
shutdown Rt.Runtime {..} mException = do
  startupLogInfo _rLoggers
    $  "Shutting down: "
    <> maybe "graceful exit" show mException
    <> "."
  ML.flushAndCloseLoggers _rLoggers
  if isJust mException then exitFailure else exitSuccess
