{-# LANGUAGE DataKinds #-}

module Curiosity.Process
  ( startServer
  , endServer
  , shutdown
  , startupLogInfo
  ) where

import Commence.Multilogging qualified as ML
import Commence.Runtime.Errors qualified as Errs
import Curiosity.Parse qualified as Command
import Curiosity.Runtime qualified as Runtime
import Curiosity.Server qualified as Srv

--------------------------------------------------------------------------------
startServer :: Command.ServerConf -> Runtime.Runtime -> IO Errs.RuntimeErr
startServer conf runtime@Runtime.Runtime {..} = do
  let Command.ServerConf port _ _ _ _ _ = conf
  startupLogInfo _rLoggers $ "Starting up server on port " <> show port <> "..."
  try @SomeException (Srv.run conf runtime)
    >>= pure
      . either
        Errs.RuntimeException
        (const $ Errs.RuntimeException UserInterrupt)

-- FIXME: improve this, incorrect error reporting here.

-- | Start cleanly shut down the server thread on an error, and re-report the error.
endServer :: ML.AppNameLoggers -> Errs.RuntimeErr -> IO ()
endServer loggers =
  startupLogInfo loggers . mappend "Server process ended: " . Errs.displayErr

-- | Startup logging using standard loggers instead of using the putStrLn blindly.
--`putStrLn` may cause issues with the REPL since both rely on STDOUT.
--
--The implementation is simple: if there are no loggers, we don't output anything. However, if there is one, we log on the first logger.
--
--FIXME: check if the logger is not using STDOUT, or, find the first non-STDOUT logger and log on that.
startupLogInfo :: MonadIO m => ML.AppNameLoggers -> Text -> m ()
startupLogInfo = ML.logInfo (<> "Boot")

--------------------------------------------------------------------------------
shutdown :: Runtime.Runtime -> Maybe SomeException -> IO ExitCode
shutdown Runtime.Runtime {..} mException = do
  startupLogInfo _rLoggers $
    "Shutting down: "
      <> maybe "graceful exit" show mException
      <> "."
  ML.flushAndCloseLoggers _rLoggers
  if isJust mException then exitFailure else exitSuccess
