{-# LANGUAGE TemplateHaskell #-}

module Curiosity.Runtime.Type
  ( Runtime (..)
  , rConf
  , rDb
  , rLoggers
  , rThreads
  , Threads (..)
  ) where

import Commence.Multilogging qualified as ML
import Control.Lens
import Curiosity.Core qualified as Core
import Curiosity.Parse qualified as Parse

--------------------------------------------------------------------------------

-- | The runtime, a central product type that should contain all our runtime
-- supporting values: the STM state, loggers, and processing threads.
data Runtime = Runtime
  { _rConf :: Parse.Conf
  -- ^ The application configuration.
  , _rDb :: Core.StmDb
  -- ^ The Storage.
  , _rLoggers :: ML.AppNameLoggers
  -- ^ Multiple loggers to log over.
  , _rThreads :: Threads
  -- ^ Additional threads running e.g. async tasks.
  }

-- | Describes the threading configuration: what the main thread is, and what
-- additional threads can be created.
data Threads
  = -- | Means that threads can't be started or stopped dynamically.
    NoThreads
  | -- | Means the main thread is the REPL, started with `cty repl`.
    ReplThreads
      { _tEmailThread :: MVar ThreadId
      }
  | -- | Means the main thread is the HTTP server, started with `cty serve`.
    HttpThreads
      { _tEmailThread :: MVar ThreadId
      , _tUnixThread :: MVar ThreadId
      -- ^ UNIX-domain socket server thread.
      }

makeLenses ''Runtime
