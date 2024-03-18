-- | This is the main server-side program to interact with the server (through
-- a UNIX-domain socket) or a state file.
module Main
  ( main
  ) where

import Curiosity.Command qualified as Command
import Curiosity.Run qualified as Run
import Options.Applicative qualified as A

--------------------------------------------------------------------------------
main :: IO ExitCode
main = A.execParser Command.parserInfoWithTarget >>= Run.run
