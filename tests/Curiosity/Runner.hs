-- This module is loaded only in the GHCi session. It defines two helper
-- functions to run the HSpec-based tests and the Golden tests.

module Curiosity.Runner
  ( runSpec
  , runScenarios
  ) where

import Curiosity.CommandSpec qualified
import Curiosity.CoreSpec qualified
import Curiosity.DataSpec qualified
import Curiosity.DslSpec qualified
import Curiosity.Interpret qualified as Interpret
import Curiosity.RunSpec qualified
import Curiosity.Runtime qualified as Runtime
import Curiosity.RuntimeSpec qualified
import CuriositySpec qualified
import Data.Text qualified as T
import System.FilePath
import Test.Hspec
import Test.Tasty
import Test.Tasty.Silver qualified as Silver

--------------------------------------------------------------------------------
runSpec :: IO ()
runSpec = hspec $ do
  CuriositySpec.spec
  Curiosity.CommandSpec.spec
  Curiosity.CoreSpec.spec
  Curiosity.DataSpec.spec
  Curiosity.DslSpec.spec
  Curiosity.RunSpec.spec
  Curiosity.RuntimeSpec.spec

--------------------------------------------------------------------------------
runScenarios :: IO ()
runScenarios = do
  -- List all scenarios, comparing them to their corresponding golden files.
  goldens <- Interpret.listScenarios "scenarios/" >>= mapM mkGoldenTest
  defaultMain $ testGroup "Tests" goldens

--------------------------------------------------------------------------------
mkGoldenTest :: FilePath -> IO TestTree
mkGoldenTest path = do
  -- `path` looks like @scenarios/quotation-flow.txt@.
  -- `testName` looks like @quotation-flow@.
  -- `goldenPath` looks like @scenarios/quotation-flow.golden@.
  let testName = takeBaseName path
      goldenPath = replaceExtension path ".golden"
  pure $ Silver.goldenVsAction testName goldenPath action convert
 where
  action :: IO [Text]
  action = do
    runtime <- Runtime.bootForTests >>= either throwIO pure
    snd . Interpret.formatOutput <$> Interpret.run' runtime path

  convert = T.unlines
