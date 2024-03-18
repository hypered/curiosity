-- This module is loaded only in the GHCi session. It defines two helper
-- functions to run the HSpec-based tests and the Golden tests.

module Curiosity.Runner
  ( runSpec
  , runScenarios
  ) where

import qualified CuriositySpec
import qualified Curiosity.CommandSpec
import qualified Curiosity.CoreSpec
import qualified Curiosity.DataSpec
import qualified Curiosity.DslSpec
import qualified Curiosity.Interpret           as Interpret
import qualified Curiosity.RunSpec
import qualified Curiosity.RuntimeSpec
import qualified Data.Text                     as T
import           System.FilePath
import           Test.Hspec
import           Test.Tasty
import qualified Test.Tasty.Silver             as Silver


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
  let testName   = takeBaseName path
      goldenPath = replaceExtension path ".golden"
  pure $ Silver.goldenVsAction testName goldenPath action convert
 where
  action :: IO [Text]
  action = snd . Interpret.formatOutput <$> Interpret.handleRun' path

  convert = T.unlines
