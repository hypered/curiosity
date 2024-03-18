-- | This test program run scripts similarly to @cty run@, ensuring their
-- outputs are identical to "golden" (expected) results. Scenarios (and their
-- golden files) can be found in the @scenarios/@ directory in the Curiosity
-- repository.
--
-- Use e.g. @scripts/run-tests.sh@ to execute this program.

import qualified Curiosity.Interpret           as Interpret
import qualified Data.Text                     as T
import           System.FilePath

import           Test.Tasty
import qualified Test.Tasty.Silver             as Silver


--------------------------------------------------------------------------------
main :: IO ()
main = do
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
