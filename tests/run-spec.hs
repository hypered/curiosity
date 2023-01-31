-- This file is not necessary for `cabal test` (which uses Spec.hs instead) but
-- makes if possible to load the tests in GHCi and ghcid.

import qualified CuriositySpec
import qualified Curiosity.CommandSpec
import qualified Curiosity.CoreSpec
import qualified Curiosity.DataSpec
import qualified Curiosity.DslSpec
import qualified Curiosity.RunSpec
import qualified Curiosity.RuntimeSpec
import           Test.Hspec


--------------------------------------------------------------------------------
main :: IO ()
main = hspec $ do
  CuriositySpec.spec
  Curiosity.CommandSpec.spec
  Curiosity.CoreSpec.spec
  Curiosity.DataSpec.spec
  Curiosity.DslSpec.spec
  Curiosity.RunSpec.spec
  Curiosity.RuntimeSpec.spec
