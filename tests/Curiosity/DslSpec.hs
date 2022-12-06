module Curiosity.DslSpec
  ( spec
  ) where

import           Curiosity.Dsl
import           Test.Hspec


--------------------------------------------------------------------------------
spec :: Spec
spec = do
  describe "Dsl" $ do
    it ("The example runs.") $ do
      result <- run db0 example0
      result `shouldBe` (True, False)
