module Curiosity.DataSpec
  ( spec
  ) where

import Test.Hspec
import Test.QuickCheck qualified as Q

--------------------------------------------------------------------------------
spec :: Spec
spec = do
  describe "Example QuickCheck property" $ do
    it "Should be easy." $ Q.property trivial

trivial :: Bool -> Bool
trivial a = a == a
