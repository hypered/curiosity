module Curiosity.RunSpec
  ( spec
  ) where

import Curiosity.Interpret
import Test.Hspec

--------------------------------------------------------------------------------
spec :: Spec
spec = do
  describe "wordsq" $ do
    let examples =
          [ ("Hello world.", ["Hello", "world."])
          , ("\"Hello world.\"", ["Hello world."])
          , ("\"Hello\" world.", ["Hello", "world."])
          , ("Hello \"world.\"", ["Hello", "world."])
          , ("Hello \"the world.\"", ["Hello", "the world."])
          , ("Hello \"the the\" world.", ["Hello", "the the", "world."])
          , ("Hel\"lo world.", ["Hel\"lo", "world."])
          ]
        f (input, expected) =
          it ("Split " <> input) $ wordsq input `shouldBe` expected
    mapM_ f examples
