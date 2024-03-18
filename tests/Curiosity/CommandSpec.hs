module Curiosity.CommandSpec
  ( spec
  ) where

import Curiosity.Command
import Curiosity.Types.User qualified as User
import Data.Text qualified as T
import Options.Applicative qualified as A
import Test.Hspec

--------------------------------------------------------------------------------
spec :: Spec
spec = do
  describe "Command serialisation" $ do
    let examples =
          [ (Reset, "reset")
          , (State False, "state")
          , (State True, "state --hs")
          ,
            ( Signup (User.Signup "alice" "a" "alice@example.com" False)
            , "user signup alice a alice@example.com"
            )
          ,
            ( Signup (User.Signup "alice" "a" "alice@example.com" True)
            , "user signup alice a alice@example.com --accept-tos"
            )
          , (SelectUser False "USER-1" False, "user get USER-1")
          , (SelectUser False "USER-1" True, "user get USER-1 --short")
          , (SelectUser True "USER-1" False, "user get USER-1 --hs")
          , (SelectUser True "USER-1" True, "user get USER-1 --hs --short")
          ]
        f (input, expected) =
          it ("Serialises '" <> show input <> "'") $
            commandToString input
              `shouldBe` Right expected
    mapM_ f examples

  describe "Command roundtrip" $ do
    let
      examples =
        [ Reset
        , State False
        , State True
        , Signup (User.Signup "alice" "a" "alice@example.com" False)
        , Signup (User.Signup "alice" "a" "alice@example.com" True)
        , SelectUser False "USER-1" False
        , SelectUser False "USER-1" True
        , SelectUser True "USER-1" False
        , SelectUser True "USER-1" True
        ]
      f input = it ("Roundtrips '" <> show input <> "'") $ do
        let Right command = commandToString input
            A.Success command' =
              A.execParserPure A.defaultPrefs parserInfo $
                map T.unpack $
                  words
                    command
        command' `shouldBe` input
    mapM_ f examples
