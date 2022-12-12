module CuriositySpec
  ( spec
  ) where

import qualified Curiosity.Command             as Command
import qualified Curiosity.Data                as Data
import qualified Curiosity.Data.Business       as Business
import qualified Curiosity.Data.Counter        as C
import qualified Curiosity.Data.Email          as Email
import qualified Curiosity.Data.Legal          as Legal
import qualified Curiosity.Data.User           as User
import qualified Curiosity.Run                 as Run
import qualified Data.Aeson                    as Aeson
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import qualified Options.Applicative           as A
import           Prelude                 hiding ( state )
import           System.Directory               ( doesFileExist, removeFile )
import           System.FilePath                ( (</>) )
import           Test.Hspec


--------------------------------------------------------------------------------
spec :: Spec
spec = do
  -- This makes sure we can parse the example data files. Otherwise we can
  -- forget to update them as we change their corresponding data types.
  describe "UserProfile JSON parser" $ do
    let go (filename, username) = it ("Parses " <> filename) $ do
          Right (result :: User.UserProfile) <- parseFile $ "data/" </> filename
          User._userCredsName (User._userProfileCreds result)
            `shouldBe` username
    mapM_
      go
      [ ("alice.json"         , "alice")
      , ("alice-with-bio.json", "alice")
      , ("bob-0.json"         , "bob")
      , ("bob-1.json"         , "bob")
      , ("bob-2.json"         , "bob")
      , ("charlie.json"       , "charlie")
      , ("mila.json"          , "mila")
      ]

  -- Same here.
  describe "Legal entity JSON parser" $ do
    let go (filename, slug) = it ("Parses " <> filename) $ do
          Right (result :: Legal.Entity) <- parseFile $ "data/" </> filename
          Legal._entitySlug result `shouldBe` slug
    mapM_ go [("one.json", "one")]

  -- Same here.
  describe "Business unit JSON parser" $ do
    let go (filename, slug) = it ("Parses " <> filename) $ do
          Right (result :: Business.Unit) <- parseFile $ "data/" </> filename
          Business._entitySlug result `shouldBe` slug
    mapM_ go [("alpha.json", "alpha")]

  -- TODO Check that all the files in data/ are in one of the above lists.

  describe "Command-line interface parser" $ do
    let go (arguments, command) =
          it ("Parses '" <> T.unpack arguments <> "'") $ do
            let A.Success x =
                  A.execParserPure A.defaultPrefs Command.parserInfo
                    $   T.unpack
                    <$> words arguments
            x `shouldBe` command
    mapM_
      go
      [ ("init"          , Command.Init Data.Normal)
      , ("init --normal" , Command.Init Data.Normal)
      , ("init --stepped", Command.Init Data.Stepped)
      , ("state"         , Command.State False)
      , ("state --hs"    , Command.State True)
      ]

  describe "Command-line interface execution" $ do
    let stateFile = "/tmp/curiosity-test-state.json"
        go (arguments, state) = it ("Runs '" <> T.unpack arguments <> "'") $ do

          let A.Success command =
                A.execParserPure A.defaultPrefs Command.parserInfo
                  $   T.unpack
                  <$> words arguments

          (Run.run $ Command.CommandWithTarget
              command
              (Command.StateFileTarget stateFile)
              (Command.User $ User.UserName "alice")
            )
            `shouldThrow` (== ExitSuccess)

          Right value <- parseFile stateFile
          value `shouldBe` state

    malice <- runIO $ parseFile "data/alice.json"
    case malice of
      Left err -> runIO $ "x" `shouldBe` err -- TODO How to make it fail ?
      Right alice -> do
        let aliceState = Data.emptyHask
              { Data._dbNextUserId   = C.CounterValue 2
              , Data._dbUserProfiles = Identity [alice]
              , Data._dbNextEmailId  = C.CounterValue 2
              , Data._dbEmails       = Identity
                                         [ Email.Email "EMAIL-1"
                                                       Email.SignupConfirmationEmail
                                                       Email.systemEmailAddr
                                                       "alice@example.com"
                                                       Email.EmailTodo
                                         ]
              , Data._dbEpochTime    = 60
              }
        -- The same state, but with the email set to DONE, and the time
        -- advanced a bit.
        let aliceState' = Data.emptyHask
              { Data._dbNextUserId   = C.CounterValue 2
              , Data._dbUserProfiles = Identity [alice]
              , Data._dbNextEmailId  = C.CounterValue 2
              , Data._dbEmails       = Identity
                                         [ Email.Email "EMAIL-1"
                                                       Email.SignupConfirmationEmail
                                                       Email.systemEmailAddr
                                                       "alice@example.com"
                                                       Email.EmailDone
                                         ]
              , Data._dbEpochTime    = 120
              }
        runIO $ do
          fileExists <- doesFileExist stateFile
          when fileExists $ removeFile stateFile
        mapM_
          go
          [ ("init --stepped", Data.emptyHask)
          , ("user signup alice a alice@example.com --accept-tos", aliceState)
          , ("step-email"    , aliceState')
          , ("reset"         , Data.emptyHask)
          ]


--------------------------------------------------------------------------------
parseFile path = do
  content <- readFile path
  pure $ Aeson.eitherDecodeStrict (T.encodeUtf8 content)
