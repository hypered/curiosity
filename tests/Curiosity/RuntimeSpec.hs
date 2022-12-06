module Curiosity.RuntimeSpec
  ( spec
  ) where

import           Curiosity.Core
import           Curiosity.Data
import           Curiosity.Data.User
import           Curiosity.Runtime.IO           ( bootDbAndLogFile )
import           Curiosity.Runtime.Type         ( _rDb )
import           Test.Hspec


--------------------------------------------------------------------------------
spec :: Spec
spec = do
  describe "Runtime" $ do
    it "Should boot." $ do
      -- TODO I don't like that this involves logging stuff.
      runtime <- bootDbAndLogFile emptyHask "/tmp/curiosity-test-xxx-1.log"
      let db = _rDb runtime
      st <- atomically $ readFullStmDbInHask' db
      st `shouldBe` emptyHask

  describe "Runtime / Users" $ do
    it "Empty state returns no user." $ do
      -- TODO powerdown shoud be called at the end of the previous test,
      -- otherwise a "resource busy (file is locked)" will occur.
      -- Which is indeed related to logging, which I'd like to remove from
      -- these tests / runtime.
      runtime <- bootDbAndLogFile emptyHask "/tmp/curiosity-test-xxx-2.log"
      let db = _rDb runtime
      muser <- atomically $ selectUserById db "USER-1"

      muser `shouldBe` Nothing

    it "Adding a user, returns a user and an email." $ do
      runtime <- bootDbAndLogFile emptyHask "/tmp/curiosity-test-xxx-3.log"
      let db = _rDb runtime
          input = Signup "alice" "secret" "alice@example.com" True
      Right (uid, eid) <- atomically $ signup db input
      Just profile <- atomically $ selectUserById db uid

      uid `shouldBe` "USER-1"
      _userProfileId profile `shouldBe` uid

      eid `shouldBe` "EMAIL-1"

    it "Blocklisted username cannot be used." $ do
      runtime <- bootDbAndLogFile emptyHask "/tmp/curiosity-test-xxx-4.log"
      let db = _rDb runtime
          input = Signup "smartcoop" "secret" "smartcoop@example.com" True
      muser <- atomically $ signup db input

      muser `shouldBe` Left (ValidationErrs [UsernameBlocked])
