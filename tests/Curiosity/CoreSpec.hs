module Curiosity.CoreSpec
  ( spec
  ) where

import Curiosity.Core
import Curiosity.Types.Counter qualified as C
import Curiosity.Types.Store qualified as Store
import Curiosity.Types.User
import Test.Hspec

--------------------------------------------------------------------------------
spec :: Spec
spec = do
  describe "Core" $ do
    it ("The first user ID is " <> show (unUserId firstUserId) <> ".") $ do
      db <- atomically instantiateEmptyStmDb
      id <- atomically $ C.newIdOf @UserId (Store._dbNextUserId db)
      id `shouldBe` firstUserId
