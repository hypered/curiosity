module Curiosity.CoreSpec
  ( spec
  ) where

import           Curiosity.Core
import qualified Curiosity.Types.Store as Store 
import qualified Curiosity.Types.Counter as C 
import           Curiosity.Types.User
import           Test.Hspec


--------------------------------------------------------------------------------
spec :: Spec
spec = do
  describe "Core" $ do
    it ("The first user ID is " <> show (unUserId firstUserId) <> ".") $ do
      db <- atomically instantiateEmptyStmDb
      id <- atomically $ C.newIdOf @UserId (Store._dbNextUserId db)
      id `shouldBe` firstUserId
