module Curiosity.CoreSpec
  ( spec
  ) where

import qualified Curiosity.Data.Counter as C 
import           Curiosity.Core
import qualified           Curiosity.Data as Data 
import           Curiosity.Data.User
import           Test.Hspec


--------------------------------------------------------------------------------
spec :: Spec
spec = do
  describe "Core" $ do
    it ("The first user ID is " <> show (unUserId firstUserId) <> ".") $ do
      db <- atomically instantiateEmptyStmDb
      id <- atomically $ C.newIdOf @UserId (Data._dbNextUserId db)
      id `shouldBe` firstUserId
