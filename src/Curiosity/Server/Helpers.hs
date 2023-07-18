{-# LANGUAGE DataKinds #-}
module Curiosity.Server.Helpers
  ( GetUserPage
  , PutUserPage
  , DeleteUserPage
  , PostUserPage
  , UserPage
  , UserAuthentication
  , PostAuthHeaders
  ) where

import           Commence.Server.Auth           ( PostAuthHeaders )
import qualified Curiosity.Types.User          as User
import           Servant.API
import qualified Servant.Auth.Server           as SAuth
import           Servant.HTML.Blaze             ( HTML )
import qualified Smart.Server.Page             as SS.P


type GetUserPage pageData = UserPage Get pageData
type PutUserPage pageData = UserPage Put pageData
type DeleteUserPage pageData = UserPage Delete pageData
type PostUserPage pageData = UserPage Post pageData

-- | A convenient alias to denote a GET endpoint to get a user-authenticated page.
type UserPage method pageData
  = method '[HTML] (SS.P.Page 'SS.P.Authd User.UserProfile pageData)

-- brittany-disable-next-binding
-- | Simple user authentication.
type UserAuthentication = SAuth.Auth '[SAuth.Cookie] User.UserId
