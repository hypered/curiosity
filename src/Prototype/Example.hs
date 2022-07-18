{- |
Module: Prototype.Example
Description: Provide example data and scenarios.

-}
module Prototype.Example where

import qualified Prototype.Data.User           as User


--------------------------------------------------------------------------------
alice :: User.UserProfile
alice = User.UserProfile "USER-1"
                         (User.Credentials "alice" "secret")
                         "Alice"
                         "alice@example.com"
