{- |
Module: Prototype.Html.Navbar
Description: A navigation bar for Curiosity.
-}
module Prototype.Html.Navbar
  ( exampleNavbarAlt
  ) where

import           Smart.Html.Avatar
import           Smart.Html.Navbar


--------------------------------------------------------------------------------
exampleNavbarAlt :: Navbar
exampleNavbarAlt = Navbar [] [userEntry]

userEntry = UserEntry userEntries NoAvatarImage

userEntries = [SubEntry "Settings" "/settings/profile" False]
