-- |
--Module: Curiosity.Html.Navbar
--Description: A navigation bar for Curiosity.
module Curiosity.Html.Navbar
  ( navbarWebsite
  , navbar
  ) where

import Curiosity.Html.LandingPage qualified as Pages
import Smart.Html.Avatar
import Smart.Html.Navbar qualified as Navbar
import Smart.Html.Shared.Html.Icons

--------------------------------------------------------------------------------
-- The (not logged-in) website navbar.
navbarWebsite :: Navbar.NavbarWebsite
navbarWebsite = Pages.navigation

--------------------------------------------------------------------------------
-- The (logged-in) application navbar.
navbar :: Text -> Navbar.Navbar
navbar name = Navbar.Navbar [] [helpEntry, plusEntry, userEntry name]

userEntry :: Text -> Navbar.RightEntry
userEntry name = Navbar.UserEntry (userEntries name) NoAvatarImage

userEntries :: Text -> [Navbar.SubEntry]
userEntries name =
  [ Navbar.SignedInAs name
  , Navbar.Divider
  , Navbar.SubEntry "Settings" "/settings/profile" False
  , Navbar.Divider
  , -- TODO: change to `POST` in the future.
    Navbar.SubEntry "Sign out" "/a/logout" False
  ]

helpEntry :: Navbar.RightEntry
helpEntry = Navbar.IconEntry divIconCircleHelp helpEntries

helpEntries :: [Navbar.SubEntry]
helpEntries = [Navbar.SubEntry "Documentation" "/documentation" False]

plusEntry :: Navbar.RightEntry
plusEntry = Navbar.IconEntry divIconAdd plusEntries

plusEntries :: [Navbar.SubEntry]
plusEntries =
  [ Navbar.SubEntry "New quotation" "/new/quotation" False
  , Navbar.SubEntry "New invoice" "/new/invoice" False
  , Navbar.SubEntry "New contract" "/new/contract" False
  , Navbar.Divider
  , Navbar.SubEntry "New business entity" "/new/unit" False
  , Navbar.SubEntry "New legal entity" "/new/entity" False
  ]
