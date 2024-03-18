{-# LANGUAGE DataKinds #-}

-- |
--Module: Curiosity.Html.LandingPage
--Description: A landing page (when the user is not logged in) for Curiosity.
module Curiosity.Html.LandingPage
  ( LandingPage (..)
  , navigation
  ) where

import Smart.Html.Alert
import Smart.Html.Button
import Smart.Html.Dsl (HtmlCanvas)
import Smart.Html.Dsl qualified as Dsl
import Smart.Html.Misc qualified as Misc
import Smart.Html.Navbar
import Smart.Html.Render qualified as Render
import Smart.Html.Shared.Html.Icons
import Text.Blaze.Html5 (Html, (!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A

--------------------------------------------------------------------------------
data LandingPage = LandingPage

instance H.ToMarkup LandingPage where
  toMarkup LandingPage = do
    Render.renderCanvas landingPage

--------------------------------------------------------------------------------
landingPage :: HtmlCanvas
landingPage = Dsl.SingletonCanvas $ do
  H.toMarkup navigation
  landing
  Misc.landingFooter "https://github.com/hypered/curiosity"

navigation :: NavbarWebsite
navigation =
  NavbarWebsite
    [Entry "Documentation" (Link "/documentation"), Entry "Login" (Link "/login"), Entry "Sign up" (Link "/signup")]

landing :: Html
landing = H.main ! A.class_ "o-container o-container--flex" $ do
  Misc.landingHero "Curiosity, a prototype system" $
    do
      H.p $ do
        "This site is a running instance of Curiosity. \
        \Curiosity is an always \
        \work-in-progress system to think, discuss, and communicate \
        \development practices and system design."
      H.toMarkup $
        Alert
          AlertWarning
          iconWarning
          "This site is up for demonstration purpose only. Data are public \
          \(including passwords) \
          \and frequently permanently erased. Please use this site only \
          \if you understand what this means."
          NoButton
  H.div ! A.class_ "o-grid" $ do
    Misc.landingPanel "Open source" $ H.p $ do
      "This project is open source "
      "and available "
      H.a ! A.href "https://github.com/hypered/curiosity" $ "on GitHub"
      "."
    Misc.landingPanel "Experimental" $ H.p $ do
      "Curiosity is being developed to inform further developments, and "
      "is not intended to be a production system."

-- TODO Move this little helper definitions to Icons.
iconWarning :: Maybe (OSvgIconDiv "warning")
iconWarning = Just $ OSvgIconDiv @"warning" svgIconWarning
