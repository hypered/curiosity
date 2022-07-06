{- |
Module: Prototype.Exe.Server.Public.Pages
Description: Public pages for the application

The goal is to exemplify the use of our DSL for some simple pages and to have something more tangible to show.

-}
module Prototype.Exe.Server.Public.Pages
  ( LoginPage(..)
  , SignupPage(..)
  , SignupResultPage(..)
  , LandingPage(..)
  , NotFoundPage(..)
  ) where

import           Control.Lens
import           Network.HTTP.Types.Method
import qualified Prototype.Exe.Data.User       as User
import           Prototype.Exe.Server.Shared.Html.Helpers.Form
                                                ( mkButton )
import qualified Smart.Html.Dsl                as Dsl
import qualified Smart.Html.Errors             as Errors
import qualified Smart.Html.Form               as Form
import qualified Smart.Html.Input              as Inp
import qualified Smart.Html.Pages.LandingPage  as Pages
import qualified Smart.Html.Render             as Render
import qualified Smart.Html.Shared.Types       as HTypes
import qualified Text.Blaze.Html5              as H

-- | A simple login page.
newtype LoginPage  = LoginPage { _loginPageAuthSubmitURL :: H.AttributeValue }

-- | For the `LoginPage` markup, we now rely on our DSL to render the login page to our liking
instance H.ToMarkup LoginPage where
  toMarkup (LoginPage submitUrl) = do
    H.form
      . H.toMarkup @Dsl.HtmlCanvas
      $ (       Form.InputGroup [username, password]
        Dsl.::~ loginButton
        Dsl.::~ Dsl.EmptyCanvas
        )
   where
    username =
      ( "Username"
      , Inp.PlainTextInput HTypes.Enabled "username" "username" Nothing
      )
    password =
      ( "Password"
      , Inp.PasswordInput HTypes.Enabled
                          "password"
                          "password"
                          Nothing
      )
    loginButton = mkButton "Login" submitUrl POST

newtype SignupPage = SignupPage { _signupPageSubmitURL :: H.AttributeValue }

instance H.ToMarkup SignupPage where
  toMarkup (SignupPage submitUrl) = do
    H.form
      . H.toMarkup @Dsl.HtmlCanvas
      $ (       Form.InputGroup
            [ username
            , password "Password"         "password"
            , password "Confirm Password" "passwordConfirmation"
            ]
        Dsl.::~ submitButton
        Dsl.::~ Dsl.EmptyCanvas
        )
   where
    username =
      ( "Username"
      , Inp.PlainTextInput HTypes.Enabled "username" "username" Nothing
      )
    password fieldName inputName =
      ( HTypes.Title fieldName
      , Inp.PasswordInput HTypes.Enabled
                          (HTypes.Id inputName)
                          (HTypes.Name inputName)
                          Nothing
      )
    submitButton = mkButton "Register" submitUrl POST

data SignupResultPage = SignupSuccess User.UserId
                      | SignupFailed Text

instance H.ToMarkup SignupResultPage where
  toMarkup = \case
    SignupSuccess userId -> withText $ "Signed up as: " <> userId ^. coerced
    SignupFailed  msg    -> withText msg
   where
    withText msg =
      H.toMarkup @Dsl.HtmlCanvas $ Dsl.SingletonCanvas (HTypes.Title $ msg)

data LandingPage = LandingPage

instance H.ToMarkup LandingPage where
  toMarkup LandingPage = do
    Render.renderCanvas $
      Pages.landingPage

data NotFoundPage = NotFoundPage

instance H.ToMarkup NotFoundPage where
  toMarkup NotFoundPage = do
    Render.renderCanvas $
      Dsl.SingletonCanvas Errors.NotFound