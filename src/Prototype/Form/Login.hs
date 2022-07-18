{-# LANGUAGE DeriveAnyClass #-}
module Prototype.Form.Login
  ( Page(..)
  , ResultPage(..)
  ) where

import           Smart.Html.Dsl                 ( HtmlCanvas )
import qualified Smart.Html.Dsl                as Dsl
import qualified Smart.Html.Render             as Render
import           Smart.Html.Shared.Html.Icons
import           Text.Blaze                     ( customAttribute )
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A


--------------------------------------------------------------------------------
data Page = Page
  { loginSubmitURL :: H.AttributeValue
  }

instance H.ToMarkup Page where
  toMarkup = Render.renderCanvas . loginPage

loginPage :: Page -> HtmlCanvas
loginPage Page {..} = Dsl.SingletonCanvas $ do
  H.header
    ! A.class_ "o-container-vertical"
    $ H.div
    ! A.class_ "o-container"
    $ H.div
    ! A.class_ "c-brand c-brand--xsmall"
    $ H.a
    ! A.href "/"
    $ H.img
    ! A.src "https://design.smart.coop/images/logo.svg"
    ! A.alt "Smart"
  H.main
    ! A.class_ "o-container-vertical"
    $ H.div
    ! A.class_ "o-container o-container--small"
    $ do
        H.div
          ! A.class_ "c-panel"
          $ H.div
          ! A.class_ "c-panel__body"
          $ H.form
          $ do
              H.h3 ! A.class_ "c-h2" $ "Log in to your account"
              H.div
                ! A.class_ "o-form-group-layout o-form-group-layout--standard"
                $ do
                    H.div ! A.class_ "o-form-group" $ do
                      H.label
                        ! A.class_ "o-form-group__label"
                        ! A.for "username"
                        $ "Username"
                      H.div
                        ! A.class_ "o-form-group__controls"
                        $ H.input
                        ! A.class_ "c-input"
                        ! A.id "username"
                        ! A.name "username"
                        ! A.type_ "text"
                    H.div ! A.class_ "o-form-group" $ do
                      H.label
                        ! A.class_ "o-form-group__label"
                        ! A.for "password"
                        $ "Password"
                      H.div
                        ! A.class_ "o-form-group__controls"
                        $ H.div
                        ! A.class_ "c-input-with-icon"
                        $ do
                            H.input
                              ! A.class_ "c-input"
                              ! A.id "password"
                              ! A.name "password"
                              ! A.type_ "password"
                            H.button
                              ! A.class_ "c-input-with-icon__toggle"
                              ! customAttribute "data-password-toggle" "1"
                              $ do
                                  H.div
                                    ! A.class_ "o-svg-icon o-svg-icon-eye"
                                    $ H.toMarkup svgIconEye
                                  H.div
                                    ! A.class_ "o-svg-icon o-svg-icon-eye-off"
                                    $ H.toMarkup svgIconEyeOff
                    H.div
                      ! A.class_ "o-form-group"
                      $ H.button
                      ! A.class_ "c-button c-button--primary c-button--block"
                      ! A.formaction loginSubmitURL
                      ! A.formmethod "POST"
                      $ H.span
                      ! A.class_ "c-button__content"
                      $ "Log in"
                    H.div ! A.class_ "o-form-group u-ta-center" $ ""
        H.div ! A.class_ "c-content u-text-center u-spacer-top-l" $ do
          H.a ! A.class_ "u-text-muted" ! A.href "/signup" $ "Sign up"
          " | "
          H.a
            ! A.class_ "u-text-muted"
            ! A.href "/password-reset"
            $ "Forgot password"


--------------------------------------------------------------------------------
data ResultPage = Success Text
                | Failure Text

instance H.ToMarkup ResultPage where
  toMarkup = \case
    Success msg -> withText msg
    Failure msg -> withText msg
   where
    withText msg =
      Render.renderCanvas $ Dsl.SingletonCanvas $
        H.div ! A.class_ "c-display" $
          H.code $ H.toMarkup msg
