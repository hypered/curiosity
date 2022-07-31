{- |
Module: Curiosity.Html.Profile
Description: Profile pages (view and edit).
-}
module Curiosity.Html.Profile
  ( ProfilePage(..)
  , ProfileView(..)
  , ProfileSaveConfirmPage(..)
  ) where

import qualified Curiosity.Data.User           as User
import           Curiosity.Html.Navbar          ( exampleNavbarAlt )
import qualified Smart.Html.Dsl                as Dsl
import qualified Smart.Html.Render             as Render
import           Smart.Html.Shared.Html.Icons   ( svgIconAdd
                                                , svgIconArrowRight
                                                , svgIconEdit
                                                )
import           Text.Blaze                     ( customAttribute )
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A


--------------------------------------------------------------------------------
data ProfilePage = ProfilePage
  { _profilePageUserProfile :: User.UserProfile
  , _profilePageSubmitURL   :: H.AttributeValue
  }

instance H.ToMarkup ProfilePage where
  toMarkup (ProfilePage profile submitUrl) =
    Render.renderCanvas
      . Dsl.SingletonCanvas
      $ H.div
      ! A.class_ "c-app-layout"
      $ do
          H.header $ H.toMarkup exampleNavbarAlt
          H.main ! A.class_ "u-maximize-width u-scroll-wrapper" $ profileForm
            profile
            submitUrl

profileForm profile submitUrl =
  H.div
    ! A.class_ "u-scroll-wrapper-body"
    $ H.div
    ! A.class_ "o-container o-container--large"
    $ H.div
    ! A.class_ "o-container-vertical"
    $ H.div
    ! A.class_ "o-container o-container--medium"
    $ do
        H.div
          ! A.class_ "u-spacer-bottom-l"
          $ H.div
          ! A.class_ "c-navbar c-navbar--unpadded c-navbar--bordered-bottom"
          $ H.div
          ! A.class_ "c-toolbar"
          $ H.div
          ! A.class_ "c-toolbar__left"
          $ H.h3
          ! A.class_ "c-h3 u-m-b-0"
          $ "User profile"
        H.div
          ! A.class_ "o-form-group-layout o-form-group-layout--horizontal"
          $ H.form
          $ do
              H.div ! A.class_ "o-form-group" $ do
                H.label
                  ! A.class_ "o-form-group__label"
                  ! A.for "username"
                  $ "Username"
                H.div
                  ! A.class_
                      "o-form-group__controls o-form-group__controls--full-width"
                  $ do
                      H.input
                        ! A.class_ "c-input"
                        ! A.id "username"
                        ! A.name "username"
                        ! A.value
                            ( H.toValue
                            . User._userCredsName
                            . User._userProfileCreds
                            $ profile
                            )
                      H.p
                        ! A.class_ "c-form-help-text"
                        $ "This is your public username"
              H.div ! A.class_ "o-form-group" $ do
                H.label
                  ! A.class_ "o-form-group__label"
                  ! A.for "password"
                  $ "Password"
                H.div
                  ! A.class_
                      "o-form-group__controls o-form-group__controls--full-width"
                  $ H.input
                  ! A.class_ "c-input"
                  ! A.type_ "password"
                  ! A.id "password"
                  ! A.name "password"
              H.div ! A.class_ "o-form-group" $ do
                H.label
                  ! A.class_ "o-form-group__label"
                  ! A.for "display-name"
                  $ "Display name"
                H.div
                  ! A.class_
                      "o-form-group__controls o-form-group__controls--full-width"
                  $ do
                      H.input
                        ! A.class_ "c-input"
                        ! A.id "display-name"
                        ! A.name "display-name"
                        ! A.value
                            (H.toValue . User._userProfileDisplayName $ profile)
                      H.p
                        ! A.class_ "c-form-help-text"
                        $ "This is the name that appears in e.g. your public profile"
              H.div ! A.class_ "o-form-group" $ do
                H.label
                  ! A.class_ "o-form-group__label"
                  ! A.for "email-addr"
                  $ "Email address"
                H.div
                  ! A.class_
                      "o-form-group__controls o-form-group__controls--full-width"
                  $ do
                      H.input
                        ! A.class_ "c-input"
                        ! A.id "email-addr"
                        ! A.name "email-addr"
                        ! A.value
                            (H.toValue . User._userProfileEmailAddr $ profile)
                      H.p
                        ! A.class_ "c-form-help-text"
                        $ "Your email address is private"
              H.div
                ! A.class_ "o-form-group"
                $ H.div
                ! A.class_ "u-spacer-left-auto"
                $ H.button
                ! A.class_ "c-button c-button--primary"
                ! A.formaction (H.toValue submitUrl)
                ! A.formmethod "POST"
                $ H.span
                ! A.class_ "c-button__content"
                $ H.span
                ! A.class_ "c-button__label"
                $ "Update profile"

-- Partial re-creation of
-- https://design.smart.coop/prototypes/old-desk/contract-create-1.html
-- TODO Move to smart-design-hs and refactor.
contractCreate1 =
  H.div
    ! A.class_ "u-scroll-wrapper-body"
    $ H.div
    ! A.class_ "o-container o-container--large"
    $ H.div
    ! A.class_ "o-container-vertical"
    $ H.div
    ! A.class_ "o-container o-container--medium"
    $ H.div
    ! A.class_ "o-form-group-layout o-form-group-layout--horizontal"
    $ do
        H.div ! A.class_ "o-form-group" $ do
          H.label ! A.class_ "o-form-group__label" ! A.for "project" $ "Project"
          H.div
            ! A.class_
                "o-form-group__controls o-form-group__controls--full-width"
            $ do
                H.button
                  ! A.class_ "c-select-custom"
                  ! customAttribute "aria-haspopup"       "listbox"
                  ! customAttribute "data-menu"           "projects"
                  ! customAttribute "data-menu-samewidth" "true"
                  ! customAttribute "aria-expanded"       "false"
                  $ H.div
                  ! A.class_ "c-select-custom__value"
                  $ "Select project"
                H.ul
                  ! A.class_ "c-menu c-menu--select-custom"
                  ! A.role "listbox"
                  ! A.id "projects"
                  $ do
                      H.li
                        ! A.class_ "c-menu__item"
                        ! A.role "option"
                        $ H.div
                        ! A.class_ "c-menu__label"
                        $ "Project 1"
                      H.li
                        ! A.class_ "c-menu__item"
                        ! A.role "option"
                        $ H.div
                        ! A.class_ "c-menu__label"
                        $ "Project 2"
                      H.li
                        ! A.class_ "c-menu__item"
                        ! A.role "option"
                        $ H.div
                        ! A.class_ "c-menu__label"
                        $ "Project 3"
                      H.li ! A.class_ "c-menu__divider" $ ""
                      H.li
                        ! A.class_ "c-menu__item"
                        $ H.div
                        ! A.class_ "c-menu__label"
                        $ do
                            H.div
                              ! A.class_ "o-svg-icon o-svg-icon-add  "
                              $ H.toMarkup svgIconAdd
                            H.span "Add new project"
                H.p
                  ! A.class_ "c-form-help-text"
                  $ "Enter an existing project or create new one"
        H.div ! A.class_ "o-form-group" $ do
          H.label
            ! A.class_ "o-form-group__label"
            ! A.for "description"
            $ "Description"
          H.div
            ! A.class_
                "o-form-group__controls o-form-group__controls--full-width"
            $ do
                H.textarea
                  ! A.class_ "c-textarea"
                  ! A.rows "5"
                  ! A.id "description"
                  $ ""
                H.p
                  ! A.class_ "c-form-help-text"
                  $ "Describe your work (minimum 10 characters)"
        H.div
          ! A.class_ "o-form-group"
          $ H.div
          ! A.class_ "u-spacer-left-auto"
          $ H.button
          ! A.class_ "c-button c-button--primary"
          ! A.type_ "button"
          $ H.span
          ! A.class_ "c-button__content"
          $ do
              H.span ! A.class_ "c-button__label" $ "Next"
              H.div
                ! A.class_ "o-svg-icon o-svg-icon-arrow-right  "
                $ H.toMarkup svgIconArrowRight


newtype ProfileView = ProfileView
  { _profileViewUserProfile :: User.UserProfile
  }

instance H.ToMarkup ProfileView where
  toMarkup (ProfileView profile) =
    Render.renderCanvas
      . Dsl.SingletonCanvas
      $ H.div
      ! A.class_ "c-app-layout"
      $ do
          H.header $ H.toMarkup exampleNavbarAlt
          H.main ! A.class_ "u-maximize-width u-scroll-wrapper" $ profileView
            profile

profileView profile =
  H.div
    ! A.class_ "u-scroll-wrapper-body"
    $ H.div
    ! A.class_ "o-container o-container--large"
    $ H.div
    ! A.class_ "o-container-vertical"
    $ H.div
    ! A.class_ "o-container o-container--medium"
    $ H.div
    ! A.class_ "u-spacer-bottom-xl"
    $ do
        H.div
          ! A.class_ "u-spacer-bottom-l"
          $ H.div
          ! A.class_ "c-navbar c-navbar--unpadded c-navbar--bordered-bottom"
          $ H.div
          ! A.class_ "c-toolbar"
          $ do
              H.div
                ! A.class_ "c-toolbar__left"
                $ H.h3
                ! A.class_ "c-h3 u-m-b-0"
                $ "User profile"
              H.div
                ! A.class_ "c-toolbar__right"
                $ H.a
                ! A.class_ "c-button c-button--secondary"
                ! A.href "/settings/profile/edit"
                $ H.span
                ! A.class_ "c-button__content"
                $ do
                    H.div ! A.class_ "o-svg-icon o-svg-icon-edit" $ H.toHtml
                      svgIconEdit
                    H.span ! A.class_ "c-button__label" $ "Edit"
        H.dl
          ! A.class_ "c-key-value c-key-value--horizontal c-key-value--short"
          $ do
              H.div ! A.class_ "c-key-value-item" $ do
                H.dt ! A.class_ "c-key-value-item__key" $ "Username"
                H.dd
                  ! A.class_ "c-key-value-item__value"
                  $ ( H.toHtml
                    . User._userCredsName
                    . User._userProfileCreds
                    $ profile
                    )
              H.div ! A.class_ "c-key-value-item" $ do
                H.dt ! A.class_ "c-key-value-item__key" $ "Password"
                H.dd ! A.class_ "c-key-value-item__value" $ ""
              H.div ! A.class_ "c-key-value-item" $ do
                H.dt ! A.class_ "c-key-value-item__key" $ "Display name"
                H.dd
                  ! A.class_ "c-key-value-item__value"
                  $ (H.toHtml . User._userProfileDisplayName $ profile)
              H.div ! A.class_ "c-key-value-item" $ do
                H.dt ! A.class_ "c-key-value-item__key" $ "Email address"
                H.dd
                  ! A.class_ "c-key-value-item__value"
                  $ (H.toHtml . User._userProfileEmailAddr $ profile)

-- TODO Move to smart-design-hs and refactor.
contractCreate1Confirm =
  H.div
    ! A.class_ "u-scroll-wrapper-body"
    $ H.div
    ! A.class_ "o-container o-container--large"
    $ H.div
    ! A.class_ "o-container-vertical"
    $ H.div
    ! A.class_ "o-container o-container--medium"
    $ H.div
    ! A.class_ "u-spacer-bottom-xl"
    $ do
        H.div
          ! A.class_ "u-spacer-bottom-l"
          $ H.div
          ! A.class_ "c-navbar c-navbar--unpadded c-navbar--bordered-bottom"
          $ H.div
          ! A.class_ "c-toolbar"
          $ do
              H.div
                ! A.class_ "c-toolbar__left"
                $ H.h3
                ! A.class_ "c-h3 u-m-b-0"
                $ "General information"
              H.div
                ! A.class_ "c-toolbar__right"
                $ H.a
                ! A.class_ "c-button c-button--secondary"
                ! A.href "#"
                $ H.span
                ! A.class_ "c-button__content"
                $ do
                    H.div ! A.class_ "o-svg-icon o-svg-icon-edit" $ H.toHtml
                      svgIconEdit
                    H.span ! A.class_ "c-button__label" $ "Edit"
        H.dl
          ! A.class_ "c-key-value c-key-value--horizontal c-key-value--short"
          $ do
              H.div ! A.class_ "c-key-value-item" $ do
                H.dt ! A.class_ "c-key-value-item__key" $ "Worker"
                H.dd ! A.class_ "c-key-value-item__value" $ "Manfred"
              H.div ! A.class_ "c-key-value-item" $ do
                H.dt ! A.class_ "c-key-value-item__key" $ "Work setting"
                H.dd
                  ! A.class_ "c-key-value-item__value"
                  $ "The work is mainly performed in places and at times freely chosen"
              H.div ! A.class_ "c-key-value-item" $ do
                H.dt ! A.class_ "c-key-value-item__key" $ "Compensation budget"
                H.dd ! A.class_ "c-key-value-item__value" $ "1000.00 EUR"
              H.div ! A.class_ "c-key-value-item" $ do
                H.dt ! A.class_ "c-key-value-item__key" $ "Project"
                H.dd ! A.class_ "c-key-value-item__value" $ "Unspecified"
              H.div ! A.class_ "c-key-value-item" $ do
                H.dt ! A.class_ "c-key-value-item__key" $ "Description"
                H.dd ! A.class_ "c-key-value-item__value" $ "Some description."


data ProfileSaveConfirmPage = ProfileSaveSuccess
                            | ProfileSaveFailure (Maybe Text)
                            deriving Show

instance H.ToMarkup ProfileSaveConfirmPage where
  toMarkup = \case
    ProfileSaveSuccess      -> "All done, you can now go back."
    ProfileSaveFailure mmsg -> do
      H.text "We had a problem saving your data."
      maybe mempty (H.text . mappend "Reason: ") mmsg
