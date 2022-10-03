{- |
Module: Curiosity.Html.User
Description: Profile pages (view and edit).
-}
module Curiosity.Html.User
  ( ProfilePage(..)
  , ProfileView(..)
  , PublicProfileView(..)
  ) where

import qualified Curiosity.Data.Legal          as Legal
import qualified Curiosity.Data.User           as User
import           Curiosity.Html.Misc
import           Curiosity.Html.Navbar          ( navbar )
import qualified Smart.Html.Dsl                as Dsl
import           Smart.Html.Layout
import qualified Smart.Html.Misc               as Misc
import qualified Smart.Html.Render             as Render
import           Smart.Html.Shared.Html.Icons   ( svgIconAdd
                                                , svgIconArrowRight
                                                )
import           Smart.Html.SideMenu            ( SideMenu(..)
                                                , SideMenuItem(..)
                                                )
import           Text.Blaze                     ( customAttribute )
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A


--------------------------------------------------------------------------------
data ProfilePage = ProfilePage
  { _profilePageUserProfile      :: User.UserProfile
  , _profilePageSubmitURL        :: H.AttributeValue
  }

instance H.ToMarkup ProfilePage where
  toMarkup (ProfilePage profile submitUrl) =
    renderForm profile $ groupLayout $ do
      title "User profile"
      disabledText
          "Username"
          "username"
          ( Just
          . H.toValue
          . User._userCredsName
          . User._userProfileCreds
          $ profile
          )
        $ Just "This is your username. It can not be changed."
      disabledText "Password" "password" (Just "") Nothing
      inputText
          "Display name"
          "display-name"
          ( Just
          . H.toValue
          . maybe "" identity
          . User._userProfileDisplayName
          $ profile
          )
        $ Just
            "This is the name that appears in e.g. your public profile and can be left empty if you prefer."
      Misc.inputTextarea "bio"
                         "Bio"
                         6
                         "The bio appears on your public profile"
                         (maybe "" identity . User._userProfileBio $ profile)
                         True
      disabledText "Email address"
                   "email-addr"
                   (Just . H.toValue . User._userProfileEmailAddr $ profile)
        $ Just "Your email address is private."
      submitButton submitUrl "Update profile"

-- Partial re-creation of
-- https://design.smart.coop/prototypes/old-desk/contract-create-1.html
-- TODO Move to smart-design-hs and refactor.
contractCreate1 =
  containerMedium
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


data ProfileView = ProfileView
  { _profileViewUserProfile   :: User.UserProfile
  , _profileViewEntitiesAndRoles :: [Legal.EntityAndRole]
  , _profileViewHasEditButton :: Maybe H.AttributeValue
  }

instance H.ToMarkup ProfileView where
  toMarkup (ProfileView profile entities hasEditButton) =
    Render.renderCanvasFullScroll
      . Dsl.SingletonCanvas
      $ H.div
      ! A.class_ "c-app-layout u-scroll-vertical"
      $ do
          H.header
            $ H.toMarkup
            . navbar
            . User.unUserName
            . User._userCredsName
            $ User._userProfileCreds profile
          withSideMenuFullScroll menu $ profileView profile entities hasEditButton

menu :: SideMenu
menu = SideMenuWithActive []
                          (SideMenuItem "User profile" "/settings/profile")
                          [SideMenuItem "Dummy" "/settings/dummy"]

profileView profile entities hasEditButton =
  containerMedium $ H.div ! A.class_ "u-spacer-bottom-xl" $ do
    title' "User profile" hasEditButton
    H.dl
      ! A.class_ "c-key-value c-key-value--horizontal c-key-value--short"
      $ do
          keyValuePair
            "Username"
            (User._userCredsName . User._userProfileCreds $ profile)
          keyValuePair @Text "Password" ""
          keyValuePair "Display name"
            $ maybe "" identity (User._userProfileDisplayName profile)
          keyValuePair "Bio" $ maybe "" identity (User._userProfileBio profile)
          keyValuePair "Email address" (User._userProfileEmailAddr profile)
          keyValuePair
            "Email addr. verified"
            (show $ User._userProfileEmailAddrVerified profile :: Text)
          keyValuePair "TOS consent" (User._userProfileTosConsent profile)

          keyValuePair
            "Address"
            (show
            . User._userProfilePostalAddress
            . User._userProfileCompletion1
            $ profile :: Text
            )
          keyValuePair
            "Telephone number"
            (show
            . User._userProfileTelephoneNbr
            . User._userProfileCompletion1
            $ profile :: Text
            )
          keyValuePair
            "Addr. and tel. verified"
            (show
            . User._userProfileAddrAndTelVerified
            . User._userProfileCompletion1
            $ profile :: Text
            )

          keyValuePair
            "EID"
            (show
            . User._userProfileEId
            . User._userProfileCompletion2
            $ profile :: Text
            )
          keyValuePair
            "EID verified"
            (show
            . User._userProfileEIdVerified
            . User._userProfileCompletion2
            $ profile :: Text
            )
          keyValuePair "Rights"
                       (show . User._userProfileRights $ profile :: Text)
    title' "Related entities" Nothing
    H.ul $ mapM_ displayEntities entities

displayEntities (Legal.EntityAndRole entity role) =
  H.li $ do
    H.a ! A.href (H.toValue $ "/entity/" <> Legal._entitySlug entity) $
      H.text . Legal.unRegistrationName $ Legal._entityName entity
    H.code . H.text $ show role

data PublicProfileView = PublicProfileView
  { _publicProfileViewUserProfile   :: (Maybe User.UserProfile)
    -- ^ The logged in user, if any
  , _publicProfileViewTargetProfile :: User.UserProfile
    -- ^ The profile being displayed
  }

instance H.ToMarkup PublicProfileView where
  toMarkup (PublicProfileView mprofile targetProfile) =
    Render.renderCanvasFullScroll
      . Dsl.SingletonCanvas
      $ H.div
      ! A.class_ "c-app-layout u-scroll-vertical"
      $ do
          header mprofile
          H.main ! A.class_ "u-maximize-width" $ publicProfileView targetProfile

publicProfileView profile =
  containerMedium $ H.div ! A.class_ "u-spacer-bottom-xl" $ do
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
            $ "Public profile"
    H.dl
      ! A.class_ "c-key-value c-key-value--horizontal c-key-value--short"
      $ do
          keyValuePair
            "Username"
            (User._userCredsName . User._userProfileCreds $ profile)
          maybe mempty
                (keyValuePair "Display name")
                (User._userProfileDisplayName profile)
          maybe mempty (keyValuePair "Bio") (User._userProfileBio profile)

-- TODO Move to smart-design-hs and refactor.
contractCreate1Confirm =
  containerMedium $ H.div ! A.class_ "u-spacer-bottom-xl" $ do
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
          editButton "#"
    H.dl
      ! A.class_ "c-key-value c-key-value--horizontal c-key-value--short"
      $ do
          keyValuePair @Text "Worker" "Manfred"
          keyValuePair @Text
            "Work setting"
            "The work is mainly performed in places and at times freely chosen"
          keyValuePair @Text "Compensation budget" "1000.00 EUR"
          keyValuePair @Text "Project" "Unspecified"
          keyValuePair @Text "Description" "Some description."