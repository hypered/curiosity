{- |
Module: Curiosity.Html.User
Description: Profile pages (view and edit).
-}
module Curiosity.Html.User
  ( EditProfilePage(..)
  , ProfileView(..)
  , PublicProfileView(..)
  ) where

import qualified Curiosity.Data.Legal          as Legal
import qualified Curiosity.Data.User           as User
import           Curiosity.Html.Misc
import           Curiosity.Html.Navbar          ( navbar )
import qualified Data.Text                     as T
import           Data.Time.Clock.POSIX          ( posixSecondsToUTCTime )
import           Data.Time.Format.ISO8601       ( iso8601Show )
import qualified Smart.Html.Dsl                as Dsl
import           Smart.Html.Layout
import qualified Smart.Html.Misc               as Misc
import qualified Smart.Html.Render             as Render
import           Smart.Html.SideMenu            ( SideMenu(..)
                                                , SideMenuItem(..)
                                                )
import           System.PosixCompat.Types       ( EpochTime )
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A

--------------------------------------------------------------------------------
data EditProfilePage = EditProfilePage
  { _profilePageUserProfile :: User.UserProfile
  , _profilePageSubmitURL   :: H.AttributeValue
  }

instance H.ToMarkup EditProfilePage where
  toMarkup (EditProfilePage profile submitUrl) =
    renderForm profile $ groupLayout $ do
      title "User profile"
      H.input
        ! A.type_ "hidden"
        ! A.id "user-id"
        ! A.name "user-id"
        ! A.value (H.toValue $ User._userProfileId profile)
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
      disabledText
          "Registered since"
          "registered-since"
          ( Just
          . H.toValue
          . formatEpochIso8601
          . User._userProfileRegistrationDate
          $ profile
          )
        $ Nothing
      inputText
          "Twitter username"
          "twitter-username"
          ( Just
          . H.toValue
          . maybe "" identity
          . User._userProfileTwitterUsername
          $ profile
          )
        $ Nothing
      submitButton submitUrl "Update profile"


--------------------------------------------------------------------------------
data ProfileView = ProfileView
  { _profileViewUserProfile      :: User.UserProfile
  , _profileViewEntitiesAndRoles :: [Legal.EntityAndRole]
  , _profileViewHasEditButton    :: Maybe H.AttributeValue
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
          withSideMenuFullScroll menu
            $ profileView profile entities hasEditButton

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
          keyValuePair "Bio"
            $ maybe "" linkifyAts (User._userProfileBio profile)
          keyValuePair
            "Email address"
            (linkEmail . User.unUserEmailAddr $ User._userProfileEmailAddr
              profile
            )
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
          keyValuePair
            "Registered since"
            ( H.toHtml
            . formatEpochIso8601
            . User._userProfileRegistrationDate
            $ profile
            )
          maybe mempty
                (keyValuePair "Twitter" . linkTwitter)
                (User._userProfileTwitterUsername profile)
          keyValuePair "Rights"
                       (show . User._userProfileRights $ profile :: Text)

    title' "Authorizations" Nothing
    H.ul $ mapM_ displayAuthorization $ User._userProfileAuthorizations profile

    title' "Advisors" Nothing
    displayAdvisors $ User._userProfileAdvisors profile

    title' "Related entities" Nothing
    H.ul $ mapM_ displayEntitie entities

displayAuthorization auth = H.li $ do
  H.code . H.text $ show auth

displayAdvisors Nothing                     = "No current or past advisors."

displayAdvisors (Just (User.Advisors {..})) = do
  "Current advisor: "
  H.text . User.unUserId $ _userAdvisorsCurrent
  "Past advisors:"
  H.ul $ mapM_
    (\(since, till, id) -> H.li
      (  H.text (User.unUserId id)
      >> H.text (" (From " <> show since <> ", to " <> show till <> ")")
      )
    )
    _userAdvisorsPast

displayEntitie (Legal.EntityAndRole entity role) = H.li $ do
  H.a
    ! A.href (H.toValue $ "/entity/" <> Legal._entitySlug entity)
    $ H.text
    . Legal.unRegistrationName
    $ Legal._entityName entity
  H.code . H.text $ show role


--------------------------------------------------------------------------------
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
          maybe mempty
                (keyValuePair "Bio" . linkifyAts)
                (User._userProfileBio profile)
          keyValuePair
            "Registered since"
            ( H.toHtml
            . formatEpochIso8601
            . User._userProfileRegistrationDate
            $ profile
            )
          maybe mempty
                (keyValuePair "Twitter" . linkTwitter)
                (User._userProfileTwitterUsername profile)

    title' "Advisors" Nothing
    displayAdvisors $ User._userProfileAdvisors profile


formatEpochIso8601 :: EpochTime -> Text
formatEpochIso8601 = T.pack . iso8601Show . posixSecondsToUTCTime . realToFrac
