{- |
Module: Curiosity.Html.Action
Description: User actions. Actions are supposed to match what is available
under e.g. `cty user do` and the action menu on table views.
-}
module Curiosity.Html.Action
  ( SetUserEmailAddrAsVerifiedPage(..)
  , SetQuotationAsSignedPage(..)
  , ActionResult(..)
  , EchoPage(..)
  , EchoPage'(..)
  ) where

import qualified Curiosity.Data.Quotation      as Quotation
import qualified Curiosity.Data.User           as User
import           Curiosity.Html.Misc
import qualified Smart.Html.Dsl                as Dsl
import           Smart.Html.Panel               ( Panel(..) )
import qualified Smart.Html.Render             as Render
import qualified Smart.Html.Shared.Types       as Types
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A


--------------------------------------------------------------------------------
-- | A form to perform the SetUserEmailAddrAsVerified action.
data SetUserEmailAddrAsVerifiedPage = SetUserEmailAddrAsVerifiedPage
  { setUserEmailAddrAsVerifiedForUser :: User.UserProfile
    -- ^ The user whose email address should be set as verified.
  }

instance H.ToMarkup SetUserEmailAddrAsVerifiedPage where
  toMarkup SetUserEmailAddrAsVerifiedPage {..} =
    let profile  = setUserEmailAddrAsVerifiedForUser
        username = User._userCredsName . User._userProfileCreds $ profile
    in  fullScrollWrapper . panelWrapper $ do
          H.toMarkup $ setUserEmailAddrAsVerifiedPanel username profile
          setUserEmailAddrAsVerifiedForm username

fullScrollWrapper content =
  Render.renderCanvasFullScroll
    . Dsl.SingletonCanvas
    $ H.div
    ! A.class_ "c-app-layout u-scroll-vertical"
    $ content

panelWrapper content = H.main ! A.class_ "u-scroll-wrapper" $ do
  H.div
    ! A.class_ "o-container o-container--large"
    $ H.div
    ! A.class_ "o-container-vertical"
    $ H.div
    ! A.class_ "u-padding-vertical-l"
    $ content

setUserEmailAddrAsVerifiedPanel username profile =
  PanelHeaderAndBody "Set email address as verified"
    $ H.dl
    ! A.class_ "c-key-value c-key-value--horizontal c-key-value--short"
    $ do
        keyValuePair "Username" username
        maybe mempty
              (keyValuePair "Display name")
              (User._userProfileDisplayName profile)
        keyValuePair "Email address" (User._userProfileEmailAddr profile)

setUserEmailAddrAsVerifiedForm username = H.form $ do
  H.input ! A.type_ "hidden" ! A.id "username" ! A.name "username" ! A.value
    (H.toValue username)
  button "/a/set-email-addr-as-verified" "Set as verified"


--------------------------------------------------------------------------------
-- | A form to perform the SetQuotationAsSigned action.
data SetQuotationAsSignedPage = SetQuotationAsSignedPage
  { setQuotationAsSignedQuotation :: Quotation.Quotation
    -- ^ The quotation to be set as signed.
  }

instance H.ToMarkup SetQuotationAsSignedPage where
  toMarkup SetQuotationAsSignedPage {..} =
    let id = Quotation._quotationId setQuotationAsSignedQuotation
    in  fullScrollWrapper . panelWrapper $ do
          H.toMarkup $ setQuotationAsSignedPanel id
          setQuotationAsSignedForm id

setQuotationAsSignedPanel id =
  PanelHeaderAndBody "Set quotation as signed"
    $ H.dl
    ! A.class_ "c-key-value c-key-value--horizontal c-key-value--short"
    $ do
        keyValuePair "ID" id

setQuotationAsSignedForm id = H.form $ do
  H.input ! A.type_ "hidden" ! A.id "quotation-id" ! A.name "quotation-id" ! A.value
    (H.toValue id)
  button "/a/set-quotation-as-signed" "Set as signed"


--------------------------------------------------------------------------------
data ActionResult = ActionResult Text Text

instance H.ToMarkup ActionResult where
  toMarkup (ActionResult title_ msg) = fullScrollWrapper . panelWrapper $ do
    H.toMarkup $ actionResultPanel title_ msg

actionResultPanel title_ msg =
  PanelHeaderAndBody (Types.Title title_) $ H.code $ H.text msg


--------------------------------------------------------------------------------
data EchoPage = EchoPage
  { _echoPageUserProfile :: (Maybe User.UserProfile)
    -- ^ The logged in user, if any
  , _echoPageContent     :: Text
    -- ^ Text, displayed as code
  }

instance H.ToMarkup EchoPage where
  toMarkup EchoPage {..} =
    renderView' _echoPageUserProfile $
      panelWrapper
        $ H.div ! A.class_ "c-display" $
          H.pre . H.code $ H.text _echoPageContent

-- | Similar to `EchoPage` but also shows validation errors
data EchoPage' = EchoPage'
  { _echoPage'UserProfile :: (Maybe User.UserProfile)
    -- ^ The logged in user, if any
  , _echoPage'Content     :: Text
    -- ^ Text, displayed as code
  , _echoPage'Errors      :: [Text]
  }

instance H.ToMarkup EchoPage' where
  toMarkup EchoPage' {..} =
    renderView' _echoPage'UserProfile $
      panelWrapper
        $ H.div ! A.class_ "c-display" $ do
          H.pre . H.code $ H.text _echoPage'Content
          if null _echoPage'Errors
            then
              H.pre . H.code $ "Valid."
            else
              H.pre . H.code . H.text $ unlines _echoPage'Errors
