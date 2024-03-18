-- |
--Module: Curiosity.Html.Homepage
--Description: The homepage for Curiosity (when a user is logged in).
module Curiosity.Html.Homepage
  ( WelcomePage (..)
  ) where

import Curiosity.Html.Email
import Curiosity.Html.Misc
import Curiosity.Html.Order (panelOrders)
import Curiosity.Html.Quotation (panelQuotations)
import Curiosity.Types.Email qualified as Email
import Curiosity.Types.Order qualified as Order
import Curiosity.Types.Quotation qualified as Quotation
import Curiosity.Types.User qualified as User
import Smart.Html.Dsl qualified as Dsl
import Smart.Html.Misc qualified as Misc
import Smart.Html.Render qualified as Render
import Text.Blaze.Html5 (Html, (!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A

--------------------------------------------------------------------------------

-- | A simple welcome page.
data WelcomePage = WelcomePage
  { welcomePageUser :: User.UserProfile
  -- ^ The logged in user.
  , welcomePageEmailAddrToVerify :: Maybe [User.UserProfile]
  -- ^ Email addr. needing verif., if the user has the right to perform the
  -- corresponding action.
  , welcomePageQuotationForms :: [(Text, Quotation.CreateQuotationAll)]
  , welcomePageQuotations :: [Quotation.Quotation]
  , welcomePageOrders :: [Order.Order]
  , welcomePageEmails :: [Email.Email]
  -- ^ Enqueued emails being sent to the logged user.
  }

instance H.ToMarkup WelcomePage where
  toMarkup WelcomePage {..} =
    Render.renderCanvasFullScroll
      . Dsl.SingletonCanvas
      $ H.div
        ! A.class_ "c-app-layout u-scroll-vertical"
      $ do
        header $ Just welcomePageUser
        H.main ! A.class_ "u-scroll-wrapper" $ do
          maybe (pure ()) panelEmailAddrToVerify welcomePageEmailAddrToVerify

          panelQuotationForms welcomePageQuotationForms

          panelQuotations welcomePageQuotations

          panelOrders welcomePageOrders

          panelSentEmails welcomePageEmails

panelEmailAddrToVerify :: [User.UserProfile] -> Html
panelEmailAddrToVerify profiles =
  panel' "Email addresses to verify" $ Misc.table "addr" titles display profiles
 where
  titles = ["ID", "Username", "Email addr."]
  display User.UserProfile {..} =
    let User.UserId i = _userProfileId
        User.UserName n = User._userCredsName _userProfileCreds
        User.UserEmailAddr e = _userProfileEmailAddr
     in ( [i, n, e]
        ,
          [
            ( Misc.divIconCheck
            , "Set as verified"
            , "/action/set-email-addr-as-verified/" <> n
            )
          ]
        , (Just $ "/" <> n)
        )

panelQuotationForms :: [(Text, Quotation.CreateQuotationAll)] -> Html
panelQuotationForms forms =
  panel' "Quotation forms" $ Misc.table "quotation-forms" titles display forms
 where
  titles = ["Key", "Client"]
  display (key, Quotation.CreateQuotationAll {..}) =
    ( [key, maybe "" User.unUserName _createQuotationClientUsername]
    , []
    , (Just $ "/edit/quotation/confirm/" <> key)
    )
