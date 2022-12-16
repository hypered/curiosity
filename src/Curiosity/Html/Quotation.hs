{- |
Module: Curiosity.Html.Quotation
Description: Quotation pages (view and edit).
-}
module Curiosity.Html.Quotation
  ( QuotationPage(..)
  , panelQuotations
  , QuotationView(..)
  , CreateQuotationPage(..)
  , ConfirmQuotationPage(..)
  ) where

import qualified Curiosity.Data.Quotation      as Quotation
import qualified Curiosity.Data.User           as User
import           Curiosity.Html.Misc
import qualified Smart.Html.Alert              as Alert
import qualified Smart.Html.Button             as Button
import qualified Smart.Html.Misc               as Misc
import           Smart.Html.Panel               ( Panel(..) )
import           Smart.Html.Shared.Types        ( Body(..) )
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!), Html )
import qualified Text.Blaze.Html5.Attributes   as A


--------------------------------------------------------------------------------
-- | The page displaye at @/quotations@ to show all quotations.
data QuotationPage = QuotationPage
  { _emailPageUser   :: Maybe User.UserProfile
    -- ^ The logged-in user, if any.
  , _emailPageQuotations :: [Quotation.Quotation]
    -- ^ All enqueued emails.
  }

instance H.ToMarkup QuotationPage where
  toMarkup (QuotationPage mprofile emails) =
    renderView' mprofile $ panelQuotations emails


--------------------------------------------------------------------------------
-- | Display quotations.
panelQuotations :: [Quotation.Quotation] -> Html
panelQuotations quotations =
  panel' "Quotations" $ Misc.table "quotations" titles display quotations
 where
  titles = ["ID", "State"]
  display Quotation.Quotation {..} =
    ( [ Quotation.unQuotationId _quotationId
      , Quotation.displayQuotationState  _quotationState
      ]
    , if _quotationState == Quotation.QuotationSent
      then [ ( Misc.divIconCheck
             , "Set as signed"
             , "/action/set-quotation-as-signed/" <> Quotation.unQuotationId _quotationId
             )
           , ( Misc.divIconClose
             , "Set as rejected"
             , "/action/set-quotation-as-rejected/" <> Quotation.unQuotationId _quotationId
             )
           ]
      else []
    , Nothing
    )


--------------------------------------------------------------------------------
data QuotationView = QuotationView
  { _quotationViewQuotation     :: Quotation.Quotation
  , _quotationViewHasEditButton :: Maybe H.AttributeValue
  }

instance H.ToMarkup QuotationView where
  toMarkup (QuotationView quotation hasEditButton) =
    renderView $ quotationView quotation hasEditButton

quotationView quotation hasEditButton = containerLarge $ do
  title' "Quotation" hasEditButton
  H.dl ! A.class_ "c-key-value c-key-value--horizontal c-key-value--short" $ do
    keyValuePair "ID" (Quotation._quotationId quotation)


--------------------------------------------------------------------------------
data CreateQuotationPage = CreateQuotationPage
  { _createQuotationPageUserProfile :: User.UserProfile
    -- ^ The user creating the quotation
  , _createQuotationPageKey         :: Maybe Text
    -- ^ The form editing session key, if the form was already saved
  , _createQuotationPageQuotation   :: Quotation.CreateQuotationAll
  , _createQuotationPageSubmitURL   :: H.AttributeValue
  }

instance H.ToMarkup CreateQuotationPage where
  toMarkup (CreateQuotationPage profile mkey quotation submitUrl) =
    renderForm profile $ groupLayout $ do
      title "New quotation"
      inputText "Client username" "client-username"
        (H.toValue . User.unUserName <$> Quotation._createQuotationClientUsername quotation) Nothing
      submitButton submitUrl
        $ maybe "Create new quotation" (const "Save quotation") mkey


--------------------------------------------------------------------------------
data ConfirmQuotationPage = ConfirmQuotationPage
  { _confirmQuotationPageUserProfile :: User.UserProfile
    -- ^ The user creating the quotation
  , _confirmQuotationPageKey         :: Text
  , _confirmQuotationPageQuotation   :: Quotation.CreateQuotationAll
  , _confirmQuotationPageErrors      :: [Quotation.Err]
    -- Validation errors, if any.
  , _confirmQuotationPageEditURL     :: Maybe H.AttributeValue
  , _confirmQuotationPageSubmitURL   :: H.AttributeValue
  }

instance H.ToMarkup ConfirmQuotationPage where
  toMarkup (ConfirmQuotationPage profile key (Quotation.CreateQuotationAll {..}) errors meditUrl submitUrl)
    = renderFormLarge profile $ do
      title' "New quotation" meditUrl

      validationErrors errors

      H.div
        ! A.class_ "u-padding-vertical-l"
        $ H.toMarkup
        $ PanelHeaderAndBody "XXX"
        $ H.dl
        ! A.class_ "c-key-value c-key-value--horizontal c-key-value--short"
        $ do
            keyValuePair "Client username" $ maybe "" User.unUserName _createQuotationClientUsername

      H.input ! A.type_ "hidden" ! A.id "key" ! A.name "key" ! A.value
        (H.toValue key)

      buttonGroup $ do
        -- TODO Add edit button
        let label = "Submit quotation"
        if null errors
          then buttonPrimary submitUrl label
          else buttonPrimaryDisabled label

validationErrors errors =
  if null errors
  then mempty
  else
    H.div ! A.class_ "u-spacer-bottom-l" $ H.toMarkup $ Alert.Alert
      Alert.AlertError
      iconError
      (Body $ "Validation errors: " <> show (map Quotation.unErr errors))
      Button.NoButton
