{- |
Module: Curiosity.Html.Business
Description: Business unit pages (view and edit).
-}
module Curiosity.Html.Business
  ( UnitView(..)
  , CreateUnitPage(..)
  ) where

import qualified Curiosity.Types.Business      as Business
import qualified Curiosity.Types.User          as User
import           Curiosity.Html.Misc
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A


--------------------------------------------------------------------------------
data UnitView = UnitView
  { _unitViewUser          :: Maybe User.UserProfile
    -- ^ The logged-in user, if any.
  , _unitViewUnit          :: Business.Unit
  , _unitViewHasEditButton :: Maybe H.AttributeValue
  }

instance H.ToMarkup UnitView where
  toMarkup (UnitView mprofile unit hasEditButton) =
    renderView' mprofile $ unitView unit hasEditButton

unitView unit hasEditButton = containerMedium $ do
  title' "Business unit" hasEditButton
  H.dl ! A.class_ "c-key-value c-key-value--horizontal c-key-value--short" $ do
    keyValuePair "ID"   (Business._entityId unit)
    keyValuePair "Name" (Business._entityName unit)
    maybe mempty
          (keyValuePair "Description" . linkifyAts)
          (Business._entityDescription unit)
    keyValuePair "Type (industry class)" (Business._entityType unit)

    title' "Holders" Nothing
    H.ul $ mapM_ displayHolder $ Business._entityHolders unit

    title' "Authorizations" Nothing
    H.ul $ mapM_ displayAuthorization $ Business._entityAuthorizations unit

    title' "Scopes" Nothing
    H.ul $ mapM_ displayScope $ Business._entityScopes unit

displayHolder holder = H.li $ do
  H.code . H.text $ User.unUserId holder

displayAuthorization auth = H.li $ do
  H.code . H.text $ show auth

displayScope scope = H.li $ do
  H.code . H.text $ show scope

--------------------------------------------------------------------------------
data CreateUnitPage = CreateUnitPage
  { _createUnitPageUserProfile :: User.UserProfile
    -- ^ The user creating the unit
  , _createUnitPageSubmitURL   :: H.AttributeValue
  }

instance H.ToMarkup CreateUnitPage where
  toMarkup (CreateUnitPage profile submitUrl) =
    renderForm profile $ groupLayout $ do
      title "New business unit"
      inputText "Unit name" "name" Nothing Nothing
      submitButton submitUrl "Create new business unit"
