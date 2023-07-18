{- |
Module: Curiosity.Html.Legal
Description: Legal entity pages (view and edit).
-}
module Curiosity.Html.Legal
  ( EntityView(..)
  , CreateEntityPage(..)
  ) where

import qualified Curiosity.Types.Legal         as Legal
import qualified Curiosity.Types.User          as User
import           Curiosity.Html.Misc
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A


--------------------------------------------------------------------------------
data EntityView = EntityView
  { _entityViewUser          :: Maybe User.UserProfile
    -- ^ The logged-in user, if any.
  , _entityViewEntity        :: Legal.Entity
  , _entityViewUsersAndRoles :: [Legal.ActingUser]
    -- ^ The resolved users for `_entityUsersAndRoles`.
  , _entityViewHasEditButton :: Maybe H.AttributeValue
  }

instance H.ToMarkup EntityView where
  toMarkup (EntityView mprofile entity users hasEditButton) =
    renderView' mprofile $ entityView entity users hasEditButton

entityView entity users hasEditButton = containerMedium $ do
  title' "Legal entity" hasEditButton
  H.dl ! A.class_ "c-key-value c-key-value--horizontal c-key-value--short" $ do
    keyValuePair "ID"                (Legal._entityId entity)
    keyValuePair "Registration name" (Legal._entityName entity)
    keyValuePair "Country"           ("BE (TODO)" :: Text) -- TODO
    keyValuePair "CBE number"        (Legal._entityCbeNumber entity)
    keyValuePair "VAT number"        (Legal._entityVatNumber entity)
    maybe mempty (keyValuePair "Description") (Legal._entityDescription entity)
    keyValuePair "Supervised" (Legal._entityIsSupervised entity)
    keyValuePair "Host" (Legal._entityIsHost entity)

  title' "Authorizations" Nothing
  H.ul $ mapM_ displayAuthorization $ Legal._entityAuthorizations entity

  title' "Acting users" Nothing
  H.ul $ mapM_ displayActingUser users

displayAuthorization auth =
  H.li $ do
    H.code . H.text $ show auth

displayActingUser (Legal.ActingUser u role) =
  H.dl ! A.class_ "c-key-value c-key-value--horizontal c-key-value--short" $ do
    keyValuePair (show role) (H.a ! A.href (H.toValue $ "/" <> username) $ H.text username)

  where username = User.unUserName . User._userCredsName . User._userProfileCreds $ u


--------------------------------------------------------------------------------
data CreateEntityPage = CreateEntityPage
  { _createEntityPageUserProfile :: User.UserProfile
    -- ^ The user creating the entity
  , _createEntityPageSubmitURL   :: H.AttributeValue
  }

instance H.ToMarkup CreateEntityPage where
  toMarkup (CreateEntityPage profile submitUrl) =
    renderForm profile $ groupLayout $ do
      title "New legal entity"
      inputText "Registration name" "name" Nothing Nothing
      inputText "CBE number" "cbe-number" Nothing $ Just "Example: 100200300"
      inputText "VAT number" "vat-number" Nothing $ Just "Example: BE0100200300"
      submitButton submitUrl "Create new legal entity"
