{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}

-- |
--Module: Curiosity.Types.Legal
--Description: Legal entities related datatypes
module Curiosity.Types.Legal
  ( Entity (..)
  , Create (..)
  , Update (..)
  , EntityId (..)
  , RegistrationName (..)
  , ActingUserId (..)
  , ActingRole (..)
  , ActingUser (..)
  , EntityAndRole (..)
  , Authorization (..)
  , Err (..)
  , encodeUBL
  , toUBL
  ) where

import Commence.Types.Wrapped qualified as W
import Curiosity.Types.PrefixedId qualified as Pre
import Curiosity.Types.User qualified as User
import Data.Aeson
import Data.ByteString.Lazy qualified as LB
import Text.Blaze.Html5 qualified as H
import Web.FormUrlEncoded
  ( FromForm (..)
  , parseMaybe
  , parseUnique
  )
import Web.HttpApiData (FromHttpApiData (..))

--------------------------------------------------------------------------------
data Create = Create
  { _createSlug :: Text
  , _createName :: RegistrationName
  , _createCbeNumber :: CbeNumber
  , _createVatNumber :: VatNumber -- TODO Is it really useful to habe both ?
  }
  deriving (Generic, Eq, Show)

instance FromForm Create where
  fromForm f =
    Create
      <$> parseUnique "slug" f
      <*> parseUnique "name" f
      <*> parseUnique "cbe-number" f
      <*> parseUnique "vat-number" f

-- | Represents the input data to update an entity profile.
data Update = Update
  { _updateSlug :: Text
  , _updateDescription :: Maybe Text
  }
  deriving (Eq, Show, Generic)

instance FromForm Update where
  fromForm f = Update <$> parseUnique "slug" f <*> parseMaybe "description" f

--------------------------------------------------------------------------------
data Entity = Entity
  { _entityId :: EntityId
  , _entitySlug :: Text
  -- ^ An identifier suitable for URLs (unique across entities).
  , _entityName :: RegistrationName
  , _entityCbeNumber :: CbeNumber
  , _entityVatNumber :: VatNumber -- TODO Is it really useful to habe both ?
  , _entityDescription :: Maybe Text
  -- ^ Public description. Similar to a user profile bio.
  , _entityUsersAndRoles :: [ActingUserId]
  -- ^ Users that can "act" within the entity, with some kind of associated rights.
  , _entityAuthorizations :: [Authorization]
  , _entityIsSupervised :: Bool
  -- ^ Whether the entity has its accounting done by the group operating
  -- Curiosity. Maybe this should be something like
  -- IsSupervisedBy :: Maybe GroupId.
  , _entityIsHost :: Bool
  -- ^ Whether the entity can hosts contracts from business units.
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Record ID of the form LENT-xxx.
newtype EntityId = EntityId {unEntityId :: Text}
  deriving (Eq, Show)
  deriving
    ( IsString
    , FromJSON
    , ToJSON
    , H.ToMarkup
    , H.ToValue
    )
    via Text
  deriving (FromForm) via W.Wrapped "legal-id" Text
  deriving (Pre.PrefixedId) via W.Wrapped "LENT-" Text

-- | A registation name.
newtype RegistrationName = RegistrationName {unRegistrationName :: Text}
  deriving
    ( Eq
    , Show
    , IsString
    , FromJSON
    , ToJSON
    , H.ToMarkup
    , H.ToValue
    )
    via Text
  deriving (FromHttpApiData, FromForm) via W.Wrapped "name" Text

-- | CBE number (without BE or leading zero).
newtype CbeNumber = CbeNumber {unCbeNumber :: Text}
  deriving (Eq, Show)
  deriving
    ( IsString
    , FromJSON
    , ToJSON
    , H.ToMarkup
    , H.ToValue
    )
    via Text
  deriving (FromHttpApiData, FromForm) via W.Wrapped "cbe-number" Text

-- | VAT number (with BE and leading zero).
newtype VatNumber = VatNumber {unVatNumber :: Text}
  deriving (Eq, Show)
  deriving
    ( IsString
    , FromJSON
    , ToJSON
    , H.ToMarkup
    , H.ToValue
    )
    via Text
  deriving (FromHttpApiData, FromForm) via W.Wrapped "vat-number" Text

data ActingUserId = ActingUserId User.UserId ActingRole
  deriving (Eq, Generic, Show)
  deriving (FromJSON, ToJSON)

data ActingRole = Titular | Director | Administrator | Validator
  deriving (Eq, Generic, Show)
  deriving (FromJSON, ToJSON)

-- | A user and their role within an entity.
data ActingUser = ActingUser User.UserProfile ActingRole

-- | The inverse of `ActingUser`: an entity in which a user acts, with the role
-- of the user.
data EntityAndRole = EntityAndRole Entity ActingRole

-- TODO Ask Roger the meaning of these.
data Authorization = AuthorizedAsBuyer | AuthorizedAsSeller | AccountingAuthorized | OnlineAccountAuthorized
  deriving (Eq, Generic, Show)
  deriving (FromJSON, ToJSON)

data Err = Err
  deriving (Eq, Exception, Show)

--------------------------------------------------------------------------------
encodeUBL :: Entity -> LB.ByteString
encodeUBL = encode . toUBL

toUBL :: Entity -> Value
toUBL Entity {..} =
  object
    [ "CompanyID" .= _entityId
    , "RegistrationName" .= _entityName
    , "CorporateRegistrationScheme"
        -- TODO Should this be really in french ?
        .= [ object ["ID" .= _entityCbeNumber, "Name" .= ("BCE" :: Text)]
           , object ["ID" .= _entityVatNumber, "Name" .= ("TVA" :: Text)]
           ]
    ]
