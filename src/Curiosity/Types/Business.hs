{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}

-- |
--Module: Curiosity.Types.Business
--Description: Business entities related datatypes
module Curiosity.Types.Business
  ( Unit (..)
  , Create (..)
  , Update (..)
  , UnitId (..)
  , ActingRole (..)
  , Authorization (..)
  , Scope (..)
  , Err (..)
  ) where

import Commence.Types.Wrapped qualified as W
import Curiosity.Types.PrefixedId qualified as Pre
import Curiosity.Types.User qualified as User
import Data.Aeson
import Text.Blaze.Html5 qualified as H
import Web.FormUrlEncoded
  ( FromForm (..)
  , parseMaybe
  , parseUnique
  )

--------------------------------------------------------------------------------
data Create = Create
  { _createSlug :: Text -- Unique
  , _createName :: Text
  }
  deriving (Generic, Eq, Show)

instance FromForm Create where
  fromForm f = Create <$> parseUnique "slug" f <*> parseUnique "name" f

-- | Represents the input data to update a business unit profile.
data Update = Update
  { _updateSlug :: Text
  , _updateDescription :: Maybe Text
  }
  deriving (Eq, Show, Generic)

instance FromForm Update where
  fromForm f = Update <$> parseUnique "slug" f <*> parseMaybe "description" f

--------------------------------------------------------------------------------
data Unit = Unit
  { _entityId :: UnitId
  , _entitySlug :: Text
  -- ^ An identifier suitable for URLs
  , _entityName :: Text
  , _entityDescription :: Maybe Text
  , _entityType :: Text
  , _entityHolders :: [User.UserId]
  , _entityAuthorizations :: [Authorization]
  , _entityScopes :: [Scope]
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Record ID of the form BENT-xxx.
newtype UnitId = UnitId {unUnitId :: Text}
  deriving (Eq, Show)
  deriving
    ( IsString
    , FromJSON
    , ToJSON
    , H.ToMarkup
    , H.ToValue
    )
    via Text
  deriving (FromForm) via W.Wrapped "unit-id" Text
  deriving (Pre.PrefixedId) via W.Wrapped "BENT-" Text

data ActingRole = Dummy | Holder
  deriving (Eq, Generic, Show)
  deriving (FromJSON, ToJSON)

-- TODO Ask Roger the meaning of these.
data Authorization = Bundle0 | Bundle1
  deriving (Eq, Generic, Show)
  deriving (FromJSON, ToJSON)

-- TODO Ask Roger the meaning of these.
data Scope = ScopeCreateQuotation | ScopeSendQuotation | ScopeCreateInvoice
  deriving (Eq, Generic, Show)
  deriving (FromJSON, ToJSON)

newtype Err = Err
  { unErr :: Text
  }
  deriving (Eq, Exception, Show)
