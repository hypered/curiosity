{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}

-- |
--Module: Curiosity.Types.Invoice
--Description: Invoice related datatypes
module Curiosity.Types.Invoice
  ( Invoice (..)
  , InvoiceId (..)
  , Err (..)
  ) where

import Commence.Types.Wrapped qualified as W
import Curiosity.Types.PrefixedId qualified as Pre
import Data.Aeson
import Text.Blaze.Html5 qualified as H
import Web.FormUrlEncoded (FromForm (..))

--------------------------------------------------------------------------------
data Invoice = Invoice
  { _entityId :: InvoiceId
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Record ID of the form INV-xxx.
newtype InvoiceId = InvoiceId {unInvoiceId :: Text}
  deriving (Eq, Show)
  deriving
    ( IsString
    , FromJSON
    , ToJSON
    , H.ToMarkup
    , H.ToValue
    )
    via Pre.Prefixed InvoiceId
  deriving (FromForm) via W.Wrapped "invoice-id" Text
  deriving (Pre.PrefixedId) via W.Wrapped "INV-" Text

data Err = Err
  { unErr :: Text
  }
  deriving (Eq, Exception, Show)
