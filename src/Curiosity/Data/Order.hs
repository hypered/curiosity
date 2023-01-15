{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{- |
Module: Curiosity.Data.Order
Description: Order -related data types.

This module contains data types used to represent orders (there is no form to
create them as they result from a signed quotation).

-}
module Curiosity.Data.Order
  ( -- * Main data representation
    Order(..)
  , OrderId(..)
  , Predicate(..)
  , applyPredicate
  , Err(..)
  ) where

import qualified Curiosity.Data.PrefixedId     as Pre
import qualified Commence.Types.Wrapped        as W
import           Data.Aeson
import qualified Text.Blaze.Html5              as H
import           Web.FormUrlEncoded             ( FromForm(..)
                                                )


--------------------------------------------------------------------------------
-- | This represents an order in database.
data Order = Order
  { _orderId :: OrderId
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Record ID of the form ORD-xxx.
newtype OrderId = OrderId { unOrderId :: Text }
               deriving (Eq, Show)
               deriving ( IsString
                        , FromJSON
                        , ToJSON
                        , H.ToMarkup
                        , H.ToValue
                        ) via Text
               deriving FromForm via W.Wrapped "order-id" Text
               deriving Pre.PrefixedId via W.Wrapped "ORD-" Text

--------------------------------------------------------------------------------
-- | Predicates to filter orders.
data Predicate = AllOrders
  deriving (Eq, Show)

applyPredicate :: Predicate -> Order -> Bool
applyPredicate AllOrders _ = True


--------------------------------------------------------------------------------
data Err = Err
  { unErr :: Text
  }
  deriving (Eq, Exception, Show)
