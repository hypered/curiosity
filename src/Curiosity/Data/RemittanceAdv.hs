{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{- |
Module: Curiosity.Data.RemittanceAdv
Description: RemittanceAdv -related data types.

This module contains data types used to represent remittance advices.

-}
module Curiosity.Data.RemittanceAdv
  ( -- * Main data representation
    RemittanceAdv(..)
  , RemittanceAdvId(..)
  , Err(..)
  ) where

import qualified Curiosity.Data.PrefixedId     as Pre
import qualified Commence.Types.Wrapped        as W
import           Data.Aeson
import qualified Text.Blaze.Html5              as H
import           Web.FormUrlEncoded             ( FromForm(..)
                                                )


--------------------------------------------------------------------------------
-- | This represents a remittance advice in database.
data RemittanceAdv = RemittanceAdv
  { _remittanceAdvId :: RemittanceAdvId
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Record ID of the form REM-xxx.
newtype RemittanceAdvId = RemittanceAdvId { unRemittanceAdvId :: Text }
               deriving (Eq, Show)
               deriving ( IsString
                        , FromJSON
                        , ToJSON
                        , H.ToMarkup
                        , H.ToValue
                        ) via Text
              deriving FromForm via W.Wrapped "remittance-advice-id" Text
              deriving Pre.PrefixedId via W.Wrapped "REM-" Text

data Err = Err
  { unErr :: Text
  }
  deriving (Eq, Exception, Show)
