{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{- |

Module: Curiosity.Data.PrefixedId
Description: Standardised ID prefixing.

-}
module Curiosity.Data.PrefixedId
  ( PrefixT(..)
  , hyphenate
  , symToPrefix
  , getPrefix
  , PrefixedIdT(..)
  , PrefixedId(..)
  ) where

import qualified Commence.Types.Wrapped        as W
import           Control.Lens
import           Data.Aeson
import qualified Data.Text                     as T
import           Web.HttpApiData

newtype PrefixT = PrefixT Text
               deriving (Eq, Show, IsString) via Text

newtype PrefixedIdT = PrefixedIdT Text
                    deriving ( Eq
                             , Show
                             , FromJSON, ToJSON
                             , FromHttpApiData, ToHttpApiData
                             , IsString
                             ) via Text

-- | Get a given symbol as prefix.  
symToPrefix :: forall prefix . KnownSymbol prefix => PrefixT
symToPrefix = PrefixT . T.pack $ symbolVal (Proxy @prefix)

-- | Add hyphens to the prefix. 
hyphenate :: PrefixT -> PrefixT
hyphenate (PrefixT noHy) = PrefixT $ noHy <> "-"

-- | Get the prefix of 
getPrefix :: forall id . PrefixedId id => PrefixT
getPrefix = symToPrefix @(Prefix id)

class KnownSymbol (Prefix id) => PrefixedId id where

  type Prefix id :: Symbol

  -- | How to add the prefix to an ID.
  addPrefix :: id -> PrefixedIdT
  default addPrefix :: Coercible id Text => id -> PrefixedIdT
  addPrefix id = PrefixedIdT $ prefix <> id ^. coerced
    where (hyphenate -> PrefixT prefix) = symToPrefix @(Prefix id)

  -- | Parse an incoming ID: the default implementation is a naive parser. 
  parsePrefixedId
    :: (Text -> Either Text id) -- ^ Parser to employ for parsing the ID without the hyphenated prefix.
    -> PrefixedIdT -- ^ Fully prefixed ID. 
    -> Either Text id -- ^ Eventual parsing result. 
  default parsePrefixedId :: (Text -> Either Text id) -> PrefixedIdT -> Either Text id
  parsePrefixedId fromText (PrefixedIdT txt) =
    let
      (hyphenate -> PrefixT prefix) = symToPrefix @(Prefix id)
    in case prefix `T.stripPrefix` txt of
      -- the prefix was not found. 
      Nothing -> Left $ T.unwords [ "ID prefix:", "'" <> prefix <> "'", "is not in input text:" , "'" <> txt <> "'." ]
      Just rest -> fromText rest

-- | Automagically derive via `W.Wrapped`. 
instance (KnownSymbol prefix, Coercible id Text) => PrefixedId (W.Wrapped prefix id) where
  type Prefix (W.Wrapped prefix id) = prefix
