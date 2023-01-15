{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{- |

Module: Curiosity.Data.PrefixedId
Description: Standardised ID prefixing.

-}
module Curiosity.Data.PrefixedId
  ( PrefixT(..)
  , symToPrefix
  , getPrefix
  , PrefixedIdT(..)
  , PrefixedId(..)
  -- * Errors
  , PrefixParseErr(..)
  ) where

import  Network.HTTP.Types.Status (unprocessableEntity422)
import qualified Commence.Runtime.Errors as Errs 
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

-- | Get the prefix of @id@: non hyphenated. 
getPrefix :: forall id . PrefixedId id => PrefixT
getPrefix = symToPrefix @(Prefix id)

class KnownSymbol (Prefix id) => PrefixedId id where

  -- | Associated symbol indicating (at the type-level) the prefix to use for these IDs. 
  type Prefix id :: Symbol

  -- | How to add the prefix to an ID.
  addPrefix :: id -> PrefixedIdT
  default addPrefix :: Coercible id Text => id -> PrefixedIdT
  addPrefix id = PrefixedIdT $ prefix <> id ^. coerced
    where (PrefixT prefix) = symToPrefix @(Prefix id)

  -- | Parse an incoming ID: the default implementation is a naive parser. 
  parsePrefixedId
    :: (Text -> Either PrefixParseErr id) -- ^ Parser to employ for parsing the ID without the hyphenated prefix.
    -> PrefixedIdT -- ^ Fully prefixed ID. 
    -> Either PrefixParseErr id -- ^ Eventual parsing result. 
  default parsePrefixedId :: (Text -> Either PrefixParseErr id) -> PrefixedIdT -> Either PrefixParseErr id
  parsePrefixedId fromText (PrefixedIdT txt) =
    let
      (PrefixT prefix) = symToPrefix @(Prefix id)
    in case prefix `T.stripPrefix` txt of
      -- the prefix was not found. 
      Nothing -> Left . PrefixAbsent $ T.unwords [ "ID prefix:", "'" <> prefix <> "'", "is not in input text:" , "'" <> txt <> "'." ]
      Just rest -> fromText rest

-- | Automagically derive via `W.Wrapped`. 
instance (KnownSymbol prefix, Coercible id Text) => PrefixedId (W.Wrapped prefix id) where
  type Prefix (W.Wrapped prefix id) = prefix

newtype PrefixParseErr = PrefixAbsent Text
                       deriving Show

instance Errs.IsRuntimeErr PrefixParseErr where
  errCode PrefixAbsent{} = "ERR.PREFIXED_ID.PREFIX_ABSENT"
  httpStatus PrefixAbsent{} = unprocessableEntity422 
  userMessage (PrefixAbsent msg) = Just msg 
  
