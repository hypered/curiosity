{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{- |

Module: Curiosity.Data.PrefixedId
Description: Standardised ID prefixing.

See the documentation of the `PrefixedId` class for motivation. 
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

-- | Newtype over value level prefixes (eg @USER-@ is one such prefix for UserIds).
-- The only purpose is type-safety. 
newtype PrefixT = PrefixT Text
               deriving (Eq, Show, IsString) via Text

-- | An ID with a generated prefix. 
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

{- | Generalisations over IDs that have a prefix. For example, UserId values are always represented as
@USER-<ID>@ where @<ID>@ is the unique ID of the user. This lets us namespace IDs to avoid conflicts and also
have more human friendly IDs. 

We use type level symbols to ensure we have type-safety and don't get into "stringly typed programming".
Eg. a UserId can be a distinct type (using `W.Wrapped`) than other IDs. 

Consider:
@
λ> import Curiosity.Data.User
λ> UserId "foo" -- create a value. 
UserId {unUserId = "foo"}
λ> import Curiosity.Data.PrefixedId
λ> addPrefix it -- add prefix to the ID and show it. 
"USER-foo"
λ> parsePrefixedId @UserId (Right . UserId {- accept all texts, but other more complex parsing also possible. -}) it -- parse it back. 
Right (UserId {unUserId = "foo"})
@ 
-}
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

-- | An error raised when we fail to parse a prefix or an ID that has been prefixed. 
newtype PrefixParseErr = PrefixAbsent Text
                       deriving Show

instance Errs.IsRuntimeErr PrefixParseErr where
  errCode PrefixAbsent{} = "ERR.PREFIXED_ID.PREFIX_ABSENT"
  httpStatus PrefixAbsent{} = unprocessableEntity422 
  userMessage (PrefixAbsent msg) = Just msg 
  
