{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}

-- |
--
--Module: Curiosity.Types.PrefixedId
--Description: Standardised ID prefixing.
--
--See the documentation of the `PrefixedId` class for motivation.
module Curiosity.Types.PrefixedId
  ( PrefixT (..)
  , symToPrefix
  , getPrefix
  , getPrefixText
  , PrefixedIdT (..)
  , PrefixedId (..)
  , showWithPrefix

    -- * Errors
  , PrefixParseErr (..)

    -- * DerivingVia
  , Prefixed (..)
  ) where

-- for fromString

import Commence.Runtime.Errors qualified as Errs
import Commence.Types.Wrapped qualified as W
import Control.Lens hiding (Prefixed)
import Data.Aeson
import Data.String qualified
import Data.Text qualified as T
import GHC.Base (error)
import Network.HTTP.Types.Status (unprocessableEntity422)
import Text.Blaze.Html5 qualified as H
import Web.FormUrlEncoded
import Web.HttpApiData

-- | Newtype over value level prefixes (eg @USER-@ is one such prefix for UserIds).
-- The only purpose is type-safety.
newtype PrefixT = PrefixT {_unPrefixT :: Text}
  deriving (Eq, Show, IsString) via Text

-- | An ID with a generated prefix.
newtype PrefixedIdT = PrefixedIdT {_unPrefixedIdT :: Text}
  deriving
    ( Eq
    , Show
    , FromJSON
    , ToJSON
    , FromHttpApiData
    , ToHttpApiData
    , IsString
    , H.ToMarkup
    , H.ToValue
    )
    via Text

-- | Get a given symbol as prefix.
symToPrefix :: forall prefix. KnownSymbol prefix => PrefixT
symToPrefix = PrefixT . T.pack $ symbolVal (Proxy @prefix)

-- | Get the prefix of @id@: non hyphenated.
getPrefix :: forall id. PrefixedId id => PrefixT
getPrefix = symToPrefix @(Prefix id)

-- | Same as `getPrefix` but generates an unsafe `Text` value instead.
getPrefixText :: forall id. PrefixedId id => Text
getPrefixText = _unPrefixT (getPrefix @id)

-- | Generalisations over IDs that have a prefix. For example, UserId values are always represented as
-- @USER-<ID>@ where @<ID>@ is the unique ID of the user. This lets us namespace IDs to avoid conflicts and also
-- have more human friendly IDs.
--
-- We use type level symbols to ensure we have type-safety and don't get into "stringly typed programming".
-- Eg. a UserId can be a distinct type (using `W.Wrapped`) than other IDs.
--
-- Consider:
-- @
-- λ> import Curiosity.Types.User
-- λ> UserId "foo" -- create a value.
-- UserId {unUserId = "foo"}
-- λ> import Curiosity.Types.PrefixedId
-- λ> addPrefix it -- add prefix to the ID and show it.
-- "USER-foo"
-- λ> parsePrefixedId @UserId (Right . UserId {\- accept all texts, but other more complex parsing also possible. -\}) it -- parse it back.
-- Right (UserId {unUserId = "foo"})
-- @
class KnownSymbol (Prefix id) => PrefixedId id where
  -- | Associated symbol indicating (at the type-level) the prefix to use for these IDs.
  type Prefix id :: Symbol

  -- | How to add the prefix to an ID.
  addPrefix :: id -> PrefixedIdT
  default addPrefix :: Coercible id Text => id -> PrefixedIdT
  addPrefix id = PrefixedIdT $ prefix <> id ^. coerced
   where
    (PrefixT prefix) = symToPrefix @(Prefix id)

  -- | Parse an incoming ID: the default implementation is a naive parser.
  parsePrefixedId
    :: (Text -> Either PrefixParseErr id)
    -- ^ Parser to employ for parsing the ID, without the prefix.
    -> PrefixedIdT
    -- ^ Fully prefixed ID.
    -> Either PrefixParseErr id
    -- ^ Eventual parsing result.
  default parsePrefixedId :: (Text -> Either PrefixParseErr id) -> PrefixedIdT -> Either PrefixParseErr id
  parsePrefixedId fromText (PrefixedIdT txt) =
    let
      (PrefixT prefix) = symToPrefix @(Prefix id)
     in
      case prefix `T.stripPrefix` txt of
        -- the prefix was not found.
        Nothing -> Left . PrefixAbsent $ T.unwords ["ID prefix:", "'" <> prefix <> "'", "is not in input text:", "'" <> txt <> "'."]
        Just rest -> fromText rest

-- | Automagically derive via `W.Wrapped`.
instance (KnownSymbol prefix, Coercible id Text) => PrefixedId (W.Wrapped prefix id) where
  type Prefix (W.Wrapped prefix id) = prefix

-- | Newtype that we can use for DerivingVia.
newtype Prefixed id = Prefixed {_unPrefixed :: id}
  deriving (Eq, Show)

instance PrefixedId id => ToJSON (Prefixed id) where
  toJSON = String . _unPrefixedIdT . addPrefix @id . _unPrefixed

instance (PrefixedId id, Coercible Text id) => FromJSON (Prefixed id) where
  parseJSON =
    parseJSON >=> \case
      -- if the JSON value is a string, succeed.
      String idWithPrefixT ->
        either mempty (pure . Prefixed) -- succeed on right.
          . parsePrefixedId @id (Right . view coerced) -- parse it, accepting any text.
          . PrefixedIdT -- view as the prefixedIdT newtype.
          $ idWithPrefixT
      _ -> mempty -- in all other cases, fail.

-- | When rendering to markup, always add the prefix.
instance PrefixedId id => H.ToMarkup (Prefixed id) where
  toMarkup = H.toMarkup . addPrefix . _unPrefixed

instance PrefixedId id => H.ToValue (Prefixed id) where
  toValue = H.toValue . addPrefix . _unPrefixed

instance (PrefixedId id, Coercible Text id) => FromHttpApiData (Prefixed id) where
  parseQueryParam =
    bimap Errs.displayErr Prefixed
      . parsePrefixedId @id (Right . view coerced)
      . PrefixedIdT

instance
  PrefixedId id
  => ToHttpApiData (Prefixed id)
  where
  toQueryParam = _unPrefixedIdT . addPrefix . _unPrefixed

showWithPrefix :: PrefixedId id => id -> Text
showWithPrefix = _unPrefixedIdT . addPrefix . _unPrefixed . Prefixed

-- instance PrefixedId id => SAuth.ToJWT (Prefixed id) where
--   encodeJWT = SAuth.encodeJWT . _unPrefixedIdT . addPrefix . _unPrefixed

instance
  ( PrefixedId id
  , Coercible Text id
  , KnownSymbol field
  )
  => FromForm (Prefixed (W.Wrapped field id))
  where
  fromForm =
    fromForm @(W.Wrapped field Text)
      >=> bimap Errs.displayErr (Prefixed . W.Wrapped)
        . parsePrefixedId @id (Right . view coerced)
        . PrefixedIdT
        . W.unWrap

instance (PrefixedId id, Coercible Text id) => IsString (Prefixed id) where
  fromString =
    either (error . T.unpack . Errs.displayErr) Prefixed
      . parsePrefixedId @id (Right . view coerced)
      . PrefixedIdT
      . T.pack

-- | An error raised when we fail to parse a prefix or an ID that has been prefixed.
newtype PrefixParseErr = PrefixAbsent Text
  deriving (Show)

instance Errs.IsRuntimeErr PrefixParseErr where
  errCode PrefixAbsent {} = "ERR.PREFIXED_ID.PREFIX_ABSENT"
  httpStatus PrefixAbsent {} = unprocessableEntity422
  userMessage (PrefixAbsent msg) = Just msg
