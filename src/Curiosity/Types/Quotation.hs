{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}

-- |
--Module: Curiosity.Types.Quotation
--Description: Quotation -related data types.
--
--This module contains data types used to represent quotations, both as used when
--filling a form, and used as proper validated data.
module Curiosity.Types.Quotation
  ( -- * Form data representation

  --
    -- $formDataTypes
    CreateQuotationAll (..)

    -- * Empty values

  --
    -- $emptyValues
  , emptyCreateQuotationAll

    -- * Form submittal and validation
  , SubmitQuotation (..)
  , validateCreateQuotation
  , validateCreateQuotation'

    -- * Main data representation
  , Quotation (..)
  , QuotationId (..)
  , QuotationState (..)
  , displayQuotationState
  , SetQuotationAsSigned (..)
  , SetQuotationAsRejected (..)
  , Predicate (..)
  , applyPredicate
  , Err (..)
  ) where

import Commence.Runtime.Errors qualified as Errs
import Commence.Types.Wrapped qualified as W
import Curiosity.Html.Errors qualified as Pages
import Curiosity.Types.Business qualified as Business
import Curiosity.Types.Legal qualified as Legal
import Curiosity.Types.Order qualified as Order
import Curiosity.Types.PrefixedId qualified as Pre
import Curiosity.Types.User qualified as User
import Data.Aeson
import Data.Text.Lazy qualified as LT
import Network.HTTP.Types qualified as HTTP
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Renderer.Text (renderMarkup)
import Web.FormUrlEncoded
  ( FromForm (..)
  , parseMaybe
  , parseUnique
  )
import Web.HttpApiData (FromHttpApiData (..))

--------------------------------------------------------------------------------

-- $formDataTypes
--
-- A quotation form, as displayed on a web page, is made of multiple input
-- groups (or panels, or even of separate pages). Different data types are
-- provided to represent those sets of input fields.

-- | This represents a form being filled in. In particular, it can represent
-- invalid inputs. As it is filled, it is kept in a Map in "Curiosity.Data",
-- where it is identified by a key. The form data are validated when they are
-- "submitted", using the `SubmitQuotation` data type below, and the key.
data CreateQuotationAll = CreateQuotationAll
  { _createQuotationClientUsername :: Maybe User.UserName
  , _createQuotationSellerUnit :: Maybe Text
  , _createQuotationSellerEntity :: Maybe Text
  , _createQuotationBuyerUnit :: Maybe Text
  , _createQuotationBuyerEntity :: Maybe Text
  }
  deriving (Generic, Eq, Show)
  deriving anyclass (ToJSON, FromJSON)

instance FromForm CreateQuotationAll where
  fromForm f =
    CreateQuotationAll
      <$> parseMaybe "client-username" f
      <*> parseMaybe "seller-unit" f
      <*> parseMaybe "seller-entity" f
      <*> parseMaybe "buyer-unit" f
      <*> parseMaybe "buyer-entity" f

-- TODO Make them Nothing if empty strings.

--------------------------------------------------------------------------------

-- $emptyValues
--
-- Since forms are designed to be submitted after a confirmation page, it
-- should be possible to re-display a form with pre-filled values. The initial
-- form, when no value has been provided by a user, is actually rendering
-- \"empty" values, defined here.

emptyCreateQuotationAll :: CreateQuotationAll
emptyCreateQuotationAll =
  CreateQuotationAll
    { _createQuotationClientUsername = Nothing
    , _createQuotationSellerUnit = Nothing
    , _createQuotationSellerEntity = Nothing
    , _createQuotationBuyerUnit = Nothing
    , _createQuotationBuyerEntity = Nothing
    }

--------------------------------------------------------------------------------

-- | This represents the submittal of a CreateQuotationAll, identified by its
-- key.
newtype SubmitQuotation = SubmitQuotation
  { _submitQuotationKey :: Text
  }
  deriving (Generic, Eq, Show)
  deriving anyclass (ToJSON, FromJSON)

instance FromForm SubmitQuotation where
  fromForm f = SubmitQuotation <$> parseUnique "key" f

-- | Given a contract form, tries to return a proper `Quotation` value, although
-- the ID is dummy. Maybe we should have separate data types (with or without
-- the ID).
-- This is a pure function: everything required to perform the validation
-- should be provided as arguments.
validateCreateQuotation
  :: User.UserProfile
  -> CreateQuotationAll
  -> Maybe User.UserProfile
  -- ^ The user profile matching the quotation client.
  -> Maybe Legal.Entity
  -- ^ The legal entity matching the quotation seller entity.
  -> Maybe Business.Unit
  -- ^ The business unit matching the quotation seller unit.
  -> Maybe Legal.Entity
  -- ^ The legal entity matching the quotation buyer entity.
  -> Maybe Business.Unit
  -- ^ The business unit matching the quotation buyer unit.
  -> Either [Err] (Quotation, User.UserProfile, Legal.Entity, Business.Unit, Legal.Entity, Business.Unit)
validateCreateQuotation _ CreateQuotationAll {..} mresolvedClient mresolvedSellerEntity mresolvedSellerUnit mresolvedBuyerEntity mresolvedBuyerUnit =
  if null errors
    then Right (quotation, resolvedClient, resolvedSellerEntity, resolvedSellerUnit, resolvedBuyerEntity, resolvedBuyerUnit)
    else Left errors
 where
  quotation =
    Quotation
      { _quotationId = QuotationId "TODO-DUMMY"
      , _quotationState = QuotationSent
      }
  Just resolvedClient = mresolvedClient
  Just resolvedSellerEntity = mresolvedSellerEntity
  Just resolvedSellerUnit = mresolvedSellerUnit
  Just resolvedBuyerEntity = mresolvedBuyerEntity
  Just resolvedBuyerUnit = mresolvedBuyerUnit
  errors =
    [Err "Missing client username." | isNothing _createQuotationClientUsername]
      <> [Err "The client username does not exist." | isNothing mresolvedClient]
      <> [Err "Missing selling entity." | isNothing _createQuotationSellerEntity]
      <> [Err "The selling entity does not exist." | isNothing mresolvedSellerEntity]
      <> [Err "Missing selling unit." | isNothing _createQuotationSellerUnit]
      <> [Err "The selling unit does not exist." | isNothing mresolvedSellerUnit]
      <> [Err "Missing buying entity." | isNothing _createQuotationBuyerEntity]
      <> [Err "The buying entity does not exist." | isNothing mresolvedBuyerEntity]
      <> [Err "Missing buying unit." | isNothing _createQuotationBuyerUnit]
      <> [Err "The buying unit does not exist." | isNothing mresolvedBuyerUnit]

-- | Similar to `validateCreateQuotation` but throw away the returned
-- contract, i.e. keep only the errors.
validateCreateQuotation' :: User.UserProfile -> CreateQuotationAll -> Maybe User.UserProfile -> Maybe Legal.Entity -> Maybe Business.Unit -> Maybe Legal.Entity -> Maybe Business.Unit -> [Err]
validateCreateQuotation' profile quotation resolvedClient resolvedSellerEntity resolvedSellerUnit resolvedBuyerEntity resolvedBuyerUnit =
  either identity (const []) $ validateCreateQuotation profile quotation resolvedClient resolvedSellerEntity resolvedSellerUnit resolvedBuyerEntity resolvedBuyerUnit

--------------------------------------------------------------------------------

-- | This represents a quotation in database.
data Quotation = Quotation
  { _quotationId :: QuotationId
  , _quotationState :: QuotationState
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Record ID of the form QUOT-xxx.
newtype QuotationId = QuotationId {unQuotationId :: Text}
  deriving (Eq, Show)
  deriving
    ( IsString
    , FromJSON
    , ToJSON
    , H.ToMarkup
    , H.ToValue
    )
    via Pre.Prefixed QuotationId
  deriving (FromHttpApiData, FromForm) via W.Wrapped "quotation-id" Text
  deriving (Pre.PrefixedId) via W.Wrapped "QUOT-" Text

data QuotationState
  = QuotationCreated
  | QuotationSent
  | -- | A signed quotation is necessarily linked to a created order.
    QuotationSigned Order.OrderId
  | -- | A possible rejection comment.
    QuotationRejected (Maybe Text)
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

displayQuotationState :: QuotationState -> Text
displayQuotationState = \case
  QuotationCreated -> "Created"
  QuotationSent -> "Sent"
  QuotationSigned oid -> "Signed (" <> Order.unOrderId oid <> ")"
  QuotationRejected _ -> "Rejected (" <> "..." <> ")"

--------------------------------------------------------------------------------
newtype SetQuotationAsSigned = SetQuotationAsSigned QuotationId

instance FromForm SetQuotationAsSigned where
  fromForm f = SetQuotationAsSigned <$> parseUnique "quotation-id" f

data SetQuotationAsRejected = SetQuotationAsRejected QuotationId (Maybe Text)

instance FromForm SetQuotationAsRejected where
  fromForm f =
    SetQuotationAsRejected
      <$> parseUnique "quotation-id" f
      <*> parseMaybe "comment" f

--------------------------------------------------------------------------------

-- | Predicates to filter quotations.
data Predicate = AllQuotations
  deriving (Eq, Show)

applyPredicate :: Predicate -> Quotation -> Bool
applyPredicate AllQuotations _ = True

--------------------------------------------------------------------------------
newtype Err = Err
  { unErr :: Text
  }
  deriving (Eq, Exception, Show)

instance Errs.IsRuntimeErr Err where
  errCode =
    errCode' . \case
      Err _ -> "QUOT_" <> "TODO"
   where
    errCode' = mappend "ERR.QUOT"

  httpStatus = \case
    Err _ -> HTTP.conflict409 -- TODO Check relevant code.

  userMessage =
    Just . \case
      Err msg ->
        LT.toStrict . renderMarkup . H.toMarkup $
          Pages.ErrorPage
            409
            "Err"
            msg
