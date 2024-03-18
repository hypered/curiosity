{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}

-- |
--Module: Curiosity.Types.Employment
--Description: Employment related datatypes
--
--This module contains data types used to represent contracts, both as used when
--filling a form, and used as proper validated data.
--
--See also the [related documentation page](/documentation/objects/invoices).
module Curiosity.Types.Employment
  ( -- * Form data representation

  --
    -- $formDataTypes
    CreateContractAll (..)
  , CreateContractAll' (..)
  , CreateContractType (..)
  , CreateContractGenInfo (..)
  , CreateContractLocDates (..)
  , CreateContractRisks (..)
  , CreateContractInvoice (..)
  , AddExpense (..)

    -- * Empty values

  --
    -- $emptyValues
  , emptyCreateContractAll
  , emptyCreateContractGenInfo
  , emptyCreateContractType
  , emptyCreateContractLocDates
  , emptyCreateContractRisks
  , emptyCreateContractInvoice
  , emptyAddExpense

    -- * Form submittal
  , SubmitContract (..)
  , validateCreateContract

    -- * Main data representation
  , Contract (..)
  , ContractId (..)
  , contractIdPrefix
  , Err (..)
  ) where

import Commence.Types.Wrapped qualified as W
import Control.Lens
import Curiosity.Types.PrefixedId qualified as Pre
import Curiosity.Types.User qualified as User
import Data.Aeson
import Text.Blaze.Html5 qualified as H
import Web.FormUrlEncoded
  ( FromForm (..)
  , parseUnique
  )

--------------------------------------------------------------------------------

-- $formDataTypes
--
-- A contract form, as displayed on a web page, is made of multiple input
-- groups (or panels, or even of separate pages). Different data types are
-- provided to represent those sets of input fields.

-- | This represents a form being filled in. In particular, it can represent
-- invalid inputs. As it is filled, it is kept in a Map in
-- "Curiosity.Types.Store", where it is identified by a key. The form data are
-- validated when they are "submitted", using the `SubmitContract` data type
-- below, and the key.
data CreateContractAll
  = CreateContractAll
      CreateContractGenInfo
      CreateContractType
      CreateContractLocDates
      CreateContractRisks
      CreateContractInvoice
      [AddExpense]
  deriving (Generic, Eq, Show)
  deriving anyclass (ToJSON, FromJSON)

-- | Same as above, but without the expenses. This is used to group together
-- the main panels into a `FromForm` instance. Simply leaving out the expenses
-- would also work but be less explicit.
data CreateContractAll'
  = CreateContractAll'
      CreateContractGenInfo
      CreateContractType
      CreateContractLocDates
      CreateContractRisks
      CreateContractInvoice
  deriving (Generic, Eq, Show)
  deriving anyclass (ToJSON, FromJSON)

instance FromForm CreateContractAll' where
  fromForm f =
    CreateContractAll'
      <$> fromForm f
      <*> fromForm f
      <*> fromForm f
      <*> fromForm f
      <*> fromForm f

data CreateContractGenInfo = CreateContractGenInfo
  { _createContractProject :: Text
  , _createContractPO :: Text
  , _createContractRole :: Text
  , _createContractType :: Text
  , _createContractDescription :: Text
  }
  deriving (Generic, Eq, Show)
  deriving anyclass (ToJSON, FromJSON)

instance FromForm CreateContractGenInfo where
  fromForm f =
    CreateContractGenInfo
      <$> parseUnique "project" f
      <*> parseUnique "po" f
      <*> parseUnique "role" f
      <*> parseUnique "type" f
      <*> parseUnique "description" f

data CreateContractType = CreateContractType
  deriving (Generic, Eq, Show)
  deriving anyclass (ToJSON, FromJSON)

instance FromForm CreateContractType where
  fromForm _ = pure CreateContractType

data CreateContractLocDates = CreateContractLocDates
  deriving (Generic, Eq, Show)
  deriving anyclass (ToJSON, FromJSON)

instance FromForm CreateContractLocDates where
  fromForm _ = pure CreateContractLocDates

data CreateContractRisks = CreateContractRisks
  deriving (Generic, Eq, Show)
  deriving anyclass (ToJSON, FromJSON)

instance FromForm CreateContractRisks where
  fromForm _ = pure CreateContractRisks

data CreateContractInvoice = CreateContractInvoice
  deriving (Generic, Eq, Show)
  deriving anyclass (ToJSON, FromJSON)

instance FromForm CreateContractInvoice where
  fromForm _ = pure CreateContractInvoice

data AddExpense = AddExpense
  { _addExpenseAmount :: Int
  }
  deriving (Generic, Eq, Show)
  deriving anyclass (ToJSON, FromJSON)

instance FromForm AddExpense where
  fromForm f = AddExpense <$> parseUnique "amount" f

--------------------------------------------------------------------------------

-- $emptyValues
--
-- Since forms are designed to be submitted after a confirmation page, it
-- should be possible to re-display a form with pre-filled values. The initial
-- form, when no value has been provided by a user, is actually rendering
-- \"empty" values, defined here.

emptyCreateContractAll :: CreateContractAll
emptyCreateContractAll =
  CreateContractAll
    emptyCreateContractGenInfo
    emptyCreateContractType
    emptyCreateContractLocDates
    emptyCreateContractRisks
    emptyCreateContractInvoice
    []

emptyCreateContractGenInfo :: CreateContractGenInfo
emptyCreateContractGenInfo =
  CreateContractGenInfo
    { _createContractProject = ""
    , _createContractPO = ""
    , _createContractRole = ""
    , _createContractType = ""
    , _createContractDescription = ""
    }

emptyCreateContractType :: CreateContractType
emptyCreateContractType = CreateContractType

emptyCreateContractLocDates :: CreateContractLocDates
emptyCreateContractLocDates = CreateContractLocDates

emptyCreateContractRisks :: CreateContractRisks
emptyCreateContractRisks = CreateContractRisks

emptyCreateContractInvoice :: CreateContractInvoice
emptyCreateContractInvoice = CreateContractInvoice

emptyAddExpense :: AddExpense
emptyAddExpense = AddExpense {_addExpenseAmount = 0}

--------------------------------------------------------------------------------

-- | This represents the submittal of a CreateContractAll, identified by its
-- key.
data SubmitContract = SubmitContract
  { _submitContractKey :: Text
  }
  deriving (Generic, Eq, Show)
  deriving anyclass (ToJSON, FromJSON)

instance FromForm SubmitContract where
  fromForm f = SubmitContract <$> parseUnique "key" f

-- | Given a contract form, tries to return a proper `Contract` value, although
-- the ID is dummy. Maybe we should have separate data types (with or without
-- the ID).
-- This is a pure function: everything required to perform the validation
-- should be provided as arguments.
validateCreateContract
  :: User.UserProfile -> CreateContractAll -> Either Err Contract
validateCreateContract profile = do
  if User.CanCreateContracts `elem` User._userProfileRights profile
    then pure $ Right Contract {_contractId = ContractId "TODO-DUMMY"}
    else pure . Left $ Err "User has not the right CanCreateContracts."

--------------------------------------------------------------------------------

-- | This represents a contract in database. TODO The notion of contract
-- includes more than amployment contract and all should share most of their
-- structure.
newtype Contract = Contract
  { _contractId :: ContractId
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Record ID of the form EMP-xxx.
newtype ContractId = ContractId {unContractId :: Text}
  deriving (Eq, Show)
  deriving
    ( IsString
    , FromJSON
    , ToJSON
    , H.ToMarkup
    , H.ToValue
    )
    via Text
  deriving (FromForm) via W.Wrapped "contract-id" Text
  deriving (Pre.PrefixedId) via W.Wrapped "EMP-" Text

contractIdPrefix :: Text
contractIdPrefix = Pre.getPrefix @ContractId ^. coerced

data Err = Err Text
  deriving (Eq, Exception, Show)
