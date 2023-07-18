{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{- |
Module: Curiosity.Types.SimpleContract
Description: Simple contract (also called 3in1) related datatypes

This module contains data types used to represent simple contracts, both as
used when filling a form, and used as proper validated data.

See also the [related documentation page](/documentation/objects/invoices).

-}
module Curiosity.Types.SimpleContract
  ( -- * Form data representation
    --
    -- $formDataTypes
    CreateContractAll(..)
  , CreateContractAll'(..)
  , CreateContractType(..)
  , CreateContractRisks(..)
  , CreateContractClient(..)
  , CreateContractInvoice(..)
  , SelectRole(..)
  , AddDate(..)
  , SelectVAT(..)
  , AddExpense(..)
    -- * Empty values
    --
    -- $emptyValues
  , emptyCreateContractAll
  , emptyCreateContractAll'
  , emptyCreateContractType
  , emptyCreateContractRisks
  , emptyCreateContractInvoice
  , emptyAddDate
  , emptyAddExpense
    -- * Form submittal and validation
  , SubmitContract(..)
  , validateCreateSimpleContract
  , validateCreateSimpleContract'
    -- * Main data representation
  , Contract(..)
  , ContractId(..)
  , Err(..)
    -- * Roles
    --
    -- $roles
  , Role0(..)
  , Role1(..)
  , Role
  , roles
  , lookupRoleLabel
    -- * VAT rates
    --
    -- $vatRates
  , vatRates
  ) where

import qualified Commence.Types.Wrapped        as W
import qualified Curiosity.Types.User          as User
import           Data.Aeson
import           Data.List                      ( lookup )
import qualified Text.Blaze.Html5              as H
import           Web.FormUrlEncoded             ( FromForm(..)
                                                , lookupMaybe
                                                , parseMaybe
                                                , parseUnique
                                                )
import           Web.HttpApiData                ( parseQueryParams )

--------------------------------------------------------------------------------
-- $formDataTypes
--
-- A contract form, as displayed on a web page, is made of multiple input
-- groups (or panels, or even of separate pages). Different data types are
-- provided to represent those sets of input fields.

-- | This represents a form being filled in. In particular, it can represent
-- invalid inputs. As it is filled, it is kept in a Map in "Curiosity.Data",
-- where it is identified by a key. The form data are validated when they are
-- "submitted", using the `SubmitContract` data type below, and the key.
data CreateContractAll = CreateContractAll
  { _createContractType     :: CreateContractType
  , _createContractRisks    :: CreateContractRisks
  , _createContractClient   :: CreateContractClient
  , _createContractInvoice  :: CreateContractInvoice
  , _createContractDates    :: [AddDate]
  , _createContractExpenses :: [AddExpense]
  }
  deriving (Generic, Eq, Show)
  deriving anyclass (ToJSON, FromJSON)

-- | Same as above, but without the expenses. This is used to group together
-- the main panels into a `FromForm` instance. Simply leaving out the expenses
-- would also work but be less explicit.
data CreateContractAll' = CreateContractAll'
  { _createContractType'    :: CreateContractType
  , _createContractRisks'   :: CreateContractRisks
  , _createContractClient'  :: CreateContractClient
  , _createContractInvoice' :: CreateContractInvoice
  }
  deriving (Generic, Eq, Show)
  deriving anyclass (ToJSON, FromJSON)

instance FromForm CreateContractAll' where
  fromForm f =
    CreateContractAll'
      <$> fromForm f
      <*> fromForm f
      <*> fromForm f
      <*> fromForm f

data CreateContractType = CreateContractType
  { _createContractRole        :: Text
  , _createContractDescription :: Text
  , _createContractWorkCountry :: Text
  , _createContractHasRisks    :: Bool
  }
  deriving (Generic, Eq, Show)
  deriving anyclass (ToJSON, FromJSON)

instance FromForm CreateContractType where
  fromForm f =
    CreateContractType
      <$> parseUnique "role"         f
      <*> parseUnique "description"  f
      <*> parseUnique "work-country" f
      <*> parseUnique "has-risks"    f

data CreateContractRisks = CreateContractRisks
  deriving (Generic, Eq, Show)
  deriving anyclass (ToJSON, FromJSON)

instance FromForm CreateContractRisks where
  fromForm _ = pure CreateContractRisks

data CreateContractClient = CreateContractClient
  { _createContractClientUsername :: Maybe User.UserName
  }
  deriving (Generic, Eq, Show)
  deriving anyclass (ToJSON, FromJSON)

instance FromForm CreateContractClient where
  fromForm f = CreateContractClient <$> p f
    -- TODO Make it a re-usable function.
    -- or add this non-empty logic directly to the UserName data type.
   where
    p f' = case (parseQueryParams <=< lookupMaybe "client-username") f' of
      Left  err       -> Left err
      Right (Just "") -> Right Nothing
      Right value     -> Right value

data CreateContractInvoice = CreateContractInvoice
  { _createContractAmount         :: Int
  , _createContractVAT            :: Int
  , _createContractIsVATIncl      :: Maybe Bool -- TODO VATInclOrExcl
  , _createContractPrepaidAmount  :: Int
  , _createContractWithholdingTax :: Int
  , _createContractContractType   :: Text -- TODO Enum
  }
  deriving (Generic, Eq, Show)
  deriving anyclass (ToJSON, FromJSON)

data VATInclOrExcl = VATIncl | VATExcl
  deriving (Generic, Eq, Show)
  deriving anyclass (ToJSON, FromJSON)

instance FromForm CreateContractInvoice where
  fromForm f =
    CreateContractInvoice
      <$> parseUnique "amount" f
      <*> parseUnique "vat"    f
      <*> parseMaybe "vat-incl-excl" f
      <*> parseUnique "prepaid-amount"  f
      <*> parseUnique "withholding-tax" f
      <*> parseUnique "contract-type"   f

data SelectRole = SelectRole
  { _selectRoleRole :: Text
  }
  deriving (Generic, Eq, Show)
  deriving anyclass (ToJSON, FromJSON)

instance FromForm SelectRole where
  fromForm f = SelectRole <$> parseUnique "role" f

data AddDate = AddDate
  { _addDateDate :: Text -- TODO Proper type.
  }
  deriving (Generic, Eq, Show)
  deriving anyclass (ToJSON, FromJSON)

instance FromForm AddDate where
  fromForm f = AddDate <$> parseUnique "date" f

data SelectVAT = SelectVAT
  { _selectVATRate :: Int
  }
  deriving (Generic, Eq, Show)
  deriving anyclass (ToJSON, FromJSON)

instance FromForm SelectVAT where
  fromForm f = SelectVAT <$> parseUnique "vat" f

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
emptyCreateContractAll = CreateContractAll emptyCreateContractType
                                           emptyCreateContractRisks
                                           emptyCreateContractClient
                                           emptyCreateContractInvoice
                                           []
                                           []

emptyCreateContractAll' :: CreateContractAll'
emptyCreateContractAll' = CreateContractAll' emptyCreateContractType
                                             emptyCreateContractRisks
                                             emptyCreateContractClient
                                             emptyCreateContractInvoice

emptyCreateContractType :: CreateContractType
emptyCreateContractType = CreateContractType
  { _createContractRole        = "coloriste"
    -- TODO Proper type, and proper default value (probably to be set in the
    -- user profile). The value here is looked up to display the right label in
    -- the forms.
  , _createContractDescription = ""
  , _createContractWorkCountry = "BE"
  , _createContractHasRisks    = False -- TODO We probably want no default choice here.
  }

emptyCreateContractRisks :: CreateContractRisks
emptyCreateContractRisks = CreateContractRisks

emptyCreateContractClient :: CreateContractClient
emptyCreateContractClient =
  CreateContractClient { _createContractClientUsername = Nothing }

emptyCreateContractInvoice :: CreateContractInvoice
emptyCreateContractInvoice = CreateContractInvoice
  { _createContractAmount         = 0
  , _createContractVAT            = 21
  , _createContractIsVATIncl      = Nothing
  , _createContractPrepaidAmount  = 0
  , _createContractWithholdingTax = 1111
  , _createContractContractType   = "none-selected"
  }

emptyAddDate :: AddDate
emptyAddDate = AddDate { _addDateDate = "1970-01-01" }

emptyAddExpense :: AddExpense
emptyAddExpense = AddExpense { _addExpenseAmount = 0 }


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
validateCreateSimpleContract
  :: User.UserProfile -> CreateContractAll -> Either [Err] Contract
validateCreateSimpleContract profile CreateContractAll {..} = if null errors
  then Right contract
  else Left errors
 where
  contract = Contract { _contractId = ContractId "TODO-DUMMY" }
  errors   = concat
    [ if User.CanCreateContracts `elem` User._userProfileRights profile
      then []
      else [Err "User has not the right CanCreateContracts."]
    , if _createContractAmount _createContractInvoice < 1
      then [Err "Amount to invoice must be strictly positive."]
      else []
    ]

-- | Similar to `validateCreateSimpleContract` but throw away the returned
-- contract, i.e. keep only the errors.
validateCreateSimpleContract' profile contract = either identity (const []) $
  validateCreateSimpleContract profile contract


--------------------------------------------------------------------------------
-- | This represents a contract in database. TODO The notion of contract
-- includes more than amployment contract and all should share most of their
-- structure.
data Contract = Contract
  { _contractId :: ContractId
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Record ID of the form EMP-xxx.
newtype ContractId = ContractId { unContractId :: Text }
               deriving (Eq, Show)
               deriving ( IsString
                        , FromJSON
                        , ToJSON
                        , H.ToMarkup
                        , H.ToValue
                        ) via Text
               deriving FromForm via W.Wrapped "contract-id" Text

data Err = Err
  { unErr :: Text
  }
  deriving (Eq, Exception, Show)


--------------------------------------------------------------------------------
-- $role
--
-- List of possible roles, when filling a simple contract form. Roles are
-- actually aranged in a three-level hierarchy: `Role0` at the top, `Role1`
-- below, then the actual `Role` at the bottom.

-- | Top level of the role hierarchy.
data Role0 = Role0 Text [Role1]
  deriving (Generic, FromJSON, ToJSON)

-- | Intermediate level of the role hierarchy.
data Role1 = Role1 Text [Role]
  deriving (Generic, FromJSON, ToJSON)

-- | A role is represented by a symbol (that can be used in database and APIs)
-- and a label meant for humans.
type Role = (Text, Text)

roles :: [Role0]
roles =
  [ Role0
      "Fonction de création artistique et artisanale"
      [ Role1 "Arts du spectacle" []
      , Role1 "Arts littéraires"  []
      , Role1
        "Arts plastiques et graphiques"
        [ ("coloriste"   , "Coloriste")
        , ("dessinateur", "Dessinateur-rice / illustrateur-rice")
        , ("graffitiste" , "Graffitiste / graffeur-euse")
        , ("graphiste", "Graphiste / infographiste / webdesigner-euse")
        , ("graveur"     , "Graveur-euse / sérigraphe")
        , ("peintre"     , "Peintre-esse")
        , ("performeur"  , "Performeur-euse")
        , ("photographe" , "Photographe")
        , ("plasticien", "Plasticien-ne / installateur-rice 3d")
        , ("scenographe" , "Scénographe")
        , ("sculpteur"   , "Sculpteur-rice")
        , ("body-painter", "Body-painter")
        , ("autre"       , "Autre")
        ]
      , Role1 "Architecture / mode / design / décoration" []
      ]
  ]

-- Flatten the roles hierarchy, adding upper titles to the labels.
roles' :: [Role]
roles' = concatMap go0 roles
 where
  go0 (Role0 title0 roles1) = concatMap (go1 title0) roles1
  go1 title0 (Role1 title1 rs) = map (go title0 title1) rs
  go title0 title1 (value, label) =
    (value, unwords [title0, ">", title1, ">", label])

lookupRoleLabel role = lookup role roles'

--------------------------------------------------------------------------------
-- $vatRates
--
-- List of possible VAT rates with their reason.

-- | List of possible VAT rates with their reason.
vatRates :: [(Text, Text)]
vatRates =
  [ ("0", "0% For some reason")
  , ("6", "6% For some reason")
  , ("21", "21% For some reason")
  ]
