-- | This is an alternative employment contract demand (or proposal)
-- representation, focusing on the generic nature of "contracts", i.e. they can
-- represent employment contracts, service or good sales, ...
module Curiosity.Types.Demand where

import qualified Curiosity.Types.Business as Business
import qualified Curiosity.Types.Legal as Legal
import qualified Curiosity.Types.User as User

--------------------------------------------------------------------------------

-- | A form to represent an employment contract /demand/. We've tried to make
-- field names independant from the employment aspect, and focus on the
-- contract.
data FormDemand = FormDemand
  { formDemandAuthor :: User.UserId
  -- ^ The user filling the form
  , formDemandBuyer :: Legal.EntityId
  -- ^ For an employment contract, this is the employer
  , formDemandBuyerAgent :: Maybe Business.UnitId
  -- ^ For an employment contract, this is an (optional) agent of the employer
  , formDemandSeller :: User.UserId
  -- ^ For an employment contract, this is the employee
  , formDemandClient :: Maybe FormClient
  -- ^ For en employment contract, this is the entity providing the paiement
  -- source to the buyer
  }
  deriving (Show)

-- | Represent the optional client in a `FormDemand`.
data FormClient = FormClient
  { formClient :: Legal.EntityId
  , formClientAgent :: Maybe Business.UnitId
  }
  deriving (Show)

-- | Well formedness: a well formed form means that the system can receive it,
-- store it, and process it.
--
-- Legal.Entity and Business.Unit are related: all agent parties must be listed
-- within the legal entities. Each unit is within 1 or more entities.
isWellFormed :: [Business.UnitId] -> FormDemand -> WellFormedness
isWellFormed buyerAgentParties FormDemand {..} =
  toWellFormedness
    [ maybe True (`elem` buyerAgentParties) formDemandBuyerAgent
        --> "A buyer agent is given, but it is not a listed party of the buyer"
    ]

data WellFormedness = WellFormed | IllFormed [Err]
  deriving (Show)

toWellFormedness :: [[Err]] -> WellFormedness
toWellFormedness errs = if null errs' then WellFormed else IllFormed errs'
 where
  errs' = concat errs

-- | If the condition is `False`, returns the error message, otherwise returns
-- nothing.
(-->) bool msg = [msg | not bool]

type Err = Text

--------------------------------------------------------------------------------
-- Examples
--
-- 24 cases:
--
-- whether formAuthor == formSeller (2 cases)
--
-- formSeller is representative of formBuyer or formBuyerAgent (4 cases)
--
-- formBuyerAgent is Department or Activity or Nothing (3 cases)

entity222AgentParties :: [Business.UnitId]
entity222AgentParties = ["UNIT-444"]

form0 =
  FormDemand
    { formDemandAuthor = "USER-111"
    , formDemandBuyer = "ENT-222"
    , formDemandBuyerAgent = Nothing
    , formDemandSeller = "USER-333"
    , formDemandClient = Nothing
    }

form1 =
  FormDemand
    { formDemandAuthor = "USER-111"
    , formDemandBuyer = "ENT-222"
    , formDemandBuyerAgent = Nothing
    , formDemandSeller = "USER-111"
    , formDemandClient = Nothing
    }

form2 =
  FormDemand
    { formDemandAuthor = "USER-111"
    , formDemandBuyer = "ENT-222"
    , formDemandBuyerAgent = Just "UNIT-444"
    , formDemandSeller = "USER-333"
    , formDemandClient = Nothing
    }

form2' =
  FormDemand
    { formDemandAuthor = "USER-111"
    , formDemandBuyer = "ENT-222"
    , formDemandBuyerAgent = Just "UNIT-555"
    , formDemandSeller = "USER-333"
    , formDemandClient = Nothing
    }
