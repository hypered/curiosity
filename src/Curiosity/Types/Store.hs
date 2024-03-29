{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- brittany-disable-next-binding

-- |
--Module: Curiosity.Types.Store
--Description: This module defines the central state data type.
module Curiosity.Types.Store
  ( Db (..)
  , SteppingMode (..)
  , HaskDb

    -- * Instantiating databases.
  , emptyHask

    -- * Serialising and deseralising DB to bytes.
  , serialiseDb
  , deserialiseDb
  , deserialiseDbStrict

    -- * Re-exports
  , Command.Command (..)
  ) where

import Commence.Runtime.Errors qualified as E
import Curiosity.Types.Business qualified as Business
import Curiosity.Types.Command qualified as Command
  ( Command (..)
  )
import Curiosity.Types.Counter qualified as C
import Curiosity.Types.Email qualified as Email
import Curiosity.Types.Employment qualified as Employment
import Curiosity.Types.Invoice qualified as Invoice
import Curiosity.Types.Legal qualified as Legal
import Curiosity.Types.Order qualified as Order
import Curiosity.Types.Quotation qualified as Quotation
import Curiosity.Types.RemittanceAdv qualified as RemittanceAdv
import Curiosity.Types.SimpleContract qualified as SimpleContract
import Curiosity.Types.User qualified as User
import Data.Aeson
import Data.Text qualified as T
import Network.HTTP.Types.Status qualified as S
import System.PosixCompat.Types (EpochTime)
import System.Random qualified as Rand
import System.Random.Internal qualified as Rand
import System.Random.SplitMix qualified as SM

--------------------------------------------------------------------------------

-- | The central database. The product type contains all values and is
--parameterised by @datastore@. The @datastore@ can be the layer dealing with
--storage. When it is @Identity@, it just means the data is stored as is. It can,
--however, also be an `STM.TVar` if the datastore is to be STM based.
--
--Additionally, we want to parameterise over a @runtime@ type parameter. This is
--a container type of the database.

-- brittany-disable-next-binding
data Db (datastore :: Type -> Type) = Db
  { _dbNextBusinessId :: C.CounterValue datastore Int
  , _dbBusinessUnits :: datastore [Business.Unit]
  , _dbNextLegalId :: C.CounterValue datastore Int
  , _dbLegalEntities :: datastore [Legal.Entity]
  , _dbNextUserId :: C.CounterValue datastore Int
  , _dbUserProfiles :: datastore [User.UserProfile]
  , _dbNextQuotationId :: C.CounterValue datastore Int
  , _dbQuotations :: datastore [Quotation.Quotation]
  , _dbNextOrderId :: C.CounterValue datastore Int
  , _dbOrders :: datastore [Order.Order]
  , _dbNextInvoiceId :: C.CounterValue datastore Int
  , _dbInvoices :: datastore [Invoice.Invoice]
  , _dbNextRemittanceAdvId :: C.CounterValue datastore Int
  , _dbRemittanceAdvs :: datastore [RemittanceAdv.RemittanceAdv]
  , _dbNextEmploymentId :: C.CounterValue datastore Int
  , _dbEmployments :: datastore [Employment.Contract]
  , _dbRandomGenState :: datastore (Word64, Word64)
  -- ^ The internal representation of a StdGen.
  , _dbEpochTime :: datastore EpochTime
  -- ^ The internal time, possibly disconnected from the real wall clock.
  -- This is used to simulate the advance of time for automated processes
  -- triggered by the `step` command.
  , _dbSteppingMode :: datastore SteppingMode
  -- ^ How the time should advance, following the system time, or manually
  -- advancing it.
  , _dbFormCreateQuotationAll
      :: datastore (Map (User.UserName, Text) Quotation.CreateQuotationAll)
  , _dbFormCreateContractAll
      :: datastore (Map (User.UserName, Text) Employment.CreateContractAll)
  , _dbFormCreateSimpleContractAll
      :: datastore (Map (User.UserName, Text) SimpleContract.CreateContractAll)
  , _dbNextEmailId :: C.CounterValue datastore Int
  , _dbEmails :: datastore [Email.Email]
  }

data SteppingMode
  = -- | The time in STM is advanced by following the system time. This
    -- is used e.g. when demoing the web interface.
    Normal
  | -- | The time is advanced by running normal commands, or commands
    -- designed to change the time (e.g. @cty time@). This is used for
    -- e.g. deterministic tests.
    Stepped
  | -- | The time is advanced by running normal commands, tracking the
    -- elapsed time. It looks like `Normal` mode with the ability to
    -- travel in the future.
    Mixed
  deriving (Eq, Generic, Show)

deriving anyclass instance ToJSON SteppingMode
deriving anyclass instance FromJSON SteppingMode

-- | Hask database type: used for starting the system, values reside in @Hask@
-- (thus `Identity`)
type HaskDb = Db Identity

deriving instance Eq HaskDb
deriving instance Show HaskDb
deriving instance Generic HaskDb
deriving anyclass instance ToJSON HaskDb
deriving anyclass instance FromJSON HaskDb

-- | Instantiate a seed database that is empty.
emptyHask :: HaskDb
emptyHask =
  Db
    (pure 1)
    (pure mempty)
    (pure 1)
    (pure mempty)
    (pure 1)
    (pure mempty)
    (pure 1)
    (pure mempty)
    (pure 1)
    (pure mempty)
    (pure 1)
    (pure mempty)
    (pure 1)
    (pure mempty)
    (pure 1)
    (pure mempty)
    (pure initialGenState)
    (pure 0) -- 01/Jan/1970:01:00:00 +0100
    (pure Stepped)
    (pure mempty)
    (pure mempty)
    (pure mempty)
    (pure 1)
    (pure mempty)

initialGenState :: (Word64, Word64)
initialGenState = randomGenState 42 -- Deterministic initial seed.

newtype DbErr = DbDecodeFailed Text
  deriving (Show)

instance E.IsRuntimeErr DbErr where
  errCode =
    errCode' . \case
      DbDecodeFailed {} -> "DECODE_FAILED"
   where
    errCode' = mappend "ERR.DB."
  httpStatus = \case
    DbDecodeFailed {} -> S.internalServerError500
  userMessage =
    Just . \case
      DbDecodeFailed msg -> msg

--------------------------------------------------------------------------------

-- | Write an entire db state to bytes.
serialiseDb :: HaskDb -> LByteString
serialiseDb = encode
{-# INLINE serialiseDb #-}

-- | Read an entire db state from bytes.
deserialiseDb :: LByteString -> Either DbErr HaskDb
deserialiseDb = first (DbDecodeFailed . T.pack) . eitherDecode
{-# INLINE deserialiseDb #-}

-- | Read an entire db state from bytes.
deserialiseDbStrict :: ByteString -> Either DbErr HaskDb
deserialiseDbStrict = first (DbDecodeFailed . T.pack) . eitherDecodeStrict
{-# INLINE deserialiseDbStrict #-}

--------------------------------------------------------------------------------
-- We use System.Random.Internal and Sytem.Random.SplitMix to be able to keep
-- the random generator internal state (a pair of Word64) in our Data.StmDb
-- structure.
randomGenState :: Int -> (Word64, Word64)
randomGenState = SM.unseedSMGen . Rand.unStdGen . Rand.mkStdGen
