module Curiosity.Runtime.Quotation
  ( filterQuotations
  , filterQuotations'
  , selectQuotationById
  , createQuotation
  , createQuotationFull
  , modifyQuotations
  , createOrderForQuotation
  ) where

import Control.Concurrent.STM qualified as STM
import Curiosity.Core qualified as Core
import Curiosity.Runtime.Order (createOrder)
import Curiosity.Runtime.Type as RType
import Curiosity.STM.Helpers (atomicallyM)
import Curiosity.Types.Counter qualified as C
import Curiosity.Types.Order qualified as Order
import Curiosity.Types.Quotation qualified as Quotation
import Curiosity.Types.Store qualified as Store
import Curiosity.Types.User qualified as User

filterQuotations
  :: Core.StmDb -> Quotation.Predicate -> STM [Quotation.Quotation]
filterQuotations db predicate = do
  let tvar = Store._dbQuotations db
  records <- STM.readTVar tvar
  pure $ filter (Quotation.applyPredicate predicate) records

filterQuotations'
  :: forall m
   . (MonadReader Runtime m, MonadIO m)
  => Quotation.Predicate
  -> m [Quotation.Quotation]
filterQuotations' predicate = do
  db <- asks _rDb
  atomicallyM $ filterQuotations db predicate

selectQuotationById
  :: Core.StmDb -> Quotation.QuotationId -> IO (Maybe Quotation.Quotation)
selectQuotationById db id = STM.atomically $ Core.selectQuotationById db id

createQuotation
  :: Core.StmDb
  -> Quotation.Quotation
  -> STM (Either Quotation.Err Quotation.QuotationId)
createQuotation db quotation = do
  STM.catchSTM (Right <$> transaction) (pure . Left)
 where
  transaction = do
    newId <- C.newIdOf @Quotation.QuotationId (Store._dbNextQuotationId db)
    let new = quotation {Quotation._quotationId = newId}
    createQuotationFull db new >>= either STM.throwSTM pure

createQuotationFull
  :: Core.StmDb
  -> Quotation.Quotation
  -> STM (Either Quotation.Err Quotation.QuotationId)
createQuotationFull db new = do
  modifyQuotations db (<> [new])
  pure . Right $ Quotation._quotationId new

modifyQuotations
  :: Core.StmDb -> ([Quotation.Quotation] -> [Quotation.Quotation]) -> STM ()
modifyQuotations db f =
  let tvar = Store._dbQuotations db in STM.modifyTVar tvar f

createOrderForQuotation
  :: Core.StmDb
  -> (User.UserProfile, Quotation.QuotationId)
  -- ^ TODO SignQuotation data type, including e.g. the signature data.
  -> STM (Either Quotation.Err Order.OrderId)
createOrderForQuotation db _ = do
  mid <- createOrder db
  case mid of
    Right id -> pure $ Right id
    Left (Order.Err err) -> pure $ Left $ Quotation.Err err
