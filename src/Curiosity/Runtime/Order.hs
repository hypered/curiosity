module Curiosity.Runtime.Order
  ( createOrder
  , createOrderFull
  , modifyOrders
  ) where

import Control.Concurrent.STM qualified as STM
import Curiosity.Core qualified as Core
import Curiosity.Types.Counter qualified as C
import Curiosity.Types.Order qualified as Order
import Curiosity.Types.Store qualified as Store

createOrder :: Core.StmDb -> STM (Either Order.Err Order.OrderId)
createOrder db = do
  STM.catchSTM (Right <$> transaction) (pure . Left)
 where
  transaction = do
    newId <- C.newIdOf @Order.OrderId (Store._dbNextOrderId db)
    let new = Order.Order newId
    createOrderFull db new >>= either STM.throwSTM pure

createOrderFull
  :: Core.StmDb -> Order.Order -> STM (Either Order.Err Order.OrderId)
createOrderFull db new =
  modifyOrders db (<> [new]) $> Right (Order._orderId new)

modifyOrders :: Core.StmDb -> ([Order.Order] -> [Order.Order]) -> STM ()
modifyOrders db f = let tvar = Store._dbOrders db in STM.modifyTVar tvar f
