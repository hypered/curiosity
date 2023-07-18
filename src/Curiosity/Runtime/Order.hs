module Curiosity.Runtime.Order
  ( createOrder
  , createOrderFull
  , modifyOrders
  ) where

import qualified Control.Concurrent.STM        as STM
import qualified Curiosity.Core                as Core
import qualified Curiosity.Data                as Data
import qualified Curiosity.Types.Counter       as C 
import qualified Curiosity.Types.Order         as Order

createOrder :: Core.StmDb -> STM (Either Order.Err Order.OrderId)
createOrder db = do
  STM.catchSTM (Right <$> transaction) (pure . Left)
 where
  transaction = do
    newId <- C.newIdOf @Order.OrderId (Data._dbNextOrderId db)
    let new = Order.Order newId
    createOrderFull db new >>= either STM.throwSTM pure

createOrderFull
  :: Core.StmDb -> Order.Order -> STM (Either Order.Err Order.OrderId)
createOrderFull db new =
  modifyOrders db (<> [new]) $> Right (Order._orderId new)

modifyOrders :: Core.StmDb -> ([Order.Order] -> [Order.Order]) -> STM ()
modifyOrders db f = let tvar = Data._dbOrders db in STM.modifyTVar tvar f

