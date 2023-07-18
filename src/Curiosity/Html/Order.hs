{- |
Module: Curiosity.Html.Order
Description: Order pages (view and edit).
-}
module Curiosity.Html.Order
  ( OrderPage(..)
  , panelOrders
  ) where

import qualified Curiosity.Types.Order         as Order
import qualified Curiosity.Types.User          as User
import           Curiosity.Html.Misc
import qualified Smart.Html.Misc               as Misc
import           Text.Blaze.Html5               ( Html )
import qualified Text.Blaze.Html5              as H


--------------------------------------------------------------------------------
-- | The page displaye at @/orders@ to show all orders.
data OrderPage = OrderPage
  { _emailPageUser   :: Maybe User.UserProfile
    -- ^ The logged-in user, if any.
  , _emailPageOrders :: [Order.Order]
    -- ^ All enqueued emails.
  }

instance H.ToMarkup OrderPage where
  toMarkup (OrderPage mprofile emails) =
    renderView' mprofile $ panelOrders emails


--------------------------------------------------------------------------------
-- | Display orders.
panelOrders :: [Order.Order] -> Html
panelOrders orders =
  panel' "Orders" $ Misc.table "orders" titles display orders
 where
  titles = ["ID"]
  display Order.Order {..} =
    ( [ Order.unOrderId _orderId
      ]
    , []
    , Nothing
    )
