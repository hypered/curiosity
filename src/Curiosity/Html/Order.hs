-- |
--Module: Curiosity.Html.Order
--Description: Order pages (view and edit).
module Curiosity.Html.Order
  ( OrderPage (..)
  , panelOrders
  ) where

import Curiosity.Html.Misc
import Curiosity.Types.Order qualified as Order
import Curiosity.Types.User qualified as User
import Smart.Html.Misc qualified as Misc
import Text.Blaze.Html5 (Html)
import Text.Blaze.Html5 qualified as H

--------------------------------------------------------------------------------

-- | The page displaye at @/orders@ to show all orders.
data OrderPage = OrderPage
  { _emailPageUser :: Maybe User.UserProfile
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
    (
      [ Order.unOrderId _orderId
      ]
    , []
    , Nothing
    )
