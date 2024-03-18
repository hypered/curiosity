-- |
--Module: Curiosity.Html.Invoice
--Description: Invoice pages (view and edit).
module Curiosity.Html.Invoice
  ( InvoiceView (..)
  , CreateInvoicePage (..)
  ) where

import Curiosity.Html.Misc
import Curiosity.Types.Invoice qualified as Invoice
import Curiosity.Types.User qualified as User
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A

--------------------------------------------------------------------------------
data InvoiceView = InvoiceView
  { _invoiceViewInvoice :: Invoice.Invoice
  , _invoiceViewHasEditButton :: Maybe H.AttributeValue
  }

instance H.ToMarkup InvoiceView where
  toMarkup (InvoiceView invoice hasEditButton) =
    renderView $ invoiceView invoice hasEditButton

invoiceView invoice hasEditButton = containerLarge $ do
  title' "Invoice" hasEditButton
  H.dl ! A.class_ "c-key-value c-key-value--horizontal c-key-value--short" $ do
    keyValuePair "ID" (Invoice._entityId invoice)

--------------------------------------------------------------------------------
data CreateInvoicePage = CreateInvoicePage
  { _createInvoicePageUserProfile :: User.UserProfile
  -- ^ The user creating the invoice
  , _createInvoicePageSubmitURL :: H.AttributeValue
  }

instance H.ToMarkup CreateInvoicePage where
  toMarkup (CreateInvoicePage profile submitUrl) =
    renderForm profile $ groupLayout $ do
      title "New invoice"
      inputText "Invoice name" "name" Nothing Nothing
      submitButton submitUrl "Create new invoice"
