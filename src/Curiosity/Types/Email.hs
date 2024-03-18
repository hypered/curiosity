{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}

-- |
--Module: Curiosity.Types.Email
--Description: Email -related data types.
--
--This module contains data types used to represent emails. Within
--"Curiosity.Types.Store", they are used to record atomically that an email
--should be sent during an STM operation. A separate thread should actually
--handle them.
module Curiosity.Types.Email
  ( -- * Useful constants
    systemEmailAddr

    -- * Main data representation
  , Email (..)
  , EmailId (..)
  , EmailTemplate (..)
  , emailTemplateName
  , displayEmailBody
  , displayEmailTitle
  , EmailState (..)
  , Predicate (..)
  , applyPredicate
  , Err (..)
  ) where

import Commence.Types.Wrapped qualified as W
import Curiosity.Types.PrefixedId qualified as Pre
import Curiosity.Types.User qualified as User
import Data.Aeson
import Text.Blaze.Html5 qualified as H
import Web.FormUrlEncoded
  ( FromForm (..)
  )
import Web.HttpApiData (FromHttpApiData (..))

--------------------------------------------------------------------------------
systemEmailAddr :: User.UserEmailAddr
systemEmailAddr = "hello@cty-2.hypered.systems"

--------------------------------------------------------------------------------

-- | This represents an email in database.
data Email = Email
  { _emailId :: EmailId
  , _emailTemplate :: EmailTemplate
  , _emailSender :: User.UserEmailAddr
  , _emailRecipient :: User.UserEmailAddr
  , _emailState :: EmailState
  -- ^ Similar to the state of a job queue item.
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Record ID of the form EMAIL-xxx.
newtype EmailId = EmailId {unEmailId :: Text}
  deriving (Eq, Show)
  deriving
    ( IsString
    , FromJSON
    , ToJSON
    , H.ToMarkup
    , H.ToValue
    )
    via Text
  deriving (FromHttpApiData, FromForm) via W.Wrapped "email-id" Text
  deriving (Pre.PrefixedId) via W.Wrapped "EMAIL-" Text

--------------------------------------------------------------------------------

-- | All the emails that can be sent by the system are given by variants of the
-- `EmailTemplate` data type. They can be sent by the
-- `Curiosity.Core.createEmail` function. (TODO This should be a link to
-- "Curiosity.Runtime".)
data EmailTemplate
  = -- | An email sent upon signup (see `Curiosity.Types.User.Signup`).
    SignupConfirmationEmail
  | -- | An email sent to invite a user. Their user profile was already created
    -- (in particular with an email address), and the email gives them a token
    -- (that serves as credentials).
    InviteEmail Text -- TODO Proper Text wrapper.
  | -- | An email sent when a quotation form is successfully submitted to the
    -- system (see `Curiosity.Types.Quotation.SubmitQuotation`).
    QuotationEmail
  | InvoiceEmail
  | InvoiceReminderEmail
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

emailTemplateName :: EmailTemplate -> Text
emailTemplateName = \case
  SignupConfirmationEmail -> "SignupConfirmation"
  InviteEmail _ -> "Invite"
  QuotationEmail -> "Quotation"
  InvoiceEmail -> "Invoice"
  InvoiceReminderEmail -> "InvoiceReminder"

displayEmailTitle :: EmailTemplate -> Text
displayEmailTitle = \case
  SignupConfirmationEmail -> "Signup confirmation"
  InviteEmail _ -> "Invitation"
  QuotationEmail -> "Quotation"
  InvoiceEmail -> "Invoice"
  InvoiceReminderEmail -> "Reminder"

displayEmailBody :: EmailTemplate -> Text
displayEmailBody = \case
  SignupConfirmationEmail -> "Hi, thank you for registering."
  InviteEmail token -> "Hi, you've been invited to Curiosity. Here is a token: " <> token
  QuotationEmail -> "Hi, here is a quotation."
  InvoiceEmail -> "Hi, here is an invoice."
  InvoiceReminderEmail -> "Hi, we didn't receive any payment for an invoice."

--------------------------------------------------------------------------------
data EmailState
  = -- | The email has been enqueued and must be processed.
    EmailTodo
  | -- | The email has been processed.
    EmailDone
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

--------------------------------------------------------------------------------

-- | Predicates to filter users.
data Predicate = AllEmails | EmailsFor User.UserEmailAddr | EmailsTodo | EmailsDone | AndEmails [Predicate]
  deriving (Eq, Show)

applyPredicate :: Predicate -> Email -> Bool
applyPredicate AllEmails _ = True
applyPredicate (EmailsFor addr) Email {..} = _emailRecipient == addr
applyPredicate EmailsTodo Email {..} = _emailState == EmailTodo
applyPredicate EmailsDone Email {..} = _emailState == EmailDone
applyPredicate (AndEmails predicates) email =
  all (`applyPredicate` email) predicates

--------------------------------------------------------------------------------
newtype Err = Err
  { unErr :: Text
  }
  deriving (Eq, Exception, Show)
