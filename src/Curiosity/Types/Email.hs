{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{- |
Module: Curiosity.Types.Email
Description: Email -related data types.

This module contains data types used to represent emails. Within
"Curiosity.Types.Store", they are used to record atomically that an email
should be sent during an STM operation. A separate thread should actually
handle them.

-}
module Curiosity.Types.Email
  ( -- * Useful constants
    systemEmailAddr
    -- * Main data representation
  , Email(..)
  , EmailId(..)
  , EmailTemplate(..)
  , emailTemplateName
  , displayEmailBody
  , displayEmailTitle
  , SignupConfirmationEmail(..)
  , InviteEmail(..)
  , QuotationEmail(..)
  , InvoiceEmail(..)
  , InvoiceReminderEmail(..)
  , EmailState(..)
  , Predicate(..)
  , applyPredicate
  , Err(..)
  ) where

import qualified Commence.Types.Wrapped        as W
import qualified Curiosity.Types.PrefixedId    as Pre
import qualified Curiosity.Types.User          as User
import           Data.Aeson
import           GHC.Show (Show(..)) -- For manual Show instances.
import qualified Text.Blaze.Html5              as H
import           Web.FormUrlEncoded             ( FromForm(..)
                                                )
import           Web.HttpApiData                ( FromHttpApiData(..) )

--------------------------------------------------------------------------------
systemEmailAddr :: User.UserEmailAddr
systemEmailAddr = "hello@cty-2.hypered.systems"


--------------------------------------------------------------------------------
-- | This represents an email in database.
data Email = Email
  { _emailId        :: EmailId
  , _emailTemplate  :: EmailTemplate
  , _emailSender    :: User.UserEmailAddr
  , _emailRecipient :: User.UserEmailAddr
  , _emailState     :: EmailState -- ^ Similar to the state of a job queue item.
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Record ID of the form EMAIL-xxx.
newtype EmailId = EmailId { unEmailId :: Text }
               deriving (Eq, Show)
               deriving ( IsString
                        , FromJSON
                        , ToJSON
                        , H.ToMarkup
                        , H.ToValue
                        ) via Text
               deriving (FromHttpApiData, FromForm) via W.Wrapped "email-id" Text
               deriving Pre.PrefixedId via W.Wrapped "EMAIL-" Text


--------------------------------------------------------------------------------
-- | All the emails that can be sent by the system are given by instances of
-- the `EmailTemplate` type class. They can be sent by the
-- `Curiosity.Core.createEmail` function. (TODO This should be a link to
-- "Curiosity.Runtime".)
data EmailTemplate = forall a . IsEmailTemplate a => EmailTemplate a

instance Show EmailTemplate where
  show (EmailTemplate a) = "EmailTemplate (" <> GHC.Show.show a <> ")"

instance Eq EmailTemplate where
  EmailTemplate a == EmailTemplate b = getTemplateName a == getTemplateName b
  -- We ignore the possible parameters to the template.

instance ToJSON EmailTemplate where
  toJSON (EmailTemplate a) = toJSON $
    object ["template" .= getTemplateName a, "parameters" .= toJSON a]

instance FromJSON EmailTemplate where
  parseJSON v = do
    name <- v .: "template"
    case name of
      "SignupConfirmation" ->  -- TODO This defeats the point of using a type class
                               -- instead of a central sum type: we still have to
                               -- enumerate all the cases in a central place (i.e.
                               -- here).

class (Show a, Eq a, Generic a, ToJSON a, FromJSON a) => IsEmailTemplate a where
  getTemplateName :: a -> Text
  getTemplateTitle :: a -> Text
  getTemplateBody :: a -> Text

emailTemplateName :: EmailTemplate -> Text
emailTemplateName (EmailTemplate t) = getTemplateName t

displayEmailTitle :: EmailTemplate -> Text
displayEmailTitle (EmailTemplate t) = getTemplateTitle t

displayEmailBody :: EmailTemplate -> Text
displayEmailBody (EmailTemplate t) = getTemplateBody t

emailTemplateName' :: Email -> Text
emailTemplateName' = emailTemplateName . _emailTemplate


--------------------------------------------------------------------------------
-- | An email sent upon signup (see `Curiosity.Types.User.Signup`).
data SignupConfirmationEmail = SignupConfirmationEmail
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance IsEmailTemplate SignupConfirmationEmail where
  getTemplateName _ = "SignupConfirmation"
  getTemplateTitle _ = "Sign up confirmation"
  getTemplateBody _ = "Hi, thank you for registering."

-- | An email sent to invite a user. Their user profile was already created
-- (in particular with an email address), and the email gives them a token
-- (that serves as credentials).
data InviteEmail = InviteEmail Text -- TODO Proper Text wrapper.
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance IsEmailTemplate InviteEmail where
  getTemplateName _ = "Invite"
  getTemplateTitle _ = "Invitation"
  getTemplateBody (InviteEmail token) =
    "Hi, you've been invited to Curiosity. Here is a token: " <> token

-- | An email sent when a quotation form is successfully submitted to the
-- system (see `Curiosity.Types.Quotation.SubmitQuotation`).
data QuotationEmail = QuotationEmail
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance IsEmailTemplate QuotationEmail where
  getTemplateName _ = "Quotation"
  getTemplateTitle _ = "Quotation"
  getTemplateBody _ = "Hi, here is a quotation."

data InvoiceEmail = InvoiceEmail
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance IsEmailTemplate InvoiceEmail where
  getTemplateName _ = "Invoice"
  getTemplateTitle _ = "Invoice"
  getTemplateBody _ = "Hi, here is an invoice."

data InvoiceReminderEmail = InvoiceReminderEmail
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance IsEmailTemplate InvoiceReminderEmail where
  getTemplateName _ = "InvoiceReminder"
  getTemplateTitle _ = "Reminder"
  getTemplateBody _ = "Hi, we didn't receive any payment for an invoice."


--------------------------------------------------------------------------------
data EmailState =
    EmailTodo -- ^ The email has been enqueued and must be processed.
  | EmailDone -- ^ The email has been processed.
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
