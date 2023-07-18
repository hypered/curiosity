{- |
Module: Curiosity.Html.Email
Description: Page and components to display emails.
-}
module Curiosity.Html.Email
  ( EmailsPage(..)
  , EmailPage(..)
  , panelSentEmails
  ) where

import qualified Curiosity.Types.Email         as Email
import qualified Curiosity.Types.User          as User
import           Curiosity.Html.Misc
import qualified Smart.Html.Misc               as Misc
import           Text.Blaze.Html5               ( (!) )
import           Text.Blaze.Html5               ( Html )
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A


--------------------------------------------------------------------------------
-- | The page is served at @/emails@ to show all enqueued emails.
data EmailsPage = EmailsPage
  { _emailsPageUser   :: Maybe User.UserProfile
    -- ^ The logged-in user, if any.
  , _emailsPageEmails :: [Email.Email]
    -- ^ All enqueued emails.
  }

instance H.ToMarkup EmailsPage where
  toMarkup (EmailsPage mprofile emails) =
    renderView' mprofile $ panelSentEmails emails


--------------------------------------------------------------------------------
-- | The page is served at @/emails/<id>@ to show a specific email.
data EmailPage = EmailPage
  { _emailPageUser   :: Maybe User.UserProfile
    -- ^ The logged-in user, if any.
  , _emailPageEmail :: Email.Email
    -- ^ The email to display.
  }

instance H.ToMarkup EmailPage where
  toMarkup (EmailPage mprofile email) =
    renderView' mprofile $ panelEmail email


--------------------------------------------------------------------------------
-- | Display emails.
panelSentEmails :: [Email.Email] -> Html
panelSentEmails emails =
  panel' "Emails" $ Misc.table "emails" titles display emails
 where
  titles = ["ID", "Template", "Sender", "Recipient", "State"]
  display Email.Email {..} =
    ( [ Email.unEmailId _emailId
      , Email.emailTemplateName _emailTemplate
      , User.unUserEmailAddr _emailSender
      , User.unUserEmailAddr _emailRecipient
      , displayEmailState _emailState
      ]
    , []
    , (Just $ "/emails/" <> Email.unEmailId _emailId)
    )

-- | Display an email.
panelEmail :: Email.Email -> Html
panelEmail Email.Email {..} =
  panel' "Email" $
    H.dl
      ! A.class_ "c-key-value c-key-value--horizontal c-key-value--short"
      $ do
          keyValuePair
            "ID"
            (Email.unEmailId _emailId)
          keyValuePair
            "Template"
            (H.code . H.text $ Email.emailTemplateName _emailTemplate)
          keyValuePair
            "Sender"
            (linkEmail $ User.unUserEmailAddr _emailSender)
          keyValuePair
            "Recipient"
            (linkEmail $ User.unUserEmailAddr _emailRecipient)
          keyValuePair
            "State"
            (H.code . H.text $ displayEmailState _emailState)
          keyValuePair
            "Title"
            (Email.displayEmailTitle _emailTemplate)
          keyValuePair
            "Body"
            (Email.displayEmailBody _emailTemplate)

displayEmailState =
  \case 
    Email.EmailTodo -> "TODO"
    Email.EmailDone -> "DONE"
