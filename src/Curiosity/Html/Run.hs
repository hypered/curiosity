-- |
--Module: Curiosity.Html.Run
--Description: A page to run a `cty run` command.
module Curiosity.Html.Run
  ( RunPage (..)
  ) where

import Curiosity.Html.Misc
import Curiosity.Types.User qualified as User
import Text.Blaze.Html5 qualified as H

--------------------------------------------------------------------------------
data RunPage = RunPage
  { _runPageUserProfile :: Maybe User.UserProfile
  , _runPageSubmitURL :: H.AttributeValue
  }

instance H.ToMarkup RunPage where
  toMarkup (RunPage mprofile submitUrl) =
    renderForm' mprofile $ groupLayout $ do
      title "Run a command"
      inputText
        "Command"
        "command"
        Nothing
        $ Just
          "A command similar to what `cty run` can accept."
      submitButton submitUrl "Run"
