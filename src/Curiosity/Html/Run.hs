{- |
Module: Curiosity.Html.Run
Description: A page to run a `cty run` command.
-}
module Curiosity.Html.Run
  ( RunPage(..)
  ) where

import qualified Curiosity.Types.User          as User
import           Curiosity.Html.Misc
import qualified Text.Blaze.Html5              as H


--------------------------------------------------------------------------------
data RunPage = RunPage
  { _runPageUserProfile :: Maybe User.UserProfile
  , _runPageSubmitURL   :: H.AttributeValue
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
