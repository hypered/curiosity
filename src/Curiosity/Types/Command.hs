{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- |
--Module: Curiosity.Types.Command
--Description: A command received from the web interface.
module Curiosity.Types.Command
  ( Command (..)
  ) where

import Web.FormUrlEncoded
  ( FromForm (..)
  , parseUnique
  )

--------------------------------------------------------------------------------

-- | Represent a command received through a web page.
-- the operation to create a user.
data Command = Command {command :: Text}
  deriving (Generic, Eq, Show)

instance FromForm Command where
  fromForm f = Command <$> parseUnique "command" f
