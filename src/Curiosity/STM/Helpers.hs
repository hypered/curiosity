module Curiosity.STM.Helpers
  ( atomicallyM
  ) where

-- | Easier on the eyes instead of a combination of liftIO and STM.atomically over the place.
atomicallyM :: forall a m. MonadIO m => STM a -> m a
atomicallyM = liftIO . atomically
