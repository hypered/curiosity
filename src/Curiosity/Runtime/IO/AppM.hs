{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
module Curiosity.Runtime.IO.AppM
  ( AppM(..)
  , runAppMSafe
  , checkCredentials
  ) where

import qualified Commence.Multilogging         as ML
import qualified Commence.Runtime.Errors       as Errs
import           Commence.Types.Secret          ( (=:=) )
import qualified Control.Concurrent.STM        as STM
import           Control.Lens                  as Lens
import "exceptions" Control.Monad.Catch         ( MonadCatch
                                                , MonadMask
                                                , MonadThrow
                                                )
import qualified Curiosity.Core                as Core
import qualified Curiosity.Data                as Data
import qualified Curiosity.Data.User           as User
import           Curiosity.Runtime.Type        as RType
import           Prelude                 hiding ( state )


--------------------------------------------------------------------------------
newtype AppM a = AppM { runAppM :: ReaderT Runtime (ExceptT Errs.RuntimeErr IO) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadReader Runtime
           , MonadError Errs.RuntimeErr
           , MonadThrow
           , MonadCatch
           , MonadMask
           )

-- | Run the `AppM` computation catching all possible exceptions.
runAppMSafe
  :: forall a m
   . MonadIO m
  => Runtime
  -> AppM a
  -> m (Either Errs.RuntimeErr a)
runAppMSafe runtime AppM {..} =
  liftIO
    . fmap (join . first Errs.RuntimeException)
    . try @SomeException
    . runExceptT
    $ runReaderT runAppM runtime


--------------------------------------------------------------------------------
-- | Support for logging for the application
instance ML.MonadAppNameLogMulti AppM where
  askLoggers = asks _rLoggers
  localLoggers modLogger =
    local (over rLoggers . over ML.appNameLoggers $ fmap modLogger)


--------------------------------------------------------------------------------
modifyUserProfiles id f userProfiles = STM.modifyTVar userProfiles f $> [id]

checkCredentials
  :: Core.StmDb -> User.Credentials -> STM (Maybe User.UserProfile)
checkCredentials db User.Credentials {..} = do
  mprofile <- Core.selectUserByUsername db _userCredsName
  case mprofile of
    Just profile | checkPassword profile _userCredsPassword ->
      pure $ Just profile
    _ -> pure Nothing
checkCredentials db User.InviteToken {..} = do
  mprofile <- Core.selectUserByInviteToken db _inviteToken
  case mprofile of
    Just profile -> pure $ Just profile
    _ -> pure Nothing

-- TODO Use constant-time string comparison.
checkPassword :: User.UserProfile -> User.Password -> Bool
checkPassword profile (User.Password passInput) = case User._userProfileCreds profile of
  User.Credentials {..} ->
    let User.Password storedPass = _userCredsPassword
    in storedPass =:= passInput
  User.InviteToken _ -> False
