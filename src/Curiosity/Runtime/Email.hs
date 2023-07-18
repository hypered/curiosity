module Curiosity.Runtime.Email
  ( filterEmails
  , filterEmails'
  , setEmailDone
  , selectEmailById
  ) where

import qualified Control.Concurrent.STM        as STM
import qualified Curiosity.Core                as Core
import qualified Curiosity.Data                as Data
import qualified Curiosity.Types.Email         as Email
import           Curiosity.Runtime.Type
import           Curiosity.STM.Helpers          ( atomicallyM )

filterEmails :: Core.StmDb -> Email.Predicate -> STM [Email.Email]
filterEmails db predicate = do
  let tvar = Data._dbEmails db
  records <- STM.readTVar tvar
  pure $ filter (Email.applyPredicate predicate) records

filterEmails'
  :: forall m
   . (MonadReader Runtime m, MonadIO m)
  => Email.Predicate
  -> m [Email.Email]
filterEmails' predicate = do
  db <- asks _rDb
  atomicallyM $ filterEmails db predicate

setEmailDone :: Core.StmDb -> Email.Email -> STM (Either Email.Err ())
setEmailDone db Email.Email {..} = do
  mrecord <- Core.selectEmailById db _emailId
  case mrecord of
    Just Email.Email{} -> do
      let replaceOlder records =
            [ if Email._emailId e == _emailId
                then e { Email._emailState = Email.EmailDone }
                else e
            | e <- records
            ]
      Core.modifyEmails db replaceOlder
      pure $ Right ()
    Nothing -> pure . Left $ Email.Err "Email not found" -- TODO

selectEmailById
  :: forall m
   . (MonadReader Runtime m, MonadIO m)
  => Email.EmailId
  -> m (Maybe Email.Email)
selectEmailById eid = do
  db <- asks _rDb
  atomicallyM $ Core.selectEmailById db eid

