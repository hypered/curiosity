{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{- |
Module: Curiosity.Data.User
Description: User related datatypes
-}
module Curiosity.Data.User
  ( Signup(..)
  , Invite(..)
  , Login(..)
  , Credentials(..)
  , getUsername
  , getInviteToken
  , Update(..)
  , userCredsName
  , userCredsPassword
  , inviteToken
  , UserProfile'(..)
  , UserProfile
  , UserCompletion1(..)
  , UserCompletion2(..)
  , AccessRight(..)
  , Authorization(..)
  , permissions
  , userProfileCreds
  , userProfileId
  , userProfileDisplayName
  , userProfileBio
  , userProfileEmailAddr
  , userProfileTosConsent
  , userProfileAddrAndTelVerified
  , userProfileEId
  , userProfileEIdVerified
  , userProfileEmailAddrVerified
  , userProfilePostalAddress
  , userProfileTelephoneNbr
  , userProfileCompletion1
  , userProfileCompletion2
  , userProfileRegistrationDate
  , userProfileTwitterUsername
  , userProfileRights
  , userProfileAuthorizations
  , userProfileAdvisors
  , UserId(..)
  , UserName(..)
  , UserDisplayName(..)
  , UserEmailAddr(..)
  , Password(..)
  , Advisors(..)
  , Predicate(..)
  , applyPredicate
  , SetUserEmailAddrAsVerified(..)
  , userIdPrefix
  , firstUserId
  , firstUserRights
  , validateSignup
  , validateSignup'
  -- * Errors
  , Err(..)
  , userNotFound
  , usernameBlocklist
  , checkPassword
  ) where

import qualified Commence.Runtime.Errors       as Errs
import           Commence.Types.Secret          ( (=:=) )
import qualified Commence.Types.Secret         as Secret
import qualified Commence.Types.Wrapped        as W
import           Control.Lens
import qualified Curiosity.Html.Errors         as Pages
import           Data.Aeson
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as LT
import qualified Network.HTTP.Types            as HTTP
import qualified Servant.Auth.Server           as SAuth
import           System.PosixCompat.Types       ( EpochTime )
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Renderer.Text       ( renderMarkup )
import           Web.FormUrlEncoded             ( FromForm(..)
                                                , parseMaybe
                                                , parseUnique
                                                )
import           Web.HttpApiData                ( FromHttpApiData(..) )


--------------------------------------------------------------------------------
-- | Represents the input data used for user registration. This in effect is
-- the operation to create a user.
data Signup = Signup
  { username   :: UserName
  , password   :: Password
  , email      :: UserEmailAddr
  , tosConsent :: Bool
  }
  deriving (Generic, Eq, Show)

instance FromForm Signup where
  fromForm f =
    Signup
      <$> parseUnique "username"   f
      <*> parseUnique "password"   f
      <*> parseUnique "email-addr" f
      <*> (   (Just "tos-consent-granted" ==)
          .   fmap T.toLower
          <$> parseMaybe "tos-consent" f
          )

-- | Represents the input data used for user invite. This creates a user
-- profile without associated username/password.
data Invite = Invite
  { _inviteEmail :: UserEmailAddr
  }
  deriving (Generic, Eq, Show)

instance FromForm Invite where
  fromForm f =
    Invite
      <$> parseUnique "email-addr"   f

-- | Represents the input data to log in a user.
data Login = Login
  { _loginUsername :: UserName
  , _loginPassword :: Password
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance FromForm Login where
  fromForm f = Login <$> parseUnique "username" f <*> parseUnique "password" f

-- | Represents the input data to update a user profile.
data Update = Update
  { _updateUserId          :: UserId
  , _updateDisplayName     :: Maybe UserDisplayName
  , _updateBio             :: Maybe Text
  , _updateTwitterUsername :: Maybe Text
  }
  deriving (Eq, Show, Generic)

instance FromForm Update where
  fromForm f =
    Update
      <$> parseUnique "user-id"          f
      <*> parseMaybe  "display-name"     f
      <*> parseMaybe  "bio"              f
      <*> parseMaybe  "twitter-username" f


--------------------------------------------------------------------------------
type UserProfile = UserProfile' Credentials UserDisplayName UserEmailAddr Bool

data UserProfile' creds userDisplayName userEmailAddr tosConsent = UserProfile
  { _userProfileId                :: UserId
  , _userProfileCreds             :: creds
    -- ^ Users credentials
  , _userProfileDisplayName       :: Maybe userDisplayName
    -- ^ User's human friendly name
  , _userProfileBio               :: Maybe Text
    -- ^ Public bio
  , _userProfileEmailAddr         :: userEmailAddr
    -- ^ User's email address
  , _userProfileEmailAddrVerified :: Maybe Text
    -- ^ TODO Last date it was checked
  , _userProfileTosConsent        :: tosConsent
  , _userProfileCompletion1       :: UserCompletion1
  , _userProfileCompletion2       :: UserCompletion2
  , _userProfileRegistrationDate  :: EpochTime
  , _userProfileTwitterUsername   :: Maybe Text
  , _userProfileRights            :: [AccessRight]
  , _userProfileAuthorizations    :: [Authorization]
  , _userProfileAdvisors          :: Maybe Advisors
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Represents user credentials (username / password pairs, or invite tokens)
data Credentials =
    Credentials
    { _userCredsName     :: UserName
    , _userCredsPassword :: Password
    }
  | InviteToken
    { _inviteToken :: Text
    }
  deriving (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

getUsername :: Credentials -> Maybe UserName
getUsername Credentials {..} = Just _userCredsName
getUsername _ = Nothing

getInviteToken :: Credentials -> Maybe Text
getInviteToken InviteToken {..} = Just _inviteToken
getInviteToken _ = Nothing

-- For Completion-1 level
data UserCompletion1 = UserCompletion1
  { _userProfilePostalAddress      :: Maybe Text -- ^ Non-structured for now.
  , _userProfileTelephoneNbr       :: Maybe Text
  , _userProfileAddrAndTelVerified :: Maybe Text -- TODO Date.
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- For Completion-2 level
data UserCompletion2 = UserCompletion2
  { _userProfileEId         :: Maybe Text -- TODO Not sure what data this is.
  , _userProfileEIdVerified :: Maybe Text -- TODO Date.
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- Enable/disable some accesses.
data AccessRight = CanCreateContracts | CanVerifyEmailAddr
  deriving (Eq, Enum, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

permissions :: [AccessRight]
permissions = [toEnum 0 ..]

-- | The username is an identifier (i.e. it is unique).
newtype UserName = UserName { unUserName :: Text }
                 deriving ( Eq
                          , Ord
                          , Show
                          , IsString
                          , FromJSON
                          , ToJSON
                          , H.ToMarkup
                          , H.ToValue
                          ) via Text
                 deriving (FromHttpApiData, FromForm) via W.Wrapped "username" Text

newtype UserDisplayName = UserDisplayName { unUserDisplayName :: Text }
                 deriving ( Eq
                          , Show
                          , IsString
                          , FromJSON
                          , ToJSON
                          , H.ToMarkup
                          , H.ToValue
                          ) via Text
                 deriving (FromHttpApiData, FromForm) via W.Wrapped "display-name" Text

newtype UserEmailAddr = UserEmailAddr { unUserEmailAddr :: Text }
                 deriving ( Eq
                          , Show
                          , IsString
                          , FromJSON
                          , ToJSON
                          , H.ToMarkup
                          , H.ToValue
                          ) via Text
                 deriving (FromHttpApiData, FromForm) via W.Wrapped "email-addr" Text

newtype Password = Password (Secret.Secret '[ 'Secret.ToJSONExp] Text)
                 deriving (Eq, IsString) via Text
                 deriving ( FromHttpApiData
                          , FromJSON
                          , ToJSON
                          ) via (Secret.Secret '[ 'Secret.ToJSONExp] Text)
                 deriving stock Show
                 deriving FromForm via W.Wrapped "password" Text

-- | Record ID of the form USER-xxx.
newtype UserId = UserId { unUserId :: Text }
               deriving (Eq, Show, SAuth.ToJWT, SAuth.FromJWT)
               deriving ( IsString
                        , FromJSON
                        , ToJSON
                        , H.ToMarkup
                        , H.ToValue
                        ) via Text
               deriving (FromHttpApiData, FromForm) via W.Wrapped "user-id" Text

userIdPrefix :: Text
userIdPrefix = "USER-"

-- TODO Ask Roger the meaning of these.
-- | Those are in addition of AccessRight, maybe they should be combined
-- together.
data Authorization = AuthorizedAsEmployee | AuthorizedAsHolder | AuthorizedAsAdvisor | AuthorizedAsSuperAdvisor | AccountingAuthorized | OnlineAccountAuthorized
  deriving (Eq, Generic, Show)
  deriving (FromJSON, ToJSON)

-- | Represents the user current advisor and past advisors.
data Advisors = Advisors
  { _userAdvisorsCurrent :: UserId
  , _userAdvisorsPast    :: [(EpochTime, EpochTime, UserId)] -- From, to, user ID
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)


--------------------------------------------------------------------------------
data SetUserEmailAddrAsVerified = SetUserEmailAddrAsVerified UserName

instance FromForm SetUserEmailAddrAsVerified where
  fromForm f = SetUserEmailAddrAsVerified <$> parseUnique "username" f


--------------------------------------------------------------------------------
-- | Predicates to filter users.
data Predicate = PredicateEmailAddrToVerify | PredicateHas AccessRight
  deriving (Eq, Show)

applyPredicate :: Predicate -> UserProfile -> Bool
applyPredicate PredicateEmailAddrToVerify UserProfile {..} =
  isNothing _userProfileEmailAddrVerified

applyPredicate (PredicateHas a) UserProfile {..} = a `elem` _userProfileRights

data Err = UserExists
             | UsernameBlocked -- ^ See `usernameBlocklist`.
             | NoTosConsent
             | UserNotFound Text -- Username or ID.
             | IncorrectUsernameOrPassword
             | EmailAddrAlreadyVerified
             | MissingRight AccessRight
             | ValidationErrs [Err]
             deriving (Eq, Exception, Show)

instance Errs.IsRuntimeErr Err where
  errCode = errCode' . \case
    UserExists                  -> "USER_EXISTS"
    UsernameBlocked             -> "USERNAME_BLOCKED"
    NoTosConsent                -> "NO_TOS_CONSENT"
    UserNotFound{}              -> "USER_NOT_FOUND"
    IncorrectUsernameOrPassword -> "INCORRECT_CREDENTIALS"
    EmailAddrAlreadyVerified    -> "EMAIL_ADDR_ALREADY_VERIFIED"
    MissingRight _              -> "MISSING_RIGHT_" <> "TODO"
    ValidationErrs _            -> "VALIDATION_ERRS"
    where errCode' = mappend "ERR.USER"

  httpStatus = \case
    UserExists                  -> HTTP.conflict409
    UsernameBlocked             -> HTTP.conflict409 -- TODO Check relevant code.
    NoTosConsent                -> HTTP.conflict409
    UserNotFound{}              -> HTTP.notFound404
    IncorrectUsernameOrPassword -> HTTP.unauthorized401
    EmailAddrAlreadyVerified    -> HTTP.conflict409
    MissingRight _              -> HTTP.unauthorized401
    ValidationErrs _            -> HTTP.conflict409

  userMessage = Just . \case
    UserExists -> LT.toStrict . renderMarkup . H.toMarkup $ Pages.ErrorPage
      409
      "User exists"
      "A user with the same username or ID already exists."
    UsernameBlocked ->
      LT.toStrict . renderMarkup . H.toMarkup $ Pages.ErrorPage
        409 -- TODO
        "Username disallowed"
        "Some usernames are not allowed. Please select another."
    NoTosConsent ->
      LT.toStrict . renderMarkup . H.toMarkup $ Pages.ErrorPage
        409 -- TODO
        "TOS consent required"
        "To use our services, accepting the Terms of Services is required."
    UserNotFound msg ->
      LT.toStrict . renderMarkup . H.toMarkup $ Pages.ErrorPage
        404
        "User not found"
        ("The supplied username or ID doesn't exist: " <> msg)
    IncorrectUsernameOrPassword ->
      LT.toStrict . renderMarkup . H.toMarkup $ Pages.ErrorPage
        401
        "Wrong credentials"
        "The supplied username or password is incorrect."
    EmailAddrAlreadyVerified ->
      LT.toStrict . renderMarkup . H.toMarkup $ Pages.ErrorPage
        409
        "Life-cycle error"
        "The user email address is already verified."
    MissingRight a ->
      LT.toStrict
        .  renderMarkup
        .  H.toMarkup
        $  Pages.ErrorPage 401 "Unauthorized action"
        $  "The user has not the required access right "
        <> show a
    ValidationErrs [err] -> maybe "TODO" identity $ Errs.userMessage err
    ValidationErrs _ ->
      LT.toStrict . renderMarkup . H.toMarkup $ Pages.ErrorPage
        409 -- TODO
        "Validation errors"
        "TODO"


--------------------------------------------------------------------------------
makeLenses ''UserCompletion2
makeLenses ''UserCompletion1
makeLenses ''Credentials
makeLenses ''UserProfile'


--------------------------------------------------------------------------------
-- | Given a Signup form, tries to return a proper `UserProfile` value, although
-- the ID is dummy. Maybe we should have separate data types (with or without
-- the ID).
-- This is a pure function: everything required to perform the validation
-- should be provided as arguments.
validateSignup
  :: EpochTime
  -> UserId
  -> Signup
  -> Either [Err] UserProfile
validateSignup now id Signup {..} = if null errors
  then Right profile
  else Left errors
 where
  profile = UserProfile
          id
          (Credentials username password)
          Nothing
          Nothing
          email
          Nothing
          tosConsent
          (UserCompletion1 Nothing Nothing Nothing)
          (UserCompletion2 Nothing Nothing)
          now
          Nothing
          -- The very first user has plenty of rights:
          (if id == firstUserId then firstUserRights else [])
          -- TODO Define some mechanism to get the initial authorizations
          [AuthorizedAsEmployee]
          Nothing
  errors = concat
    [ if not tosConsent
      then [NoTosConsent]
      else []
    , if username `elem` usernameBlocklist
      then [UsernameBlocked]
      else []
    ]

-- | Similar to `validateCreateQuotation` but throw away the returned
-- contract, i.e. keep only the errors.
validateSignup' :: EpochTime -> UserId -> Signup -> [Err]
validateSignup' now id signup =
  either identity (const []) $ validateSignup now id signup


--------------------------------------------------------------------------------
-- | Define the first user ID. A test exists to make sure this matches the
-- behavior of `generateUserId`.
firstUserId :: UserId
firstUserId = UserId $ userIdPrefix <> "1"

firstUserRights :: [AccessRight]
firstUserRights = [CanCreateContracts, CanVerifyEmailAddr]


--------------------------------------------------------------------------------
-- | In addition of dynamic rules (e.g. the username should not already be
-- taken), we disallow some names because they might be used later by the
-- system or the company, or cause confusion (because usernames are used as
-- e.g. smartcoop.sh/<username>).
usernameBlocklist :: [UserName]
usernameBlocklist =
  [ "a"
  , "about"
  , "data"
  , "documentation"
  , "echo"
  , "errors"
  , "forms"
  , "login"
  , "messages"
  , "settings"
  , "signup"
  , "smart"
  , "smartcoop" -- In a real syste, I guess this one should be a username that
                -- we ensure is created.
  , "state"
  , "views"
  ]

-- | Convenient way to create a `UserNotFound` value on `Left.`
userNotFound :: forall a . Text -> Either Err a
userNotFound = Left . UserNotFound . mappend "User not found: "


--------------------------------------------------------------------------------
-- TODO Use constant-time string comparison.
checkPassword :: UserProfile -> Password -> Bool
checkPassword profile (Password passInput) = case _userProfileCreds profile of
  Credentials {..} ->
    let Password storedPass = _userCredsPassword
    in storedPass =:= passInput
  InviteToken _ -> False
