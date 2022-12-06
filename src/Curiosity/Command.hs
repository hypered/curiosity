{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
module Curiosity.Command
  ( Command(..)
  , RunOutput(..)
  , QueueName(..)
  , Queues(..)
  , ParseConf(..)
  , CommandWithTarget(..)
  , CommandTarget(..)
  , CommandUser(..)
  , ObjectType(..)
  , parserInfo
  , parserInfoWithTarget
  , commandToString
  ) where

import qualified Curiosity.Data                as Data
import qualified Curiosity.Data.Business       as Business
import qualified Curiosity.Data.Email          as Email
import qualified Curiosity.Data.Employment     as Employment
import qualified Curiosity.Data.Invoice        as Invoice
import qualified Curiosity.Data.Legal          as Legal
import qualified Curiosity.Data.Order          as Order
import qualified Curiosity.Data.Quotation      as Quotation
import qualified Curiosity.Data.SimpleContract as SimpleContract
import qualified Curiosity.Data.User           as User
import qualified Curiosity.Parse               as P
import qualified Data.Text                     as T
import qualified Options.Applicative           as A


--------------------------------------------------------------------------------
-- | Describes the command available from the command-line with `cty`, or
-- within the UNIX-domain socket server, `cty sock`, or the `cty repl` REPL, or
-- the `cty run` interpreter.
data Command =
    Layout
    -- ^ Display the routing layout of the web server.
  | Init Data.SteppingMode
    -- ^ Initialise a new, empty state file.
  | Reset
    -- ^ Set the state file to the empty state.
  | Repl P.Conf
    -- ^ Run a REPL.
  | Run P.Conf FilePath RunOutput
    -- ^ Interpret a script. If True, outputs traces, otherwise only the final state.
  | Serve P.Conf P.ServerConf
    -- ^ Run an HTTP server.
  | Sock P.Conf
    -- ^ Run a UNIX-domain socket server.
    -- This is a simple UNIX-domain socket REPL server. The accepted commands
    -- re-use the optparse-applicative parsers behind the `cty` command-line tool,
    -- ensuring a similar experience. It is possible to interact with this server
    -- with e.g.:
    --
    --   nc -U curiosity.sock
  | Parse ParseConf
    -- ^ Parse a single command.
  | State Bool
    -- ^ Show the full state. If True, use Haskell format instead of JSON.
  | Threads
    -- ^ Show the state of the threads.
  | StartEmail
    -- ^ Spwan the thread processing enqueued emails.
  | StopEmail
    -- ^ Kill the thread processing enqueued emails.
  | StepEmail
    -- ^ Run one iteration of the email processing loop.
  | CreateBusinessUnit Business.Create
  | UpdateBusinessUnit Business.Update
  | LinkBusinessUnitToUser Text User.UserId Business.ActingRole
  | CreateLegalEntity Legal.Create
  | UpdateLegalEntity Legal.Update
  | LinkLegalEntityToUser Text User.UserId Legal.ActingRole
  | UpdateLegalEntityIsSupervised Text Bool
    -- ^ Make a legal entity as 'supervised' (True) or 'not supervised'
    -- (False).
  | UpdateLegalEntityIsHost Text Bool
    -- ^ Make a legal entity as 'host' (True) or 'not host' (False).
  | Signup User.Signup
  | Invite User.Invite
  | SelectUser Bool User.UserId Bool
    -- ^ Show a given user. If True, use Haskell format instead of JSON. If
    -- True, show only the user ID and username.
  | FilterUsers User.Predicate
  | UpdateUser User.Update
  | DeleteUser User.UserId
  | SetUserEmailAddrAsVerified User.UserName
    -- ^ High-level operations on users.
  | SignQuotation Quotation.QuotationId
  | RejectQuotation Quotation.QuotationId (Maybe Text)
  | CreateEmployment Employment.CreateContractAll
  | CreateInvoice
  | EmitInvoice Order.OrderId
    -- ^ Generate and invoice and send an email with it (or a link to it).
  | MatchPayment Invoice.InvoiceId
    -- ^ Notify the system that a payment matching an invoice was done.
  | SendReminder Invoice.InvoiceId
    -- ^ Send an email to remind of an unpaid invoice.
  | FormNewQuotation Quotation.CreateQuotationAll
    -- ^ Create a new instance of the quotation creation form.
  | FormValidateQuotation Text
    -- ^ Run validation rules only (the same ones used in `FormSubmitQuotation`).
  | FormSubmitQuotation Quotation.SubmitQuotation
    -- ^ Submit a quotation.
  | FormNewSimpleContract SimpleContract.CreateContractAll'
  | FormValidateSimpleContract Text
  | FilterEmails Email.Predicate
  | ViewQueue QueueName
    -- ^ View queue. The queues can be filters applied to objects, not
    -- necessarily explicit list in database.
  | ViewQueues Queues
  | Step Bool Bool
    -- ^ Execute the next automated action when using stepped (non-wallclock)
    -- mode, or mixed-mode, or the next automated action when the automation is
    -- "disabled".
    -- If True, execute all of them.
    -- If True, don't actually execute them but report what whould be done.
  | Time Bool Bool
    -- ^ Report or set the simulated time.
    -- If True, advance the time by a second.
    -- If True, advance the time to the next minute.
  | Log Text P.Conf
    -- Log a line of text to the logs.
  | ShowId Text
    -- ^ If not a command per se, assume it's an ID to be looked up.
  deriving (Eq, Show)

-- | Select if traces and/or the final state should be output.
--   run --silent      # no traces, no final state
--   run --final-only  # no traces,    final state, for streaming
--   run --final       #    traces,    final state
--   run               #    traces, no final state, equivalent to
--   run --traces-only #    traces, no final state
data RunOutput = RunOutput Bool Bool
  deriving (Eq, Show)


data QueueName = EmailAddrToVerify | EmailsToSend
  deriving (Eq, Show)

data Queues = CurrentUserQueues | AllQueues | AutomatedQueues | ManualQueues | UserQueues User.UserName
  deriving (Eq, Show)

data ParseConf =
    ConfCommand Text
  | ConfFileName FilePath
  | ConfStdin
  | ParseObject ObjectType FilePath
  deriving (Eq, Show)

data ObjectType = ParseState | ParseUser
  deriving (Eq, Show)

-- | The same commands, defined above, can be used within the UNIX-domain
-- socket server, `cty sock`, but also from a real command-line tool, `cty`.
-- In the later case, a user might want to direct the command-line tool to
-- interact with a server, or a local state file. This data type is meant to
-- augment the above commands with such options. In addition, the command is
-- supposed to be performed by the given user. This is to be set by SSH's
-- ForceCommand.
data CommandWithTarget = CommandWithTarget Command CommandTarget CommandUser
  deriving (Eq, Show)

data CommandTarget = MemoryTarget | StateFileTarget FilePath | UnixDomainTarget FilePath
  deriving (Eq, Show)

-- Running a command can be done by passing explicitely a user (this is
-- intended to be used behind SSH, or possibly when administring the system)
-- or, for convenience, by taking the UNIX login from the current session.
data CommandUser = UserFromLogin | User User.UserName
  deriving (Eq, Show)


--------------------------------------------------------------------------------
parserInfo :: A.ParserInfo Command
parserInfo =
  A.info (parser <**> A.helper)
    $  A.fullDesc
    <> A.header "cty - Curiosity, a prototype application for Smart Coop"
    <> A.progDesc
         "Curiosity is a prototype application to explore the design space \
        \of a web application for Smart."

parserInfoWithTarget :: A.ParserInfo CommandWithTarget
parserInfoWithTarget =
  A.info (parser' <**> A.helper)
    $  A.fullDesc
    <> A.header "cty - Curiosity, a prototype application for Smart Coop"
    <> A.progDesc
         "Curiosity is a prototype application to explore the design space \
        \of a web application for Smart."
 where
  parser' = do
    user <-
      A.option (A.eitherReader (Right . User . User.UserName . T.pack))
      $  A.short 'u'
      <> A.long "user"
      <> A.value UserFromLogin
      <> A.help
           "A username performing this command. If not given, the username is \
           \the current UNIX login name."
      <> A.metavar "USERNAME"
    target <-
      (A.flag' MemoryTarget $ A.short 'm' <> A.long "memory" <> A.help
        "Don't use a state file or a UNIX-domain socket."
      )
      <|> StateFileTarget
      <$> (  A.strOption
          $  A.short 's'
          <> A.long "state"
          <> A.value "state.json"
          <> A.help "A state file. Default is 'state.json'."
          <> A.metavar "FILEPATH"
          )
      <|> UnixDomainTarget
      <$> (  A.strOption
          $  A.short 't'
          <> A.long "socket"
          <> A.help "A UNIX-domain socket"
          <> A.metavar "FILEPATH"
          )
    command <- parser
    pure $ CommandWithTarget command target user


--------------------------------------------------------------------------------
parser :: A.Parser Command
parser =
  A.subparser
      (  A.command
          "layout"
          ( A.info (parserLayout <**> A.helper)
          $ A.progDesc "Display the routing layout of the web server"
          )

      <> A.command
           "init"
           ( A.info (parserInit <**> A.helper)
           $ A.progDesc "Initialise a new, empty state file"
           )

      <> A.command
           "reset"
           ( A.info (parserReset <**> A.helper)
           $ A.progDesc "Reset a state file to the empty state"
           )

      <> A.command
           "repl"
           (A.info (parserRepl <**> A.helper) $ A.progDesc "Start a REPL")

      <> A.command
           "run"
           (A.info (parserRun <**> A.helper) $ A.progDesc "Interpret a script"
           )

      <> A.command
           "serve"
           ( A.info (parserServe <**> A.helper)
           $ A.progDesc "Run the Curiosity HTTP server"
           )

      <> A.command
           "sock"
           ( A.info (parserSock <**> A.helper)
           $ A.progDesc "Run the Curiosity UNIX-domain server"
           )

      <> A.command
           "parse"
           ( A.info (parserParse <**> A.helper)
           $ A.progDesc "Parse a single command"
           )

      <> A.command
           "state"
           ( A.info (parserState <**> A.helper)
           $ A.progDesc "Show the full state"
           )

      <> A.command
           "threads"
           ( A.info (parserThreads <**> A.helper)
           $ A.progDesc "Show the threads state"
           )

      <> A.command
           "start-email"
           ( A.info (parserStartEmail <**> A.helper)
           $ A.progDesc "Start the thread processing enqueued emails"
           )

      <> A.command
           "stop-email"
           ( A.info (parserStopEmail <**> A.helper)
           $ A.progDesc "Stop the thread processing enqueued emails"
           )

      <> A.command
           "step-email"
           ( A.info (parserStepEmail <**> A.helper)
           $ A.progDesc "Run one iteration of the email processing loop"
           )

      <> A.command
           "unit"
           ( A.info (parserUnit <**> A.helper)
           $ A.progDesc "Commands related to business units"
           )

      <> A.command
           "entity"
           ( A.info (parserLegal <**> A.helper)
           $ A.progDesc "Commands related to legal entities"
           )

      <> A.command
           "user"
           ( A.info (parserUser <**> A.helper)
           $ A.progDesc "User-related commands"
           )

      <> A.command
           "users"
           ( A.info (parserUsers <**> A.helper)
           $ A.progDesc "Users-related commands"
           )

      <> A.command
           "employment"
           ( A.info (parserEmployment <**> A.helper)
           $ A.progDesc "Commands related to employment contracts"
           )

      <> A.command
           "quotation"
           ( A.info (parserQuotation <**> A.helper)
           $ A.progDesc "Commands related to quotations"
           )

      <> A.command
           "invoice"
           ( A.info (parserInvoice <**> A.helper)
           $ A.progDesc "Commands related to invoices"
           )

      <> A.command
           "payment"
           ( A.info (parserPayment <**> A.helper)
           $ A.progDesc "Commands related to payments"
           )

      <> A.command
           "reminder"
           ( A.info (parserReminder <**> A.helper)
           $ A.progDesc "Commands related to reminders"
           )

      <> A.command
           "forms"
           (A.info (parserForms <**> A.helper) $ A.progDesc
             "Fill and validate forms (i.e. data in the staging area)"
           )

      <> A.command
           "queue"
           ( A.info (parserQueue <**> A.helper)
           $ A.progDesc "Display a queue's content"
           )

      <> A.command
           "queues"
           ( A.info (parserQueues <**> A.helper)
           $ A.progDesc "Display all queues content"
           )

      <> A.command
           "step"
           ( A.info (parserStep <**> A.helper)
           $ A.progDesc "Run the next automated action(s)"
           )

      <> A.command
           "time"
           ( A.info (parserTime <**> A.helper)
           $ A.progDesc "Report or set the simulated time"
           )

      <> A.command
           "log"
           ( A.info (parserLog <**> A.helper)
           $ A.progDesc "Log a message to the logs"
           )
      )
    <|> parserShowId

parserLayout :: A.Parser Command
parserLayout = pure Layout

parserInit :: A.Parser Command
parserInit =
  Init
    <$> (   (A.flag' Data.Normal $ A.long "normal" <> A.help
              "Select normal stepping mode (default)."
            )
        <|> (A.flag' Data.Stepped $ A.long "stepped" <> A.help
              "Select stepped stepping mode."
            )
        <|> (A.flag' Data.Mixed $ A.long "mixed" <> A.help
              "Select mixed stepping mode."
            )
        <|> (pure Data.Normal)
        )

parserReset :: A.Parser Command
parserReset = pure Reset

parserRepl :: A.Parser Command
parserRepl = Repl <$> P.confParser

parserRun :: A.Parser Command
parserRun =
  Run
    <$> P.confParser
    <*> A.argument
          A.str
          (A.metavar "FILE" <> A.action "file" <> A.help "Script to run.")
    <*> (   (A.flag' (RunOutput False False) $ A.long "silent" <> A.help
              "Don't display traces, nor the final state."
            )
        <|> (A.flag' (RunOutput False True) $ A.long "final-only" <> A.help
              "Don't display traces, but display the final state."
           -- Showing the final state ensures that the effect of the commands
           -- are applied.
            )
        <|> (A.flag' (RunOutput True True) $ A.long "final" <> A.help
              "Display both traces and the final state."
            )
        <|> (  A.flag (RunOutput True False) (RunOutput True False)
            $  A.long "traces-only"
            <> A.help
                 "Display traces but not the final state. This is the default."
            )
        )

parserServe :: A.Parser Command
parserServe = Serve <$> P.confParser <*> P.serverParser

parserSock :: A.Parser Command
parserSock = Sock <$> P.confParser

parserParse :: A.Parser Command
parserParse = Parse <$> (parserCommand <|> parserFileName <|> parserObject)

parserCommand :: A.Parser ParseConf
parserCommand = ConfCommand <$> A.strOption
  (A.long "command" <> A.short 'c' <> A.metavar "COMMAND" <> A.help
    "Command to parse."
  )

parserFileName :: A.Parser ParseConf
parserFileName = A.argument (A.eitherReader f)
                            (A.metavar "FILE" <> A.help "Command to parse.")
 where
  f "-" = Right ConfStdin
  f s   = Right $ ConfFileName s

parserObject :: A.Parser ParseConf
parserObject =
  ParseObject
    <$> A.option
          (A.eitherReader f)
          (A.long "object" <> A.metavar "TYPE" <> A.help
            "Type of the object to parse."
          )
    <*> A.argument A.str (A.metavar "FILE" <> A.help "Object to parse.")
 where
  f "state" = Right ParseState
  f "user"  = Right ParseUser
  f _       = Left "Unrecognized object type."

parserState :: A.Parser Command
parserState = State <$> A.switch
  (A.long "hs" <> A.help "Use the Haskell format (default is JSON).")

parserThreads :: A.Parser Command
parserThreads = pure Threads

parserStartEmail :: A.Parser Command
parserStartEmail = pure StartEmail

parserStopEmail :: A.Parser Command
parserStopEmail = pure StopEmail

parserStepEmail :: A.Parser Command
parserStepEmail = pure StepEmail

parserUnit :: A.Parser Command
parserUnit =
  A.subparser
    $  A.command
         "create"
         ( A.info (parserCreateBusinessUnit <**> A.helper)
         $ A.progDesc "Create a new business unit"
         )
    <> A.command
         "update"
         ( A.info (parserUpdateBusinessUnit <**> A.helper)
         $ A.progDesc "Update a business unit"
         )
    <> A.command
         "link-user"
         ( A.info (parserBusinessUnitLinkUser <**> A.helper)
         $ A.progDesc "Link a user to the unit, specifying a role"
         )

parserCreateBusinessUnit :: A.Parser Command
parserCreateBusinessUnit = do
  slug <- A.argument
    A.str
    (A.metavar "SLUG" <> A.help "An identifier suitable for URLs")
  name <- A.argument A.str (A.metavar "NAME" <> A.help "A name")
  pure $ CreateBusinessUnit $ Business.Create slug name

parserUpdateBusinessUnit :: A.Parser Command
parserUpdateBusinessUnit = do
  slug        <- argumentUnitSlug
  description <- A.argument A.str (A.metavar "TEXT" <> A.help "A description")
  pure $ UpdateBusinessUnit $ Business.Update slug (Just description)

parserBusinessUnitLinkUser :: A.Parser Command
parserBusinessUnitLinkUser = do
  slug <- argumentUnitSlug
  uid  <- argumentUserId
  role <-
    (A.flag' Business.Holder $ A.long "holder" <> A.help
      "TODO Add a description for the holder flag"
    )
  pure $ LinkBusinessUnitToUser slug uid role

argumentUnitSlug = A.argument A.str metavarUnitSlug

metavarUnitSlug = A.metavar "SLUG" <> A.completer complete <> A.help
  "A business unit slug"
  where complete = A.mkCompleter . const $ pure ["alpha", "beta", "gamma"]
        -- TODO I'd like to lookup slugs in the state, but here we don't
        -- know where the state is (it depends on other command-line options).

parserLegal :: A.Parser Command
parserLegal =
  A.subparser
    $  A.command
         "create"
         ( A.info (parserCreateLegalEntity <**> A.helper)
         $ A.progDesc "Create a new legal entity"
         )
    <> A.command
         "update"
         ( A.info (parserUpdateLegalEntity <**> A.helper)
         $ A.progDesc "Update a legal entity"
         )
    <> A.command
         "link-user"
         ( A.info (parserLegalEntityLinkUser <**> A.helper)
         $ A.progDesc "Link a user to the entity, specifying a role"
         )
    <> A.command
         "set-supervised"
         ( A.info (parserUpdateLegalEntityIsSupervised True <**> A.helper)
         $ A.progDesc "Mark the entity as 'supervised'"
         )
    <> A.command
         "unset-supervised"
         ( A.info (parserUpdateLegalEntityIsSupervised False <**> A.helper)
         $ A.progDesc "Mark the entity as 'not supervised'"
         )
    <> A.command
         "set-host"
         ( A.info (parserUpdateLegalEntityIsHost True <**> A.helper)
         $ A.progDesc "Mark the entity as 'host'"
         )
    <> A.command
         "unset-host"
         ( A.info (parserUpdateLegalEntityIsHost False <**> A.helper)
         $ A.progDesc "Mark the entity as 'not host'"
         )

parserCreateLegalEntity :: A.Parser Command
parserCreateLegalEntity = do
  slug <- A.argument
    A.str
    (A.metavar "SLUG" <> A.help "An identifier suitable for URLs")
  name <- A.argument A.str (A.metavar "NAME" <> A.help "A registration name")
  cbeNumber <- A.argument
    A.str
    (  A.metavar "CBE-NUMBER"
    <> A.help "CBE number (without prefix or leading zero)"
    )
  vatNumber <- A.argument
    A.str
    (A.metavar "VAT-NUMER" <> A.help "VAT number (with prefix and leading zero")
  pure $ CreateLegalEntity $ Legal.Create slug name cbeNumber vatNumber

parserUpdateLegalEntity :: A.Parser Command
parserUpdateLegalEntity = do
  slug        <- argumentEntitySlug
  description <- A.argument A.str (A.metavar "TEXT" <> A.help "A description")
  pure $ UpdateLegalEntity $ Legal.Update slug (Just description)

parserLegalEntityLinkUser :: A.Parser Command
parserLegalEntityLinkUser = do
  slug <- argumentEntitySlug
  uid  <- argumentUserId
  role <-
    (A.flag' Legal.Validator $ A.long "validator" <> A.help
      "TODO Add a description for the validator flag"
    )
  pure $ LinkLegalEntityToUser slug uid role

parserUpdateLegalEntityIsSupervised :: Bool -> A.Parser Command
parserUpdateLegalEntityIsSupervised b = do
  slug <- argumentEntitySlug
  pure $ UpdateLegalEntityIsSupervised slug b

parserUpdateLegalEntityIsHost :: Bool -> A.Parser Command
parserUpdateLegalEntityIsHost b = do
  slug <- argumentEntitySlug
  pure $ UpdateLegalEntityIsHost slug b

argumentEntitySlug = A.argument A.str metavarEntitySlug

metavarEntitySlug = A.metavar "SLUG" <> A.completer complete <> A.help
  "A legal entity slug"
  where complete = A.mkCompleter . const $ pure ["one", "two", "three"]
        -- TODO I'd like to lookup slugs in the state, but here we don't
        -- know where the state is (it depends on other command-line options).

parserUser :: A.Parser Command
parserUser = A.subparser
  (  A.command
      "signup"
      (A.info (parserSignup <**> A.helper) $ A.progDesc "Create a new user")
  <> A.command
       "invite"
       (A.info (parserInvite <**> A.helper) $ A.progDesc "Invite a new user")
  <> A.command
       "update"
       (A.info (parserUpdateUser <**> A.helper) $ A.progDesc "Update a user")
  <> A.command
       "delete"
       (A.info (parserDeleteUser <**> A.helper) $ A.progDesc "Delete a user")
  <> A.command
       "get"
       (A.info (parserGetUser <**> A.helper) $ A.progDesc "Select a user")
  <> A.command
       "do"
       ( A.info (parserUserLifeCycle <**> A.helper)
       $ A.progDesc "Perform a high-level operation on a user"
       )
  )

parserUsers :: A.Parser Command
parserUsers = do
  predicate <-
    (  A.flag' User.PredicateEmailAddrToVerify
    $  A.long "email-addr-to-verify"
    <> A.help "Show users with an email address to verify."
    )
    <|> (  A.flag' (User.PredicateHas User.CanCreateContracts)
        $  A.long "can-create-contracts"
        <> A.help "Show users with the right to create contracts."
        )
    <|> (  A.flag' (User.PredicateHas User.CanVerifyEmailAddr)
        $  A.long "can-verify-email-addr"
        <> A.help "Show users with the right to verify email addresses."
        )
  pure $ FilterUsers predicate

parserSignup :: A.Parser Command
parserSignup = do
  username   <- A.argument A.str (A.metavar "USERNAME" <> A.help "A username")
  password   <- A.argument A.str (A.metavar "PASSWORD" <> A.help "A password")
  email <- A.argument A.str (A.metavar "EMAIL" <> A.help "An email address")
  tosConsent <- A.switch
    (  A.long "accept-tos"
    <> A.help "Indicate if the user being created consents to the TOS."
    )
  pure $ Signup $ User.Signup username password email tosConsent

parserInvite :: A.Parser Command
parserInvite = do
  email <- A.argument A.str (A.metavar "EMAIL" <> A.help "An email address")
  pure $ Invite $ User.Invite email

parserUpdateUser :: A.Parser Command
parserUpdateUser = do
  uid      <- argumentUserId
  name     <- A.argument A.str (A.metavar "NAME" <> A.help "A display name")
  bio      <- A.argument A.str (A.metavar "TEXT" <> A.help "A bio")
  mtwitter <- A.option
    A.auto
    (  A.long "twitter"
    <> A.value Nothing
    <> A.help "Twitter username."
    <> A.metavar "USERNAME"
    )
  pure $ UpdateUser $ User.Update uid (Just name) (Just bio) mtwitter

parserDeleteUser :: A.Parser Command
parserDeleteUser = DeleteUser <$> argumentUserId

parserGetUser :: A.Parser Command
parserGetUser =
  SelectUser
    <$> A.switch
          (A.long "hs" <> A.help "Use the Haskell format (default is JSON).")
    <*> argumentUserId
    <*> A.switch (A.long "short" <> A.help "Show only the ID and username.")

argumentUserId = User.UserId <$> A.argument A.str metavarUserId

metavarUserId = A.metavar "USER-ID" <> A.completer complete <> A.help
  "A user ID"
  where complete = A.mkCompleter . const $ pure ["USER-", "USER-1", "USER-2"]
        -- TODO I'd like to lookup IDs in the state, but here we don't know
        -- where the state is (it depends on other command-line options).

argumentUserName = User.UserName <$> A.argument A.str metavarUserName

metavarUserName = A.metavar "USERNAME" <> A.completer complete <> A.help
  "A username"
  where complete = A.mkCompleter . const $ pure ["alice", "bob", "charlie"]
        -- TODO I'd like to lookup usernames in the state, but here we don't
        -- know where the state is (it depends on other command-line options).

parserUserLifeCycle :: A.Parser Command
parserUserLifeCycle = A.subparser $ A.command
  "set-email-addr-as-verified"
  ( A.info (p <**> A.helper)
  $ A.progDesc "Perform a high-level operation on a user"
  )
  where p = SetUserEmailAddrAsVerified <$> argumentUserName

parserEmployment :: A.Parser Command
parserEmployment = A.subparser $ A.command
  "create"
  ( A.info (parserCreateEmployment <**> A.helper)
  $ A.progDesc "Create a new employment contract"
  )

parserCreateEmployment :: A.Parser Command
parserCreateEmployment =
  pure $ CreateEmployment Employment.emptyCreateContractAll

parserQuotation :: A.Parser Command
parserQuotation =
  A.subparser
    $  A.command
         "sign"
         ( A.info (parserSignQuotation <**> A.helper)
         $ A.progDesc "Accept (sign) a quotation"
         )
    <> A.command
         "reject"
         ( A.info (parserRejectQuotation <**> A.helper)
         $ A.progDesc "Reject a quotation"
         )

parserSignQuotation :: A.Parser Command
parserSignQuotation = do
  id <- A.argument
    A.str
    (A.metavar "QUOTATION-ID" <> A.help "A quotation identifier.")
  pure $ SignQuotation id

parserRejectQuotation :: A.Parser Command
parserRejectQuotation = do
  id <- A.argument
    A.str
    (A.metavar "QUOTATION-ID" <> A.help "A quotation identifier.")
  mcomment <-
    optional $ A.strOption $ A.long "comment" <> A.metavar "COMMENT" <> A.help
      "A possible comment accompanying the rejection."
  pure $ RejectQuotation id mcomment

parserInvoice :: A.Parser Command
parserInvoice =
  A.subparser
    $  A.command
         "create"
         ( A.info (parserCreateInvoice <**> A.helper)
         $ A.progDesc "Create a new invoice"
         )
    <> A.command
         "emit"
         ( A.info (parserEmitInvoice <**> A.helper)
         $ A.progDesc "Generate and send an invoice"
         )

parserCreateInvoice :: A.Parser Command
parserCreateInvoice = pure CreateInvoice

parserEmitInvoice :: A.Parser Command
parserEmitInvoice = do
  id <- A.strOption $ A.long "from" <> A.metavar "ORDER-ID" <> A.help
    "An order identifier."
  pure $ EmitInvoice $ Order.OrderId id


--------------------------------------------------------------------------------
parserForms :: A.Parser Command
parserForms =
  A.subparser
    $  A.command
         "quotation"
         ( A.info (parserFormQuotation <**> A.helper)
         $ A.progDesc "Fill and submit quotation forms"
         )
    <> A.command
         "simple-contract"
         ( A.info (parserFormSimpleContract <**> A.helper)
         $ A.progDesc "Fill and submit simple contract forms"
         )


--------------------------------------------------------------------------------
parserFormQuotation :: A.Parser Command
parserFormQuotation =
  A.subparser
    $  A.command
         "new"
         ( A.info (parserFormNewQuotation <**> A.helper)
         $ A.progDesc "Create a new form session"
         )
    <> A.command
         "validate"
         ( A.info (parserFormValidateQuotation <**> A.helper)
         $ A.progDesc "Run validation rules against a form"
         )
    <> A.command
         "submit"
         ( A.info (parserFormSubmitQuotation <**> A.helper)
         $ A.progDesc "Submit a form"
         )

parserFormNewQuotation :: A.Parser Command
parserFormNewQuotation = do
  client <- A.option
    (A.eitherReader (Right . User.UserName . T.pack))
    (A.long "client" <> A.help "Client username." <> A.metavar "USERNAME")
  pure $ FormNewQuotation $ Quotation.CreateQuotationAll (Just client)

parserFormValidateQuotation :: A.Parser Command
parserFormValidateQuotation = do
  key <- A.argument A.str
                    (A.metavar "KEY" <> A.help "A quotation form identifier.")
  pure $ FormValidateQuotation key

parserFormSubmitQuotation :: A.Parser Command
parserFormSubmitQuotation = do
  key <- A.argument A.str
                    (A.metavar "KEY" <> A.help "A quotation form identifier.")
  pure $ FormSubmitQuotation $ Quotation.SubmitQuotation key


--------------------------------------------------------------------------------
parserPayment :: A.Parser Command
parserPayment = A.subparser $ A.command
  "match"
  ( A.info (parserMatchPayment <**> A.helper)
  $ A.progDesc "Match a payment to an invoice"
  )

parserMatchPayment :: A.Parser Command
parserMatchPayment = do
  id <- A.argument A.str
                   (A.metavar "INVOICE-ID" <> A.help "An invoice identifier.")
  pure $ MatchPayment id

parserReminder :: A.Parser Command
parserReminder = A.subparser $ A.command
  "send"
  ( A.info (parserSendReminder <**> A.helper)
  $ A.progDesc "Send an invoice payment reminder"
  )

parserSendReminder :: A.Parser Command
parserSendReminder = do
  id <- A.argument A.str
                   (A.metavar "INVOICE-ID" <> A.help "An invoice identifier.")
  pure $ SendReminder id


--------------------------------------------------------------------------------
parserFormSimpleContract :: A.Parser Command
parserFormSimpleContract =
  A.subparser
    $  A.command
         "new"
         ( A.info (parserFormNewSimpleContract <**> A.helper)
         $ A.progDesc "Create a new form session"
         )
    <> A.command
         "validate"
         ( A.info (parserFormValidateSimpleContract <**> A.helper)
         $ A.progDesc "Run validation rules against a form"
         )

parserFormNewSimpleContract :: A.Parser Command
parserFormNewSimpleContract = do
  amount <- A.option
    A.auto
    (A.long "amount" <> A.value 0 <> A.help "Invoicing amount." <> A.metavar
      "EURO"
    )
  pure $ FormNewSimpleContract SimpleContract.emptyCreateContractAll'
    { SimpleContract._createContractInvoice' =
      SimpleContract.emptyCreateContractInvoice
        { SimpleContract._createContractAmount = amount
        }
    }

parserFormValidateSimpleContract :: A.Parser Command
parserFormValidateSimpleContract = do
  key <- A.argument
    A.str
    (A.metavar "KEY" <> A.help "A simple contract form identifier.")
  pure $ FormValidateSimpleContract key

-- TODO I'm using subcommands to have all queue names appear in `--help` but
-- the word COMMAND seems wrong:
--
--     cty queue --help
--     Usage: <interactive> queue COMMAND
--       Display a queue's content
--
--     Available options:
--       -h,--help                Show this help text
--
--     Available commands:
--       user-email-addr-to-verify
--
-- The advantage compared to a metavar and a completer is that there is a help
-- text.
parserQueue :: A.Parser Command
parserQueue = ViewQueue <$> A.subparser
  (  A.command
      "user-email-addr-to-verify"
      ( A.info (pure EmailAddrToVerify <**> A.helper)
      $ A.progDesc
          "Show users with an email address that need verification. \
      \Use `cty user do set-email-addr-as-verified` to mark an email address \
      \as verified."
      )
  <> A.command
       "emails-to-send"
       ( A.info (pure EmailsToSend <**> A.helper)
       $ A.progDesc
           "Show users with an email address that need verification. \
      \Use `cty user do set-email-addr-as-verified` to mark an email address \
      \as verified."
       )
  )

parserQueues :: A.Parser Command
parserQueues =
  ViewQueues
    <$> (   (A.flag' CurrentUserQueues $ A.long "current-user" <> A.help
              "Display the queues of the current user. This is the default."
            )
        <|> (A.flag' AllQueues $ A.long "all" <> A.help
              "Display all the queues of the system."
            )
        <|> (  A.flag' AutomatedQueues
            $  A.long "automated"
            <> A.help
                 "Display the queues that can be handled automatically by the system."
            )
        <|> (A.flag' ManualQueues $ A.long "manual" <> A.help
              "Display the queues that can be handled by users."
            )
        <|> (  A.option
                (A.eitherReader (Right . UserQueues . User.UserName . T.pack))
            $  A.long "from"
            <> A.value CurrentUserQueues
            <> A.help "Display the queues of the give user."
            <> A.metavar "USERNAME"
            )
        )

parserStep :: A.Parser Command
parserStep =
  Step
    <$> A.switch
          (  A.long "all"
          <> A.help "Execute all possible actions (default is one action)."
          )
    <*> A.switch
          (A.long "dry" <> A.help
            "Don't actually execute actions, but report what would be done."
          )

parserTime :: A.Parser Command
parserTime =
  Time
    <$> A.switch (A.long "step" <> A.help "Advance the time of 1 second.")
    <*> A.switch
          (A.long "minute" <> A.help "Advance the time to the next minute.")

parserLog :: A.Parser Command
parserLog =
  Log
    <$> A.argument A.str (A.metavar "MESSAGE" <> A.help "A line of text")
    <*> P.confParser

parserShowId :: A.Parser Command
parserShowId =
  ShowId <$> A.argument A.str (A.metavar "ID" <> A.help "An object ID")


--------------------------------------------------------------------------------
-- | Serialise a `Command` to a string, suitable to be sent to `cty sock` for
-- instance.
commandToString :: Command -> Either Text Text
commandToString = \case
  Init _      -> Left "Can't send `init` to a server."
  Reset       -> Right "reset"
  State useHs -> Right $ "state" <> if useHs then " --hs" else ""
  Signup User.Signup {..} ->
    Right $ "user signup " <> User.unUserName username
      <> " " <> {- TODO -} T.take 1 (User.unUserName username)
      <> " " <> User.unUserEmailAddr email
      <> (if tosConsent then " --accept-tos" else "")
  SelectUser useHs userId isShort ->
    Right $ "user get " <> User.unUserId userId
      <> (if useHs then " --hs" else "")
      <> (if isShort then " --short" else "")
  _           -> Left "Unimplemented"
