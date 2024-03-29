module Curiosity.Run
  ( run
  ) where

import Commence.Multilogging qualified as ML
import Commence.Runtime.Errors qualified as Errs
import Curiosity.Command qualified as Command
import Curiosity.Interpret qualified as Interpret
import Curiosity.Parse qualified as P
import Curiosity.Process qualified as P
import Curiosity.Runtime qualified as Runtime
import Curiosity.Server qualified as Srv
import Curiosity.Types.Email qualified as Email
import Curiosity.Types.Store qualified as Store
import Curiosity.Types.User qualified as User
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BS
import Data.String qualified as S
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Network.Socket
import Network.Socket.ByteString
  ( recv
  , send
  )
import Options.Applicative qualified as A
import System.Console.Haskeline qualified as HL
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import System.Posix.User (getLoginName)

--------------------------------------------------------------------------------
run :: Command.CommandWithTarget -> IO ExitCode
-- Convert UserFromLogin to User.
run (Command.CommandWithTarget command target Command.UserFromLogin) = do
  login <- User.UserName . T.pack <$> getLoginName
  run (Command.CommandWithTarget command target $ Command.User login)
run (Command.CommandWithTarget Command.Layout _ _) = do
  Srv.routingLayout >>= putStrLn
  exitSuccess
run (Command.CommandWithTarget (Command.Init mode) (Command.StateFileTarget path) _) =
  do
    exists <- liftIO $ doesFileExist path
    if exists
      then do
        putStrLn @Text $ "The file '" <> T.pack path <> "' already exists."
        putStrLn @Text "Aborting."
        exitFailure
      else do
        let bs = Store.serialiseDb Store.emptyHask {Store._dbSteppingMode = pure mode}
        try @SomeException (BS.writeFile path bs)
          >>= either
            (\e -> print e >> exitFailure)
            ( const $ do
                putStrLn @Text $ "State file '" <> T.pack path <> "' created."
                exitSuccess
            )
run (Command.CommandWithTarget Command.Reset (Command.StateFileTarget path) _) =
  do
    runtime <-
      Runtime.bootConf P.defaultConf {P._confDbFile = Just path} Runtime.NoThreads >>= either throwIO pure
    Runtime.runRunM runtime Runtime.reset
    Runtime.powerdown runtime
    putStrLn @Text "State is now empty."
    exitSuccess
run (Command.CommandWithTarget (Command.Repl conf) (Command.StateFileTarget _) (Command.User user)) =
  do
    threads <- Runtime.emptyReplThreads
    runtime <- Runtime.bootConf conf threads >>= either throwIO pure
    let handleExceptions = (`catch` P.shutdown runtime . Just)
    handleExceptions $ do
      repl runtime user
      P.shutdown runtime Nothing
run (Command.CommandWithTarget (Command.Serve conf serverConf) target _) = do
  -- Allow to set the static and data directories through environment
  -- variable. This is useful to bake the correct values in a Docker image
  -- or a virtual machine image, instead of having the user provide them.
  mstaticDir <- lookupEnv "CURIOSITY_STATIC_DIR"
  mdataDir <- lookupEnv "CURIOSITY_DATA_DIR"
  mscenariosDir <- lookupEnv "CURIOSITY_SCENARIOS_DIR"
  let serverConf' =
        maybe identity (\a c -> c {P._serverDataDir = a}) mdataDir
          . maybe identity (\a c -> c {P._serverStaticDir = a}) mstaticDir
          . maybe identity (\a c -> c {P._serverScenariosDir = a}) mscenariosDir
          $ serverConf

  case target of
    Command.MemoryTarget -> handleServe conf serverConf'
    Command.StateFileTarget path ->
      handleServe conf {P._confDbFile = Just path} serverConf'
    Command.UnixDomainTarget _ -> do
      putStrLn @Text "TODO"
      exitFailure
run (Command.CommandWithTarget (Command.Sock conf) target _) = do
  case target of
    Command.MemoryTarget -> handleSock conf
    Command.StateFileTarget path ->
      handleSock conf {P._confDbFile = Just path}
    Command.UnixDomainTarget _ -> do
      putStrLn @Text "TODO"
      exitFailure
run (Command.CommandWithTarget (Command.Run conf scriptPath runOutput) target (Command.User user)) =
  case target of
    Command.MemoryTarget -> do
      runtime <- Runtime.bootConf conf Runtime.NoThreads >>= either throwIO pure
      let Command.RunOutput withTraces withFinal = runOutput
       in if withTraces
            then Interpret.run runtime user scriptPath withFinal
            else Interpret.runNoTrace runtime user scriptPath withFinal
    Command.StateFileTarget path -> do
      runtime <- Runtime.bootConf conf {P._confDbFile = Just path} Runtime.NoThreads >>= either throwIO pure
      let Command.RunOutput withTraces withFinal = runOutput
       in if withTraces
            then Interpret.run runtime user scriptPath withFinal
            else Interpret.runNoTrace runtime user scriptPath withFinal
    Command.UnixDomainTarget _ -> do
      putStrLn @Text "TODO"
      exitFailure
run (Command.CommandWithTarget (Command.Parse confParser) _ _) =
  case confParser of
    Command.ConfCommand input -> do
      let result =
            A.execParserPure A.defaultPrefs Command.parserInfo $
              T.unpack
                <$> T.words input
      case result of
        A.Success command -> do
          print command
          exitSuccess
        A.Failure err -> do
          print err
          exitFailure
        A.CompletionInvoked _ -> do
          print @IO @Text "Shouldn't happen"
          exitFailure
    Command.ParseObject typ fileName -> do
      content <- readFile fileName
      case typ of
        Command.ParseState -> do
          let result = Aeson.eitherDecodeStrict (T.encodeUtf8 content)
          case result of
            Right (value :: Store.HaskDb) -> do
              print value
              exitSuccess
            Left err -> do
              print err
              exitFailure
        Command.ParseUser -> do
          let result = Aeson.eitherDecodeStrict (T.encodeUtf8 content)
          case result of
            Right (value :: User.UserProfile) -> do
              print value
              exitSuccess
            Left err -> do
              print err
              exitFailure

    -- TODO We need a parser for multiple commands separated by newlines.
    Command.ConfFileName fileName -> do
      content <- T.lines <$> readFile fileName
      mapM_ print content
      exitSuccess
    Command.ConfStdin -> do
      content <- T.lines <$> getContents
      print content
      exitSuccess
run (Command.CommandWithTarget (Command.ViewQueue name) target (Command.User user)) =
  do
    case target of
      Command.MemoryTarget -> do
        handleViewQueue P.defaultConf user name
      Command.StateFileTarget path -> do
        handleViewQueue P.defaultConf {P._confDbFile = Just path} user name
      Command.UnixDomainTarget _ -> do
        putStrLn @Text "TODO"
        exitFailure
run (Command.CommandWithTarget (Command.ViewQueues queues) target (Command.User user)) =
  do
    case target of
      Command.MemoryTarget -> do
        handleCommand
          P.defaultConf
          user
          (Command.ViewQueues queues)
      Command.StateFileTarget path -> do
        handleCommand
          P.defaultConf {P._confDbFile = Just path}
          user
          (Command.ViewQueues queues)
      Command.UnixDomainTarget _ -> do
        putStrLn @Text "TODO"
        exitFailure
run (Command.CommandWithTarget (Command.Step isAll dryRun) target (Command.User user)) = do
  case target of
    Command.MemoryTarget -> do
      handleCommand P.defaultConf user $ Command.Step isAll dryRun
    Command.StateFileTarget path -> do
      handleCommand
        P.defaultConf {P._confDbFile = Just path}
        user
        (Command.Step isAll dryRun)
    Command.UnixDomainTarget _ -> do
      putStrLn @Text "TODO"
      exitFailure
run (Command.CommandWithTarget (Command.Log conf msg) (Command.StateFileTarget path) _) =
  do
    runtime <-
      Runtime.bootConf conf {P._confDbFile = Just path} Runtime.NoThreads >>= either throwIO pure
    ML.logInfo (<> "CLI" <> "Log") (Runtime._rLoggers runtime) msg
    Runtime.powerdown runtime
    exitSuccess
run (Command.CommandWithTarget (Command.ShowId i) target (Command.User user)) =
  do
    case target of
      Command.MemoryTarget -> do
        handleShowId P.defaultConf user i
      Command.StateFileTarget path -> do
        handleShowId P.defaultConf {P._confDbFile = Just path} user i
      Command.UnixDomainTarget _ -> do
        putStrLn @Text "TODO"
        exitFailure
run (Command.CommandWithTarget command target (Command.User user)) = do
  case target of
    Command.MemoryTarget -> do
      handleCommand P.defaultConf user command
    Command.StateFileTarget path -> do
      handleCommand P.defaultConf {P._confDbFile = Just path} user command
    Command.UnixDomainTarget path -> do
      client path command

--------------------------------------------------------------------------------
handleServe :: P.Conf -> P.ServerConf -> IO ExitCode
handleServe conf serverConf = do
  threads <- Runtime.emptyHttpThreads
  runtime@Runtime.Runtime {..} <- Runtime.bootConf conf threads >>= either throwIO pure
  Runtime.runRunM runtime Runtime.spawnEmailThread
  when (P._serverUnixDomain serverConf) $
    void $
      Runtime.runRunM runtime Runtime.spawnUnixThread
  P.startServer serverConf runtime >>= P.endServer _rLoggers
  mPowerdownErrs <- Runtime.powerdown runtime
  maybe exitSuccess throwIO mPowerdownErrs

--------------------------------------------------------------------------------
handleSock :: P.Conf -> IO ExitCode
handleSock conf = do
  putStrLn @Text "Creating runtime..."
  runtime <- Runtime.bootConf conf Runtime.NoThreads >>= either throwIO pure
  Runtime.runWithRuntime runtime
  exitSuccess

--------------------------------------------------------------------------------
handleViewQueue conf user name = do
  case name of
    Command.EmailAddrToVerify -> do
      putStrLn @Text "Email addresses to verify:"
      handleCommand
        conf
        user
        (Command.FilterUsers User.PredicateEmailAddrToVerify)
    Command.EmailsToSend -> do
      putStrLn @Text "Emails to send:"
      handleCommand
        conf
        user
        (Command.FilterEmails Email.EmailsTodo)

--------------------------------------------------------------------------------
handleShowId conf user i = do
  case T.splitOn "-" i of
    "USER" : _ -> do
      handleCommand conf user (Command.SelectUser False (User.UserId i) False)
    prefix : _ -> do
      putStrLn $ "Unknown ID prefix " <> prefix <> "."
      exitFailure
    [] -> do
      putStrLn @Text "Please provide an ID."
      exitFailure

--------------------------------------------------------------------------------
handleCommand conf user command = do
  runtime <- Runtime.bootConf conf Runtime.NoThreads >>= either handleError pure

  (exitCode, output) <- Runtime.handleCommand runtime user command
  mapM_ putStrLn output

  Runtime.powerdown runtime
  -- TODO shutdown runtime, loggers, save state, ...
  exitWith exitCode

handleError e
  | -- TODO What's the right way to pattern-match on the right type,
    -- i.e. IOErr.
    Errs.errCode e == "ERR.FILE_NOT_FOUND" = do
      putStrLn (fromMaybe "Error." $ Errs.userMessage e)
      putStrLn @Text "You may want to create a state file with `cty init`."
      exitFailure
  | otherwise = do
      putStrLn (fromMaybe "Error." $ Errs.userMessage e)
      exitFailure

--------------------------------------------------------------------------------

-- | This is the UNIX-domain client code (i.e. meant to interact with
-- `cty sock`).
client :: FilePath -> Command.Command -> IO ExitCode
client path command = do
  sock <- socket AF_UNIX Stream 0
  connect sock $ SockAddrUnix path

  _ <- recv sock 1024
  -- Just swallow the initial server banner for now.
  -- let response = T.decodeUtf8 msg -- TODO decodeUtf8
  -- putStrLn response

  let ecommand = Command.commandToString command
  case ecommand of
    Right command' -> do
      send sock $ T.encodeUtf8 command'
      msg <- recv sock 1024
      let response = T.decodeUtf8 msg -- TODO decodeUtf8
      putStrLn response
    Left err -> putStrLn $ "ERROR: " <> err

  close sock
  exitSuccess

--------------------------------------------------------------------------------
repl :: Runtime.Runtime -> User.UserName -> IO ()
repl runtime user = HL.runInputT settings loop
 where
  settings =
    (HL.defaultSettings :: HL.Settings IO)
      { HL.complete =
          HL.completeWordWithPrev Nothing " \t\n" complete
      }
  complete begin str = do
    let begin' = S.words (reverse begin)
        cmd = "cty" : begin' ++ ([str | not (null str)])
        cmd' =
          [ "--bash-completion-index"
          , show (length begin' + 1)
          ]
            ++ concatMap (\w -> ["--bash-completion-word", w]) cmd
    case A.execParserPure A.defaultPrefs Command.parserInfo cmd' of
      A.CompletionInvoked (A.CompletionResult f) ->
        map HL.simpleCompletion . S.lines <$> f str
      _ -> pure [] -- Shouldn't happen.
  loop =
    HL.getInputLine prompt >>= \case
      Nothing -> output' ""
      Just "" -> loop
      Just "quit" -> pure ()
      Just input -> do
        let result =
              A.execParserPure A.defaultPrefs Command.parserInfo $ Interpret.wordsq input
        case result of
          A.Success command -> do
            case command of
              -- We ignore the Configuration here. Probably this should be moved
              -- to Runtime.handleCommand too.
              Command.Run _ scriptPath _ -> do
                (code, _) <- liftIO $ Interpret.interpret runtime user scriptPath
                case code of
                  ExitSuccess -> pure ()
                  ExitFailure _ -> output' "Script failed."
              _ -> do
                (_, output) <- Runtime.handleCommand runtime user command
                mapM_ output' output
          A.Failure (A.ParserFailure f) -> let (err, _, _) = f "" in output' $ show err
          A.CompletionInvoked _ -> output' "Shouldn't happen"

        loop

  prompt = "> "

  output' = HL.outputStrLn . T.unpack
