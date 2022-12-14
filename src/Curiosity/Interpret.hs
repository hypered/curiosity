{-# LANGUAGE TupleSections #-}
{- |
Module: Curiosity.Interpret
Description: This module defines the core of the Curiosity interpreter: it
parses `cty` commands (one per line), and apply them one at a time to an
initial state.

This interpretation is used in multiple places:

- `cty run` is the CLI interpreter
- `cty run` supports a nested interpreter through the `run` command
- `cty serve` exposes the interpreter at the `/run` route (where currently a
  single command is supported at a time)
- `run-scenarions.hs` uses the interpreter and compares the results to golden
  files

In addition, `cty run --final-only` is intended to benchmark Curiosity by
feeding (possibly) very long scripts.

-}

module Curiosity.Interpret
  ( Trace(..)
  , handleRun
  , handleRunNoTrace
  , handleRun'
  , interpret
  , interpretFile
  , interpretLines
  , formatOutput
  , flatten
  , pad
  , listScenarios
  , wordsq
  ) where

import qualified Curiosity.Command             as Command
import qualified Curiosity.Data                as Data
import qualified Curiosity.Data.User           as User
import qualified Curiosity.Parse               as P
import qualified Curiosity.Runtime             as Rt
import           Data.List                      ( isInfixOf, last )
import qualified Data.Text                     as T
import qualified Options.Applicative           as A
import           System.FilePath                ( (</>)
                                                , takeDirectory
                                                )
import qualified System.FilePath.Glob          as Glob


--------------------------------------------------------------------------------
handleRun :: P.Conf -> User.UserName -> FilePath -> Bool -> IO ExitCode
handleRun conf user scriptPath withFinal = do
  runtime <- Rt.bootConf conf Rt.NoThreads >>= either throwIO pure
  (code, output) <- interpret runtime user scriptPath
  Rt.powerdown runtime
  when withFinal $ print output
  exitWith code

handleRunNoTrace :: P.Conf -> User.UserName -> FilePath -> Bool -> IO ExitCode
handleRunNoTrace conf user scriptPath withFinal = do
  runtime <- Rt.bootConf conf Rt.NoThreads >>= either throwIO pure
  output <- interpretFile' runtime user scriptPath 0
  Rt.powerdown runtime
  when withFinal $ print output
  exitWith ExitSuccess

-- | Similar to `handleRun`, but capturing the output, and logging elsewhere
-- than normally: this is used in tests and in the `/scenarios` handler.
handleRun' :: FilePath -> IO [Trace]
handleRun' scriptPath = do
  let conf = P.Conf
        { P._confLogging = P.noLoggingConf
            -- P.mkLoggingConf "/tmp/cty-serve-explore.log"
            -- TOOD Multiple concurrent calls to the same log file
            -- end up with
            -- RuntimeException openFile: resource busy (file is locked)
        , P._confDbFile  = Nothing
        }
  runtime <- Rt.bootConf conf Rt.NoThreads >>= either throwIO pure
  output  <- interpretFile runtime "system" scriptPath 0
  Rt.powerdown runtime
  pure output

interpret :: Rt.Runtime -> User.UserName -> FilePath -> IO (ExitCode, Data.HaskDb)
interpret runtime user path = do
  output <- interpretFile runtime user path 0
  let (exitCode, ls) = formatOutput output
      finalState = case ls of
        [] -> Data.emptyHask
        _ -> traceState $ last output
  mapM_ putStrLn ls
  pure (exitCode, finalState)


--------------------------------------------------------------------------------
data Trace = Trace
  { traceNumber   :: Int -- ^ Flat numbering of commands within the script.
  , traceLineNbr  :: Int
  , traceCommand  :: Text
  , traceComment  :: Maybe Text
  , traceNesting  :: Int
  , traceUser     :: User.UserName
  , traceOutput   :: [Text]
  , traceExitCode :: ExitCode
  , traceNested   :: [Trace]
  , traceState    :: Data.HaskDb -- ^ The resulting state.
  }

-- | Keep all traces, but removes the `traceNested` indirection.
flatten :: [Trace] -> [Trace]
flatten [] = []
flatten (t:ts) = t { traceNested = [] } : flatten (traceNested t) ++ flatten ts

interpretFile :: Rt.Runtime -> User.UserName -> FilePath -> Int -> IO [Trace]
interpretFile runtime user path nesting = do
  let dir = takeDirectory path
  content <- T.lines <$> readFile path
  interpretLines runtime user dir content nesting [] (\t acc -> acc ++ [t])

-- | TODO This is similar to `interpretFile` but this doesn't collect the
-- individual traces. Still, a call to `Rt.state` is done after each command
-- and maybe this could be avoided.
-- The idea is to be able to use this function on some large script to
-- benchmark the system and make sure it can process each supported operation
-- quickly.
interpretFile' :: Rt.Runtime -> User.UserName -> FilePath -> Int -> IO Data.HaskDb
interpretFile' runtime user path nesting = do
  let dir = takeDirectory path
  content <- T.lines <$> readFile path
  interpretLines runtime user dir content nesting Data.emptyHask (\t _ -> traceState t)

interpretLines
  :: Rt.Runtime
  -> User.UserName
  -> FilePath
  -> [Text]
  -> Int
  -> acc
  -> (Trace -> acc -> acc)
  -> IO acc
interpretLines runtime user dir content nesting acc0 accumulate = go user acc0 0
  $ zip [1 :: Int ..] content
 where
  go _ acc _ []                  = pure acc
  go user' acc nbr ((ln, line) : rest) = do
    let (prefix, comment) = T.breakOn "#" line
        separated         = map T.pack . wordsq $ T.unpack prefix
        grouped           = T.unwords separated
        nbr'  = succ nbr
        trace' = Trace nbr
                       ln
                       grouped
                       (if T.null comment then Nothing else Just comment)
                       nesting
                       user'
    case separated of
      []               -> go user' acc nbr rest
      ["as", username] -> do
        st <- Rt.runRunM runtime Rt.state
        let t    = trace' ["Modifying default user."] ExitSuccess [] st
            acc' = accumulate t acc
        go (User.UserName username) acc' nbr' rest
      ["quit"] -> do
        st <- Rt.runRunM runtime Rt.state
        let t    = trace' ["Exiting."] ExitSuccess [] st
            acc' = accumulate t acc
        go user' acc' nbr' rest
      input -> do
        let result =
              A.execParserPure A.defaultPrefs Command.parserInfo
                $   T.unpack
                <$> input
        case result of
          A.Success command -> do
            case command of
              Command.Run _ scriptPath _ -> do
                let scriptPath' = dir </> scriptPath
                if dir `isPrefixOf` scriptPath' &&
                   (not $ ".." `isInfixOf` scriptPath')
                then do
                  output' <- liftIO $ interpretFile runtime
                                                    user
                                                    scriptPath'
                                                    (succ nesting)
                  st <- Rt.runRunM runtime Rt.state
                  let t    = trace' [] ExitSuccess output' st
                      acc' = accumulate t acc
                  go user' acc' nbr' rest
                else do
                  st <- Rt.runRunM runtime Rt.state
                  let t    = trace'
                               ["Script path can't be outside initial directory."]
                               (ExitFailure 1) [] st
                      acc' = accumulate t acc
                  go user' acc' nbr' rest
              _ -> do
                (_, output) <- Rt.handleCommand runtime user' command
                st <- Rt.runRunM runtime Rt.state
                let t    = trace' output ExitSuccess [] st
                    acc' = accumulate t acc
                go user' acc' nbr' rest
          A.Failure err -> do
            st <- Rt.runRunM runtime Rt.state
            let t    = trace' [show err] (ExitFailure 1) [] st
                acc' = accumulate t acc
            go user' acc' nbr' rest
          A.CompletionInvoked _ -> do
            st <- Rt.runRunM runtime Rt.state
            let t    = trace' ["Shouldn't happen."] (ExitFailure 1) [] st
                acc' = accumulate t acc
            go user' acc' nbr' rest


--------------------------------------------------------------------------------
formatOutput :: [Trace] -> (ExitCode, [Text])
formatOutput output =
  let ls = concatMap showTrace output
      exitCode =
        if null output then ExitSuccess else traceExitCode $ last output
  in  (exitCode, ls)

showTrace :: Trace -> [Text]
showTrace Trace {..} =
  map (pad traceNesting <>)
    $  (show traceLineNbr <> ": " <> traceCommand)
    :  traceOutput
    ++ concatMap showTrace traceNested

pad 0 = ""
pad 1 = "> "
pad n = T.concat (replicate ((n - 1) * 2) ">") <> "> "


--------------------------------------------------------------------------------
listScenarios :: FilePath -> IO [FilePath]
listScenarios scenariosDir = sort <$> Glob.globDir1 pat scenariosDir
  where pat = Glob.compile "*.txt"


--------------------------------------------------------------------------------
-- From https://stackoverflow.com/questions/4334897/functionally-split-a-string-by-whitespace-group-by-quotes
wordsq = outside [] . (' ' :)

add c res = if null res then [[c]] else map (++ [c]) res

outside res xs = case xs of
  ' ' : ' '  : ys -> outside res $ ' ' : ys
  ' ' : '\"' : ys -> res ++ inside [] ys
  ' '        : ys -> res ++ outside [] ys
  c          : ys -> outside (add c res) ys
  _               -> res

inside res xs = case xs of
  ' '  : ' ' : ys -> inside res $ ' ' : ys
  '\"' : ' ' : ys -> res ++ outside [] (' ' : ys)
  '\"'       : [] -> res
  c          : ys -> inside (add c res) ys
  _               -> res

