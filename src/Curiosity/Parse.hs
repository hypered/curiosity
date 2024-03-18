{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}

module Curiosity.Parse
  ( Conf (..)
  , confLogging
  , confDbFile
  , ServerConf (..)
  , defaultConf
  , defaultLoggingConf
  , mkLoggingConf
  , noLoggingConf
  , confParser
  , serverParser
  , defaultServerConf
  ) where

import Commence.Multilogging qualified as ML
import Control.Lens as Lens
import Control.Monad.Log qualified as L
import Options.Applicative qualified as A
import Servant.Auth.Server qualified as SAuth

--------------------------------------------------------------------------------

-- | Application config.
data Conf = Conf
  { _confLogging :: ML.LoggingConf
  -- ^ Logging configuration.
  , _confDbFile :: Maybe FilePath
  -- ^ An optional filepath to write the DB to, or read it from. If the file
  -- is absent, it will be created on server exit, with the latest DB state
  -- written to it.
  }
  deriving (Eq, Show)

makeLenses ''Conf

-- | HTTP server config.
data ServerConf = ServerConf
  { _serverPort :: Int
  , _serverStaticDir :: FilePath
  , _serverDataDir :: FilePath
  , _serverScenariosDir :: FilePath
  , _serverCookie :: SAuth.CookieSettings
  -- ^ Settings for setting cookies as a server (for authentication etc.).
  , _serverUnixDomain :: Bool
  -- ^ Enable (when True) the UNIX-domain socket server.
  }
  deriving (Eq, Show)

--------------------------------------------------------------------------------
defaultConf :: Conf
defaultConf =
  let _confDbFile = Nothing
   in Conf {_confLogging = defaultLoggingConf, ..}

defaultLoggingConf :: ML.LoggingConf
defaultLoggingConf = mkLoggingConf "./curiosity.log"

mkLoggingConf :: FilePath -> ML.LoggingConf
mkLoggingConf path =
  ML.LoggingConf
    (ML.LoggingFile path)
    "Curiosity"
    L.levelInfo

noLoggingConf :: ML.LoggingConf
noLoggingConf =
  ML.LoggingConf
    ML.NoLogging
    "Curiosity"
    L.levelInfo

--------------------------------------------------------------------------------
confParser :: A.Parser Conf
confParser = do
  _confDbFile <- dbFileParser
  _confLogging <- ML.parseLoggingConf "./curiosity.log" "Curiosity"
  pure Conf {..}

serverParser :: A.Parser ServerConf
serverParser = do
  _serverPort <-
    abs
      <$> A.option
        A.auto
        ( A.long "server-port"
            <> A.value 9000
            <> A.metavar "PORT"
            <> A.help
              "Port to run the HTTP server on."
        )
  _serverStaticDir <-
    A.strOption
      ( A.long "static-dir"
          <> A.value "./_site/"
          <> A.metavar "DIR"
          <> A.help
            "A directory served as static assets, in particular HTML \
            \documentation."
      )
  _serverDataDir <-
    A.strOption
      ( A.long "data-dir"
          <> A.value "./data/"
          <> A.metavar "DIR"
          <> A.help
            "A directory containing example data."
      )
  _serverScenariosDir <-
    A.strOption
      ( A.long "scenarios-dir"
          <> A.value "./scenarios/"
          <> A.metavar "DIR"
          <> A.help
            "A directory containing scenarios."
      )
  _serverUnixDomain <-
    not
      <$> A.switch
        (A.long "no-socket" <> A.help "Disable the UNIX-domain socket server.")

  pure
    ServerConf
      { -- FIXME: Add support for cookie-settings parsing.
        _serverCookie =
          SAuth.defaultCookieSettings
            { SAuth.cookieIsSecure = SAuth.NotSecure -- Use temporarily NotSecure for easier local testing with cURL.
            , SAuth.cookieXsrfSetting = Nothing -- XSRF disabled to simplify curl calls (same as start-servant)
            , SAuth.cookieSameSite = SAuth.SameSiteStrict
            }
      , ..
      }

defaultServerConf :: ServerConf
defaultServerConf =
  ServerConf
    { _serverCookie =
        SAuth.defaultCookieSettings
          { SAuth.cookieIsSecure = SAuth.NotSecure
          , SAuth.cookieXsrfSetting = Nothing
          , SAuth.cookieSameSite = SAuth.SameSiteStrict
          }
    , _serverPort = 9000
    , _serverStaticDir = "./_site/"
    , _serverDataDir = "./data/"
    , _serverScenariosDir = "./scenarios/"
    , _serverUnixDomain = True
    }

--------------------------------------------------------------------------------
dbFileParser :: A.Parser (Maybe FilePath)
dbFileParser =
  A.optional $
    A.strOption $
      A.long "db-file"
        <> A.help helpTxt
        <> A.metavar
          "FILEPATH"
 where
  helpTxt =
    "DB file to read initial DB state from. The file may be empty or may not exist, in which case(s), the file will be \
    \created on application exit."
