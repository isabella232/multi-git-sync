{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

-- | Serve the API as an HTTP server.
module MultiGitSync
  ( api
  , server
  , startApp
  ) where

import Protolude

import Control.Logging (LogLevel(..), log', warn', setLogLevel, setLogTimeFormat, withStdoutLogging)
import GHC.Stats (getGCStatsEnabled)
import Network.Wai.Handler.Warp
       (Port, Settings, defaultSettings, runSettings, setBeforeMainLoop,
        setPort)
import qualified Network.Wai.Middleware.RequestLogger as RL
import Options.Applicative
       (ParserInfo, auto, eitherReader, execParser, fullDesc, header,
        help, helper, info, long, metavar, option, progDesc, str, switch, value)
import qualified Prometheus as Prom
import qualified Prometheus.Metric.GHC as Prom
import Servant (serve)

import MultiGitSync.API (api, server)
import MultiGitSync.Server.Instrument
       (defaultPrometheusSettings, prometheus, requestDuration)
import qualified MultiGitSync.Server.Logging as Log
import MultiGitSync.Sync (GitSync, newGitSync, runGitSync)


-- | Configuration for the application.
data Config = Config
  { port :: Port
  , accessLogs :: AccessLogs
  , logLevel :: LogLevel
  , enableGhcMetrics :: Bool
  , configFile :: FilePath
  } deriving (Show)

-- | What level of access logs to show.
data AccessLogs
  = Disabled -- ^ Don't show access logs.
  | Enabled -- ^ Show Apache-style access logs.
  | DevMode -- ^ Show detailed, colorful access logs. Not suitable in production.
  deriving (Eq, Show)

-- | Run the service.
startApp :: IO ()
startApp = do
  opts <- execParser options
  setLogTimeFormat "%Y-%m-%d %H:%M:%S.%q"
  setLogLevel (logLevel opts)
  withStdoutLogging $ do
    syncer <- atomically $ newGitSync (configFile opts)
    withAsync (runGitSync syncer) $ \_ -> runApp syncer opts

options :: ParserInfo Config
options = info (helper <*> parser) description
  where
    parser =
      Config <$>
      option auto (fold [long "port", metavar "PORT", help "Port to listen on"]) <*>
      option
        (eitherReader parseAccessLogs)
        (fold
           [long "access-logs", help "How to log HTTP access", value Disabled]) <*>
      option
        (eitherReader (pure . Log.fromKeyword . toS))
        (fold
           [ long "log-level"
           , help "Minimum severity for log messages"
           , value LevelInfo
           ]) <*>
      switch
        (fold
           [ long "ghc-metrics"
           , help "Export GHC metrics. Requires running with +RTS."
           ]) <*>
      option str
        (fold
           [ long "config-file"
           , help "Path to YAML file describing Git repositories to sync."
           ])

    parseAccessLogs "none" = pure Disabled
    parseAccessLogs "basic" = pure Enabled
    parseAccessLogs "dev" = pure DevMode
    parseAccessLogs _ = throwError "One of 'none', 'basic', or 'dev'"

    description =
      fold
        [ fullDesc
        , progDesc "Sidecar to keep multiple git repositories in sync"
        , header "multi-git-sync - TODO fill this in"
        ]

runApp :: GitSync -> Config -> IO ()
runApp syncer config@Config{..} = do
  requests <- Prom.registerIO requestDuration
  when enableGhcMetrics $
    do statsEnabled <- getGCStatsEnabled
       unless statsEnabled $
         warn' "Exporting GHC metrics but GC stats not enabled. Re-run with +RTS -T."
       void $ Prom.register Prom.ghcMetrics
  runSettings settings (middleware requests)
  where
    settings = warpSettings config
    middleware r = logging . prometheus defaultPrometheusSettings r "multi_git_sync" $ app
    logging =
      case accessLogs of
        Disabled -> identity
        Enabled -> RL.logStdout
        DevMode -> RL.logStdoutDev
    app = serve api (server syncer)

-- | Generate warp settings from config
--
-- Serve from a port and print out where we're serving from.
warpSettings :: Config -> Settings
warpSettings Config {..} =
  setBeforeMainLoop printPort (setPort port defaultSettings)
  where
    printPort = log' ("Listening on :" <> show port)
