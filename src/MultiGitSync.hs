{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}

-- | Serve the API as an HTTP server.
module MultiGitSync
  ( api
  , server
  , startApp
  ) where

import Protolude

import Control.Monad.Except (ExceptT(..))
import Control.Monad.Log (Severity(..))
import qualified Data.List as List
import GHC.Stats (getGCStatsEnabled)
import Network.Wai.Handler.Warp
       (Port, Settings, defaultSettings, runSettings, setBeforeMainLoop,
        setPort)
import qualified Network.Wai.Middleware.RequestLogger as RL
import Options.Applicative
       (ParserInfo, auto, eitherReader, execParser, fullDesc, header,
        help, helper, info, long, metavar, option, progDesc, switch, value)
import qualified Prometheus as Prom
import qualified Prometheus.Metric.GHC as Prom
import Servant (Server, ServantErr, serve, (:~>)(..), enter)
import Text.PrettyPrint.Leijen.Text (Doc, int, text)

import MultiGitSync.Server.Instrument
       (defaultPrometheusSettings, prometheus, requestDuration)
import qualified MultiGitSync.Server.Logging as Log

import Network.HTTP.Media ((//), (/:))

import qualified NeatInterpolation as NI
import Servant.API (Accept(..), Get, MimeRender(..))


-- | HTML content type.
data HTML

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")


-- | multi-git-sync API definition.
type API = Get '[HTML] RootPage

-- | Value-level representation of API.
api :: Proxy API
api = Proxy

-- | Represents the root page of the service.
data RootPage =
  RootPage

-- | Very simple root HTML page. Replace this with your own simple page that
-- describes your API to other developers and sysadmins.
instance MimeRender HTML RootPage where
  mimeRender _ _ =
    toS
      [NI.text|
         <!doctype html>
         <html>
         <head><title>multi-git-sync</title></head>
         <body>
         <h1>multi-git-sync</h1>
         <ul>
         <li><a href="/metrics"><code>/metrics</code></a></li>
         </ul>
         <p>
         Source code at <a href="https://github.com/jml/multi-git-sync">https://github.com/jml/multi-git-sync/</a>
         </p>
         </body>
         <html>
         |]

-- | multi-git-sync API implementation.
server :: Severity -> Server API
server logLevel = enter (toHandler logLevel) handlers
  where
    handlers = pure RootPage

-- | Our custom handler type.
type Handler = ExceptT ServantErr (Log.LogM Doc IO)

-- | Translate our custom monad into a Servant handler.
--
-- See http://haskell-servant.readthedocs.io/en/stable/tutorial/Server.html#using-another-monad-for-your-handlers
-- for the details.
toHandler
  :: Severity -> (Handler :~> ExceptT ServantErr IO)
toHandler logLevel = Nat toHandler'
  where
    toHandler' :: Handler a -> ExceptT ServantErr IO a
    toHandler' = ExceptT . Log.withLogging logLevel . runExceptT


-- | Configuration for the application.
data Config = Config
  { port :: Port
  , accessLogs :: AccessLogs
  , logLevel :: Severity
  , enableGhcMetrics :: Bool
  } deriving (Show)

-- | What level of access logs to show.
data AccessLogs
  = Disabled -- ^ Don't show access logs.
  | Enabled -- ^ Show Apache-style access logs.
  | DevMode -- ^ Show detailed, colorful access logs. Not suitable in production.
  deriving (Eq, Show)

-- | Run the service.
startApp :: IO ()
startApp = runApp =<< execParser options

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
        (eitherReader
           (maybe (throwError (toS invalidLogLevel)) pure . Log.fromKeyword . toS))
        (fold
           [ long "log-level"
           , help "Minimum severity for log messages"
           , value Informational
           ]) <*>
      switch
        (fold
           [ long "ghc-metrics"
           , help "Export GHC metrics. Requires running with +RTS."
           ])
    invalidLogLevel = "Log level must be one of: " <> allLogLevels
    allLogLevels = fold . List.intersperse "," . map Log.toKeyword $ enumFrom minBound
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

runApp :: Config -> IO ()
runApp config@Config {..} = do
  requests <- Prom.registerIO requestDuration
  when enableGhcMetrics $
    do statsEnabled <- getGCStatsEnabled
       unless statsEnabled $
         Log.withLogging logLevel $
         Log.log
           Warning
           (text
              "Exporting GHC metrics but GC stats not enabled. Re-run with +RTS -T.")
       void $ Prom.register Prom.ghcMetrics
  runSettings settings (middleware requests)
  where
    settings = warpSettings config
    middleware r =
      logging . prometheus defaultPrometheusSettings r "multi_git_sync" $ app
    logging =
      case accessLogs of
        Disabled -> identity
        Enabled -> RL.logStdout
        DevMode -> RL.logStdoutDev
    app = serve api (server logLevel)

-- | Generate warp settings from config
--
-- Serve from a port and print out where we're serving from.
warpSettings :: Config -> Settings
warpSettings Config {..} =
  setBeforeMainLoop
    (Log.withLogging logLevel printPort)
    (setPort port defaultSettings)
  where
    printPort = Log.log Informational (text "Listening on :" `mappend` int port)
