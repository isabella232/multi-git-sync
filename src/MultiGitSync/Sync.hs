{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
module MultiGitSync.Sync
  ( GitSync
  , getConfig
  , getConfigFile
  , runGitSync
  , newGitSync
  , GitRepo(..)
  ) where

import Protolude

import Control.Concurrent.STM (TVar, readTVar, newTVar, writeTVar)
import qualified Control.Logging as Log
import qualified Data.Map as Map
import Data.String (String)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Data.Yaml (FromJSON, ParseException, decodeFileEither)
import Numeric.Natural (Natural)
import System.Directory (canonicalizePath)
import System.FilePath ((</>), takeDirectory)
import System.FSNotify (Event(..), StopListening, WatchManager, eventPath, watchDir, withManager)

import qualified Prometheus as Prom

import MultiGitSync.Git

data Metrics = Metrics { configFileChanges :: Prom.Metric (Prom.Vector String Prom.Counter)
                       , numWorkers :: Prom.Metric Prom.Gauge
                       , gitSyncSeconds :: Prom.Metric (Prom.Vector String Prom.Summary)
                       , gitSyncCount :: Prom.Metric (Prom.Vector String Prom.Counter)
                       }

success, failure :: String
success = "success"
failure = "failure"


initMetrics :: IO Metrics
initMetrics = Metrics
  <$> Prom.registerIO (Prom.vector ("status" :: String) (Prom.counter (Prom.Info "multigitsync_config_file_changes_total" "Number of config file changes detected")))
  <*> Prom.registerIO (Prom.gauge (Prom.Info "multigitsync_workers" "Number of active workers"))
  <*> Prom.registerIO (Prom.vector ("status" :: String) (Prom.summary (Prom.Info "multigitsync_git_sync_seconds" "Time to sync a git repo") Prom.defaultQuantiles))
  <*> Prom.registerIO (Prom.vector ("status" :: String) (Prom.counter (Prom.Info "multigitsync_git_sync_seconds_count" "Number of Git repos synced")))

-- XXX: What I really want is Go-style Durations.
type Seconds = Int

-- | Configuration for syncing a single Git repository.
data GitConfig
  = GitConfig
  { url :: GitUrl
  , branch :: Maybe Branch
  , revSpec :: Maybe RevSpec
  , depth :: Maybe Natural
  } deriving (Eq, Ord, Show, Generic, FromJSON)

-- | Configuration for syncing many Git repositories.
data SyncConfig
  = SyncConfig
  { root :: FilePath
  , interval :: Seconds
  , repos :: Map FilePath GitConfig
  } deriving (Eq, Ord, Show, Generic, FromJSON)


-- | All the information necessary to sync a repository.
data GitRepo
  = GitRepo
    { url :: GitUrl
    , branch :: Branch
    , revSpec :: RevSpec
    , depth :: Maybe Natural
    , repoPath :: FilePath
    , workingTreePath :: FilePath
    , interval :: Seconds
    } deriving (Eq, Ord, Show)

-- | A git syncing process
data GitSync
  = GitSync
  { configFile :: FilePath
  , configState :: TVar (Map FilePath GitRepo)
  }

-- | Create a new 'GitSync' process.
newGitSync :: FilePath -> STM GitSync
newGitSync configFile = GitSync configFile <$> newTVar Map.empty

-- XXX: Current model is that we've got one thread per git repo. Could we
-- perhaps go for a priority queue & scheduler approach?

-- | Synchronize based on the given 'GitSync' forever.
runGitSync :: MonadIO m => GitSync -> m ()
runGitSync sync@GitSync{..} = do
  metrics <- liftIO initMetrics
  Log.log' $ "Running git synchronizer based on " <> toS configFile
  liftIO $ withManager $ \mgr -> do
    _stopWatching <- watchFile mgr configFile (configFileChanged metrics sync)
    updateConfigs metrics sync

-- | Get the configuration file
getConfigFile :: GitSync -> FilePath
getConfigFile = configFile

-- | Get the current configuration.
getConfig :: GitSync -> STM (Map FilePath GitRepo)
getConfig GitSync{..} = readTVar configState


updateConfigs :: (Prom.MonadMonitor m, MonadIO m) => Metrics -> GitSync -> m ()
updateConfigs metrics GitSync{..} = do
  configs <- liftIO $ atomically (readTVar configState)
  workers <- liftIO $ startWorkers configs
  loop configs workers
  where
    loop cfgs wrkrs = do
      Prom.setGauge (fromIntegral (length wrkrs)) (numWorkers metrics)
      newCfgs <- liftIO $ atomically $ blockUntil (/= cfgs) configState
      Log.log' "Config changed. Restarting workers."
      stopWorkers wrkrs
      newWorkers <- liftIO $ startWorkers newCfgs
      Log.debug' $ "Workers restarted: " <> show (length newWorkers)
      loop newCfgs newWorkers

    blockUntil p var = do
      val <- readTVar var
      if p val
        then pure val
        else retry

    startWorkers = traverse (liftIO . async . syncRepoLoop metrics)
    stopWorkers = traverse_ (liftIO . cancel)

-- | Keep a repository in sync.
--
-- TODO: This should have logic for catching exceptions during sync and
-- retrying, enforcing a backoff & max retries policy.
syncRepoLoop :: (Prom.MonadMonitor m, MonadIO m) => Metrics -> GitRepo -> m ()
syncRepoLoop metrics sync@GitRepo{..} = do
  -- XXX: Not sure ExceptT is pulling its weight here

  -- XXX: I think we want to mask async exceptions here? We really don't want
  -- the thread to stop partway through a sync. Probably a better thing to do
  -- is make sure that syncRepo itself is safe for async exceptions.
  Log.debug' $ "Syncing repo: " <> show sync
  (result, duration) <- timeAction $ runExceptT $ syncRepo url branch revSpec depth repoPath workingTreePath
  case result of
    Left err -> do
      recordResult failure duration
      Log.warn' $ "Failed to sync repo at " <> show url <> ": " <> show err
    Right _ -> do
      Log.debug' $ "Successfully synced repo: " <> show url
      recordResult success duration
      liftIO $ threadDelay (1000000 * interval)
      syncRepoLoop metrics sync


  where
    timeAction action = do
      start <- liftIO getCurrentTime
      result <- liftIO action
      end <- liftIO getCurrentTime
      let duration = fromRational $ toRational (end `diffUTCTime` start)
      pure (result, duration)

    recordResult status duration = do
      Prom.withLabel status (Prom.observe duration) (gitSyncSeconds metrics)
      Prom.withLabel status Prom.incCounter (gitSyncCount metrics)


-- | Called when the config file changed.
configFileChanged :: (Prom.MonadMonitor m, MonadIO m) => Metrics -> GitSync -> Event -> m ()
configFileChanged _ _ (Removed _ _) = pass
configFileChanged metrics GitSync{..} event = do
  result <- readConfig metrics (eventPath event)
  case result of
    Left err -> do
      Log.warn' $ "Failed to parse config file: " <> show err
      Prom.withLabel failure Prom.incCounter (configFileChanges metrics)
    Right cfg -> do
      let newCfg = transformConfig cfg
      liftIO $ atomically $ writeTVar configState newCfg
      Prom.withLabel success Prom.incCounter (configFileChanges metrics)


-- | The 'SyncConfig' data type is optimized for humans reading & writing
-- configuration. We need something different for actually running the program.
transformConfig :: SyncConfig -> Map FilePath GitRepo
transformConfig syncConfig =
  flip Map.mapWithKey (repos syncConfig) $
  \path cfg -> GitRepo { url = url (cfg :: GitConfig)
                       , branch = fromMaybe master (branch (cfg :: GitConfig))
                       , revSpec = fromMaybe head' (revSpec (cfg :: GitConfig))
                       , depth = depth (cfg :: GitConfig)
                       , repoPath = getRepoPath path
                       , workingTreePath = getWorkTreePath path
                       , interval = i
                       }
  where
    i = interval (syncConfig :: SyncConfig)

    master = Branch "master"
    head' = RevSpec "HEAD"

    getRepoPath path = root syncConfig </> ".repos" </> path
    getWorkTreePath path = root syncConfig </> path


-- | Watch for changes to a single file, performing 'action' when it happens.
watchFile :: WatchManager -> FilePath -> (Event -> IO ()) -> IO StopListening
watchFile mgr filePath action = do
  canonicalPath <- canonicalizePath filePath
  let dir = takeDirectory canonicalPath
  watchDir mgr dir ((== canonicalPath) . eventPath) action


-- | Read the configuration file from disk.
readConfig :: MonadIO m => Metrics -> FilePath -> m (Either ParseException SyncConfig)
readConfig _ = liftIO . decodeFileEither
