{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
module MultiGitSync.Sync
  ( syncFromConfigFile
  ) where

import Protolude

import Control.Concurrent.STM (TVar, readTVar, newTVar, writeTVar)
import qualified Data.Map as Map
import Data.Yaml (FromJSON, ParseException, decodeFileEither)
import Numeric.Natural (Natural)
import System.Directory (canonicalizePath)
import System.FilePath ((</>), takeDirectory)
import System.FSNotify (Event(..), StopListening, WatchManager, eventPath, watchDir, withManager)

import MultiGitSync.Git

-- XXX: What I really want is Go-style Durations.
type Seconds = Int

-- | Configuration for syncing a single Git repository.
data GitConfig
  = GitConfig
  { url :: GitUrl
  , branch :: Branch
  , revSpec :: RevSpec
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
data GitSync
  = GitSync
    { url :: GitUrl
    , branch :: Branch
    , revSpec :: RevSpec
    , depth :: Maybe Natural
    , repoPath :: FilePath
    , workingTreePath :: FilePath
    , interval :: Seconds
    } deriving (Eq, Ord, Show)

-- XXX: Current model is that we've got one thread per git repo. Could we
-- perhaps go for a priority queue & scheduler approach?

syncFromConfigFile :: FilePath -> IO ()
syncFromConfigFile configFile =
  withManager $ \mgr -> do
    configs <- atomically $ newTVar Map.empty
    _stopWatching <- watchFile mgr configFile (configFileChanged configs)
    updateConfigs configs


updateConfigs :: TVar (Map FilePath GitSync) -> IO ()
updateConfigs configState = do
  configs <- atomically (readTVar configState)
  workers <- startWorkers configs
  loop configs workers
  where
    loop cfgs wrkrs = do
      newCfgs <- atomically $ blockUntil (/= cfgs) configState
      stopWorkers wrkrs
      newWorkers <- startWorkers cfgs
      loop newCfgs newWorkers

    blockUntil p var = do
      val <- readTVar var
      if p val
        then pure val
        else retry

    startWorkers = traverse (async . syncRepoLoop)
    stopWorkers = traverse_ cancel

-- | Keep a repository in sync.
--
-- TODO: This should have logic for catching exceptions during sync and
-- retrying, enforcing a backoff & max retries policy.
syncRepoLoop :: GitSync -> IO ()
syncRepoLoop sync@GitSync{..} = do
  -- XXX: Not sure ExceptT is pulling its weight here

  -- XXX: I think we want to mask async exceptions here? We really don't want
  -- the thread to stop partway through a sync. Probably a better thing to do
  -- is make sure that syncRepo itself is safe for async exceptions.
  result <- runExceptT $ syncRepo url branch revSpec depth repoPath workingTreePath
  case result of
    Left _ -> notImplemented -- XXX: Do something better with errors
    Right _ -> do
      threadDelay (1000000 * interval)
      syncRepoLoop sync


-- | Called when the config file changed.
configFileChanged :: TVar (Map FilePath GitSync) -> Event -> IO ()
configFileChanged _ (Removed _ _) = pass
configFileChanged configState event = do
  result <- readConfig (eventPath event)
  case result of
    Left _ -> pass -- XXX: Should probably warn, bump a metric, etc.
    Right cfg -> do
      let newCfg = transformConfig cfg
      atomically $ writeTVar configState newCfg


-- | The 'SyncConfig' data type is optimized for humans reading & writing
-- configuration. We need something different for actually running the program.
transformConfig :: SyncConfig -> Map FilePath GitSync
transformConfig syncConfig =
  flip Map.mapWithKey (repos syncConfig) $
  \path cfg -> GitSync { url = url (cfg :: GitConfig)
                       , branch = branch (cfg :: GitConfig)
                       , revSpec = revSpec (cfg :: GitConfig)
                       , depth = depth (cfg :: GitConfig)
                       , repoPath = getRepoPath path
                       , workingTreePath = getWorkTreePath path
                       , interval = i
                       }
  where
    i = interval (syncConfig :: SyncConfig)

    getRepoPath path = root syncConfig </> ".repos" </> path
    getWorkTreePath path = root syncConfig </> path


-- | Watch for changes to a single file, performing 'action' when it happens.
watchFile :: WatchManager -> FilePath -> (Event -> IO ()) -> IO StopListening
watchFile mgr filePath action = do
  canonicalPath <- canonicalizePath filePath
  let dir = takeDirectory canonicalPath
  watchDir mgr dir ((== canonicalPath) . eventPath) action


-- | Read the configuration file from disk.
readConfig :: FilePath -> IO (Either ParseException SyncConfig)
readConfig = decodeFileEither
