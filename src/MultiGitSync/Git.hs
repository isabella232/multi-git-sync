{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# LANGUAGE FlexibleContexts #-}
module MultiGitSync.Git
  ( syncRepo
  ) where

import Protolude

import qualified Data.Text as Text
import Numeric.Natural (Natural)
import System.Directory (removeDirectoryRecursive)
import System.FilePath ((</>), makeRelative, takeDirectory)
import System.IO.Error (isDoesNotExistError)
import System.Posix.Files
  ( createSymbolicLink
  , fileExist
  , readSymbolicLink
  , rename
  )
import System.Process (CmdSpec(..), CreateProcess(..), proc, readCreateProcessWithExitCode, showCommandForUser)

-- | Information about a Git repository to sync, and where to sync it
data GitConfig
  = GitConfig GitUrl FilePath
  deriving (Eq, Ord, Show)

-- | The URL to a Git repository.
type GitUrl = Text

-- | A Git branch.
type GitBranch = Text

type RevSpec = Text
type Hash = Text

-- XXX: Not sure this is a good idea. Maybe use exceptions all the way
-- through?
-- | An error that occurs while we're doing stuff.
data GitError
  -- | An error occurred running the 'git' subprocess.
  = GitProcessError Text Int Text Text (Maybe FilePath)
  deriving (Eq, Show)


syncRepo :: HasCallStack => GitUrl -> GitBranch -> RevSpec -> Maybe Natural -> FilePath -> FilePath -> ExceptT GitError IO ()
syncRepo url branch rev depth repoPath workingTreePath = do
  changed <- ensureRepo
  case changed of
    Nothing -> pure ()
    Just hash -> do
      let canonicalTree = repoPath </> ("rev-" <> toS hash)
      addWorkTree repoPath canonicalTree branch
      resetWorkTree canonicalTree hash
      oldTree <- liftIO $ swapSymlink workingTreePath canonicalTree
      case oldTree of
        Nothing -> pure ()
        Just path -> do
          liftIO $ removeDirectoryRecursive path
          void $ runGitInRepo repoPath ["worktree", "prune"]

  where
    ensureRepo = do
      repoExists <- liftIO $ fileExist repoPath
      if repoExists
        then do
          remote <- needsUpdate repoPath branch rev
          case remote of
            Nothing -> pure Nothing
            Just hash -> do
              void $ fetchRepo repoPath
              pure (Just hash)
        else do
          repoPath' <- cloneRepo url branch depth repoPath
          Just <$> hashForRev repoPath' rev


-- | Clone a Git repository.
cloneRepo :: (HasCallStack, MonadError GitError m, MonadIO m)
          => GitUrl -> GitBranch -> Maybe Natural -> FilePath
          -> m FilePath
cloneRepo url branch depth gitRoot = do
  let args = ["clone", "--mirror", "-b", branch]
             <> case depth of
                  Nothing -> []
                  Just d -> ["--depth", show d]
             <> [url, toS gitRoot]
  -- TODO: logging
  void $ runGit args
  pure gitRoot


-- | Fetch the latest changes to a Git repository.
fetchRepo :: (HasCallStack, MonadIO m, MonadError GitError m) => FilePath -> m ()
fetchRepo repoPath = void $ runGitInRepo repoPath ["fetch", "--all", "--prune"]

-- | Do we need to update repo?
needsUpdate :: (HasCallStack, MonadError GitError m, MonadIO m) => FilePath -> GitBranch -> RevSpec -> m (Maybe Hash)
needsUpdate repoPath branch rev = do
  localHash <- hashForRev repoPath rev
  remote <- remoteHashForRev repoPath branch rev
  pure $ if localHash == remote
         then Nothing
         else Just remote

-- | Get the SHA1 of a revision.
hashForRev :: (HasCallStack, MonadError GitError m, MonadIO m) => FilePath -> RevSpec -> m Hash
hashForRev repoPath rev = Text.strip . fst <$> runGitInRepo repoPath ["rev-list", "-n1", rev]


-- | Get the SHA1 of a revision spec remotely.
remoteHashForRev :: (HasCallStack, MonadError GitError m, MonadIO m) => FilePath -> GitBranch -> RevSpec -> m Hash
remoteHashForRev repoPath branch rev = do
  (out, _) <- runGitInRepo repoPath ["ls-remote", "-q", "origin", ref]
  pure . Text.strip . fst . Text.breakOn "\t" $ out
  where
    ref = case rev of
            "HEAD" -> "refs/heads/" <> branch
            _ -> "refs/tags/" <> rev <> "^{}"


-- | Run 'git' in a repository.
runGitInRepo :: (HasCallStack, MonadError GitError m, MonadIO m) => FilePath -> [Text] -> m (Text, Text)
runGitInRepo repoPath args = runProcess $ gitCommand (Just repoPath) args

-- | Run 'git' on the command line.
runGit :: (HasCallStack, MonadError GitError m, MonadIO m) => [Text] -> m (Text, Text)
runGit args = runProcess $ gitCommand Nothing args

-- | Run a process.
runProcess :: (HasCallStack, MonadError GitError m, MonadIO m) => CreateProcess -> m (Text, Text)
runProcess process = do
  -- TODO: logging
  (exitCode, out, err) <- liftIO $ readCreateProcessWithExitCode process ""
  let out' = toS out
  let err' = toS err
  case exitCode of
    ExitFailure e -> throwError $ GitProcessError (toS cmdInfo) e out' err' (cwd process)
    ExitSuccess -> pure (out', err')
  where
    cmdInfo =
      case spec of
        ShellCommand s -> s
        RawCommand path args -> showCommandForUser path args
    spec = cmdspec process

-- | Get the CreateProcess for running git.
gitCommand :: HasCallStack => Maybe FilePath -> [Text] -> CreateProcess
gitCommand repoPath args = (proc "git" (map toS args)) { cwd = repoPath }


-- | Ensure the symlink at 'linkPath' points to 'newPath'. Return the target
-- of the old path.
swapSymlink :: HasCallStack => FilePath -> FilePath -> IO (Maybe FilePath)
swapSymlink linkPath newPath = do
  -- TODO: Catch errors, don't bork if ENOENT
  currentPath <- getSymlink linkPath
  let base = takeDirectory linkPath
  -- TODO: 'makeRelative' will never return paths with '..', in a noble
  -- attempt to prevent us shooting ourselves in the foot. (c.f.
  -- http://neilmitchell.blogspot.co.uk/2015/10/filepaths-are-subtle-symlinks-are-hard.html)
  -- However, we need a relative link, because the volume that we're doing
  -- this on might be mounted at a different path in the other container.
  let newPathRelative = makeRelative base newPath
  -- TODO: Handle tmp-link existing
  createSymbolicLink newPathRelative (base </> "tmp-link")
  rename (base </> "tmp-link") linkPath
  pure currentPath


getSymlink :: HasCallStack => FilePath -> IO (Maybe FilePath)
getSymlink path = do
  result <- tryJust (guard . isDoesNotExistError) (readSymbolicLink path)
  pure $ hush result


addWorkTree :: (HasCallStack, MonadIO m, MonadError GitError m) => FilePath -> FilePath -> GitBranch -> m ()
addWorkTree repoPath workTreePath branch = do
  -- TODO: figure out scheme for paths for concrete working trees
  void $ runGitInRepo repoPath ["worktree", "add", toS workTreePath, "origin/" <> branch]
  -- XXX: See comment in swapSymlink
  let relativeWorkingPath = makeRelative repoPath (toS workTreePath)
  let gitDirRef = "gitdir: " <> ("../worktrees" </> relativeWorkingPath) <> "\n"
  liftIO $ writeFile (toS workTreePath </> ".git") (toS gitDirRef)


resetWorkTree :: (HasCallStack, MonadIO m, MonadError GitError m) => FilePath -> Hash -> m ()
resetWorkTree workTreePath hash =
  void $ runGitInRepo workTreePath ["reset", "--hard", hash]
