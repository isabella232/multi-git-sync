{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module MultiGitSync.API
  ( api
  , server
  ) where

import Protolude

import Control.Monad.Except (ExceptT(..))
import Control.Monad.Log (Severity(..))
import qualified Data.Map as Map
import qualified Data.Text.IO as Text
import qualified Lucid as L
import Network.HTTP.Media ((//), (/:))
import Servant (Server, ServantErr, (:~>)(..), enter, (:<|>)(..), (:>))
import Servant.API (Accept(..), Get, MimeRender(..))
import Text.PrettyPrint.Leijen.Text (Doc)

import qualified MultiGitSync.Server.Logging as Log
import MultiGitSync.Sync (GitSync, GitRepo(..), getConfig, getConfigFile)

-- | HTML content type.
data HTML

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")


-- | multi-git-sync API definition.
type API = "status" :> Get '[HTML] GitSyncPage :<|> Get '[HTML] RootPage

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
    L.renderBS $ L.doctypehtml_ $ do
      L.head_ (L.title_ title)
      L.body_ $ do
        L.h1_ title
        L.ul_ $ do
          L.li_ $ L.a_ [L.href_ "/metrics"] (L.code_ "/metrics")
          L.li_ $ L.a_ [L.href_ "/status"] (L.code_ "/status")
          L.p_ $ do
            "Source code at"
            L.a_ [L.href_ sourceURL] (L.toHtml sourceURL)
   where
     title = "multi-git-sync"
     sourceURL = "https://github.com/jml/multi-git-sync"


data GitSyncPage = GitSyncPage FilePath Text (Map FilePath GitRepo) deriving (Eq, Show)

instance MimeRender HTML GitSyncPage where
  mimeRender _ (GitSyncPage configPath rawConfig repos) =
    L.renderBS $ L.doctypehtml_ $ do
      L.head_ (L.title_ title)
      L.body_ $ do
        L.h1_ title
        L.h2_ (L.toHtml configPath)
        L.pre_ (L.toHtml rawConfig)
        L.h2_ "Repositories"
        if Map.null repos
          then L.p_ "None found"
          else L.table_ $ do
            L.tr_ (mconcat (map L.th_ ["Path", "URL", "Branch", "Revision", "Depth", "Repository path", "Working tree path", "Update interval"]))
            mconcat (map renderRow (Map.toAscList repos))
    where
      title = "multi-git-sync :: status"

      renderRow :: (FilePath, GitRepo) -> L.Html ()
      renderRow (path, GitRepo{..}) =
        L.tr_ (mconcat (map (L.td_ . L.toHtml) [path, show url, show revSpec, show depth, toS repoPath, toS workingTreePath, show interval]))


-- | multi-git-sync API implementation.
server :: Severity -> GitSync -> Server API
server logLevel syncer = enter (toHandler logLevel) handlers
  where
    handlers = serveConfig :<|> pure RootPage

    serveConfig :: Handler GitSyncPage
    serveConfig = do
      config <- liftIO $ atomically $ getConfig syncer
      rawConfig <- liftIO $ Text.readFile configPath
      pure (GitSyncPage configPath rawConfig config)

      where configPath = getConfigFile syncer

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
