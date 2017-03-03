{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}

module MultiGitSync.API
  ( api
  , server
  ) where

import Protolude

import Control.Monad.Except (ExceptT(..))
import Control.Monad.Log (Severity(..))
import Servant (Server, ServantErr, (:~>)(..), enter)
import Text.PrettyPrint.Leijen.Text (Doc)

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
