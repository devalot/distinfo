{-

This file is part of the Haskell package distinfo. It is subject to
the license terms in the LICENSE file found in the top-level directory
of this distribution and at git://pmade.com/distinfo/LICENSE. No part
of the distinfo package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

--------------------------------------------------------------------------------
module DistInfo.Web (forkWebServer) where

--------------------------------------------------------------------------------
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (void)
import Control.Monad.IO.Class
import Control.Monad.Reader.Class (ask)
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.Map as Map
import DistInfo.Info
import Snap.Core
import Snap.Http.Server (httpServe)
import qualified Snap.Http.Server.Config as Config
import Snap.Snaplet

--------------------------------------------------------------------------------
-- | Snaplet data.
data App = App Info

--------------------------------------------------------------------------------
-- | Web server configuration.
conf :: MonadSnap m => Int -> Config.Config m a
conf port = Config.setVerbose   False $
            Config.setErrorLog  Config.ConfigNoLog $
            Config.setAccessLog Config.ConfigNoLog $
            Config.setPort      port $
            Config.setBind      "0.0.0.0" $
            Config.defaultConfig

--------------------------------------------------------------------------------
-- | Route URLs to handlers.
routes :: [(ByteString, Handler b App ())]
routes = [ ("server.json", serverNodeHandler)
         , ("peers.json",  peerNodesHandler)
         ]

--------------------------------------------------------------------------------
-- | Fork a new thread and run a web server in it.
forkWebServer :: Int -> Info -> IO ()
forkWebServer port info =
  void $ forkIO $ do
    (_, snap, _) <- runSnaplet Nothing (appInit info)
    httpServe (conf port) snap

--------------------------------------------------------------------------------
-- | Initialize the main snaplet.
appInit :: Info -> SnapletInit b App
appInit info = makeSnaplet "app" "DistInfo" Nothing $ do
  addRoutes routes
  return $! App info

--------------------------------------------------------------------------------
-- | Handler to return information about this node.
serverNodeHandler :: Handler b App ()
serverNodeHandler = do
  (App info) <- ask
  writeLBS $! Aeson.encode (serverNode info)

--------------------------------------------------------------------------------
-- | Handler to return information about all peer nodes.
peerNodesHandler :: Handler b App ()
peerNodesHandler = do
  (App info) <- ask
  peers <- liftIO $ readTVarIO (peerNodes info)
  writeLBS $! Aeson.encode (Map.elems peers)
