{-

This file is part of the Haskell package distinfo. It is subject to
the license terms in the LICENSE file found in the top-level directory
of this distribution and at git://pmade.com/distinfo/LICENSE. No part
of the distinfo package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

--------------------------------------------------------------------------------
module DistInfo.Server (distInfoServer) where

--------------------------------------------------------------------------------
import Control.Applicative
import Control.Concurrent.STM
import Control.Distributed.Process
import Control.Monad (forever, unless, void)
import qualified Data.Map as Map
import DistInfo.Info
import DistInfo.Messages
import DistInfo.Node
import DistInfo.Web
import Prelude hiding (map)

--------------------------------------------------------------------------------
-- | The distributed information server.
distInfoServer :: Maybe Int -> Process ()
distInfoServer webPort = do
  info <- Info <$> localNode <*> liftIO (newTVarIO Map.empty)

  case webPort of
    Just port -> liftIO (forkWebServer port info)
    _         -> return ()

  forever $ receiveWait
    [ match $ handleNodeRequest info
    , match $ handleNodeReply info
    , match $ handleNotification info
    , matchIf correctWhereIs (handleWhereIs info)
    , matchAny $ const (return ()) -- ignore other messages
    ]

  where
    correctWhereIs :: WhereIsReply -> Bool
    correctWhereIs (WhereIsReply x _) = x == "distInfoServer"

--------------------------------------------------------------------------------
-- | A distributed process just died.
handleNotification :: Info -> ProcessMonitorNotification -> Process ()
handleNotification info (ProcessMonitorNotification _ pid _) = do
  say $ "peer died " ++ show pid
  handlePeerDisconnect info pid

--------------------------------------------------------------------------------
handleWhereIs :: Info -> WhereIsReply -> Process ()
handleWhereIs _        (WhereIsReply _ Nothing)    = return ()
handleWhereIs Info{..} (WhereIsReply _ (Just pid)) = do
  say $ "sending node request to " ++ show pid
  send pid $! NodeRequest serverNode

--------------------------------------------------------------------------------
-- | A new peer connected and requested our node information.
handleNodeRequest :: Info -> NodeRequest -> Process ()
handleNodeRequest info (NodeRequest node) = do
  say $ "node requested from " ++ show (nodePID node)
  handlePeerConnect info node
  send (nodePID node) $! NodeReply (serverNode info)

--------------------------------------------------------------------------------
-- | An existing peer responded with its node information.
handleNodeReply :: Info -> NodeReply -> Process ()
handleNodeReply info (NodeReply node) = do
  say $ "node reply from " ++ show (nodePID node)
  handlePeerConnect info node

--------------------------------------------------------------------------------
-- | A new peer has connected, add it to the map of peers.
handlePeerConnect :: Info -> Node -> Process ()
handlePeerConnect Info{..} node@Node{..} = do
  oldPeers <- liftIO $ atomically $ do
    map <- readTVar peerNodes
    writeTVar peerNodes $! Map.insert nodePID node map
    return map

  -- Start monitoring new peers.
  unless (nodePID `Map.member` oldPeers) $ void (monitor nodePID)

--------------------------------------------------------------------------------
-- | A peer disconnected, remove it from the map of peers.
handlePeerDisconnect :: Info -> ProcessId -> Process ()
handlePeerDisconnect Info{..} pid =
  liftIO $ atomically $ modifyTVar' peerNodes $ Map.delete pid
