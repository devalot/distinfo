{-

This file is part of the Haskell package distinfo. It is subject to
the license terms in the LICENSE file found in the top-level directory
of this distribution and at git://pmade.com/distinfo/LICENSE. No part
of the distinfo package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

--------------------------------------------------------------------------------
module DistInfo.Node (Node (..), localNode) where

--------------------------------------------------------------------------------
import Control.Distributed.Process (Process, ProcessId, getSelfPid)
import Control.Monad.IO.Class
import Data.Aeson
import Data.Binary
import Data.Typeable
import GHC.Generics (Generic)
import qualified System.Info as System
import qualified System.Posix.Unistd as System

--------------------------------------------------------------------------------
-- | Information about a remote node.
data Node = Node
  { nodeName :: String          -- ^ The name of the remote node.
  , nodePID  :: ProcessId       -- ^ Distributed process ID.
  , nodeOS   :: String          -- ^ The node's operating system name.
  , nodeArch :: String          -- ^ The node's hardware architecture.
  } deriving (Typeable, Generic)

--------------------------------------------------------------------------------
instance Binary Node

--------------------------------------------------------------------------------
-- | Need this because 'ProcessId' doesn't have a 'ToJSON' instance.
instance ToJSON Node where
  toJSON (Node{..}) = object [ "name" .= nodeName
                             , "pid"  .= show nodePID
                             , "os"   .= nodeOS
                             , "arch" .= nodeArch
                             ]

--------------------------------------------------------------------------------
-- | Information about this node.
localNode :: Process Node
localNode = do
  sysid <- liftIO System.getSystemID
  pid   <- getSelfPid

  return Node { nodeName = System.nodeName sysid
              , nodePID  = pid
              , nodeOS   = System.os
              , nodeArch = System.arch
              }
