{-

This file is part of the Haskell package distinfo. It is subject to
the license terms in the LICENSE file found in the top-level directory
of this distribution and at git://pmade.com/distinfo/LICENSE. No part
of the distinfo package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

--------------------------------------------------------------------------------
module DistInfo.Info (Info (..)) where

--------------------------------------------------------------------------------
import Control.Concurrent.STM (TVar)
import Control.Distributed.Process (ProcessId)
import Data.Map (Map)
import DistInfo.Node

--------------------------------------------------------------------------------
-- | Information about the local server and its peers.
data Info = Info
  { serverNode :: Node
  , peerNodes  :: TVar (Map ProcessId Node)
  }
