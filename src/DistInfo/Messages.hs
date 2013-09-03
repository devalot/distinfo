{-

This file is part of the Haskell package distinfo. It is subject to
the license terms in the LICENSE file found in the top-level directory
of this distribution and at git://pmade.com/distinfo/LICENSE. No part
of the distinfo package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

--------------------------------------------------------------------------------
module DistInfo.Messages
       ( NodeRequest (..)
       , NodeReply (..)
       ) where

--------------------------------------------------------------------------------
import Data.Binary
import Data.Typeable
import DistInfo.Node (Node)
import GHC.Generics (Generic)

--------------------------------------------------------------------------------
newtype NodeRequest = NodeRequest Node deriving (Typeable, Generic)
instance Binary NodeRequest

--------------------------------------------------------------------------------
newtype NodeReply = NodeReply Node deriving (Typeable, Generic)         
instance Binary NodeReply
