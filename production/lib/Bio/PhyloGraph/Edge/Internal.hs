-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.PhyloGraph.Edge.Internal
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Types for Edge representation
--
-----------------------------------------------------------------------------
{-# LANGUAGE MultiParamTypeClasses #-}

module Bio.PhyloGraph.Edge.Internal where

import Data.IntSet
import Data.IntMap
import qualified Bio.PhyloGraph.Edge.Class as C
import Bio.PhyloGraph.Node

-- TODO: discuss this further
-- | Edge type: info is stored at the out connections of a node
data EdgeSet
   = EdgeSet
   { inNodes  :: IntSet
   , outNodes :: IntMap EdgeInfo
   } deriving (Eq,Show)

-- | Edge info type holding length, origin, and terminal
data EdgeInfo 
   = EdgeInfo
   { len      :: Double
   , origin   :: Node
   , terminal :: Node
   , virtualNode :: Maybe Node
   } deriving (Eq, Show)

instance C.StandardEdge EdgeInfo Node where
  getEdgeLen  = len
  setEdgeLen e f = e {len = f}
  getOrigin   = origin
  getTerminal = terminal