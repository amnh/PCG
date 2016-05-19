-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.PhyloGraph.DAG.Internal
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Types for DAG representation
--
-----------------------------------------------------------------------------
{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses #-}

module Bio.PhyloGraph.DAG.Internal where

import Bio.Character.Dynamic.Coded
import Bio.PhyloGraph.Edge
import Bio.PhyloGraph.Node
import Bio.PhyloGraph.Node.Topological
import Data.Vector

-- | Alias for Node used in 'DAG'
type NodeInfo = Node

--TODO: This shouldn't be tightly bound to DynamicChar
-- | Alias for Node used in 'TopoDAG'
type Topo = TopoNode DynamicChar

-- | A dag is an element of a forest, stored referentially
data DAG 
   = DAG
   { nodes :: Vector NodeInfo 
   , edges :: Vector EdgeSet
   , root  :: Int
   } deriving (Eq, Show)

-- | A topodag is an alternative forest element stored topologically
data TopoDAG 
   = TopoDAG 
   { structure :: Topo
   }
