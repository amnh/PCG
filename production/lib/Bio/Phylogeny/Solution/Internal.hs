-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Phylogeny.Solution.Data
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Types for Solution representation
--
-----------------------------------------------------------------------------

module Bio.Phylogeny.Solution.Internal where

import Bio.Sequence.Parsed
import Bio.Sequence.Coded
import Bio.Phylogeny.Node
import Bio.Phylogeny.Node.Topological
import Bio.Metadata.Internal

import Data.BitVector
import Data.HashMap.Strict
import Data.IntSet
import Data.IntMap
import Data.Vector

-- | A forest is a list of dag structures where dags can be referential or topological
type Forest d = [d]

type Identifier = String

type Sequences = ParsedSequences

-- | We'll have two types of node: topological and referential
type NodeInfo = Node

type Topo   = TopoNode BitVector

type StandardMetadata = CharacterMetadata EncodedSeq

type StandardSolution = Solution DAG

-- TODO: Move edge concrete type
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
   , origin   :: NodeInfo
   , terminal :: NodeInfo
   , virtualNode :: Maybe NodeInfo
   } deriving (Eq, Show)

-- | A solution is an array of forests
-- character data and names are common across all forests and so stored at this level
data Solution d 
   = Solution
   { parsedChars :: HashMap Identifier Sequences
   , metadata   :: Vector StandardMetadata
   , forests    :: [Forest d]
   } deriving (Eq, Show)

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
   { structure :: Topo}
