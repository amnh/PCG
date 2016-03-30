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

module Bio.Phylogeny.Solution.Data where

import Bio.Sequence.Parsed
import Bio.Sequence.Coded

import Data.BitVector
import Data.HashMap.Strict
import Data.Vector

type Forest d = [d]

type Identifier = String

type Sequences = ParsedSequences

type NodeInfo   = Node BitVector

type Topo   = TopoNode BitVector

type CharacterMetadata = PhyloCharacter (EncodedSeq BitVector)

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

data Solution d 
    = Solution
    { parsedSeqs :: HashMap Identifier Sequences
    , metadata   :: Vector CharacterMetadata
    , forests    :: [Forest d]
    } deriving (Eq, Show)

data DAG 
    = DAG
    { nodes :: Vector NodeInfo 
    , edges :: Vector EdgeSet
    , root  :: Int
    } deriving (Eq, Show)

data TopoDAG 
    = TopoDAG 
    { root :: Topo}