-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Phylogeny.Graph.Data
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Types for Graph representation
--
-----------------------------------------------------------------------------

{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Bio.Phylogeny.Graph.Data (Graph(..), Tree(..), EdgeSet(..), EdgeInfo(..), Identifier, CharInfo, NodeInfo) where

import Data.Vector 
import Data.Int
import Data.BitVector
import Data.IntSet
import Data.IntMap
import Data.HashMap.Strict

import Bio.Phylogeny.PhyloCharacter
import Bio.Phylogeny.Tree.Node
import Bio.Sequence.Parsed
import Bio.Sequence.Coded


-- | Identifier is just a string name
type Identifier = String
-- | CharInfo is PhyloCharacter for now
type CharInfo   = PhyloCharacter (EncodedSeq BitVector)
-- | Nodes can store with bitvectors for now
type NodeInfo   = Node BitVector
-- TODO: rename NodeInfo

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

-- | Tree structure holding nodes, their original sequences, their edges, and a root reference
data Tree
   = Tree
   { nodeNames  :: IntMap  Identifier
   , parsedSeqs :: HashMap Identifier ParsedSequences
   , characters :: Vector  CharInfo
   , nodes      :: Vector  NodeInfo
   , edges      :: Vector  EdgeSet
   , root       :: Int
   } deriving (Eq,Show)
   -- TODO add structure that knows if a section is already optimized (possibly store at node?)
   -- TODO rename this to DAG (DirectedAcyclicGraph) - make aliases
   -- TODO make phylogenetic sanity requirement typeclass or refinement type

-- | A graph is defined as a list of trees
newtype Graph = Graph [Tree] deriving (Show, Eq)

