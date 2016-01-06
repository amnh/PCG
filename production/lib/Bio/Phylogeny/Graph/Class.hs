-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Phylogeny.Graph.Class
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

module Bio.Phylogeny.Graph.Class (Graph(..), Tree(..), EdgeSet(..), EdgeInfo(..), Identifier, Sequence, CharInfo, NodeInfo) where

import Data.Vector 
import Data.Int
import Data.BitVector
import Data.IntSet
import Data.IntMap
import Data.HashMap.Strict

import Bio.Phylogeny.PhyloCharacter
import Bio.Phylogeny.Tree.Node


-- | Identifier is just a string name
type Identifier = String
-- | An un-coded sequence is a vector of list of strings
type Sequence   = Vector [String]
-- | CharInfo is PhyloCharacter for now
type CharInfo   = PhyloCharacter Int64
-- | Nodes can store with bitvectors for now
type NodeInfo   = Node BitVector

-- | Edge type: info is stored at the out connections of a node
data EdgeSet
   = EdgeSet
   { inNodes  :: IntSet
   , outNodes :: IntMap EdgeInfo
   } deriving (Eq,Show)

-- | Edge info type holding length, origin, and terminal
data EdgeInfo 
   = EdgeInfo
   { len :: Float
   , origin :: NodeInfo
   , terminal :: NodeInfo
   } deriving (Eq, Show)

-- | Tree structure holding nodes, their original sequences, their edges, and a root reference
data Tree
   = Tree
   { taxaNodes  :: IntMap  Identifier
   , taxaSeqs   :: HashMap Identifier Sequence
   , characters :: Vector  CharInfo
   , nodes      :: Vector  NodeInfo
   , edges      :: Vector  EdgeSet
   , root       :: Int
   } deriving (Eq,Show)

-- | A graph is defined as a list of trees
newtype Graph = Graph [Tree] deriving (Show)