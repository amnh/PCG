{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Phylogeny.Graph
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Exploritory types for Graph representations
--
-----------------------------------------------------------------------------
module Bio.Phylogeny.Graph where

import Prelude 
import Data.IntMap hiding (filter, map, (!))
import Data.IntSet hiding (filter, map)
import Data.HashMap.Strict hiding (filter, map, (!))
import Data.Monoid
import Data.Vector hiding (filter, map, head, (!?))
import Bio.Phylogeny.PhyloCharacter
import Bio.Phylogeny.Tree.Node
import Bio.Phylogeny.Forest
import Data.Int
import Data.BitVector
import qualified Bio.Phylogeny.Network as N
import Bio.Phylogeny.Tree.Binary
import Data.Keyed hiding ((!))
import Safe
import Bio.Phylogeny.Tree.Rose
import qualified Bio.Phylogeny.Tree.Edge.Standard  as E
import qualified Bio.Phylogeny.Tree.EdgeAware      as ET
import qualified Bio.Phylogeny.Tree.CharacterAware as CT

-- | Standard graph types defined
type Identifier = String
type Sequence   = Vector [String]
type CharInfo   = PhyloCharacter Int64
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

-- | Make all types instances of monoid to allow for mempty and mappend usage
instance Monoid Graph where
  mempty = Graph []
  mappend (Graph g1) (Graph g2) = Graph (g1 <> g2)

instance Monoid Tree where
  mempty = Tree mempty mempty mempty mempty mempty 0
  mappend (Tree a b c d e f) (Tree a' b' c' d' e' f') = Tree (a<>a') (b<>b') (c<>c') (d<>d') (e<>e') (f + f')

instance Monoid EdgeSet where
  mempty = EdgeSet mempty mempty
  mappend (EdgeSet in1 out1) (EdgeSet in2 out2) = EdgeSet (in1 <> in2) (out1 <> out2)

-- | Make this tree structure an instance of the tree types
instance N.Network Tree NodeInfo where
  parents n t  = map (\i -> nodes t ! i) (parents n)
  root t       = nodes t ! root t
  children n t = map (\i -> nodes t ! i) (children n)
  isLeaf n _   = isLeaf n
  isRoot n _   = isRoot n
  update t new = t {nodes = nodes t // map (\n -> (code n, n)) new}

instance BinaryTree Tree NodeInfo where
  parent     n t = headMay $ map (\i -> nodes t ! i) (parents n)
  leftChild  n t = map (\i -> nodes t ! i) (children n) !? 0
  rightChild n t = map (\i -> nodes t ! i) (children n) !? 1

instance RoseTree Tree NodeInfo where
  parent n t = headMay $ map (\i -> nodes t ! i) (parents n)

-- | Make the graph structure an instance of a forest
instance Forest Graph Tree where
  trees (Graph f) = f
  setTrees _ = Graph
  filterTrees (Graph f) func = Graph $ filter func f

-- | Make it an instance of data storage type classes
instance E.StandardEdge EdgeInfo NodeInfo where
  edgeLen  = len
  setEdgeLen e f = e {len = f}
  origin   = origin
  terminal = terminal

instance ET.EdgedTree Tree NodeInfo EdgeSet where
  edges    n t   = edges t ! code n
  setEdges n t e = t {edges = edges t // [(code n, e)]}

instance CT.CharacterTree Tree CharInfo where
  characters = characters
  setCharacters t c = t {characters = c} 
