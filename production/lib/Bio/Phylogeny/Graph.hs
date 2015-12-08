{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
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
import Data.Vector hiding (filter, map)
import Bio.Phylogeny.PhyloCharacter
import Bio.Phylogeny.Tree.Node
import Bio.Phylogeny.Forest
import Data.Int
import Data.BitVector
import qualified Bio.Phylogeny.Network as N

type Identifier = String
type Sequence   = Vector [String]
type CharInfo   = PhyloCharacter Int64
type NodeInfo   = Node BitVector

data EdgeSet
   = EdgeSet
   { origins  :: IntSet
   , terminals :: IntSet
   } deriving (Eq,Show)

data Tree
   = Tree
   { taxaNodes  :: IntMap  Identifier
   , taxaSeqs   :: HashMap Identifier Sequence
   , characters :: Vector  CharInfo
   , nodes      :: Vector  NodeInfo
   , edges      :: Vector  EdgeSet
   , root       :: Int
   } deriving (Eq,Show)

newtype Graph = Graph [Tree]

-- Seems like this could be derived... silly GHC
instance Monoid Graph where
  mempty = Graph []
  mappend (Graph g1) (Graph g2) = Graph (g1 <> g2)

instance Monoid Tree where
  mempty = Tree mempty mempty mempty mempty mempty 0
  mappend (Tree a b c d e f) (Tree a' b' c' d' e' f') = Tree (a<>a') (b<>b') (c<>c') (d<>d') (e<>e') (f + f')

instance N.Network Tree NodeInfo where
  parents n t = map (\i -> (nodes t) ! i) (parents n)
  root t = (nodes t) ! (root t)
  children n t = map (\i -> (nodes t) ! i) (children n)
  isLeaf n t = isLeaf n
  isRoot n t = isRoot n
  update t new = t {nodes = (nodes t) // (map (\n -> (code n, n)) new) }

instance Forest Graph Tree where
  trees (Graph f) = f
  setTrees _ f2 = Graph f2
  filterTrees (Graph f) func = Graph $ filter func f
