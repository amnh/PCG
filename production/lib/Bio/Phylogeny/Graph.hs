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
-- Instances and other stuff for a Graph representation
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, NoImplicitPrelude, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Bio.Phylogeny.Graph (Graph(..), Tree(..), EdgeSet(..), EdgeInfo(..), Identifier, Sequence, CharInfo, NodeInfo, toTopo, simpleAppend, fromTopo) where

import           Bio.Phylogeny.Forest
import           Bio.Phylogeny.Graph.Data
import           Bio.Phylogeny.Tree.Binary
import qualified Bio.Phylogeny.Tree.CharacterAware as CT
import qualified Bio.Phylogeny.Tree.Edge.Standard  as E
import qualified Bio.Phylogeny.Tree.EdgeAware      as ET
import           Bio.Phylogeny.Tree.Node
import qualified Bio.Phylogeny.Tree.Referential    as RT
import           Bio.Phylogeny.Tree.Rose
import qualified Bio.Phylogeny.Network.Subsettable as SN
import           Bio.Phylogeny.Graph.Utilities

import           Data.Key                                (lookup)
import           Data.Monoid
import           Data.Vector                             ((++),(//), elemIndex, (!))

import           Prelude                       hiding    ((++), length, lookup, replicate)          
import           Safe


--import Debug.Trace

-- | Edge Sets are monoids
instance Monoid EdgeSet where
  mempty = EdgeSet mempty mempty
  mappend (EdgeSet in1 out1) (EdgeSet in2 out2) = EdgeSet (in1 <> in2) (out1 <> out2)

-- | This tree can be a binary tree
instance BinaryTree Tree NodeInfo where
  parent     n t = headMay $ map (\i -> nodes t ! i) (parents n)
  leftChild  n t = lookup 0 $ (\i -> nodes t ! i) <$> children n
  rightChild n t = lookup 1 $ (\i -> nodes t ! i) <$> children n

-- | Or this tree can be a rose tree
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

-- | This tree knows its edges
instance ET.EdgedTree Tree NodeInfo EdgeSet where
  edges    n t   = edges t ! code n
  setEdges n t e = t {edges = edges t // [(code n, e)]}

-- | And the tree is aware of its character info
instance CT.CharacterTree Tree CharInfo where
  characters = characters
  setCharacters t c = t {characters = c} 

-- | This particular tree is referential
instance RT.ReferentialTree Tree NodeInfo where
  code node tree = elemIndex node (nodes tree)
  getNthNode tree pos = nodes tree ! pos

instance SN.SubsettableNetwork Tree NodeInfo where
  appendSubtree = appendAt
  accessSubtree = grabAt

-- | Simply add in the offending nodes without updating edges or characters
simpleAppend :: Tree -> Tree -> NodeInfo -> Tree
simpleAppend (Tree names seqs chars n e r) (Tree names' seqs' _ n' e' r') hangNode = 
  let resetRoot = n' // [(r', (n' ! r') {isRoot = False, parents = [code hangNode]})]
  in Tree (names <> names') (seqs <> seqs') chars (n ++ resetRoot) (e ++ e') r

grabAt :: Tree -> NodeInfo -> Tree
grabAt inTree hangNode = fromTopo $ nodeToTopo inTree hangNode