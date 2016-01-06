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

module Bio.Phylogeny.Graph (Graph(..), Tree(..), EdgeSet(..), EdgeInfo(..), Identifier, Sequence, CharInfo, NodeInfo) where

import           Bio.Phylogeny.Forest
import           Bio.Phylogeny.Graph.Class
import           Bio.Phylogeny.Tree.Binary
import qualified Bio.Phylogeny.Tree.CharacterAware as CT
import qualified Bio.Phylogeny.Tree.Edge.Standard  as E
import qualified Bio.Phylogeny.Tree.EdgeAware      as ET
import           Bio.Phylogeny.Tree.Node
import qualified Bio.Phylogeny.Tree.Referential    as RT
import           Bio.Phylogeny.Tree.Rose
import qualified Bio.Phylogeny.Network             as N
import qualified Data.HashMap.Strict               as H  (insert)
import           Data.IntMap                             (size, insert, foldWithKey)
import           Data.IntSet                             (fromList)
import qualified Data.IntSet                       as IS (map)
import           Data.Key                                (lookup)
import           Data.Monoid
import           Prelude                       hiding    ((++),length,lookup)
import           Safe
import           Data.Vector                             ((++),(//), length, singleton, elemIndex, (!))

-- | Make all types instances of monoid to allow for mempty and mappend usage
instance Monoid Graph where
  mempty = Graph []
  mappend (Graph g1) (Graph g2) = Graph (g1 <> g2)

-- | Trees are monoids
instance Monoid Tree where
  mempty = Tree mempty mempty mempty mempty mempty 0
  mappend (Tree a b c d e f) (Tree a' b' c' d' e' _) = 
    let 
      shift = length d
      allNodes = d ++ recodeAll d' shift
      allEdges = e ++ recodeEdges shift e'
      allNames = a <> shiftNames a' shift
    in Tree allNames (b <> b') (c <> c') allNodes allEdges f

    where
      shiftFun shift list = pure ((+) shift) <*> list
      newEdge val shift = EdgeInfo 0 (recodeFun (origin val) shift) (recodeFun (terminal val) shift)
      recodeFun node shift = node {code = (code node + shift), children = shiftFun shift (children node), parents = shiftFun shift (parents node)}
      recodeAll innodes shift = (\node -> recodeFun node shift) <$> innodes
      shiftEdges shift set = EdgeSet (IS.map ((+) shift) (inNodes set))
                              (foldWithKey (\k val acc -> insert (k + shift) (newEdge val shift) acc) mempty (outNodes set))
      recodeEdges shift vec = (shiftEdges shift) <$> vec
      shiftNames names shift = foldWithKey (\k val acc -> insert (k + shift) val acc) mempty names

-- | Edge Sets are monoids
instance Monoid EdgeSet where
  mempty = EdgeSet mempty mempty
  mappend (EdgeSet in1 out1) (EdgeSet in2 out2) = EdgeSet (in1 <> in2) (out1 <> out2)

-- | Make this tree structure an instance of the tree types
instance N.Network Tree NodeInfo where
  parents n t   = map (\i -> nodes t ! i) (parents n)
  root t        = nodes t ! root t
  children n t  = map (\i -> nodes t ! i) (children n)
  isLeaf n _    = isLeaf n
  isRoot n _    = isRoot n
  update t new  = t {nodes = nodes t // map (\n -> (code n, n)) new}
  numNodes      = length . nodes 
  addNode t n   = Tree names2 seqs2 (characters t) nodes2 edges2 (root t)
    where
      addPos = (size $ taxaNodes t)
      names2 = insert addPos (show addPos) (taxaNodes t)
      seqs2  = H.insert (show addPos) mempty (taxaSeqs t)
      nodes2 = (nodes t) ++ singleton n
      edges2 = (edges t) ++ singleton (makeEdges n t)

makeEdges :: NodeInfo -> Tree -> EdgeSet
makeEdges node inTree = EdgeSet (fromList $ parents node) out
  where
    out  = foldr (\i acc -> insert i (info $ (nodes inTree) ! i) acc) mempty (children node)
    info = EdgeInfo 0 node 

-- | This tree can be a binary tree
instance BinaryTree Tree NodeInfo where
  parent     n t = headMay $ map (\i -> nodes t ! i) (parents n)
  leftChild  n t = lookup 0 $ (\i -> nodes t ! i) <$> (children n)
  rightChild n t = lookup 1 $ (\i -> nodes t ! i) <$> (children n)

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
