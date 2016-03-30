-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Phylogeny.Solution
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Instances and other stuff for a solution representation
--
-----------------------------------------------------------------------------
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}

module Bio.Phylogeny.Solution where

import qualified Bio.Phylogeny.Forest           as FC
import qualified Bio.Phylogeny.Network          as N
import           Bio.Phylogeny.Solution.Data 
import           Bio.Phylogeny.Tree.Binary
import           Bio.Phylogeny.Tree.Node
import           Bio.Phylogeny.Tree.Rose

import           Data.Foldable
import qualified Data.IntSet                    as IS
import qualified Data.IntMap                    as IM
import           Data.Key                       (lookup)
import           Data.Monoid
import           Data.Vector                    ((!), (//), Vector)
import qualified Data.Vector                    as V
import           Safe
import           Prelude                        hiding (lookup)

instance BinaryTree DAG NodeInfo where
    parent     n t = headMay $ map (\i -> nodes t ! i) (parents n)
    leftChild  n t = lookup 0 $ (\i -> nodes t ! i) <$> children n
    rightChild n t = lookup 1 $ (\i -> nodes t ! i) <$> children n
    verifyBinary t = V.foldr (\n acc -> length (children n) <= 2 && acc) True (nodes t)

instance RoseTree DAG NodeInfo where
    parent n t = headMay $ map (\i -> nodes t ! i) (parents n)

instance N.Network DAG NodeInfo where
    parents n t   = map (\i -> nodes t ! i) (parents n)
    root t        = nodes t ! root t
    children n t  = map (\i -> nodes t ! i) (children n)
    isLeaf n _    = isLeaf n
    isRoot n _    = isRoot n
    update t new  = t {nodes = nodes t // map (\n -> (code n, n)) new}
    numNodes      = length . nodes 
    addNode t n   = DAG nodes2 edges2 reroot
        where
            addPos = length $ nodes t
            newNode = resetPos n t addPos
            newEdge = makeEdges newNode t
            edges2 = edges t V.++ pure newEdge
            nodes2 = addConnections newNode (nodes t) V.++ pure newNode
            reroot = if isRoot n && null (nodes t) then addPos else root t

instance Monoid EdgeSet where
  mempty = EdgeSet mempty mempty
  mappend (EdgeSet in1 out1) (EdgeSet in2 out2) = EdgeSet (in1 <> in2) (out1 <> out2)

instance Monoid DAG where
    mempty = DAG mempty mempty 0
    mappend dag1 dag2 = appendAt dag1 dag2 (N.root dag1)

-- | Function to append two dags
-- TODO define this
appendAt :: DAG -> DAG -> NodeInfo -> DAG
appendAt = undefined

-- | makeEdges is a small function assisting appendAt
-- it creates the edge set for a given node in the given tree
makeEdges :: NodeInfo -> DAG -> EdgeSet
makeEdges node inDAG = EdgeSet (IS.fromList $ parents node) out
  where
    out  = foldr (\i acc -> IM.insert i (info $ nodes inDAG ! i) acc) mempty (children node)
    info input = EdgeInfo 0 node input Nothing

-- | resetPos is a small function assisting the joining of two subtrees
-- simple function to reset positioning of a node
resetPos :: NodeInfo -> DAG -> Int -> NodeInfo
resetPos node prevDAG index =
  let
    leaf = null $ children node
    nroot = null (parents node) && null (nodes prevDAG)
  in node {code = index, isLeaf = leaf, isRoot = nroot}

-- | addConnections is a small function assiting subtree joins
-- it adds edges between a new node and an existing tree
addConnections :: NodeInfo -> Vector NodeInfo -> Vector NodeInfo
addConnections newNode myNodes = 
  let 
    setIn curPos curNodes = curNodes // [(curPos, (curNodes ! curPos) {children = code newNode : children (curNodes ! curPos), isLeaf = False})]
    withIn = foldr setIn myNodes (parents newNode)
    setOut curPos curNodes = curNodes // [(curPos, (curNodes ! curPos) {parents = code newNode : parents (curNodes ! curPos), isRoot = False})]
    withOut = foldr setOut withIn (children newNode)
  in withOut 