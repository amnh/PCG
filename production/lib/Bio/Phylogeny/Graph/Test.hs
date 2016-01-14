-----------------------------------------------------------------------------
-- |
-- Module      :  Analysis.DirectOptimization.Test
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Test suite for graph operations
--
-----------------------------------------------------------------------------

module Bio.Phylogeny.Graph.Test where

import Bio.Phylogeny.Graph
import Bio.Phylogeny.Tree.Node
import Bio.Phylogeny.Network

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.IntMap       as IM  (insert)
import qualified Data.HashMap.Lazy as HM  (insert)
import Data.Vector                        (singleton)

testSuite :: TestTree
testSuite = testGroup "Graph operations" [joinOps]

joinOps :: TestTree
joinOps = testGroup "Check correct joining of trees" [nulladd]
    where
        nulladd = testCase "New node added to empty tree gives a one node tree" (expectedTree @=? result)
        newNode = Node 0 True True [] [] mempty mempty mempty mempty mempty mempty 0
        noEdges = EdgeSet mempty mempty
        expectedTree = Tree (IM.insert 0 "0" mempty) (HM.insert "0" mempty mempty) mempty (singleton newNode) (singleton noEdges) 0
        result = addNode mempty newNode


