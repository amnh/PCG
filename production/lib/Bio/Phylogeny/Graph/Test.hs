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
import qualified Bio.Phylogeny.Network as N
import Bio.Phylogeny.PhyloCharacter
import Bio.Phylogeny.Network.Subsettable
import Bio.Phylogeny.Graph.Random

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import qualified Data.IntMap       as IM  (insert, singleton, fromList)
import qualified Data.HashMap.Lazy as HM  (insert, fromList, singleton)
import Data.Vector                        (singleton, fromList, cons, (!))
import qualified Data.Vector       as V   ((++))
import qualified Data.IntSet       as IS  (singleton)
import Data.Monoid

-- import Debug.Trace

testSuite :: TestTree
testSuite = testGroup "Graph operations" [joinOps, subsetting]

joinOps :: TestTree
joinOps = testGroup "Check correct joining of trees" [nulladd, smalladd, nullJoin, twoJoin, threeJoin, seqJoin]
    where
        nulladd = testCase "New node added to empty tree gives a one node tree" (expectedTree @=? result)
        newNode = Node 0 True True [] [] mempty mempty mempty mempty mempty mempty 0
        noEdges = EdgeSet mempty mempty
        expectedTree = Tree (IM.insert 0 "0" mempty) (HM.insert "0" mempty mempty) mempty (singleton newNode) (singleton noEdges) 0
        result = N.addNode mempty newNode

        smalladd = testCase "Node added to small tree gives expected result" (expected2 @=? result2)
        node2 = Node 0 True True [1] [] mempty mempty mempty mempty mempty mempty 0
        node0 = Node 0 True False [] [1] mempty mempty mempty mempty mempty mempty 0
        node1 = Node 1 False True [0] [] mempty mempty mempty mempty mempty mempty 0
        edges0 = [EdgeSet mempty (IM.singleton 1 (EdgeInfo 0 node0 node1)), EdgeSet (IS.singleton 0) (IM.singleton 2 (EdgeInfo 0 node1 node2)), EdgeSet (IS.singleton 1) mempty]
        names = [(0, "0"), (1, "1"), (2, "2")]
        seqs = [("0", mempty), ("1", mempty), ("2", mempty)]
        expected2 = Tree (IM.fromList names) (HM.fromList seqs) mempty (fromList [node0, node1 {children = [2], isLeaf = False}, node2 {code = 2, isRoot = False}]) (fromList edges0) 0
        smallTree = Tree (IM.fromList $ init names) (HM.fromList $ init seqs) mempty (fromList [node0, node1]) (fromList $ init edges0) 0
        result2 = N.addNode smallTree node2

        twoJoin = testCase "Two one node trees joined together give expected result" (expected4 @=? result4)
        node4a = Node 0 True True [] [] mempty mempty mempty mempty mempty mempty 0
        node4b = Node 0 True True [] [] mempty mempty mempty mempty mempty mempty 0
        node4bUpdate = node4b {code = 1, isRoot = False, parents = [0]}
        node4aUpdate = node4a {children = [1], isLeaf = False}
        edges4 = fromList [EdgeSet mempty (IM.singleton 1 (EdgeInfo 0 node4aUpdate node4bUpdate)), EdgeSet (IS.singleton 0) mempty]
        names4 = IM.fromList [(0, "0"), (1, "0a1")]
        seqs4 = HM.fromList [("0", mempty), ("0a1", mempty)]
        expected4 = Tree names4 seqs4 mempty (fromList [node4aUpdate, node4bUpdate]) edges4 0
        tree4a = Tree (IM.fromList [(0, "0")]) (HM.fromList [("0", mempty)]) mempty (fromList [node4a]) mempty 0
        tree4b = Tree (IM.fromList [(0, "0")]) (HM.fromList [("0", mempty)]) mempty (fromList [node4b]) mempty 0
        result4 = tree4a <> tree4b

        nullJoin = testCase "Two node tree joined to empty tree gives a two node tree" (expected4 @=? result3)
        result3 = mempty <> expected4

        threeJoin = testCase "One and two node trees joined together give expected result" (expected5 @=? result5)
        node5aUpdate = node4a {code = 2, isRoot = False, parents = [0]}
        node4aUpdate' = node4a {children = [2, 1], isLeaf = False}
        edges5 = fromList [EdgeSet mempty (IM.fromList [(1, EdgeInfo 0 node4aUpdate' node4bUpdate), (2, EdgeInfo 0 node4aUpdate' node5aUpdate)]), EdgeSet (IS.singleton 0) mempty, EdgeSet (IS.singleton 0) mempty]
        names5 = IM.insert 2 "0a2" names4
        seqs5 = HM.insert "0a2" mempty seqs4
        expected5 = Tree names5 seqs5 mempty (fromList [node4aUpdate', node4bUpdate, node5aUpdate]) edges5 0
        result5 = result4 <> tree4a

        seqJoin = testCase "Two one node trees with sequences join properly" (expected6 @=? result6)
        chars1 = singleton $ DNA "" True (mempty, mempty) (fromList ["A", "C", "G", "T", "-"]) mempty mempty False
        chars2 = singleton $ DNA "" True (mempty, mempty) (fromList ["A", "C", "G"]) mempty mempty False
        node6a = Node 0 True True [] [] (singleton $ Just $ fromList [4, 8, 1]) mempty mempty mempty mempty mempty 2
        node6b = Node 0 True True [] [] (singleton $ Just $ fromList [16]) mempty mempty mempty mempty mempty 2
        node6aUpadate = node6a {isLeaf = False, children = [1], encoded = (encoded node6a) V.++ (singleton Nothing)}
        node6bUpdate = node6b {isRoot = False, parents = [0], code = 1, encoded = Nothing `cons` (encoded node6b)}
        edges6 = fromList [EdgeSet mempty (IM.singleton 1 (EdgeInfo 0 node6aUpadate node6bUpdate)), EdgeSet (IS.singleton 0) mempty]
        expected6 = Tree names4 seqs4 (chars1 V.++ chars2) (fromList [node6aUpadate, node6bUpdate]) edges6 0
        tree6a = Tree (IM.fromList [(0, "0")]) (HM.fromList [("0", mempty)]) chars1 (fromList [node6a]) mempty 0
        tree6b = Tree (IM.fromList [(0, "0")]) (HM.fromList [("0", mempty)]) chars2 (fromList [node6b]) mempty 0
        result6 = tree6a <> tree6b

subsetting :: TestTree
subsetting = testGroup "Check correct subsetting of trees" [twoNode, smallerThan, threeNode]
    where
        smallerThan = testProperty "The subset of a tree is always smaller or the same" checkSmall
            where
                checkSmall :: Tree -> Bool
                checkSmall myTree
                    | null $ nodes myTree = True
                    | otherwise = 
                        let result = accessSubtree myTree (nodes myTree ! 0)
                        in (length $ nodes result) <= (length $ nodes myTree)

        twoNode = testCase "Subtree of a two node tree gives expected result" (expected0 @=? result0)
        node0a = Node 0 True False [] [1] mempty mempty mempty mempty mempty mempty 0 :: NodeInfo
        node0b = Node 1 False True [0] [] mempty mempty mempty mempty mempty mempty 0 :: NodeInfo
        edges0 = fromList $ [EdgeSet mempty (IM.singleton 1 (EdgeInfo 0 node0a node0b)), EdgeSet (IS.singleton 0) mempty]
        tree0 = Tree mempty mempty mempty (fromList [node0a, node0b]) edges0 0
        result0 = accessSubtree tree0 node0b
        expected0 = Tree mempty mempty mempty (singleton $ node0b {code = 0, parents = []}) mempty 0

        threeNode = testCase "Subtree of a three node tree gives expected result" (expected1 @=? result1)
        node1a = Node 0 True False [] [1] mempty mempty mempty mempty mempty mempty 0 :: NodeInfo
        node1b = Node 1 False False [0] [2] mempty mempty mempty mempty mempty mempty 0 :: NodeInfo
        node1c = Node 2 False True [1] [] mempty mempty mempty mempty mempty mempty 0 :: NodeInfo
        edges1 = fromList $ [EdgeSet mempty (IM.singleton 1 (EdgeInfo 0 node1a node1b)), EdgeSet (IS.singleton 0) (IM.singleton 2 (EdgeInfo 0 node1b node1c)), EdgeSet (IS.singleton 1) mempty]
        tree1 = Tree mempty mempty mempty (fromList [node1a, node1b, node1c]) edges1 0
        edgeExpect1 = fromList $ [EdgeSet mempty (IM.singleton 1 (EdgeInfo 0 node1b node1c)), EdgeSet (IS.singleton 0) mempty]
        result1 = accessSubtree tree1 node1b
        expected1 = Tree mempty mempty mempty (fromList [node1b {code = 0, parents = []}, node1c {code = 1, parents = [0]}]) edgeExpect1 0