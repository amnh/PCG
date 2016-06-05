-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.PhyloGraph.DAG.Internal
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Test suite for DAGs
--
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}


module Bio.PhyloGraph.DAG.Test
    ( testSuite
    ) where

import           Bio.PhyloGraph.DAG.Internal  hiding (root)
import           Bio.PhyloGraph.Network
import           Bio.PhyloGraph.Node          hiding (parents, children)
import qualified Data.Vector                  as V
import           Data.MonoTraversable
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

import Debug.Trace

testSuite :: TestTree
testSuite = testGroup "DAG tests" [typeClassLawsForNetwork, updateWorksCorrectlyTests]

typeClassLawsForNetwork :: TestTree
typeClassLawsForNetwork = testGroup "DAG is an appropriate instance of Network" [ nodeIsRootTest
                                                                                , nonSingletonNetworkRootIsNotLeafTest
                                                                                , allNonrootNodesAreNotRootTest
                                                                                , allRootNodesHaveNoParentsTest
                                                                                , allLeafNodesHaveNoChildrenTest
                                                                               -- , afterAddingNodeNumNodesIncreasesAppropriatelyTest
                                                                                , afterUpdatingSingleNodeNumNodesDoesntChangeTest
                                                                                ]

nodeIsRootTest :: TestTree
nodeIsRootTest = testProperty "nodeIsRoot (root t) t" nodeIsRoot'

nodeIsRoot' :: DAG -> Bool
nodeIsRoot' dag = nodeIsRoot (root dag) dag

nonSingletonNetworkRootIsNotLeafTest :: TestTree
nonSingletonNetworkRootIsNotLeafTest = testProperty "(numNodes t) > 1 ==> not (nodeIsLeaf (root t) t)" f
    where
        f :: DAG -> Property
        f dag = numNodes dag > 1 ==> property (not (nodeIsLeaf (root dag) dag))

allNonrootNodesAreNotRootTest :: TestTree
allNonrootNodesAreNotRootTest = testProperty "forall a. (root t) /= a ==> not (nodeIsRoot a t)" onlyNodeIsRoot
        
onlyNodeIsRoot :: DAG -> Bool
onlyNodeIsRoot dag = oall (\node -> {- trace (show node) $ -} (root dag /= node) /= nodeIsRoot node dag) dag
                                                                       {- ^^this /= is not working the way I thought it would. -}

-- Alex pulled this out. Now it's back in?
rootConsistency dag = oall (\node -> (root dag /= node) /= nodeIsRoot node dag) dag

allRootNodesHaveNoParentsTest :: TestTree
allRootNodesHaveNoParentsTest = testProperty "forall a. null (parents (nodeIsRoot a t) t)" f
    where
        f :: DAG -> Bool
        f dag = oall (\node -> null (parents node dag) == nodeIsRoot node dag) dag

allLeafNodesHaveNoChildrenTest :: TestTree
allLeafNodesHaveNoChildrenTest = testProperty "forall a. null (children (nodeIsLeaf a t) t)" f
    where
        f :: DAG -> Bool
        f dag = oall (\node -> null (children node dag) == nodeIsLeaf node dag) dag

-- addNode is unused, so this test is commented out
{-
afterAddingNodeNumNodesIncreasesAppropriatelyTest :: TestTree
afterAddingNodeNumNodesIncreasesAppropriatelyTest = testProperty "numNodes (addNode t a) == numNodes t + 1" f
    where
        f :: DAG -> NodeInfo -> Bool
        f dag node = numNodes dag + 1 == numNodes newDag
            where
                newDag = addNode dag node
-}

afterUpdatingSingleNodeNumNodesDoesntChangeTest :: TestTree
afterUpdatingSingleNodeNumNodesDoesntChangeTest = testProperty "numNodes (update  t a) == numNodes t" f
    where
        f :: DAG -> Gen Bool
        f dag = do
              i <- (getNonNegative <$> arbitrary) `suchThat` (< numNodes dag)
              let newDag = update dag [node']
                  node   = nodes dag V.! i
                  node'  = node { name = "Changed" }
              pure $ nodeIsRoot' newDag


-- TODO: run update and make sure above laws still hold.
-- remember to add a leaf and add a root.

updateWorksCorrectlyTests :: TestTree
updateWorksCorrectlyTests = testGroup "update isn't breaking the tree" [ rootIsStillRootTest
                                                                       , stillNoOtherNodesAreRootTest
                                                                       , updateDoesSomething
                                                                      -- , allNonrootNodesAreNotRootTest
                                                                      -- , allRootNodesHaveNoParentsTest
                                                                      -- , allLeafNodesHaveNoChildrenTest
                                                                      ---- , afterAddingNodeNumNodesIncreasesAppropriatelyTest
                                                                      -- , afterUpdatingSingleNodeNumNodesDoesntChangeTest
                                                                       ]

rootIsStillRootTest :: TestTree
rootIsStillRootTest = testProperty "After update making a random node root, nodeIsRoot (root t) t" f
    where
        f :: DAG -> Gen Bool
        f dag = do
              i <- (getNonNegative <$> arbitrary) `suchThat` (< numNodes dag)
              let newDag = update dag [node']
                  node   = nodes dag V.! i
                  node'  = node { isRoot = True }
              pure $ nodeIsRoot (root newDag) newDag

stillNoOtherNodesAreRootTest :: TestTree
stillNoOtherNodesAreRootTest = testProperty 
    "After update making a random node root, forall a. (root t) /= a ==> not (nodeIsRoot a t)" f
    where
        f :: DAG -> Gen Bool
        f dag = do
              i <- (getNonNegative <$> arbitrary) `suchThat` (< numNodes dag)
              let newDag = update dag [node']
                  node   = nodes dag V.! i
                  node'  = node { isRoot = True }
              pure $ {- trace ("New node: " ++ (show node') ++ "\n\nDAG: " ++ (show newDag) ++ "\n\n") $ -} onlyNodeIsRoot newDag

updateDoesSomething :: TestTree
updateDoesSomething = testProperty "After update, DAG has changed" f
    where
        f :: DAG -> Gen Bool
        f dag = do
              i <- (getNonNegative <$> arbitrary) `suchThat` (< numNodes dag)
              let newDag = update dag [node']
                  node   = nodes dag V.! i
                  node'  = node { name = "Changed" }
              pure $ newDag /= dag

{-
nonSingletonNetworkRootIsNotLeafTest :: TestTree
nonSingletonNetworkRootIsNotLeafTest = testProperty "(numNodes t) > 1 ==> not (nodeIsLeaf (root t) t)" f
    where
        f :: DAG -> Property
        f dag = (numNodes dag) > 1 ==> property (not (nodeIsLeaf (root dag) dag))

allNonrootNodesAreNotRootTest :: TestTree
allNonrootNodesAreNotRootTest = testProperty "forall a. (root t) /= a ==> not (nodeIsRoot a t)" f
    where
        f :: DAG -> Bool
        f dag = oall (\node -> ((root dag) /= node) /= (nodeIsRoot node dag)) dag

allRootNodesHaveNoParentsTest :: TestTree
allRootNodesHaveNoParentsTest = testProperty "forall a. null (parents (nodeIsRoot a t) t)" f
    where
        f :: DAG -> Bool
        f dag = oall (\node -> (null $ parents node dag) == (nodeIsRoot node dag)) dag

allLeafNodesHaveNoChildrenTest :: TestTree
allLeafNodesHaveNoChildrenTest = testProperty "forall a. null (children (nodeIsLeaf a t) t)" f
    where
        f :: DAG -> Bool
        f dag = oall (\node -> (null $ children node dag) == (nodeIsLeaf node dag)) dag
-}
