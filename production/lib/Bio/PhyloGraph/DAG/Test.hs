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
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}


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
testSuite = testGroup "DAG tests" [typeClassLawsForNetwork]

typeClassLawsForNetwork :: TestTree
typeClassLawsForNetwork = testGroup "DAG is an appropriate instance of Network" [ nodeIsRootTest
                                                                                , nonSingletonNetworkRootIsNotLeafTest
                                                                                , allNonrootNodesIsNotRootTest
                                                                                , allRootNodesHaveNoParentsTest
                                                                                , allLeafNodesHaveNoChildrenTest
                                                                                , afterAddingNodeNumNodesIncreasesAppropriatelyTest
                                                                                , afterUpdatingSingleNodeNumNodesDoesntChangeTest
                                                                                ]

nodeIsRootTest :: TestTree
nodeIsRootTest = testProperty "nodeIsRoot (root t) t" f
    where
        f :: DAG -> Bool
        f dag = nodeIsRoot (root dag) dag

nonSingletonNetworkRootIsNotLeafTest :: TestTree
nonSingletonNetworkRootIsNotLeafTest = testProperty "(numNodes t) > 1 ==> not (nodeIsLeaf (root t) t)" f
    where
        f :: DAG -> Property
        f dag = (numNodes dag) > 1 ==> property (not (nodeIsLeaf (root dag) dag))

allNonrootNodesIsNotRootTest :: TestTree
allNonrootNodesIsNotRootTest = testProperty "forall a. (root t) /= a ==> not (nodeIsRoot a t)" rootConsistency

-- I pulled this out
rootConsistency :: DAG -> Bool
rootConsistency dag = oall (\node -> ((root dag) /= node) /= (nodeIsRoot node dag)) dag

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

afterAddingNodeNumNodesIncreasesAppropriatelyTest :: TestTree
afterAddingNodeNumNodesIncreasesAppropriatelyTest = testProperty "numNodes (addNode t a) == numNodes t + 1" f
    where
        f :: DAG -> NodeInfo -> Bool
        f dag node = numNodes dag + 1 == numNodes newDag
            where
                newDag = addNode dag node

afterUpdatingSingleNodeNumNodesDoesntChangeTest :: TestTree
afterUpdatingSingleNodeNumNodesDoesntChangeTest = testProperty "numNodes (update  t a) == numNodes t" f
    where
        f :: DAG -> Gen Bool
        f dag = do
              i <- (getNonNegative <$> arbitrary) `suchThat` (< numNodes dag)
              let newDag = update dag [node']
                  node   = nodes dag V.! i
--                  node'  = setEncoded node mempty
                  node'  = node { encoded = mempty }
              pure $ numNodes dag == numNodes newDag

{- | Type class Laws:

     numNodes (addNode t a) == numNodes t + 1
     numNodes (update  t a) == numNodes t

 -}
