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

import Bio.PhyloGraph.DAG.Internal hiding (root)
import Bio.PhyloGraph.Network
import Data.MonoTraversable
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Debug.Trace

testSuite :: TestTree
testSuite = testGroup "DAG tests" [typeClassLawsForNetwork]

typeClassLawsForNetwork :: TestTree
typeClassLawsForNetwork = testGroup "DAG is an appropriate instance of Network" [ nodeIsRootTest
                                                                                , nonSingletonNetworkRootIsNotLeafTest
                                                                                , allNonrootNodesIsNotRootTest
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
allNonrootNodesIsNotRootTest = testProperty "forall a. (root t) /= a ===> not (nodeIsRoot a t)" f
    where
        f :: DAG -> Bool
        f dag = oall (\node -> ((root dag) /= node) /= (nodeIsRoot node dag)) dag

{- | Type class Laws:

     forall a. null (parents  (nodeIsRoot a t) t)
     forall a. null (children (nodeIsLeaf a t) t)
     numNodes (addNode t a) == numNodes t + 1
     numNodes (update  t a) == numNodes t

 -}
