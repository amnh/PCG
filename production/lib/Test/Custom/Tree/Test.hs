-----------------------------------------------------------------------------
-- |
-- Module      :  Test.Custom.Tree.Test
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Test suite for TestTree to make sure they behave sanely.
--
-----------------------------------------------------------------------------
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}


module Test.Custom.Tree.Test
  ( testSuite
  ) where

import           Bio.PhyloGraph.Network
import           Bio.PhyloGraph.Node.Encoded
import           Bio.PhyloGraph.Tree.Referential
import           Data.MonoTraversable
import qualified Data.Vector           as V
import           Test.Custom.Tree
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

import Debug.Trace

testSuite :: TestTree
testSuite = testGroup "SimpleTree tests" [typeClassLawsForNetwork]

typeClassLawsForNetwork :: TestTree
typeClassLawsForNetwork = testGroup "SimpleTree is an appropriate instance of Network"
    [ nodeIsRootTest
    , nonSingletonNetworkRootIsNotLeafTest
    , allNonrootNodesIsNotRootTest
    , allRootNodesHaveNoParentsTest
    , allLeafNodesHaveNoChildrenTest
    , afterUpdatingSingleNodeNumNodesDoesntChangeTest
    ]

nodeIsRootTest :: TestTree
nodeIsRootTest = testProperty "nodeIsRoot (root t) t" f
    where
        f :: SimpleTree -> Bool
        f dag = nodeIsRoot (root dag) dag

nonSingletonNetworkRootIsNotLeafTest :: TestTree
nonSingletonNetworkRootIsNotLeafTest = testProperty "(numNodes t) > 1 ==> not (nodeIsLeaf (root t) t)" f
    where
        f :: SimpleTree -> Property
        f dag = numNodes dag > 1 ==> property (not (nodeIsLeaf (root dag) dag))

allNonrootNodesIsNotRootTest :: TestTree
allNonrootNodesIsNotRootTest = testProperty "forall a. (root t) /= a ==> not (nodeIsRoot a t)" rootConsistency

-- I pulled this out
rootConsistency :: SimpleTree -> Bool
rootConsistency dag = oall (\node -> (root dag /= node) /= nodeIsRoot node dag) dag

allRootNodesHaveNoParentsTest :: TestTree
allRootNodesHaveNoParentsTest = testProperty "forall a. null (parents (nodeIsRoot a t) t)" f
    where
        f :: SimpleTree -> Bool
        f dag = oall (\node -> null (parents node dag) == nodeIsRoot node dag) dag

allLeafNodesHaveNoChildrenTest :: TestTree
allLeafNodesHaveNoChildrenTest = testProperty "forall a. null (children (nodeIsLeaf a t) t)" f
    where
        f :: SimpleTree -> Bool
        f dag = oall (\node -> null (children node dag) == nodeIsLeaf node dag) dag

afterUpdatingSingleNodeNumNodesDoesntChangeTest :: TestTree
afterUpdatingSingleNodeNumNodesDoesntChangeTest = testProperty "numNodes (update  t a) == numNodes t" f
    where
        f :: SimpleTree -> Gen Bool
        f dag = do
              i <- (getNonNegative <$> arbitrary) `suchThat` (< numNodes dag)
              let newDag = update dag [node']
                  node   = getNthNode dag i
                  node'  = setEncoded node mempty
              pure $ numNodes dag == numNodes newDag

{- | Type class Laws:

     numNodes (addNode t a) == numNodes t + 1
     numNodes (update  t a) == numNodes t

 -}
