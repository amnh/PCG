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
-- Test suite for direct optimization
--
-----------------------------------------------------------------------------

module Analysis.DirectOptimization.Test where

import Prelude hiding (length)

import Test.Tasty
import Test.Tasty.QuickCheck

import Data.Vector (length)
import Data.Matrix (nrows)
import Data.BitVector (BitVector)

import Bio.Phylogeny.Graph.Random
import Bio.Phylogeny.Graph
import Bio.Sequence.Coded
import Bio.Sequence.Random
import Bio.Phylogeny.Tree.Node.Random
import Bio.Phylogeny.Tree.Node

import Analysis.DirectOptimization.Naive
import Analysis.DirectOptimization.ImpliedAlign
import Analysis.DirectOptimization.Utilities

--main :: IO()
--main = do
--    defaultMain (testGroup "Tests of Direct Optimization" [subtreeVerify, doVerify])

testSuite :: TestTree
testSuite = testGroup "Direct Optimization" [subtreeVerify, doVerify]

subtreeVerify :: TestTree
subtreeVerify = testGroup "Check correct generation of subtrees for recursion" [subLength, correctOnes]
    where
        subLength = testProperty "Subtree generation returns matrix of correct size" subLen
            where
                subLen :: Tree -> Bool
                subLen tree = nrows subtrees == length (nodes tree)
                    where subtrees = getSubtrees tree

        correctOnes = testProperty "Subtree matrix has 2n - 4 ones where n is the number of nodes" subContent
            where
                subContent :: Tree -> Bool
                subContent tree = 
                    let 
                        numOnes = foldr (+) 0 (getSubtrees tree)
                        n = length $ nodes tree
                    in numOnes == (2 * n) - 4 || numOnes == 0

doVerify :: TestTree
doVerify = testGroup "Check direct optimization function" [compareWrappers, checkLen]
    where
        compareWrappers = testProperty "Two and three node wrappers give same result" compWrap
            where
                compWrap :: Node BitVector -> Node BitVector -> Bool
                compWrap node1 node2 = 
                    let 
                        (n1a, n2a, _, l1) = naiveDOTwo node1 node2 
                        (n1b, n1c, n2b, l2) = naiveDOThree node1 node1 node2
                    in n1a == n1b && n2a == n2b && l1 == l2
        checkLen = testProperty "Length of DO result is the same or longer than inputs" alignLen
            where
                alignLen :: EncodedSeq BitVector -> EncodedSeq BitVector -> Bool
                alignLen seq1 seq2 = 
                    let (optimized, _, _, _, _) = naiveDO seq1 seq2
                    in (numChars optimized >= numChars seq1) || (numChars optimized >= numChars seq2)

iaVerify :: TestTree
iaVerify = undefined