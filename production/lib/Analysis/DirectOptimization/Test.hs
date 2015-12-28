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

import Bio.Phylogeny.Graph.Random
import Bio.Phylogeny.Graph

import Analysis.DirectOptimization.Naive
import Analysis.DirectOptimization.ImpliedAlign
import Analysis.DirectOptimization.Utilities

main :: IO()
main = do
    defaultMain (testGroup "Tests of Direct Optimization" [subtreeVerify, doVerify, iaVerify])

testSuite :: TestTree
testSuite = testGroup "Direct Optimization" [subtreeVerify]

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
doVerify = undefined

iaVerify :: TestTree
iaVerify = undefined