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

import Prelude hiding (length, or, zipWith, replicate)

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import Data.Vector (length, or, zipWith, singleton, replicate, fromList)
import Data.Matrix.NotStupid (nrows)
import Data.BitVector (BitVector)
import Data.Bits
import Data.Monoid

import Bio.Phylogeny.Graph.Random
import Bio.Phylogeny.Graph
import Bio.Sequence.Coded
import Bio.Sequence.Random
import Bio.Phylogeny.Tree.Node.Random
import Bio.Phylogeny.Tree.Node

import Analysis.DirectOptimization.Naive
import Analysis.DirectOptimization.ImpliedAlign
import Analysis.DirectOptimization.Utilities

import Debug.Trace

testSuite :: TestTree
testSuite = testGroup "Direct Optimization" [subtreeVerify, doVerify, edgeCases]

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
                    in ((numChars optimized >= numChars seq1) && (numChars optimized >= numChars seq2)) || numChars optimized == 0

        checkID = testProperty "Two copies of the same sequence result in an ID result" isID
            where
                isID :: EncodedSeq BitVector -> Bool
                isID inSeq = 
                    let (optimized, _, _, _, _) = naiveDO inSeq inSeq
                    in optimized == inSeq

edgeCases :: TestTree
edgeCases = testGroup "Check function of direct optimization on edge cases" [oneEmpty, lenOne, oneOne, shortCase]
    where
        -- encode over ACGT
        seq1a = encodeOverAlphabet (fromList $ [["A"], ["G"], ["T"]]) ["A", "G", "T", "C"] :: EncodedSeq BitVector
        seq1b = encodeOverAlphabet mempty ["A", "G", "T", "C"] :: EncodedSeq BitVector
        (align, _, gapped, a, b) = naiveDO seq1a seq1b
        oneEmpty = testCase "Good behavior with one sequence empty" (Nothing @=? gapped)

        seq2a = encodeOverAlphabet (singleton ["T"]) ["A", "G", "T", "C"] :: EncodedSeq BitVector
        seq2b = encodeOverAlphabet (singleton ["G"]) ["A", "G", "T", "C"] :: EncodedSeq BitVector
        (_, _, _, a2, b2) = naiveDO seq2a seq2b
        lenOne = testCase "Good behavior with two sequences of length one" (a2 @=? seq2a)

        (_, _, _, _, b3) = trace ("encoding of test seqs " ++ show seq1a ++ show seq2b) $ naiveDO seq1a seq2b
        expected = (charToSeq gapChar) <> (encodeOverAlphabet (singleton ["G"]) ["A", "G", "T", "C"]) <> (charToSeq gapChar) :: EncodedSeq BitVector
        oneOne = testCase "Good behavior where one sequence is much shorter" (expected @=? b3)

        seq3a = encodeOverAlphabet (fromList $ [["A"], ["C", "T"], ["G"], ["C"], ["T"]]) ["A", "G", "T", "C"] :: EncodedSeq BitVector
        seq3b = encodeOverAlphabet (fromList $ [["T"], ["G"], ["C"], ["T"]]) ["A", "G", "T", "C"] :: EncodedSeq BitVector
        (result, _, _, _, _) = naiveDO seq3a seq3b
        trueval = (charToSeq gapChar) <> (encode $ fromList [["T"], ["G"], ["C"], ["T"]]) :: EncodedSeq BitVector
        shortCase = testCase "Expected result from a small test case" (trueval @=? result)


--matrixVerify :: TestTree
--matrixVerify = testGroup "Check the basic functionality of the traversal matrix generation" [checkDirs]
--    where
--        seq1 = encodeOverAlphabet (fromList $ [["A"], ["G"], ["T"]]) ["A", "G", "T", "C"] :: EncodedSeq BitVector
--        seq2 = (encodeOverAlphabet (singleton ["G"]) ["A", "G", "T", "C"]) :: EncodedSeq BitVector
--        prevRow = (fromList [0, 1, 2, 3], mempty, mempty)
--        (costs, seqs, dirs) = generateRow seq1 seq2 (1, 1) 2 prevRow (0, 0)
--        correctDirs = fromList [DownDir, LeftDir, DiagDir, LeftDir]

--        checkDirs = testCase "Row evaluates correct directions" (correctDirs @=? dirs)


iaVerify :: TestTree
iaVerify = testGroup "Check implied alignment function" [checkLen]
    where
        checkLen = testProperty "Length of IA result is the same or longer than inputs" alignLen
            where
                alignLen :: Tree -> Bool
                alignLen tree = 
                    let result = implyMain tree
                    in checkLens tree result

                checkLens :: Tree -> Tree -> Bool
                checkLens tree1 tree2 | trace ("checkLens " ++ show (length $ nodes tree1) ++ " " ++ show (length $ nodes tree2)) False = undefined
                checkLens tree1 tree2 = 
                    let compVals = zipWith (\n1 n2 -> (length $ aligned n2) >= (length $ encoded n1) || (length $ aligned n2) == 0) (nodes tree1) (nodes tree2)
                    in trace (show compVals) 
                        and compVals

