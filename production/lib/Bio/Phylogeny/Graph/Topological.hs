-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Phylogeny.Graph.Random
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Module making a topological graph similar to the main graph
-- this allows for some nice behavior with random generation
--
-----------------------------------------------------------------------------

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Bio.Phylogeny.Graph.Topological where

import Test.Tasty.QuickCheck

import Debug.Trace

import Bio.Phylogeny.Graph
import Bio.Phylogeny.Tree.Node.Topological
import Bio.Sequence.Coded

import Data.IntMap (IntMap)
import Data.HashMap.Strict (HashMap)
import Data.BitVector (BitVector, fromBits)
import Data.Vector (fromList, Vector)

maxDepth = 100
minDepth = 0

type TopoTree = TopoNode BitVector
type MultiSeq = Vector (EncodedSeq BitVector)

data TopoGraph = TopoGraph [TopoTree] deriving (Show)

instance Arbitrary TopoTree where
    arbitrary = do
        children <- listOf $ internalRandom 0
        seq1 <- arbitrary :: Gen MultiSeq
        seq2 <- arbitrary :: Gen MultiSeq
        seq3 <- arbitrary :: Gen MultiSeq
        seq4 <- arbitrary :: Gen MultiSeq
        seq5 <- arbitrary :: Gen MultiSeq
        seq6 <- arbitrary :: Gen MultiSeq
        cost <- arbitrary :: Gen Float
        let name = show 0 ++ show cost
        if null children then return $ TopoNode True True name children seq1 seq2 seq3 seq4 seq5 seq6 cost
            else return $ TopoNode True False name children seq1 seq2 seq3 seq4 seq5 seq6 cost

internalRandom :: Int -> Gen TopoTree
internalRandom depth | trace ("internal random generation " ++ show depth) False = undefined
internalRandom myDepth = do
    children <- listOf (internalRandom (myDepth + 1))
    seq1 <- arbitrary :: Gen MultiSeq
    seq2 <- arbitrary :: Gen MultiSeq
    seq3 <- arbitrary :: Gen MultiSeq
    seq4 <- arbitrary :: Gen MultiSeq
    seq5 <- arbitrary :: Gen MultiSeq
    seq6 <- arbitrary :: Gen MultiSeq
    cost <- arbitrary :: Gen Float
    let name = show myDepth ++ show cost
    rand <- choose (0, maxDepth)
    let terminate = chooseTerminate myDepth rand
    if terminate then return $ TopoNode False True name [] seq1 seq2 seq3 seq4 seq5 seq6 cost
        else return $ TopoNode False False name children seq1 seq2 seq3 seq4 seq5 seq6 cost

chooseTerminate :: Int -> Int -> Bool
chooseTerminate curDepth rand
    | curDepth < minDepth || curDepth < rand = False
    | curDepth >= maxDepth || curDepth >= rand = True
    | otherwise = False

instance Arbitrary MultiSeq where
    arbitrary = fromList <$> listOf (arbitrary :: Gen (EncodedSeq BitVector))

instance Arbitrary (EncodedSeq BitVector) where
    arbitrary = do
        there <- arbitrary :: Gen Bool
        if there then Just <$> fromList <$> listOf (arbitrary :: Gen BitVector)
            else return Nothing

instance Arbitrary BitVector where
    arbitrary = fromBits <$> listOf (arbitrary :: Gen Bool)

instance Arbitrary TopoGraph where
    arbitrary = TopoGraph <$> listOf (arbitrary :: Gen TopoTree)