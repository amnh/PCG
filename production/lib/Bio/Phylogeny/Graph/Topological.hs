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

import Bio.Phylogeny.Tree.Node.Topological
import Bio.Sequence.Coded
import Bio.Sequence.Random

import Data.BitVector (BitVector, fromBits)
import Data.Vector (fromList, Vector)

maxDepth, minDepth, maxChildren :: Int
maxDepth = 20
minDepth = 0
maxChildren = 3

type TopoTree = TopoNode BitVector
type MultiSeq = EncodedSequences BitVector

newtype TopoGraph = TopoGraph [TopoTree] 

instance Arbitrary TopoTree where
    arbitrary = do
        numChildren <- choose (1, maxChildren) :: Gen Int
        children <- vectorOf numChildren (internalRandom 0)
        seqs <- vectorOf 6 (arbitrary :: Gen MultiSeq)
        cost <- arbitrary :: Gen Float
        let name = show 0 ++ show cost
        if null children then return $ TopoNode True True name [] (seqs !! 0) (seqs !! 1) (seqs !! 2) (seqs !! 3) (seqs !! 4) (seqs !! 5) cost
            else return $ TopoNode True False name children (seqs !! 0) (seqs !! 1) (seqs !! 2) (seqs !! 3) (seqs !! 4) (seqs !! 5) cost

internalRandom :: Int -> Gen TopoTree
--internalRandom depth | trace ("internal random generation " ++ show depth) False = undefined
internalRandom myDepth = do
    numChildren <- choose (1, maxChildren) :: Gen Int
    children <- vectorOf numChildren (internalRandom (myDepth + 1))
    seqs <- vectorOf 6 (arbitrary :: Gen MultiSeq)
    cost <- arbitrary :: Gen Float
    let name = show myDepth ++ show cost
    rand <- choose (0, maxDepth)
    let terminate = chooseTerminate myDepth rand
    if terminate then --trace "terminate" 
            (return $ TopoNode False True name [] (seqs !! 0) (seqs !! 1) (seqs !! 2) (seqs !! 3) (seqs !! 4) (seqs !! 5) cost)
        else --trace "continue" 
            (return $ TopoNode False False name children (seqs !! 0) (seqs !! 1) (seqs !! 2) (seqs !! 3) (seqs !! 4) (seqs !! 5) cost)

chooseTerminate :: Int -> Int -> Bool
chooseTerminate curDepth rand
    | curDepth < minDepth || curDepth < rand = False
    | curDepth >= maxDepth || curDepth >= rand = True
    | otherwise = False

instance Arbitrary TopoGraph where
    arbitrary = TopoGraph <$> listOf (arbitrary :: Gen TopoTree)