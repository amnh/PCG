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
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Bio.Phylogeny.Graph.Topological where

import Test.Tasty.QuickCheck

-- import Debug.Trace

import Bio.Phylogeny.Tree.Node.Topological
import Bio.Sequence.Coded
import Bio.Sequence.Random ()

import Data.BitVector (BitVector)

maxDepth, minDepth, maxChildren :: Int
maxDepth = 3
minDepth = 0
maxChildren =  2

type TopoTree = TopoNode BitVector
type MultiSeq = EncodedSequences BitVector

newtype TopoGraph = TopoGraph [TopoTree] 

instance Arbitrary TopoTree where
    arbitrary = do
        numChildren <- choose (1, maxChildren) :: Gen Int
        randChildren <- vectorOf numChildren (internalRandom 0)
        seqs <- vectorOf 6 (arbitrary :: Gen MultiSeq)
        randCost <- arbitrary :: Gen Double
        let randName = (show (0 :: Int)) ++ show randCost
        if null randChildren then return $ TopoNode True True  randName [] (seqs !! 0) (seqs !! 1) (seqs !! 2) (seqs !! 3) (seqs !! 4) (seqs !! 5) randCost
                         else return $ TopoNode True False randName randChildren (seqs !! 0) (seqs !! 1) (seqs !! 2) (seqs !! 3) (seqs !! 4) (seqs !! 5) randCost

internalRandom :: Int -> Gen TopoTree
--internalRandom depth | trace ("internal random generation " ++ show depth) False = undefined
internalRandom myDepth = do
    numChildren <- choose (1, maxChildren) :: Gen Int
    randChildren <- vectorOf numChildren (internalRandom (myDepth + 1))
    seqs <- vectorOf 6 (arbitrary :: Gen MultiSeq)
    randCost <- arbitrary :: Gen Double
    let randName = show myDepth ++ show randCost
    rand <- choose (0, maxDepth)
    let terminate = chooseTerminate myDepth rand
    if terminate then --trace "terminate" 
            (return $ TopoNode False True randName [] (seqs !! 0) (seqs !! 1) (seqs !! 2) (seqs !! 3) (seqs !! 4) (seqs !! 5) randCost)
        else --trace "continue" 
            (return $ TopoNode False False randName randChildren (seqs !! 0) (seqs !! 1) (seqs !! 2) (seqs !! 3) (seqs !! 4) (seqs !! 5) randCost)

chooseTerminate :: Int -> Int -> Bool
chooseTerminate curDepth rand
    | curDepth < minDepth || curDepth < rand = False
    | curDepth >= maxDepth || curDepth >= rand = True
    | otherwise = False

instance Arbitrary TopoGraph where
    arbitrary = TopoGraph <$> listOf (arbitrary :: Gen TopoTree)