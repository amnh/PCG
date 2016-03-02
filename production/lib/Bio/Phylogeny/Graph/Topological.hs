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
import Bio.Sequence.Parsed
import Bio.Phylogeny.Graph.Data
import Data.BitVector (BitVector)
import Data.Vector    (Vector)
import qualified Data.Vector as V (filter)

maxDepth, minDepth, maxChildren :: Int
maxDepth = 3
minDepth = 0
maxChildren =  2

data TopoTree = TopoTree {tree :: TopoNode BitVector, characters :: Vector CharInfo} deriving (Eq, Show)
type MultiSeq = EncodedSequences BitVector

newtype TopoGraph = TopoGraph [TopoTree]

instance Monoid TopoTree where
    mempty = TopoTree mempty mempty
    mappend t1 t2 = 
        let
            (recoded1, recoded2) = recodeTopoChars t1 t2
            topNode = tree recoded1
        in recoded1 {tree = topNode {children = tree recoded2 : children topNode}}

instance Arbitrary TopoTree where
    arbitrary = do
        numChildren <- choose (1, maxChildren) :: Gen Int
        randChildren <- vectorOf numChildren (internalRandom 0)
        randSeq <- arbitrary :: Gen ParsedSequences
        let encodedRand = encodeAll randSeq
        seqs <- vectorOf 5 (arbitrary :: Gen MultiSeq)
        randCost <- arbitrary :: Gen Double
        let randName = show (0 :: Int) ++ show randCost
        let outNode = TopoNode True True randName randSeq [] encodedRand (head seqs) (seqs !! 1) (seqs !! 2) (seqs !! 3) (seqs !! 4) randCost
        if null randChildren then return $ TopoTree outNode mempty
                         else return $ TopoTree (outNode {isLeaf = False, children = randChildren}) mempty

-- | Function to recode characters in a topoTree
-- allows for coherent joining of trees over different alphabets
recodeTopoChars :: TopoTree -> TopoTree -> (TopoTree, TopoTree)
recodeTopoChars t1 t2 = undefined

internalRandom :: Int -> Gen (TopoNode BitVector)
--internalRandom depth | trace ("internal random generation " ++ show depth) False = undefined
internalRandom myDepth = do
    numChildren <- choose (1, maxChildren) :: Gen Int
    randChildren <- vectorOf numChildren (internalRandom (myDepth + 1))
    seqs <- vectorOf 5 (arbitrary :: Gen MultiSeq)
    randCost <- arbitrary :: Gen Double
    let randName = show myDepth ++ show randCost
    rand <- choose (0, maxDepth)
    randSeq <- arbitrary :: Gen ParsedSequences
    let encodedRand = encodeAll randSeq
    let terminate = chooseTerminate myDepth rand
    let outNode = TopoNode False True randName randSeq [] encodedRand (head seqs) (seqs !! 1) (seqs !! 2) (seqs !! 3) (seqs !! 4) randCost
    if terminate then --trace "terminate" 
            return outNode
        else --trace "continue" 
            return (outNode {isLeaf = False, children = randChildren})

chooseTerminate :: Int -> Int -> Bool
chooseTerminate curDepth rand
    | curDepth < minDepth || curDepth < rand = False
    | curDepth >= maxDepth || curDepth >= rand = True
    | otherwise = False

instance Arbitrary TopoGraph where
    arbitrary = TopoGraph <$> listOf (arbitrary :: Gen TopoTree)