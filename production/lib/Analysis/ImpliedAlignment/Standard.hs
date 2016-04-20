-----------------------------------------------------------------------------
-- |
-- Module      :  Analysis.ImpliedAlignment.Standard
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Standard algorithm for implied alignment
-----------------------------------------------------------------------------

module Analysis.ImpliedAlignment.Standard where

import Analysis.ImpliedAlignment.Internal
import Analysis.Parsimony.Binary.DirectOptimization
import Bio.Metadata
import Bio.PhyloGraph.Network
import Bio.PhyloGraph.Node
import Bio.PhyloGraph.Tree
import Bio.Sequence.Coded

import Data.Maybe
import Data.Vector (foldr, zip3, cons, fromList, zipWith5, unzip, Vector, imap, (!), zipWith3, generate)
import Prelude hiding (foldr, zip3, unzip, zipWith3)

type Counts = Vector Int

-- | Function to do a numeration on an entire node
-- given the ancestor node, ancestor node, current counter vector, and vector of metadata
-- returns a tuple with the node with homologies incorporated, and a returned vector of counters
numerateNode :: (NodeConstraint n s, Metadata m s) => n -> n -> Counts -> Vector m -> (n, Counts)
numerateNode ancestorNode childNode initCounters metadata = (setHomologies childNode homologs, counts)
        where
            numeration = zipWith5 numerateOne (getFinalGapped ancestorNode) (getHomologies ancestorNode) (getFinalGapped childNode) initCounters metadata
            (homologs, counts) = unzip numeration

-- | Function to do a numeration on one character at a node
-- given the ancestor sequence, ancestor homologies, child sequence, and current counter for position matching
-- returns a tuple of the Homologies vector and an integer count
numerateOne :: (SeqConstraint s, Metadata m s) => s -> Homologies -> s -> Int -> m -> (Homologies, Int)
numerateOne ancestorSeq ancestorHomologies childSeq initCounter meta = foldr determineHomology (mempty, initCounter) foldIn
    where
        alphLen = length $ getAlphabet meta
        getAllSubs s = foldr (\p acc -> grabSubChar s p alphLen `cons` acc) mempty (fromList [0..(numChars s alphLen)])
        foldIn = zip3 (getAllSubs childSeq) (getAllSubs ancestorSeq) ancestorHomologies

        -- Finds the homology position between any two characters
        determineHomology :: SeqConstraint s => (s, s, Int) -> (Homologies, Int) -> (Homologies, Int)
        determineHomology (childChar, ancestorChar, ancestorHomolog) (homologSoFar, counterSoFar)
            | ancestorChar == gapChar alphLen = (counterSoFar `cons` homologSoFar, counterSoFar)
            | childChar /= gapChar alphLen    = (ancestorHomolog `cons` homologSoFar, counterSoFar + 1)
            | otherwise                       = (counterSoFar `cons` homologSoFar, counterSoFar + 1) --TODO: check this case

-- | Main recursive function that assigns homology traces to every node
-- takes in a tree, a current node, a vector of metadata, and a vector of counters
-- outputs a resulting vector of counters and a tree with the assignments
-- TODO: something seems off about doing the DO twice here
numeratePreorder :: (TreeConstraint t n s, NodeConstraint n s, Metadata m s) => t -> n -> Vector m -> Counts -> (Counts, t)
numeratePreorder inTree curNode metadata curCounts 
    | nodeIsRoot curNode inTree = (curCounts, inTree `update` [setHomologies curNode defaultHomologs])
    | leftOnly && rightOnly = (curCounts, inTree)
    | leftOnly = 
        let
            (curLeftAligned, leftWithAligned)     = alignAndAssign curNode (fromJust $ leftChild curNode inTree)
            (curLeftHomolog, counterLeft)         = numerateNode curLeftAligned leftWithAligned curCounts metadata
            editedTreeLeft                        = inTree `update` [curLeftHomolog, leftWithAligned]
            (leftRecurseCount, leftRecurseTree)   = numeratePreorder editedTreeLeft (fromJust $ leftChild curNode inTree) metadata counterLeft
        in (leftRecurseCount, leftRecurseTree)
    | rightOnly = 
        let
            (curRightAligned, rightWithAligned)   = alignAndAssign curNode (fromJust $ rightChild curNode inTree)
            (curRightHomolog, counterRight)       = numerateNode curRightAligned rightWithAligned curCounts metadata
            editedTreeRight                       = inTree `update` [curRightHomolog, rightWithAligned]
            (rightRecurseCount, rightRecurseTree) = numeratePreorder editedTreeRight (fromJust $ rightChild curNode inTree) metadata counterRight
        in (rightRecurseCount, rightRecurseTree)
    | otherwise = 
        let
            (curLeftAligned, leftWithAligned)     = alignAndAssign curNode (fromJust $ leftChild curNode inTree)
            (curLeftHomolog, counterLeft)         = numerateNode curLeftAligned leftWithAligned curCounts metadata
            (curBothAligned, rightBothAligned)    = alignAndAssign curLeftHomolog (fromJust $ rightChild curNode inTree)
            (curBothHomolog, counterBoth)         = numerateNode curBothAligned rightBothAligned counterLeft metadata
            editedTreeBoth                        = inTree `update` [curBothHomolog, leftWithAligned, rightBothAligned]
            (leftRecurseCount, leftRecurseTree)   = numeratePreorder editedTreeBoth (fromJust $ rightChild curBothHomolog editedTreeBoth) metadata counterBoth
            output                                = numeratePreorder leftRecurseTree (fromJust $ leftChild curBothHomolog leftRecurseTree) metadata leftRecurseCount
        in output

        where
            curSeqs = getFinalGapped curNode
            leftOnly = isNothing $ rightChild curNode inTree
            rightOnly = isNothing $ leftChild curNode inTree
            -- TODO: check if this is really the default
            defaultHomologs = imap (\i m -> generate (numChars (curSeqs ! i) (length $ getAlphabet m)) (+ 1)) metadata

            -- Simple wrapper to align and assign using DO
            --alignAndAssign :: NodeConstraint n s => n -> n -> (n, n)
            alignAndAssign node1 node2 = (setFinalGapped (fst allUnzip) node1, setFinalGapped (snd allUnzip) node2)
                where 
                    allUnzip = unzip allDO
                    allDO = zipWith3 doOne (getFinalGapped node1) (getFinalGapped node2) metadata
                    doOne s1 s2 m = (gapped1, gapped2)
                        where (_, _, _, gapped1, gapped2) = naiveDO s1 s2 m
