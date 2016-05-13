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

-- TODO: Make an AppliedAlignment.hs file for exposure of appropriate functions

module Analysis.ImpliedAlignment.Standard where

import Analysis.ImpliedAlignment.Internal
import Analysis.Parsimony.Binary.DirectOptimization
import Bio.Metadata
import Bio.PhyloGraph.DAG
import Bio.PhyloGraph.Forest
import Bio.PhyloGraph.Network
import Bio.PhyloGraph.Node
import Bio.PhyloGraph.Solution
import Bio.PhyloGraph.Tree hiding (code)
import Bio.Character.Dynamic.Coded

import Data.BitVector      hiding (foldr, replicate, foldl)
import Data.IntMap                (insert)
import Data.Maybe
import Data.Monoid
import Data.Vector                (Vector, (!), cons, filter, foldr, fromList, generate, imap, replicate, unzip, zip3, zipWith, zipWith3, zipWith5, foldl)
import Prelude             hiding (filter, foldr, replicate, unzip, zip3, zipWith, zipWith3, foldl)
import qualified Data.Vector as V

import Debug.Trace

-- | Top level wrapper to do an IA over an entire solution
-- takes a solution
-- returns an AlignmentSolution
iaSolution :: SolutionConstraint r m f t n e s => r -> AlignmentSolution s
iaSolution inSolution | trace ("iaSolution " ++ show inSolution) False = undefined
iaSolution inSolution = fmap (flip iaForest (getMetadata inSolution)) (getForests inSolution)

-- | Simple wrapper to do an IA over a forest
-- takes in a forest and some metadata
-- returns an alignment forest
iaForest :: (ForestConstraint f t n e s, Metadata m s) => f -> Vector m -> AlignmentForest s
iaForest inForest inMeta = fmap (flip impliedAlign inMeta) (trees inForest)

-- TODO: used concrete BitVector type instead of something more appropriate, like EncodableDynamicCharacter. 
-- This also means that there are a bunch of places below that could be using EDC class methods that are no longer.
-- The same will be true in DO.
-- | Function to perform an implied alignment on all the leaves of a tree
-- takes a tree and some metadata
-- returns an alignment object (an intmap from the leaf codes to the aligned sequence)
-- TODO: Consider building the alignment at each step of a postorder rather than grabbing wholesale
impliedAlign :: (TreeConstraint t n e s, Metadata m s) => t -> Vector m -> Alignment s
--impliedAlign inTree inMeta | trace ("impliedAlign with tree " ++ show inTree) False = undefined
impliedAlign inTree inMeta = foldr (\n acc -> insert (getCode n) (makeAlignment n) acc) mempty allLeaves
    where
        (_, curTree) = numeratePreorder inTree (getRoot inTree) inMeta (replicate (length inMeta) 0)
        allLeaves = filter (flip nodeIsLeaf curTree) (getNodes curTree)

-- | Simple function to generate an alignment from a numerated node
-- Takes in a Node
-- returns a vector of characters
makeAlignment :: (NodeConstraint n s) => n -> Vector s
makeAlignment n = makeAlign (getFinalGapped n) (getHomologies n)
    where
         -- oneTrace :: s -> Homologies -> m -> s
        oneTrace dynChar homolog = foldr (\pos acc -> unsafeCons (grabSubChar dynChar pos) acc) emptyChar homolog
        --makeAlign :: Vector s -> HomologyTrace -> Vector s
        makeAlign dynChar homologies = zipWith oneTrace dynChar homologies

-- | Main recursive function that assigns homology traces to every node
-- takes in a tree, a current node, a vector of metadata, and a vector of counters
-- outputs a resulting vector of counters and a tree with the assignments
-- TODO: something seems off about doing the DO twice here
numeratePreorder :: (TreeConstraint t n e s, Metadata m s) => t -> n -> Vector m -> Counts -> (Counts, t)
numeratePreorder _ curNode _ _ | trace ("numeratePreorder at " ++ show curNode) False = undefined
numeratePreorder inTree curNode inMeta curCounts
    | nodeIsRoot curNode inTree = (curCounts, inTree `update` [setHomologies curNode defaultHomologs])
    | leftOnly && rightOnly = (curCounts, inTree)
    | leftOnly =
        let
            (curLeftAligned, leftWithAligned)     = alignAndAssign curNode (fromJust $ leftChild curNode inTree)
            (curLeftHomolog, counterLeft)         = numerateNode curLeftAligned leftWithAligned curCounts inMeta
            editedTreeLeft                        = inTree `update` [curLeftHomolog, leftWithAligned]
            (leftRecurseCount, leftRecurseTree)   = numeratePreorder editedTreeLeft (fromJust $ leftChild curNode inTree) inMeta counterLeft
        in (leftRecurseCount, leftRecurseTree)
    | rightOnly =
        let
            (curRightAligned, rightWithAligned)   = alignAndAssign curNode (fromJust $ rightChild curNode inTree)
            (curRightHomolog, counterRight)       = numerateNode curRightAligned rightWithAligned curCounts inMeta
            editedTreeRight                       = inTree `update` [curRightHomolog, rightWithAligned]
            (rightRecurseCount, rightRecurseTree) = numeratePreorder editedTreeRight (fromJust $ rightChild curNode inTree) inMeta counterRight
        in (rightRecurseCount, rightRecurseTree)
    | otherwise =
        let
            -- TODO: should I switch the order of align and numerate? probs
            (curLeftAligned, leftWithAligned)     = alignAndAssign curNode (fromJust $ leftChild curNode inTree)
            (curLeftHomolog, counterLeft)         = numerateNode curLeftAligned leftWithAligned curCounts inMeta
            (curBothAligned, rightBothAligned)    = alignAndAssign curLeftHomolog (fromJust $ rightChild curNode inTree)
            (curBothHomolog, counterBoth)         = numerateNode curBothAligned rightBothAligned counterLeft inMeta
            editedTreeBoth                        = inTree `update` [curBothHomolog, leftWithAligned, rightBothAligned]
            (leftRecurseCount, leftRecurseTree)   = numeratePreorder editedTreeBoth (fromJust $ rightChild curBothHomolog editedTreeBoth) inMeta counterBoth
            -- TODO: need another align and assign between the left and right as a last step?
            output                                = numeratePreorder leftRecurseTree (fromJust $ leftChild curBothHomolog leftRecurseTree) inMeta leftRecurseCount
        in output

        where
            curSeqs = getFinalGapped curNode
            leftOnly = isNothing $ rightChild curNode inTree
            rightOnly = isNothing $ leftChild curNode inTree
            -- TODO: check if this is really the default
            defaultHomologs = imap (\i _ -> generate (numChars (curSeqs ! i)) (+ 1)) inMeta

            -- Simple wrapper to align and assign using DO
            --alignAndAssign :: NodeConstraint n s => n -> n -> (n, n)
            -- TODO: Don't use the gapped here
            alignAndAssign node1 node2 = (setFinalGapped (fst allUnzip) node1, setFinalGapped (snd allUnzip) node2)
                where
                    allUnzip = unzip allDO
                    allDO = zipWith3 doOne (getFinalGapped node1) (getFinalGapped node2) inMeta
                    doOne s1 s2 m = (gapped1, gapped2)
                        where (_, _, _, gapped1, gapped2) = naiveDO s1 s2 m

-- | Function to do a numeration on an entire node
-- given the ancestor node, ancestor node, current counter vector, and vector of metadata
-- returns a tuple with the node with homologies incorporated, and a returned vector of counters
numerateNode :: (NodeConstraint n s, Metadata m s) => n -> n -> Counts -> Vector m -> (n, Counts) 
numerateNode ancestorNode childNode initCounters inMeta = (setHomologies childNode homologs, counts)
        where
            numeration = zipWith5 numerateOne (generateGapChar <$> inMeta) (getFinalGapped ancestorNode) (getHomologies ancestorNode) (getFinalGapped childNode) initCounters 
            (homologs, counts) = unzip numeration
            generateGapChar m = setBit (bitVec 0 (0 :: Integer)) (length (getAlphabet m) - 1)

-- | Function to do a numeration on one character at a node
-- given the ancestor sequence, ancestor homologies, child sequence, and current counter for position matching
-- returns a tuple of the Homologies vector and an integer count
numerateOne :: (SeqConstraint s) => BitVector -> s -> Homologies -> s -> Int -> (Homologies, Int)
--numerateOne _ a h c _ | trace ("numerateOne with ancestor " ++ show a ++ " and child " ++ show c ++ " and homologies " ++ show h) False = undefined
numerateOne gapCharacter ancestorSeq ancestorHomologies childSeq initCounter = foldl (determineHomology gapCharacter) (mempty, initCounter) foldIn
    where
        getAllSubs s = foldr (\p acc -> grabSubChar s p `cons` acc) mempty (fromList [0..(numChars s) - 1])
        -- TODO: verify that ancestorHomologies has the correct length as the allSubs
        foldIn = --trace ("calls to homology " ++ show (getAllSubs childSeq) ++ show (getAllSubs ancestorSeq)) $
                    zip3 (getAllSubs childSeq) (getAllSubs ancestorSeq) ancestorHomologies

-- Finds the homology position between any two characters
determineHomology :: BitVector -> (Homologies, Int) -> (BitVector, BitVector, Int) -> (Homologies, Int)
--determineHomology _ i (c, a, h) | trace ("one homology " ++ show i ++ " on " ++ show (c, a, h)) False = undefined
determineHomology gapCharacter (homologSoFar, counterSoFar) (childChar, ancestorChar, ancestorHomolog)
    | ancestorChar == gapCharacter = (homologSoFar V.++ pure counterSoFar, counterSoFar    )
    | childChar    /= gapCharacter = (homologSoFar V.++ pure ancestorHomolog, counterSoFar + 1)
    | otherwise                    = (homologSoFar V.++ pure counterSoFar, counterSoFar + 1) --TODO: check this case
