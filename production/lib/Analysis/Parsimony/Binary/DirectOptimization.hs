-----------------------------------------------------------------------------
-- |
-- Module      :  Analysis.Parsimony.Binary.DirectOptimization
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Direct optimization functionality for binary trees
--
-----------------------------------------------------------------------------

module Analysis.Parsimony.Binary.DirectOptimization where

import Analysis.Parsimony.Binary.Internal
import Bio.Metadata
import Bio.Sequence.Coded
import Data.Bits
import Data.Vector   (Vector, cons, toList, singleton, (!))
import Data.Foldable (minimumBy)
import Data.Ord
import Data.Matrix   (Matrix, getElem, nrows, ncols, (<->), matrix, fromList)
import Data.Monoid

--import Debug.Trace

-- | The direction to align the character at a given matrix point.
data Direction = LeftDir | DiagDir | DownDir deriving (Eq, Show)

-- | A row of the 'AlignMatrix'
-- constructed as a tuple of vectors for easy joining to the full matrix
type AlignRow s = (Vector (Double, Direction), s)

-- | A representation of an alignment matrix
-- The matrix itself stores tuples of the cost and direction at that position
-- We also store a vector of sequences that are generated
data AlignMatrix s
   = AlignMatrix
   { mat     :: Matrix (Double, Direction)
   , seqs      :: Vector s
   } deriving (Eq, Show)

-- | The weighing criteria for the characters?
type Costs = (Double, Double)

-- | Performs a naive direct optimization
-- Takes in two sequences to run DO on and a metadata object
-- Returns an assignment sequence, the cost of that assignment, the assignment sequence with gaps included, 
-- the aligned version of the first input sequence, and the aligned version of the second input sequence
-- The process for this algorithm is to generate a traversal matrix, then perform a traceback
naiveDO :: (Metadata m s, SeqConstraint' s, CodedChar s) => s -> s -> m -> (s, Double, s, s, s)
--naiveDO s1 s2 | trace ("Sequences of length " ++ show (numChars s1 alphLen) ++ show (numChars s2 alphLen)) False = undefined
naiveDO seq1 seq2 meta
    | isEmpty seq1 || isEmpty seq2 || numChars seq1 alphLen == 0 || numChars seq2 alphLen == 0 = (emptySeq, 0, emptySeq, emptySeq, emptySeq)
    | otherwise = 
        let
            seq1Len = numChars seq1 alphLen
            seq2Len = numChars seq2 alphLen
            (shorter, _, longer, longlen) = if seq1Len > seq2Len
                                                   then (seq2, seq2Len, seq1, seq1Len)
                                                   else (seq1, seq1Len, seq2, seq2Len)
            firstMatRow = firstAlignRow (getIndelCost meta) longer longlen 0 0 alphLen
            traversalMat = firstMatRow `joinMat` getAlignRows longer shorter (getIndelCost meta, getSubCost meta) 1 firstMatRow alphLen
            cost = getMatrixCost traversalMat
            (gapped, left, right) = --trace ("get seqs " ++ show traversalMat)
                                    traceback traversalMat shorter longer alphLen
            ungapped = filterGaps gapped (gapChar alphLen) (getAlphabet meta)
            (out1, out2) = if seq1Len > seq2Len
                                then (right, left)
                                else (left, right)
        in (ungapped, cost, gapped, out1, out2)

        where
            alphLen = length $ getAlphabet meta
            getMatrixCost :: (SeqConstraint' s) => AlignMatrix s -> Double
            --getMatrixCost inAlign | trace ("Get cost " ++ show (nrows $ costs inAlign) ++ " " ++ show (ncols $ costs inAlign)) False = undefined
            getMatrixCost inAlign = fst $ getElem (nrows (mat inAlign) - 1) (ncols (mat inAlign) - 1) (mat inAlign)

-- | Joins an alignment row to the rest of a matrix
-- Takes in an alignment row and an alignment matrix
-- Returns an alignment matrix with the new row added
joinMat :: CodedSequence s => AlignRow s -> AlignMatrix s -> AlignMatrix s
joinMat (inRow, inSeq) inMat = AlignMatrix (inRow `joinRow` mat inMat) (inSeq `cons` seqs inMat)
    where
        joinRow vec curMat = fromList 1 (length vec) (toList vec) <-> curMat

-- | Gets the initial row of a naive alignment matrix
-- Takes in an indelCost, the sequence generating the row, the row length, the current position, the previous cost from the algorithm, and the length of the alphabet
-- Returns an alignment row
-- This row will have a diagonal at the leftmost position and will otherwise have only lefts
-- the cost is just added to every time there's a gap
firstAlignRow :: (SeqConstraint' s, CodedChar s) => Double -> s -> Int -> Int -> Double -> Int -> AlignRow s
--firstAlignRow indelCost inSeq rowLength position prevCost | trace ("firstAlignRow " ++ show inSeq) False = undefined
firstAlignRow indCost inSeq rowLength position prevCost alphLen
    | position == (rowLength + 1) = (mempty, mempty)
    | position == 0 = (singleton (0, DiagDir), gapChar alphLen) <> firstAlignRow indCost inSeq rowLength (position + 1) 0 alphLen
    | newState /= gapChar alphLen = --trace ("new state on first row " ++ show newState) $ -- if there's no indel overlap
        (singleton (prevCost + indCost, LeftDir), newState) <> firstAlignRow indCost inSeq rowLength (position + 1) (prevCost + indCost) alphLen
    | otherwise = --trace ("new state on first row, otherwise " ++ show newState) $ -- matching indel so no cost
        (singleton (prevCost, LeftDir), newState) <> firstAlignRow indCost inSeq rowLength (position + 1) prevCost alphLen
        where
            newState = getOverlapState (gapChar alphLen) (grabSubChar inSeq (position - 1) alphLen)

-- | Gets the overlap state: intersect if possible and union if that's empty
-- Takes two sequences and returns another
getOverlapState :: SeqConstraint' s => s -> s -> s
getOverlapState char1 char2 = if isEmpty char1 || isEmpty char2 
                                  then emptySeq 
                                  else char1 `op` char2
              where op = if char1 .&. char2 == zeroBits 
                    then (.|.) 
                    else (.&.)

-- | Main recursive function to get alignment rows
-- Takes two sequence, the indel and sub costs, the current row number, the previous row, and the alphabet length
-- returns an alignment matrix
getAlignRows :: (SeqConstraint' s, CodedChar s) => s -> s -> Costs -> Int -> AlignRow s -> Int -> AlignMatrix s
getAlignRows seq1 seq2 costValues rowNum prevRow alphLen
    | rowNum == numChars seq2 alphLen + 1 = AlignMatrix (matrix 0 0 (const (0, LeftDir))) mempty 
    | otherwise = 
        let thisRow = generateRow seq1 seq2 costValues rowNum prevRow (0, 0) alphLen
        in thisRow `joinMat` getAlignRows seq1 seq2 costValues (rowNum + 1) thisRow alphLen

-- | Generates a single alignment row
-- Takes two sequences, the indel and sub costs, the current row number, the previous row, the position and previous cost, and the alphabet length
-- returns an alignment row
-- Essentially gets values for left, down, and diagonal moves using overlap functionality
-- then selects the minimum value to set the correct value at the given positions
generateRow :: (SeqConstraint' s, CodedChar s) => s -> s -> Costs -> Int -> AlignRow s -> (Int, Double) -> Int -> AlignRow s
--generateRow seq1 seq2 costvals@(indelCost, subCost) rowNum prevRow@(costs, _, _) (position, prevCost)  | trace ("generateRow " ++ show seq1 ++ show seq2) False = undefined
generateRow seq1 seq2 weights@(indCost, sCost) rowNum prevRow@(vals, _) (position, prevCost) alphLen
    | length vals < (position - 1) = error "Problem with row generation, previous costs not generated"
    | position == numChars seq1 alphLen + 1 = (mempty, emptySeq)
    | position == 0 && newState /= gapChar alphLen = (singleton (upValue + indCost, DownDir), newState) <> nextCall (upValue + indCost)
    | position == 0 = (singleton (upValue, DownDir), newState) <> nextCall upValue
    | otherwise = --trace "minimal case" $ 
        (singleton (minCost, minDir), minState) <> nextCall minCost
        where
            newState      = getOverlapState (gapChar alphLen) (grabSubChar seq2 (rowNum - 1) alphLen)
            upValue       = fst $ vals ! position
            nextCall cost = generateRow seq1 seq2 weights rowNum prevRow (position + 1, cost) alphLen
            char1         = grabSubChar seq1 (position - 1) alphLen
            char2         = grabSubChar seq2 (rowNum - 1) alphLen
            iuChar1       = getOverlapState (gapChar alphLen) char1
            iuChar2       = getOverlapState (gapChar alphLen) char2
            leftCost      = overlapCost char1 indCost + prevCost
            downCost      = overlapCost char2 indCost + upValue
            diagVal       = fst $ vals ! (position - 1)
            intersect     = char1 .&. char2
            union         = char1 .|. char2

            (diagCost, diagState) = if intersect == zeroBits 
                                    then (diagVal + sCost, union)
                                    else (diagVal, intersect)
            (minCost, minState, minDir) = minimumBy (comparing (\(a,_,_) -> a))
                                                [(leftCost, iuChar1, LeftDir), (downCost, iuChar2, DownDir), (diagCost, diagState, DiagDir)]

            --overlapCost :: CharConstraint s => s -> Double -> Double
            overlapCost char cost 
                | gapChar alphLen .&. char == zeroBits = cost
                | otherwise = 0 

-- | Performs the traceback of an alignment matrix
-- Takes in an alignment matrix, two sequences, and the alphabet length
-- returns the assignment sequence and the aligned version of the two inputs
-- Essentially follows the arrows from the bottom right corner, accumulating the sequences as it goes
traceback :: (SeqConstraint' s, CodedChar s) => AlignMatrix s -> s -> s -> Int -> (s, s, s)
--traceback alignMat seq1 seq2 | trace ("traceback with matrix " ++ show alignMat) False = undefined
traceback alignMat' seq1' seq2' alphLen = tracebackInternal alignMat' seq1' seq2' (numChars seq1' alphLen, numChars seq2' alphLen)
    where
        -- read it from the matrix instead of grabbing
        tracebackInternal :: (SeqConstraint' s, CodedChar s) => AlignMatrix s -> s -> s -> (Int, Int) -> (s, s, s)
        --tracebackInternal alignMat seq1 seq2 (row, col)  | trace ("traceback " ++ show (traversal alignMat) ++ show (getElem row col (traversal alignMat))++ " with position " ++ show (row, col)) False = undefined
        tracebackInternal alignMat seq1 seq2 (row, col) 
            | length (seqs alignMat) < row - 1 || nrows (mat alignMat) < row - 1 || ncols (mat alignMat) < col - 1 = error "Traceback cannot function because matrix is incomplete"
            | row == 0 && col == 0 = (emptySeq, emptySeq, emptySeq)
            | curDirect == LeftDir = tracebackInternal alignMat seq1 seq2 (row, col - 1) <> (curState, gapChar alphLen, grabSubChar seq2 (col - 1) alphLen)
            | curDirect == DownDir = tracebackInternal alignMat seq1 seq2 (row - 1, col) <> (curState, grabSubChar seq1 (row - 1) alphLen, gapChar alphLen)
            | curDirect == DiagDir = tracebackInternal alignMat seq1 seq2 (row - 1, col - 1) <> (curState, grabSubChar seq1 (row - 1) alphLen, grabSubChar seq2 (col - 1) alphLen)
            | otherwise = error "Incorrect direction in matrix traversal for alignment"
                where
                    curDirect = snd $ getElem row col (mat alignMat)
                    curState  = grabSubChar (seqs alignMat ! row) col alphLen

