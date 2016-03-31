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

import Data.Bits
import Data.Vector (Vector, cons, toList, singleton, (!))
import Data.List (minimumBy)
import Data.Ord
import Data.Matrix (Matrix, getElem, nrows, ncols, (<->), zero, matrix, fromList)
import Data.Maybe
import Data.Monoid

import Bio.Metadata.Class (InternalMetadata(..))
import Bio.Sequence.Coded

data Direction = LeftDir | DiagDir | DownDir deriving (Eq, Show)

type AlignRow s = (Vector Double, s, Vector Direction)
data AlignMatrix s = AlignMatrix {costs :: Matrix Double, seqs :: Vector s, traversal :: Matrix Direction} deriving (Eq, Show)

type Costs = (Double, Double)

defaultCosts :: Costs
defaultCosts = (1,1)

-- | Performs a naive direct optimization
naiveDO :: (CodedSequence s, Bits s, CodedChar s, InternalMetadata m s) => s -> s -> m -> (s, Double, s, s, s)
--naiveDO s1 s2 | trace ("Sequences of length " ++ show (numChars s1) ++ show (numChars s2)) False = undefined
naiveDO seq1 seq2 meta
    | isEmpty seq1 || isEmpty seq2 || numChars seq1 == 0 || numChars seq2 == 0 = (emptySeq, 0, emptySeq, emptySeq, emptySeq)
    | otherwise = 
        let
            seq1Len = numChars seq1
            seq2Len = numChars seq2
            (shorter, _, longer, longlen) = if seq1Len > seq2Len
                                                   then (seq2, seq2Len, seq1, seq1Len)
                                                   else (seq1, seq1Len, seq2, seq2Len)
            firstMatRow = firstAlignRow (fst defaultCosts) longer longlen 0 0 alphLen
            traversalMat = firstMatRow `joinMat` getAlignRows longer shorter defaultCosts 1 firstMatRow (length $ alphabet meta)
            cost = getMatrixCost traversalMat
            (gapped, left, right) = --trace ("get seqs " ++ show traversalMat)
                                    traceback traversalMat shorter longer alphLen
            ungapped = filterGaps gapped (gapChar $ length $ alphabet meta) (alphabet meta)
            (out1, out2) = if seq1Len > seq2Len
                                then (right, left)
                                else (left, right)
        in (ungapped, cost, gapped, out1, out2)

        where
            alphLen = (length $ alphabet meta)
            getMatrixCost :: (Bits s, Monoid s) => AlignMatrix s -> Double
            --getMatrixCost inAlign | trace ("Get cost " ++ show (nrows $ costs inAlign) ++ " " ++ show (ncols $ costs inAlign)) False = undefined
            getMatrixCost inAlign = 
                let mat = costs inAlign
                in getElem (nrows mat - 1) (ncols mat - 1) mat

-- | Joins an alignment row to the rest of a matrix
joinMat :: CodedSequence s => AlignRow s -> AlignMatrix s -> AlignMatrix s
joinMat (inCosts, inSeq, directions) inMat = AlignMatrix (inCosts `joinRow` costs inMat) (inSeq `cons` seqs inMat) (directions `joinRow` traversal inMat) 
    where
        joinRow vec mat = fromList 1 (length vec) (toList vec) <-> mat

-- | Gets the initial row of a naive alignment matrix
firstAlignRow :: (CodedChar s, CodedSequence s) => Double -> s -> Int -> Int -> Double -> Int -> AlignRow s
--firstAlignRow indelCost inSeq rowLength position prevCost | trace ("firstAlignRow " ++ show inSeq) False = undefined
firstAlignRow indelCost inSeq rowLength position prevCost alphLen
    | position == (rowLength + 1) = (mempty, mempty, mempty)
    | position == 0 = (singleton 0, charToSeq $ gapChar alphLen, singleton DiagDir) <> firstAlignRow indelCost inSeq rowLength (position + 1) 0 alphLen
    | newState /= gapChar alphLen = --trace ("new state on first row " ++ show newState) $ -- if there's no indel overlap
        (singleton $ prevCost + indelCost, charToSeq newState, singleton LeftDir) <> firstAlignRow indelCost inSeq rowLength (position + 1) (prevCost + indelCost) alphLen
    | otherwise = --trace ("new state on first row, otherwise " ++ show newState) $ -- matching indel so no cost
        (singleton prevCost, charToSeq newState, singleton LeftDir) <> firstAlignRow indelCost inSeq rowLength (position + 1) prevCost alphLen
        where
            newState = getOverlapState (gapChar alphLen) (grabSubChar inSeq (position - 1) alphLen)

-- | Gets the overlap state: intersect if possible and union if that's empty
getOverlapState :: CharConstraint b => b -> Maybe b -> b
getOverlapState        _  Nothing = zeroBits
getOverlapState compChar (Just x) = compChar `op` x
    where op = if compChar .&. x == zeroBits then (.|.) else (.&.)

-- | Main recursive function to get alignment rows
getAlignRows :: (CodedChar s, CodedSequence s) => s -> s -> Costs -> Int -> AlignRow s -> Int -> AlignMatrix s
getAlignRows seq1 seq2 costValues rowNum prevRow alphLen
    | rowNum == numChars seq2 + 1 = AlignMatrix (zero 0 0) mempty (matrix 0 0 (const LeftDir))
    | otherwise = 
        let thisRow = generateRow seq1 seq2 costValues rowNum prevRow (0, 0) alphLen
        in thisRow `joinMat` getAlignRows seq1 seq2 costValues (rowNum + 1) thisRow alphLen

-- | Generates a single alignment row
generateRow :: (CodedChar s, CodedSequence s) => s -> s -> Costs -> Int -> AlignRow s -> (Int, Double) -> Int -> AlignRow s
--generateRow seq1 seq2 costvals@(indelCost, subCost) rowNum prevRow@(costs, _, _) (position, prevCost)  | trace ("generateRow " ++ show seq1 ++ show seq2) False = undefined
generateRow seq1 seq2 costvals@(indelCost, subCost) rowNum prevRow@(costValues, _, _) (position, prevCost) alphLen
    | length costValues < (position - 1) = error "Problem with row generation, previous costs not generated"
    | position == (numChars seq1 + 1) = (mempty, emptySeq, mempty)
    | position == 0 && newState /= (gapChar alphLen) = (singleton $ upValue + indelCost, charToSeq newState, singleton DownDir) <> nextCall (upValue + indelCost)
    | position == 0 = (singleton upValue, charToSeq newState, singleton DownDir) <> nextCall upValue
    | otherwise = --trace "minimal case" $ 
        (singleton minCost, charToSeq minState, singleton minDir) <> nextCall minCost
        where
            newState      = getOverlapState (gapChar alphLen) (grabSubChar seq2 (rowNum - 1) alphLen)
            upValue       = costValues ! position
            nextCall cost = generateRow seq1 seq2 costvals rowNum prevRow (position + 1, cost) alphLen
            char1         = unwrapSub $ grabSubChar seq1 (position - 1) alphLen
            char2         = unwrapSub $ grabSubChar seq2 (rowNum - 1) alphLen
            iuChar1       = getOverlapState (gapChar alphLen) (Just char1)
            iuChar2       = getOverlapState (gapChar alphLen) (Just char2)
            leftCost      = overlapCost char1 indelCost + prevCost
            downCost      = overlapCost char2 indelCost + upValue
            diagVal       = costValues ! (position - 1)
            intersect     = char1 .&. char2
            union         = char1 .|. char2

            (diagCost, diagState) = if intersect == zeroBits 
                                    then (diagVal + subCost, union)
                                    else (diagVal, intersect)
            (minCost, minState, minDir) = --trace ("get minimum choice " ++ show [(leftCost, char1, LeftDir), (diagCost, diagState, DiagDir), (downCost, char2, DownDir)])
                                           minimumBy (comparing (\(a,_,_) -> a))
                                                [(leftCost, iuChar1, LeftDir), (downCost, iuChar2, DownDir), (diagCost, diagState, DiagDir)]

            overlapCost :: CharConstraint b => b -> Double -> Double
            overlapCost char cost 
                | gapChar .&. char == zeroBits = cost
                | otherwise                    = 0 

            unwrapSub :: CharConstraint b => Maybe b -> b
            unwrapSub = fromMaybe (error "Cannot access sequence at given position for matrix generation")

-- | Performs the traceback of an alignment matrix
traceback :: CodedSequence s => AlignMatrix s -> s -> s -> Int -> (s, s, s)
--traceback alignMat seq1 seq2 | trace ("traceback with matrix " ++ show alignMat) False = undefined
traceback alignMat' seq1' seq2' alphLen = tracebackInternal alignMat' seq1' seq2' (numChars seq1', numChars seq2')
    where
        -- read it from the matrix instead of grabbing
        tracebackInternal :: (Bits s, Monoid s) => AlignMatrix s -> s -> s -> (Int, Int) -> (s, s, s)
        --tracebackInternal alignMat seq1 seq2 (row, col)  | trace ("traceback " ++ show (traversal alignMat) ++ show (getElem row col (traversal alignMat))++ " with position " ++ show (row, col)) False = undefined
        tracebackInternal alignMat seq1 seq2 (row, col) 
            | length (seqs alignMat) < row - 1 || nrows (traversal alignMat) < row - 1 || ncols (traversal alignMat) < col - 1 = error "Traceback cannot function because matrix is incomplete"
            | row == 0 && col == 0 = (emptySeq, emptySeq, emptySeq)
            | curDirect == LeftDir = tracebackInternal alignMat seq1 seq2 (row, col - 1) <> (curState, charToSeq gapChar, charToUnMaybe $ grabSubChar seq2 (col - 1) alphLen)
            | curDirect == DownDir = tracebackInternal alignMat seq1 seq2 (row - 1, col) <> (curState, charToUnMaybe $ grabSubChar seq1 (row - 1) alphLen, charToSeq $ gapChar alphLen)
            | curDirect == DiagDir = tracebackInternal alignMat seq1 seq2 (row - 1, col - 1) <> (curState, charToUnMaybe $ grabSubChar seq1 (row - 1) alphLen, charToUnMaybe $ grabSubChar seq2 (col - 1) alphLen)
            | otherwise = error "Incorrect direction in matrix traversal for alignment"
                where
                    curDirect = getElem row col (traversal alignMat)
                    curState = charToUnMaybe $ seqs alignMat ! grabSubChar row col alphLen

                    charToUnMaybe :: SeqConstraint s b => Maybe b -> s
                    charToUnMaybe inBit = case inBit of
                                            Nothing -> emptySeq
                                            Just b  -> charToSeq b
