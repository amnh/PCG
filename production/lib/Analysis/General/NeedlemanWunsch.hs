-----------------------------------------------------------------------------
-- |
-- Module      :  Analysis.General.NeedlemanWunsch
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
{-# LANGUAGE ConstraintKinds #-}

module Analysis.General.NeedlemanWunsch where

import Bio.Metadata
import Bio.Character.Dynamic.Coded
import Data.Bits
import Data.BitVector hiding (foldr)
import Data.Foldable (minimumBy)
import Data.Function.Memoize
import Data.Matrix.NotStupid   (Matrix, getElem, nrows, ncols, (<->), matrix, fromList)
import Data.Ord
import Data.Vector   (Vector, cons, toList, (!))

import Debug.Trace

-- | The direction to align the character at a given matrix point.
data Direction = LeftDir | DiagDir | DownDir deriving (Eq, Show)

-- | A row of the 'AlignMatrix'
-- constructed as a tuple of vectors for easy joining to the full matrix
type AlignRow s = Vector (Double, Direction, BitVector)

-- | A representation of an alignment matrix
-- The matrix itself stores tuples of the cost and direction at that position.
-- We also store a vector of characters that are generated.
type AlignMatrix s = Matrix (Double, Direction, BitVector)

type SeqConstraint s = (EncodableDynamicCharacter s, Bits s, Show s, Memoizable s)

-- | Wrapper function to do a needleman wunsch algorithm
-- Takes in two sequences and a metadata
-- returns two aligned sequences
needlemanWunsch :: (SeqConstraint s, Metadata m s) => s -> s -> m -> (s, s)
needlemanWunsch char1 char2 meta = (seq1Align, seq2Align)
    where
        char1Len = numChars char1
        char2Len = numChars char2
        (shorterChar, longerChar, longLen) = if char1Len > char2Len
                                     then (char2, char1, char1Len)
                                     else (char1, char2, char2Len)
        traversalMat = getAlignMat longerChar shorterChar meta
        (_, seq1Align, seq2Align) = traceback traversalMat shorterChar longerChar

-- | Main function to generate an alignment matrix
-- Takes in two sequences (the longer first) and the metadata
-- Returns an alignment matrix
getAlignMat :: (SeqConstraint s, Metadata m s) => s -> s -> m -> AlignMatrix s
--getAlignMat char1 char2 meta | trace ("get alignment matrix of characters " ++ show char1 ++", " ++ show char2) False = undefined
getAlignMat char1 char2 meta = result
    where
        result = matrix (numChars char2 + 1) (numChars char1 + 1) generateMat
        -- | Internal generator function for the matrix
        -- Deals with both first row and other cases, a merge of two previous algorithms
        generateMat :: (Int, Int) -> (Double, Direction, BitVector)
        --generateMat (row, col) | trace ("generateMat at " ++ show (row,col)) False = undefined
        generateMat (row, col)
            | row == 0 && col == 0                  = (0, DiagDir, gapChar char2)
            | row == 0 && leftChar /= gapChar char1   = (prevCost + indCost, LeftDir, leftChar)
            | row == 0                              = (prevCost, LeftDir, leftChar)
            | col == 0 && downChar /= gapChar char1 = (upValue + indCost, DownDir, downChar)
            | col == 0                              = (upValue, DownDir, downChar)
            | otherwise                             = (minCost, minDir, minState)
                where
                    indCost             = getGapCost meta
                    subChar1            = grabSubChar char1 (col - 1)
                    subChar2            = grabSubChar char2 (row - 1)
                    (upValue, _, _)     = getElem (row - 1) col result
                    (diagVal, _, _)     = getElem (row - 1) (col - 1) result
                    (prevCost, _, _)    = getElem row (col - 1) result
                    (downChar, dCost)   = getOverlap (gapChar char2) subChar2 meta
                    downCost            = dCost + upValue
                    (leftChar, lCost)   = getOverlap (gapChar char1) subChar1 meta
                    leftCost            = lCost + prevCost
                    (diagChar, dgCost)  = getOverlap subChar1 subChar2 meta
                    diagCost            = diagVal + dgCost
                    (minCost, minState, minDir) = minimumBy (comparing (\(a,_,_) -> a))
                                                        [(diagCost, diagChar, DiagDir), (leftCost, leftChar, LeftDir), (downCost, downChar, DownDir)]

-- | Performs the traceback of an alignment matrix
-- Takes in an alignment matrix, two sequences, and the alphabet length
-- returns the assignment sequence and the aligned version of the two inputs
-- Essentially follows the arrows from the bottom right corner, accumulating the sequences as it goes
traceback :: (SeqConstraint s) => AlignMatrix s -> s -> s -> (s, s, s)
--traceback alignMat char1 char2 | trace ("traceback with matrix " ++ show alignMat) False = undefined
traceback alignMat' char1' char2' = (fromChars t1, fromChars t2, fromChars t3)
    where
        (t1, t2, t3) = tracebackInternal alignMat' char1' char2' (nrows alignMat' - 1, ncols alignMat' - 1)
        -- read it from the matrix instead of grabbing
        tracebackInternal :: (SeqConstraint s) => AlignMatrix s -> s -> s -> (Int, Int) -> ([BitVector], [BitVector], [BitVector])
        --tracebackInternal alignMat char1 char2 (row, col)  | trace ("traceback with position " ++ show (row, col) ++ " on mat " ++ show alignMat) False = undefined
        tracebackInternal alignMat char1 char2 (row, col)
            | nrows alignMat < row - 1 || ncols alignMat < col - 1 = error "Traceback cannot function because matrix is incomplete"
            | row == 0 && col == 0 = (mempty, mempty, mempty)
            | otherwise = 
                let t@(trace1, trace2, trace3) = tracebackInternal alignMat char1 char2 (i, j)
                in (curState : trace1, leftCharacter : trace2, rightCharacter : trace3)
            where
              (_, curDirect, curState) = getElem row col alignMat
              leftCharacter  = if row == i then gapChar char2 else grabSubChar char1 i
              rightCharacter = if col == j then gapChar char1 else grabSubChar char2 j
              (i, j) =
                case curDirect of
                  LeftDir -> (row    , col - 1)
                  DownDir -> (row - 1, col    )
                  DiagDir -> (row - 1, col - 1)

-- | Simple function to get the cost from an alignment matrix
getMatrixCost :: AlignMatrix s -> Double
--getMatrixCost inAlign | trace ("Get cost " ++ show (nrows inAlign) ++ " " ++ show (ncols inAlign)) False = undefined
getMatrixCost inAlign = c
    where (c, _, _) = getElem (nrows inAlign - 1) (ncols inAlign - 1) inAlign


-- TODO: used concrete BitVector type instead of something more appropriate, like EncodableDynamicCharacter. 
-- This also means that there are a bunch of places below that could be using EDC class methods that are no longer.
-- The same will be true in IA.
-- | Memoized wrapper of the overlap function
getOverlap :: (Metadata m s) => BitVector -> BitVector -> m -> (BitVector, Double)
--getOverlap inChar1 inChar2 meta | trace ("getOverlap on " ++ show inChar1 ++", " ++ show inChar2) False = undefined
getOverlap inChar1 inChar2 meta = result
    where
        result = memoize2 (overlap meta) inChar1 inChar2
        -- | Gets the overlap state: intersect if possible and union if that's empty
        -- Takes two sequences and returns another
        overlap :: (Metadata m s) => m -> BitVector -> BitVector -> (BitVector, Double)
        --overlap _ c1 c2 | trace ("overlap on " ++ show c1 ++ " and " ++ show c2) False = undefined
        overlap inMeta char1 char2
            | 0 == char1 || 0 == char2 = --trace ("overlap case 1: equals 0 ") $
                                            (zeroBitVec, 0)
            | char1 .&. char2 == 0 = --trace ("overlap case 2: and empty " ++ show allPossible) $
                                        foldr1 ambigChoice allPossible
            | otherwise = --trace ("overlap case 3: and nonempty") $
                            (char1 .&. char2, 0)
            where
                alphLen    = width char1
                zeroBitVec = --trace ("alphlen " ++ show alphLen) $
                                bitVec (width char1) (0 :: Integer)
                gap = setBit zeroBitVec (alphLen - 1)
                -- Given characters without ambiguity, determine the cost
                -- getCost :: SeqConstraint' s => CostStructure -> (Int, s) -> (Int, s) -> (s, Double)
                --getCost c (p1, _) (p2, _) | trace ("getCost on " ++ show c ++ " at pos " ++ show (p1, p2)) False = undefined
                getCost (TCM mtx) (pos1, c1) (pos2, c2) = (c1 .|. c2, getElem pos1 pos2 mtx)
                getCost (GeneralCost indel sub) (_, c1) (_, c2) = if c1 == gap || c2 == gap then (c1 .|. c2, indel) else (c1 .|. c2, sub)
                getCost (AffineCost _ _ _) _ _ = error "Cannot apply DO algorithm on affine cost"

                -- get single character subsets from both
                getSubs fullChar = foldr (\i acc -> if testBit fullChar i then (i, setBit (zeros $ width fullChar) i) : acc else acc) mempty [0..(width fullChar)]
                -- make possible combinations with a double fold
                matchSubs subList oneSub = foldr (\c acc -> getCost (getCosts inMeta) c oneSub : acc) mempty subList
                matchBoth list1 list2 = foldr (\e acc -> matchSubs list1 e ++ acc) mempty list2
                allPossible = matchBoth (getSubs char1) (getSubs char2)
                -- now take an ambiguous minimum
                --ambigChoice (val1, cost1) (val2, cost2) | trace ("ambigChoice on " ++ show val1 ++ show val2) False = undefined
                ambigChoice (val1, cost1) (val2, cost2)
                    | cost1 == cost2 = (val1 .|. val2, cost1)
                    | cost1 < cost2 = (val1, cost1)
                    | otherwise = (val2, cost2)