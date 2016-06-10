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
import Data.BitVector hiding (foldr, reverse)
import Data.Foldable         (minimumBy)
import Data.Function.Memoize
import Data.Key              ((!))
import Data.Matrix.NotStupid (Matrix, getElem, nrows, ncols, matrix)
import Data.MonoTraversable
import Data.Ord
import Data.Vector           (Vector)

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
        (shorterChar, longerChar, _longLen) = if char1Len > char2Len
                                     then (char2, char1, char1Len)
                                     else (char1, char2, char2Len)
        traversalMat = getAlignMat longerChar shorterChar (costs meta)
        (_, alignL, alignR) = traceback traversalMat shorterChar longerChar
        (seq1Align, seq2Align) = if char1Len > char2Len
                                then (alignR, alignL)
                                else (alignL, alignR)

-- | Main function to generate an alignment matrix
-- Takes in two sequences (the longer first) and the metadata
-- Returns an alignment matrix
getAlignMat :: (SeqConstraint s) => s -> s -> CostStructure -> AlignMatrix s
getAlignMat char1 char2 costStruct = result
  where
    result = matrix (numChars char2 + 1) (numChars char1 + 1) generateMat
    -- | Internal generator function for the matrix
    -- Deals with both first row and other cases, a merge of two previous algorithms
    generateMat :: (Int, Int) -> (Double, Direction, BitVector)
    generateMat (row, col)
      | row == 0 && col == 0        = (0                      , DiagDir, gap     )
      | row == 0 && riteChar /= gap = (leftwardValue + indCost, LeftDir, riteChar)
      | row == 0                    = (leftwardValue          , LeftDir, riteChar)
      | col == 0 && downChar /= gap = (  upwardValue + indCost, DownDir, downChar)
      | col == 0                    = (  upwardValue          , DownDir, downChar)
      | otherwise                   = (minCost                , minDir , minState)
      where
        indCost                     = getGapCost meta
        gap                         = gapChar char1
        subChar1                    = grabSubChar char1 (col - 1)
        subChar2                    = grabSubChar char2 (row - 1)
        (leftwardValue, _, _)       = result ! (row    , col - 1)
        (diagonalValue, _, _)       = result ! (row - 1, col - 1)
        (  upwardValue, _, _)       = result ! (row - 1, col    )
        (riteChar, riteOverlapCost) = getOverlap subChar1 gap      costStruct
        (diagChar, diagOverlapCost) = getOverlap subChar1 subChar2 costStruct
        (downChar, downOverlapCost) = getOverlap gap      subChar2 costStruct
        riteCost                    = riteOverlapCost + leftwardValue
        diagCost                    = diagOverlapCost + diagonalValue
        downCost                    = downOverlapCost +   upwardValue
        (minCost, minState, minDir) = minimumBy (comparing (\(a,_,_) -> a))
                                      -- This order is important!
                                      -- In the event of equal cost we want to
                                      -- prioritize elements earlier in the list.
{-
                                        [ (diagCost, diagChar, DiagDir)
                                        , (downCost, downChar, DownDir)
                                        , (leftCost, leftChar, LeftDir)
                                        ]
-}
                                        [ (riteCost, riteChar, LeftDir)
                                        , (downCost, downChar, DownDir)
                                        , (diagCost, diagChar, DiagDir)
                                        ]

-- | Performs the traceback of an alignment matrix
-- Takes in an alignment matrix, two sequences, and the alphabet length
-- returns the assignment sequence and the aligned version of the two inputs
-- Essentially follows the arrows from the bottom right corner, accumulating the sequences as it goes
traceback :: (SeqConstraint s) => AlignMatrix s -> s -> s -> (s, s, s)
traceback alignMat' char1' char2' = (fromChars $ reverse t1, fromChars $ reverse t2, fromChars $ reverse t3)
    where
        (t1, t2, t3) = tracebackInternal alignMat' char1' char2' (nrows alignMat' - 1, ncols alignMat' - 1)
        -- read it from the matrix instead of grabbing
        tracebackInternal :: (SeqConstraint s) => AlignMatrix s -> s -> s -> (Int, Int) -> ([BitVector], [BitVector], [BitVector])
        tracebackInternal alignMat char1 char2 (row, col)
            | nrows alignMat < row - 1 || ncols alignMat < col - 1 = error "Traceback cannot function because matrix is incomplete"
            | row == 0 && col == 0 = (mempty, mempty, mempty)
            | otherwise = 
                let (trace1, trace2, trace3) = tracebackInternal alignMat char1 char2 (i, j)
                in (curState : trace1, leftCharacter : trace2, rightCharacter : trace3)
            where
              (_, curDirect, curState) = getElem row col alignMat
              leftCharacter  = if row == i then gapChar char2 else grabSubChar char1 i
              rightCharacter = if col == j then gapChar char1 else grabSubChar char2 j
              (i, j) =
                case curDirect of
                  LeftDir -> (row    , col - 1)
                  DownDir -> (row - 1, col    ) -- Isn't this "up"
                  DiagDir -> (row - 1, col - 1)

-- | Simple function to get the cost from an alignment matrix
getMatrixCost :: AlignMatrix s -> Double
getMatrixCost inAlign = c
    where (c, _, _) = getElem (nrows inAlign - 1) (ncols inAlign - 1) inAlign


-- TODO: used concrete BitVector type instead of something more appropriate, like EncodableDynamicCharacter. 
-- This also means that there are a bunch of places below that could be using EDC class methods that are no longer.
-- The same will be true in IA.
-- | Memoized wrapper of the overlap function
getOverlap :: (EncodableStaticCharacter s, Memoizable s) => s -> s -> CostStructure -> (s, Double)
getOverlap inChar1 inChar2 costStruct = result
    where
        result = memoize2 (overlap costStruct) inChar1 inChar2
        
-- | Gets the overlap state: intersect if possible and union if that's empty
-- Takes two sequences and returns another
overlap :: EncodableStaticCharacter s => CostStructure -> s -> s -> (s, Double)
overlap costStruct char1 char2
    | intersectionStates == zeroBits = foldr1 ambigChoice $ allPossible costStruct char1 char2
    | otherwise                      = (intersectionStates, 0)
    -- | 0 == char1 || 0 == char2 = (zeroBitVec, 0) -- Commented out, because nonsense. Problem for testing?
    -- | char1 .&. char2 == 0 = foldr1 ambigChoice allPossible
    -- | otherwise            = (char1 .&. char2, 0)
    where
      intersectionStates = char1 .&. char2
--        alphLen    = width char1
--        z = char1 `xor` char1
        -- make possible combinations with a double fold
        
        -- now take an ambiguous minimum
      ambigChoice (val1, cost1) (val2, cost2)
        | cost1 == cost2 = (val1 .|. val2, cost1)
        | cost1 < cost2  = (val1         , cost1)
        | otherwise      = (val2         , cost2)

-- | What does this actually do?
allPossible :: EncodableStaticCharacter s => CostStructure -> s -> s -> [(s, Double)]
allPossible costStruct char1 char2 = [ getCost costStruct x y | x <- getSubs char1
                                                              , y <- getSubs char2
                                     ]

-- Given characters without ambiguity, determine the cost
getCost :: EncodableStaticCharacter s => CostStructure -> (Int, s) -> (Int, s) -> (s, Double)
getCost costs seqTup1 seqTup2 = 
    case (costs, seqTup1, seqTup2) of
        (AffineCost {}        , _         , _         ) -> error "Cannot apply DO algorithm on affine cost" 
        (TCM mtx              , (pos1, c1), (pos2, c2)) -> (c1 .|. c2, getElem pos1 pos2 mtx)
        (GeneralCost indel sub, (_   , c1), (_   , c2)) -> if c1 == gap || c2 == gap 
                                                           then (c1 .|. c2, indel) 
                                                           else (c1 .|. c2, sub)
    where
      s   = snd seqTup1
      z   = s `xor` s
      gap = z `setBit` (stateCount s - 1)

-- get single-character subsets from both somethings?
getSubs :: (EncodableStaticCharacter s) => s -> [(Int, s)]
getSubs fullChar = foldr (\i acc -> if testBit fullChar i 
                                    then (i, z `setBit` i) : acc 
                                    else acc 
                         ) mempty [0..(stateCount fullChar)]
  where
    z = fullChar `xor` fullChar
