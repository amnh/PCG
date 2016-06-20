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
-- import Data.MonoTraversable
import Data.Ord
import Data.Vector           (Vector)

-- | The direction to align the character at a given matrix point.
data Direction = LeftArrow | DiagArrow | UpArrow deriving (Eq, Show)

-- | A row of the 'AlignMatrix'
-- constructed as a tuple of vectors for easy joining to the full matrix
type AlignRow s = Vector (Double, Direction, BitVector)

-- | A representation of an alignment matrix
-- The matrix itself stores tuples of the cost and direction at that position.
-- We also store a vector of characters that are generated.
type AlignMatrix s = Matrix (Double, Direction, BitVector)

type NWSeqConstraint s = (EncodableDynamicCharacter s, Show s, Memoizable s)

-- | Wrapper function to do an enhanced Needleman-Wunsch algorithm.
-- Calls 'createAlignMtx' to generate an 'AlignMatrix', then 'traceback' to get 
-- the aligned characters
-- Takes in two 'EncodableDynamicCharacter's and a 'CostStructure'.
-- Returns two _aligned_ 'EncodableDynamicCharacter's.
needlemanWunsch :: (NWSeqConstraint s, Metadata m s) => s -> s -> m -> (s, s)
needlemanWunsch char1 char2 meta = (seq1Align, seq2Align)
    where
        char1Len               = numChars char1
        char2Len               = numChars char2
        (shorterChar, longerChar, _) = if   char1Len > char2Len
                                              then (char2, char1, char1Len)
                                              else (char1, char2, char2Len)
        traversalMat           = createAlignMtx longerChar shorterChar (getCosts meta)
        (_, alignL, alignR)    = traceback traversalMat shorterChar longerChar
        (seq1Align, seq2Align) = if   char1Len > char2Len
                                 then (alignR, alignL)
                                 else (alignL, alignR)

-- | Main function to generate an 'AlignMatrix'. Works as in Needleman-Wunsch,
-- but allows for multiple indel/replacement costs, depending on the 'CostStructure'.
-- Takes in two 'EncodableDynamicCharacter's and a 'CostStructure'. The first character
-- must be the longer of the two and is the top labeling of the matrix.
-- Returns an 'AlignMatrix'.
-- TODO: See if we can move topDynChar logic inside here. It's also necessary in DO. 
-- Or maybe DO can just call needlemanWunsch?
createAlignMtx :: (EncodableDynamicCharacter s) => s -> s -> CostStructure -> AlignMatrix s
createAlignMtx topDynChar leftDynChar costStruct = result
    where
        result = matrix (numChars leftDynChar + 1) (numChars topDynChar + 1) generateMat

        -- TODO: attempt to make tail recursive? Maybe not possible, given multiple tuple values.
        -- | Internal generator function for the matrix
        -- Deals with both first row and other cases, a merge of two previous algorithms
        generateMat :: (Int, Int) -> (Double, Direction, BitVector)
        generateMat (row, col)
          | row == 0 && col == 0         = (0                               , DiagArrow, gap      )
          | row == 0 && rightChar /= gap = (leftwardValue + rightOverlapCost, LeftArrow, staticCharFromLeft)
          | row == 0                     = (leftwardValue                   , LeftArrow, staticCharFromLeft)
          | col == 0 && downChar /= gap  = (upwardValue + downOverlapCost   , UpArrow  , staticCharFromTop )
          | col == 0                     = (upwardValue                     , UpArrow  , staticCharFromTop )
          | otherwise                    = (minCost                         , minDir   , minState )
          where
            gap                           = gapChar topDynChar
            staticCharFromLeft            = grabSubChar topDynChar (col - 1)
            staticCharFromTop             = grabSubChar leftDynChar (row - 1)
            (leftwardValue, _, _)         = result ! (row    , col - 1)
            (diagonalValue, _, _)         = result ! (row - 1, col - 1)
            (upwardValue  , _, _)         = result ! (row - 1, col)
            (rightChar, rightOverlapCost) = overlap costStruct staticCharFromLeft gap               
            (diagChar , diagOverlapCost)  = overlap costStruct staticCharFromLeft staticCharFromTop 
            (downChar , downOverlapCost)  = overlap costStruct gap                staticCharFromTop 
            rightCost                     = rightOverlapCost + leftwardValue
            diagCost                      = diagOverlapCost  + diagonalValue
            downCost                      = downOverlapCost  + upwardValue
            (minCost, minState, minDir)   = minimumBy (comparing (\(a,_,_) -> a))
                                          -- This order is important!
                                          -- In the event of equal cost we want to
                                          -- prioritize elements earlier in the list.
                                          -- TODO: POY prioritizes gaps to shorter char, make sure it prioritizes
                                          -- right on equal-length chars
                                          [ (rightCost, rightChar, LeftArrow)
                                          , (downCost , downChar , UpArrow)
                                          , (diagCost , diagChar , DiagArrow)
                                          ]

-- | Performs the traceback of an 'AlignMatrix'.
-- Takes in an 'AlignMatrix', two 'EncodableDynamicCharacter's, and the 'Alphabet' length.
-- Returns an aligned 'EncodableDynamicCharacter', as well as the aligned versions of the two inputs.
-- Essentially does the second step of Needleman-Wunsch, following the arrows from the bottom right corner, 
-- accumulating the sequences as it goes, but only returns a single alignment. This alignment *should* be biased
-- toward the shorter of the two sequences.
traceback :: (NWSeqConstraint s) => AlignMatrix s -> s -> s -> (s, s, s)
traceback alignMat' char1' char2' = (fromChars $ reverse t1, fromChars $ reverse t2, fromChars $ reverse t3)
    where
        (t1, t2, t3) = tracebackInternal alignMat' char1' char2' (nrows alignMat' - 1, ncols alignMat' - 1)
        -- read it from the matrix instead of grabbing
        tracebackInternal :: (NWSeqConstraint s) => AlignMatrix s -> s -> s -> (Int, Int) -> ([BitVector], [BitVector], [BitVector])
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
                  LeftArrow -> (row    , col - 1)
                  UpArrow -> (row - 1, col    ) -- Isn't this "up"
                  DiagArrow -> (row - 1, col - 1)

-- | Simple function to get the cost from an alignment matrix
getTotalAlignmentCost :: AlignMatrix s -> Double
getTotalAlignmentCost inAlign = c
    where (c, _, _) = getElem (nrows inAlign - 1) (ncols inAlign - 1) inAlign

-- | Memoized wrapper of the overlap function
getOverlap :: (EncodableStaticCharacter s, Memoizable s) => s -> s -> CostStructure -> (s, Double)
getOverlap inChar1 inChar2 costStruct = result
    where
        result = memoize2 (overlap costStruct) inChar1 inChar2
        
-- | Takes two 'EncodableStaticCharacter' and a 'CostStructure' and returns a tuple of a new character, 
-- along with the cost of obtaining that character. The return character may be (or is even likely to be)
-- ambiguous. Will attempt to intersect the two characters, but will union them if that is not possible,
-- based on the 'CostStructure'. 
--
-- To clarify, the return character is an intersection of all possible least-cost combinations, so for instance,
-- if @ char1 == A,T @ and @ char2 == G,C @, and the two (non-overlapping) least cost pairs are A,C and T,G, then
-- the return value is A,C,G,T. 
-- Tests exist in the test suite.
overlap :: EncodableStaticCharacter s => CostStructure -> s -> s -> (s, Double)
overlap costStruct char1 char2
    | intersectionStates == zeroBits = foldr1 ambigChoice $ allPossibleBaseCombosCosts costStruct char1 char2
    | otherwise                      = (intersectionStates, 0)
    -- | 0 == char1 || 0 == char2 = (zeroBitVec, 0) -- Commented out, because nonsense. Problem for testing?
    -- | char1 .&. char2 == 0 = foldr1 ambigChoice allPossibleBaseCombosCosts
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

-- TODO: Can we eliminate all characters from below, and just pass around Ints?
-- | Finds the cost of a pairing of two static characters.
-- Takes in a 'CostStructure' and two ambiguous 'EncodableStaticCharacter's. Returns a list of tuples of all possible unambiguous
-- pairings, along with their costs. 
allPossibleBaseCombosCosts :: EncodableStaticCharacter s => CostStructure -> s -> s -> [(s, Double)]
allPossibleBaseCombosCosts costStruct char1 char2 = [ getCost costStruct x y | x <- getSubChars char1
                                                                             , y <- getSubChars char2
                                                    ]

-- | Given a 'CostStructure' and two tuples of an 'Int' and an unambiguous 'EncodableStaticCharacter', determines the cost 
-- of a pairing (intersection) of those characters into an ambiguous character. The 'Int's are the set bits in each character
-- and are used as lookup into the 'CostStructure'. 
-- Tests exist in the test suite.
-- TODO: This won't work with an asymmetric TCM.
getCost :: EncodableStaticCharacter s => CostStructure -> (Int, s) -> (Int, s) -> (s, Double)
getCost costStruct seqTup1 seqTup2 = 
    case (costStruct, seqTup1, seqTup2) of
       -- (AffineCost {}        , _         , _         ) -> error "Cannot apply DO algorithm on affine cost" -- When this is added, remember to write a test.
        (TCM mtx              , (pos1, c1), (pos2, c2)) -> (c1 .|. c2, getElem pos1 pos2 mtx)
        (GeneralCost indel sub, (_   , c1), (_   , c2)) -> if c1 == gap || c2 == gap 
                                                           then (c1 .|. c2, indel) 
                                                           else (c1 .|. c2, sub)
    where
      s   = snd seqTup1
      z   = s `xor` s
      gap = z `setBit` (stateCount s - 1)

-- | Takes in a 'EncodableStaticCharacter', possibly with more than one bit set, and returns a list of tuples of 
-- 'Int's and 'EncodableStaticCharacter's, such that, for each set bit in the input, there is one element in the output list, 
-- a tuple with an 'Int', @ x @, giving the location of the set bit, as well as an 'EncodableStaticCharacter' of the same
-- length as the input, but with only the bit at location @ x @ set.
-- Tests exist in the test suite.
getSubChars :: (EncodableStaticCharacter s) => s -> [(Int, s)]
getSubChars fullChar = foldr (\i acc -> if testBit fullChar i 
                                    then (i, z `setBit` i) : acc 
                                    else acc 
                         ) mempty [0..(stateCount fullChar)]
  where
    z = fullChar `xor` fullChar
