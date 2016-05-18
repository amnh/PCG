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
import Bio.Character.Dynamic.Coded
import Data.Bits
import Data.BitVector hiding (foldr)
import Data.Vector   (Vector, cons, toList, (!))
import Data.Foldable (minimumBy)
import Data.Function.Memoize
import Data.Ord
import Data.Matrix.NotStupid   (Matrix, getElem, nrows, ncols, (<->), matrix, fromList)
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

-- | Performs a naive direct optimization
-- Takes in two characters to run DO on and a metadata object
-- Returns an assignment character, the cost of that assignment, the assignment character with gaps included,
-- the aligned version of the first input character, and the aligned version of the second input character
-- The process for this algorithm is to generate a traversal matrix, then perform a traceback.
naiveDO :: (Metadata m s, SeqConstraint' s) => s -> s -> m -> (s, Double, s, s, s)
--naiveDO s1 s2 _ | trace ("Sequences of length " ++ show (numChars s1) ++ show (numChars s2)) False = undefined
naiveDO char1 char2 meta
    | isEmpty char1 = (char1, 0, char1, char1, char1)
    | isEmpty char2 = (char2, 0, char2, char2, char2)
    | otherwise =
        let
            char1Len = numChars char1
            char2Len = numChars char2
            (shorterChar, longerChar, longLen) = if char1Len > char2Len
                                         then (char2, char1, char1Len)
                                         else (char1, char2, char2Len)
            firstMatRow = firstAlignRow longerChar longLen 0 0 meta
            traversalMat = {-trace ("first row " ++ show firstMatRow) $-} firstMatRow `joinMat` getAlignRows longerChar shorterChar 1 firstMatRow meta
            cost = {-trace ("get cost on " ++ show traversalMat) $-}
                    getMatrixCost traversalMat
            (gapped, left, right) = --trace ("get seqs " ++ show traversalMat)
                                    traceback traversalMat shorterChar longerChar
            ungapped = {-trace ("filter gaps of " ++ show gapped) $ -}filterGaps gapped
            (out1, out2) = if char1Len > char2Len
                                then (right, left)
                                else (left, right)
        in --trace ("ungapped " ++ show ungapped)
            (ungapped, cost, gapped, out1, out2)

        where
            getMatrixCost :: AlignMatrix s -> Double
            --getMatrixCost inAlign | trace ("Get cost " ++ show (nrows inAlign) ++ " " ++ show (ncols inAlign)) False = undefined
            getMatrixCost inAlign = c
                where (c, _, _) = getElem (nrows inAlign - 1) (ncols inAlign - 1) inAlign

-- | Joins an alignment row to the rest of a matrix
-- Takes in an alignment row and an alignment matrix
-- Returns an alignment matrix with the new row added
joinMat :: AlignRow s -> AlignMatrix s -> AlignMatrix s
--joinMat inRow inMat | trace ("join mat " ++ show inRow ++ " on " ++ show inMat) False = undefined
joinMat inRow inMat = inRow `joinRow` inMat
    where
        joinRow vec curMat = fromList 1 (length vec) (toList vec) <-> curMat

-- | Gets the initial row of a naive alignment matrix
-- Takes in an indelCost, the sequence generating the row, the row length, the current position, the previous cost from the algorithm, and the length of the alphabet
-- Returns an alignment row
-- This row will have a diagonal at the leftmost position and will otherwise have only lefts
-- the cost is just added to every time there's a gap
firstAlignRow :: (SeqConstraint' s, Metadata m s) => s -> Int -> Int -> Double -> m -> AlignRow s
--firstAlignRow inChar rowLength position prevCost _ | trace ("firstAlignRow " ++ show inChar ++ " with len " ++ show rowLength ++ show position) False = undefined
firstAlignRow inChar rowLength position prevCost meta
    | position == (rowLength + 1) = --trace ("terminate ") $
                                    mempty
    | position == 0 =
        let recurse0 = firstAlignRow inChar rowLength (position + 1) 0 meta
        in --trace ("cons with gap ") $
            (0, DiagDir, gapChar inChar) `cons` recurse0
    | newState /= gapChar inChar = --trace ("new state on first row " ++ show newState) $ -- if there's no indel overlap
        let recurse1 = firstAlignRow inChar rowLength (position + 1) (prevCost + indCost) meta
        in --trace ("cons with new ") $
            (prevCost + indCost, LeftDir, newState) `cons` recurse1
    | otherwise = --trace ("new state on first row, otherwise " ++ show newState) $ -- matching indel so no cost
        let recurse2 = firstAlignRow inChar rowLength (position + 1) prevCost meta
        in --trace ("cons with new 2 ") $
            (prevCost, LeftDir, newState) `cons` recurse2
        where
            newState = fst $ getOverlap (gapChar inChar) (grabSubChar inChar (position - 1)) meta
            indCost = getGapCost meta

-- TODO: used concrete BitVector type instead of something more appropriate, like EncodableDynamicCharacter. 
-- This also means that there are a bunch of places below that could be using EDC class methods that are no longer.
-- The same will be true in IA.
-- | Memoized wrapper of the overlap function
getOverlap :: (Metadata m s) => BitVector -> BitVector -> m -> (BitVector, Double)
--getOverlap inChar1 inChar2 meta | trace ("getOverlap on " ++ show inChar1) False = undefined
getOverlap inChar1 inChar2 meta = memoize2 (overlap meta) inChar1 inChar2
    where
        -- | Gets the overlap state: intersect if possible and union if that's empty
        -- Takes two sequences and returns another
        overlap :: (Metadata m s) => m -> BitVector -> BitVector -> (BitVector, Double)
        --overlap _ c1 c2 | trace ("overlap on " ++ show c1 ++ " and " ++ show c2) False = undefined
        overlap inMeta char1 char2
            | 0 == char1 || 0 == char2 = --trace ("overlap case 1: equals 0 ") $
                                            (zeroBitVec, 0)
            | char1 .&. char2 == 0 = --trace ("overlap case 2: and empty") $
                                        foldr ambigChoice (zeroBitVec, 0) allPossible
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
                getSubs fullChar = foldr (\i acc -> if testBit fullChar i then (i, bit i) : acc else acc) mempty [0..(length $ getAlphabet inMeta)]
                -- make possible combinations with a double fold
                matchSubs subList oneSub = foldr (\c acc -> getCost (getCosts inMeta) c oneSub : acc) mempty subList
                matchBoth list1 list2 = foldr (\e acc -> matchSubs list1 e ++ acc) mempty list2
                allPossible = matchBoth (getSubs char1) (getSubs char2)
                -- now take an ambiguous minimum
                ambigChoice (val1, cost1) (val2, cost2)
                    | cost1 == cost2 = (val1 .|. val2, cost1)
                    | cost1 < cost2 = (val1, cost1)
                    | otherwise = (val2, cost2)

-- | Main recursive function to get alignment rows
-- Takes two dynamic characters, the indel and sub costs, the current row number, the previous row, and the alphabet length
-- returns an alignment matrix
-- TODO: Turn this into a matrix generator function with indexing previous elements
getAlignRows :: (SeqConstraint' s, Metadata m s) => s -> s -> Int -> AlignRow s -> m -> AlignMatrix s
getAlignRows char1 char2 rowNum prevRow meta
    | rowNum == numChars char2 + 1 = matrix 0 0 (const (0, LeftDir, mempty))
    | otherwise = thisRow `joinMat` getAlignRows char1 char2 (rowNum + 1) thisRow meta
        where
            thisRow = generateRow char1 char2 rowNum prevRow (0, 0) meta

-- | Generates a single alignment row
--   Takes two dynamic chars, the indel and sub costs, the current row number,
--   the previous row, the position and previous cost, and the alphabet length
--   returns an alignment row.
--   Essentially gets values for left, down, and diagonal moves using overlap functionality
--   then selects the minimum value to set the correct value at the given positions
generateRow :: (SeqConstraint' s, Metadata m s) => s -> s -> Int -> AlignRow s -> (Int, Double) -> m -> AlignRow s
--generateRow char1 char2 _ _ (position, _)  _ | trace ("generateRow " ++ show char1 ++ show char2 ++ show position) False = undefined
generateRow char1 char2 rowNum vals (position, prevCost) meta
    | length vals < (position - 1) = error "Problem with row generation, previous costs not generated"
    | position >= numChars char1 + 1 = mempty
    | position == 0 && downChar /= gapChar char1 = --trace "case 4" $
        (upValue + indCost, DownDir, downChar)`cons` (nextCall (upValue + indCost))
    | position == 0 = --trace "case 5" $
        (upValue, DownDir, downChar) `cons` (nextCall upValue)
    | otherwise = --trace "minimal case" $
        (minCost, minDir, minState) `cons` nextCall minCost
        where
            indCost            = getGapCost meta
            subChar1           = grabSubChar char1 (position - 1)
            subChar2           = grabSubChar char2 (rowNum - 1)
            (upValue, _, _)    = vals ! position
            (diagVal, _, _)    = vals ! (position - 1)
            (downChar, dCost)  = getOverlap (gapChar char2) subChar2 meta
            downCost           = dCost + upValue
            (leftChar, lCost)  = getOverlap (gapChar char1) subChar1 meta
            leftCost           = lCost + prevCost
            (diagChar, dgCost) = getOverlap subChar1 subChar2 meta
            diagCost           = diagVal + dgCost

            nextCall cost      = {-trace "next row" $-} generateRow char1 char2 rowNum vals (position + 1, cost) meta

            (minCost, minState, minDir) = {-trace ("minimum with diag " ++ show (diagCost, diagChar)) $-} minimumBy (comparing (\(a,_,_) -> a))
                                                [(diagCost, diagChar, DiagDir), (leftCost, leftChar, LeftDir), (downCost, downChar, DownDir)]

-- | Performs the traceback of an alignment matrix
-- Takes in an alignment matrix, two sequences, and the alphabet length
-- returns the assignment sequence and the aligned version of the two inputs
-- Essentially follows the arrows from the bottom right corner, accumulating the sequences as it goes
traceback :: (SeqConstraint' s) => AlignMatrix s -> s -> s -> (s, s, s)
--traceback alignMat char1 char2 | trace ("traceback with matrix " ++ show alignMat) False = undefined
traceback alignMat' char1' char2' = (fromChars t1, fromChars t2, fromChars t3)
    where
        (t1, t2, t3) = tracebackInternal alignMat' char1' char2' (nrows alignMat' - 1, ncols alignMat' - 1)
        -- read it from the matrix instead of grabbing
        tracebackInternal :: (SeqConstraint' s) => AlignMatrix s -> s -> s -> (Int, Int) -> ([BitVector], [BitVector], [BitVector])
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

