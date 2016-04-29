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
import Data.Function.Memoize
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
-- The matrix itself stores tuples of the cost and direction at that position.
-- We also store a vector of characters that are generated.
data AlignMatrix s
   = AlignMatrix
   { mat     :: Matrix (Double, Direction)
   , seqs      :: Vector s
   } deriving (Eq, Show)

-- | Performs a naive direct optimization
-- Takes in two characters to run DO on and a metadata object
-- Returns an assignment character, the cost of that assignment, the assignment character with gaps included, 
-- the aligned version of the first input character, and the aligned version of the second input character
-- The process for this algorithm is to generate a traversal matrix, then perform a traceback.
naiveDO :: (Metadata m s, SeqConstraint' s) => s -> s -> m -> (s, Double, s, s, s)
--naiveDO s1 s2 | trace ("Sequences of length " ++ show (numChars s1) ++ show (numChars s2)) False = undefined
naiveDO char1 char2 meta
    | isEmpty char1 || isEmpty char2 || numChars char1 == 0 || numChars char2 == 0 = (emptyChar, 0, emptyChar, emptyChar, emptyChar)
    | otherwise = 
        let
            char1Len = numChars char1
            char2Len = numChars char2
            (shorterChar, longerChar, longLen) = if char1Len > char2Len
                                         then (char2, char1, char1Len)
                                         else (char1, char2, char2Len)
            firstMatRow = firstAlignRow longerChar longLen 0 0 meta
            traversalMat = firstMatRow `joinMat` getAlignRows longerChar shorterChar 1 firstMatRow meta
            cost = getMatrixCost traversalMat
            (gapped, left, right) = --trace ("get seqs " ++ show traversalMat)
                                    traceback traversalMat shorterChar longerChar
            ungapped = filterGaps gapped
            (out1, out2) = if char1Len > char2Len
                                then (right, left)
                                else (left, right)
        in (ungapped, cost, gapped, out1, out2)

        where
            getMatrixCost :: (SeqConstraint' s) => AlignMatrix s -> Double
            --getMatrixCost inAlign | trace ("Get cost " ++ show (nrows $ costs inAlign) ++ " " ++ show (ncols $ costs inAlign)) False = undefined
            getMatrixCost inAlign = fst $ getElem (nrows (mat inAlign) - 1) (ncols (mat inAlign) - 1) (mat inAlign)

-- | Joins an alignment row to the rest of a matrix
-- Takes in an alignment row and an alignment matrix
-- Returns an alignment matrix with the new row added
joinMat :: EncodableDynamicCharacter s => AlignRow s -> AlignMatrix s -> AlignMatrix s
joinMat (inRow, inChar) inMat = AlignMatrix (inRow `joinRow` mat inMat) (inChar `cons` seqs inMat)
    where
        joinRow vec curMat = fromList 1 (length vec) (toList vec) <-> curMat

-- | Gets the initial row of a naive alignment matrix
-- Takes in an indelCost, the sequence generating the row, the row length, the current position, the previous cost from the algorithm, and the length of the alphabet
-- Returns an alignment row
-- This row will have a diagonal at the leftmost position and will otherwise have only lefts
-- the cost is just added to every time there's a gap
firstAlignRow :: (SeqConstraint' s, Metadata m s) => s -> Int -> Int -> Double -> m -> AlignRow s
--firstAlignRow indelCost inChar rowLength position prevCost | trace ("firstAlignRow " ++ show inChar) False = undefined
firstAlignRow inChar rowLength position prevCost meta
    | position == (rowLength + 1) = (mempty, mempty)
    | position == 0 = (singleton (0, DiagDir), gapChar inChar) <> firstAlignRow inChar rowLength (position + 1) 0 meta
    | newState /= gapChar inChar = --trace ("new state on first row " ++ show newState) $ -- if there's no indel overlap
        (singleton (prevCost + indCost, LeftDir), newState) <> firstAlignRow inChar rowLength (position + 1) (prevCost + indCost) meta
    | otherwise = --trace ("new state on first row, otherwise " ++ show newState) $ -- matching indel so no cost
        (singleton (prevCost, LeftDir), newState) <> firstAlignRow inChar rowLength (position + 1) prevCost meta
        where
            newState = fst $ getOverlap (gapChar inChar) (grabSubChar inChar (position - 1)) meta
            indCost = getGapCost meta

-- | Memoized wrapper of the overlap function
getOverlap :: (SeqConstraint' s, Metadata m s) => s -> s -> m -> (s, Double)
getOverlap inChar1 inChar2 meta = memoize2 (overlap meta) inChar1 inChar2
    where
        -- | Gets the overlap state: intersect if possible and union if that's empty
        -- Takes two sequences and returns another
        overlap :: (SeqConstraint' s, Metadata m s) => m -> s -> s -> (s, Double)
        overlap inMeta char1 char2
            | isEmpty char1 || isEmpty char2 = (emptyChar, 0)
            | char1 .&. char2 == zeroBits = foldr1 ambigChoice allPossible
            | otherwise = (char1 .&. char2, 0)
            where
                gap = gapChar char1
                -- Given characters without ambiguity, determine the cost
                -- getCost :: SeqConstraint' s => CostStructure -> (Int, s) -> (Int, s) -> (s, Double)
                getCost (TCM mtx) (pos1, c1) (pos2, c2) = (c1 .|. c2, getElem pos1 pos2 mtx)
                getCost (GeneralCost indel sub) (_, c1) (_, c2) = if c1 == gap || c2 == gap then (c1 .|. c2, indel) else (c1 .|. c2, sub)
                getCost (AffineCost _ _ _) _ _ = error "Cannot apply DO algorithm on affine cost"

                -- get single character subsets from both
                getSubs fullChar = foldr (\i acc -> if testBit fullChar i then (i, bit i) : acc else acc) mempty [0..getAlphLen char1]
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
getAlignRows :: (SeqConstraint' s, Metadata m s) => s -> s -> Int -> AlignRow s -> m -> AlignMatrix s
getAlignRows char1 char2 rowNum prevRow meta
    | rowNum == numChars char2 + 1 = AlignMatrix (matrix 0 0 (const (0, LeftDir))) mempty 
    | otherwise = thisRow `joinMat` getAlignRows char1 char2 (rowNum + 1) thisRow meta
        where
            thisRow = generateRow char1 char2 rowNum prevRow (0, 0) meta 

-- | Generates a single alignment row
-- Takes two dynamic chars, the indel and sub costs, the current row number, the previous row, the position and previous cost, and the alphabet length
-- returns an alignment row
-- Essentially gets values for left, down, and diagonal moves using overlap functionality
-- then selects the minimum value to set the correct value at the given positions
generateRow :: (SeqConstraint' s, Metadata m s) => s -> s -> Int -> AlignRow s -> (Int, Double) -> m -> AlignRow s
--generateRow char1 char2 costvals@(indelCost, subCost) rowNum prevRow@(costs, _, _) (position, prevCost)  | trace ("generateRow " ++ show char1 ++ show char2) False = undefined
generateRow char1 char2 rowNum prevRow@(vals, _) (position, prevCost) meta
    | length vals < (position - 1) = error "Problem with row generation, previous costs not generated"
    | position == numChars char1 + 1 = (mempty, emptyChar)
    | position == 0 && downChar /= gapChar char1 = (singleton (upValue + indCost, DownDir), downChar) <> nextCall (upValue + indCost)
    | position == 0 = (singleton (upValue, DownDir), downChar) <> nextCall upValue
    | otherwise = --trace "minimal case" $ 
        (singleton (minCost, minDir), minState) <> nextCall minCost
        where
            indCost            = getGapCost meta
            subChar1           = grabSubChar char1 (position - 1)
            subChar2           = grabSubChar char2 (rowNum - 1)
            upValue            = fst $ vals ! position
            diagVal            = fst $ vals ! (position - 1)
            (downChar, dCost)  = getOverlap (gapChar char2) subChar2 meta
            downCost           = dCost + upValue
            (leftChar, lCost)  = getOverlap (gapChar char1) subChar1 meta
            leftCost           = lCost + prevCost
            (diagChar, dgCost) = getOverlap subChar1 subChar2 meta
            diagCost           = diagVal + dgCost

            nextCall cost      = generateRow char1 char2 rowNum prevRow (position + 1, cost) meta
   
            (minCost, minState, minDir) = minimumBy (comparing (\(a,_,_) -> a))
                                                [(leftCost, leftChar, LeftDir), (downCost, downChar, DownDir), (diagCost, diagChar, DiagDir)]

-- | Performs the traceback of an alignment matrix
-- Takes in an alignment matrix, two sequences, and the alphabet length
-- returns the assignment sequence and the aligned version of the two inputs
-- Essentially follows the arrows from the bottom right corner, accumulating the sequences as it goes
traceback :: (SeqConstraint' s) => AlignMatrix s -> s -> s -> (s, s, s)
--traceback alignMat char1 char2 | trace ("traceback with matrix " ++ show alignMat) False = undefined
traceback alignMat' char1' char2' = tracebackInternal alignMat' char1' char2' (numChars char1', numChars char2')
    where
        -- read it from the matrix instead of grabbing
        tracebackInternal :: (SeqConstraint' s) => AlignMatrix s -> s -> s -> (Int, Int) -> (s, s, s)
        --tracebackInternal alignMat char1 char2 (row, col)  | trace ("traceback " ++ show (traversal alignMat) ++ show (getElem row col (traversal alignMat))++ " with position " ++ show (row, col)) False = undefined
        tracebackInternal alignMat char1 char2 (row, col) 
            | length (seqs alignMat) < row - 1 || nrows (mat alignMat) < row - 1 || ncols (mat alignMat) < col - 1 = error "Traceback cannot function because matrix is incomplete"
            | row == 0 && col == 0 = (emptyChar, emptyChar, emptyChar)
            | curDirect == LeftDir = tracebackInternal alignMat char1 char2 (row, col - 1) <> (curState, gapChar char2, grabSubChar char2 (col - 1))
            | curDirect == DownDir = tracebackInternal alignMat char1 char2 (row - 1, col) <> (curState, grabSubChar char1 (row - 1), gapChar char1)
            | curDirect == DiagDir = tracebackInternal alignMat char1 char2 (row - 1, col - 1) <> (curState, grabSubChar char1 (row - 1), grabSubChar char2 (col - 1))
            | otherwise = error "Incorrect direction in matrix traversal for alignment"
                where
                    curDirect = snd $ getElem row col (mat alignMat)
                    curState  = grabSubChar (seqs alignMat ! row) col
