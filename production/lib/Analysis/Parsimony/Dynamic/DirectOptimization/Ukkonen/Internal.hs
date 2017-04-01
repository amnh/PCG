-----------------------------------------------------------------------------
-- |
-- Module      :  Analysis.Parsimony.Dynamic.DirectOptimization.Ukkonen.Internal
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Direct optimization functionality for binary trees.
-- Implement's Ukkonen's space & time saving algorithm.
--
-- Allocates a "ribbon" down the diagonal of the matrix rather than the entire matrix.
--
-----------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds, FlexibleContexts, TypeFamilies #-}

module Analysis.Parsimony.Dynamic.DirectOptimization.Ukkonen.Internal where


import Bio.Character.Encodable
import Data.Bits
--import Data.BitVector hiding (foldr, reverse)
import Data.Foldable         (minimumBy)
--import Data.Function.Memoize
import Data.Key              ((!))
import Data.Matrix.NotStupid (Matrix, matrix, nrows, ncols)
import Data.MonoTraversable
import Data.Ord

--import Debug.Trace (trace)


{- |
 - The Ukkonen code from the prototype codebase
 -}


-- |
-- setLeftRight returns sequence that is longer first,
-- shorter second.  Handles equal length by returning max first.
setLeftRight :: BaseChar -> BaseChar -> (BaseChar, Int, BaseChar, Int)
setLeftRight inL inR
    | V.length inL < V.length inR = (inR, V.length inR, inL, V.length inL)
    | V.length inL > V.length inR = (inL, V.length inL, inR, V.length inR)
    | otherwise = (outL, V.length outL, outR, V.length outR)
        where
            outL = max inL inR
            outR = min inL inR 


-- |
-- ukkonenCore core functions of Ukkonen to allow for recursing with maxGap
-- doubled if not large enough (returns Nothing)  
ukkonenCore :: BaseChar -> Int -> BaseChar -> Int -> Int -> Int -> Int -> (BaseChar, Float, BaseChar, BaseChar, BaseChar)
ukkonenCore lSeq lLength rSeq rLength maxGap indelCost subCost | trace "ukkonenCore" False = undefined
ukkonenCore lSeq lLength rSeq rLength maxGap indelCost subCost
    | V.head median /= (0 :: Int64)  = --trace (show nwMatrix) 
        (median, fromIntegral cost, medianGap, alignLeft, alignRight)
    | otherwise = --trace ("Going back!! " ++ show cost) 
        ukkonenCore lSeq lLength rSeq rLength (2 * maxGap) indelCost subCost
        where
            firstRow = getFirstRowUkkonen indelCost lLength 0 0 lSeq maxGap
            nwMatrix = V.cons firstRow (getRowsUkkonen lSeq rSeq indelCost subCost 1 firstRow maxGap)
            (cost, _, _) = V.last (V.last nwMatrix) -- V.! rLength) --V.! (transformFullYShortY lLength rLength  maxGap) --fix for offset
            (medianGap, alignLeft, alignRight) = V.unzip3 $ V.reverse (tracebackUkkonen nwMatrix lSeq rSeq rLength lLength maxGap 0 0)
            median = V.filter (/= inDelBit) medianGap


--FOR both DO's  lseq is a row, acrosss so num columns = length of lseq
--There are rseq rows
-- |
-- UkkonenDO takes two input sequences and returns median sequence and cost
-- only 1:1 for now. Uses Ukkonen's space/time saving algorithm
-- need to make sure Left/Right and diag/ins/del orders consistent and with
-- POY4/5
-- lseq > rseq appeard more efficient--could be wrong
-- move to C via FFI
-- Still occasional error in cost and median (disagreement) show in Chel.seq
ukkonenDO :: BaseChar -> BaseChar -> CharInfo -> (BaseChar, Float, BaseChar, BaseChar, BaseChar)
ukkonenDO inlSeq inrSeq charInfo | trace ("calling ukonnen DO with seqs " ++ show inlSeq ++ show inrSeq) False = undefined
ukkonenDO inlSeq inrSeq charInfo
    | V.null inlSeq = (inrSeq, 0, inrSeq, V.replicate (V.length inrSeq) (maxBound :: Int64), inrSeq)
    | V.null inrSeq = (inlSeq, 0, inlSeq, inlSeq, V.replicate (V.length inlSeq) (maxBound :: Int64))
    | otherwise = trace ("got stuff "++ show alignRight) 
                    (median, cost, medGap, alignLeft, alignRight)
        where
            indelCost = 1
            subCost = 1
            --this for left right constant--want longer in left for Ukkonnen
            (lSeq, lLength, rSeq, rLength) = setLeftRight inlSeq inrSeq
            maxGap = 1 + lLength - rLength  --10000 :: Int --holder lseq - rSeq + 1
            (median, cost, medGap, alignLeft, alignRight) = ukkonenCore lSeq lLength rSeq rLength maxGap indelCost subCost


-- |
-- tracebackUkkonen creates REVERSE mediian from nwMatrix, reverse to make tail
-- recusive, for Ukkonen space/time saving offsets
-- need to count gaps in traceback for threshold/barrier stuff
-- CHANGE TO MAYBE (V.Vector Int64) FOR BARRIER CHECK
tracebackUkkonen :: V.Vector (V.Vector (Int, Int64, Direction)) -> BaseChar -> BaseChar -> Int -> Int -> Int -> Int -> Int -> V.Vector (Int64, Int64, Int64)
tracebackUkkonen nwMatrix inlSeq inrSeq posR posL maxGap rInDel lInDel | trace ("tracebackUkkonen " ++ show posR ++ show posL ++ show inlSeq ++ show inrSeq) False = undefined
tracebackUkkonen nwMatrix inlSeq inrSeq posR posL maxGap rInDel lInDel
--trace ("psLR " ++ show posR ++ " " ++ show posL ++ " Left " ++ show lInDel ++ " Right " ++ show rInDel ++ " maxGap " ++ show maxGap) (
    | (rInDel  > (maxGap - 2)) || (lInDel > (maxGap - 2)) = V.singleton (0, 0, 0) :: V.Vector (Int64,Int64,Int64)  
    | posL <= 0 && posR <= 0 = trace "not y" V.empty
    | otherwise = trace "y" $ let 
        y   | direction == LeftDir = V.cons (state, inDelBit, inrSeq V.! (posR - 1)) (tracebackUkkonen nwMatrix inlSeq inrSeq posR (posL - 1) maxGap rInDel (lInDel + 1))
            | direction == DownDir = V.cons (state, inlSeq V.! (posL - 1), inDelBit) (tracebackUkkonen nwMatrix inlSeq inrSeq (posR - 1) posL maxGap (rInDel + 1) lInDel)  
            | otherwise = V.cons (state, inlSeq V.! (posL - 1), inrSeq V.! (posR - 1)) (tracebackUkkonen nwMatrix inlSeq inrSeq (posR - 1) (posL - 1) maxGap rInDel lInDel)
        in trace (show y) y
        where (_, state, direction) = (nwMatrix V.! posR) V.! transformFullYShortY posL posR  maxGap --(transformFullYShortY posL posR maxGap)


-- |
-- getFirstRowUkkonen initializes first row of NW-Ukkonen matrix
getFirstRowUkkonen :: Int -> Int -> Int -> Int -> BaseChar -> Int -> V.Vector (Int, Int64, Direction)
--getFirstRowUkkonen _ rowLen position _ lSeq _ | trace ("getFirstRowUkkonen " ++ show lSeq ++ show position ++ show rowLen) False = undefined
getFirstRowUkkonen indelCost rowLength position prevCost lSeq  maxGap
 --trace ("row 0 pos " ++ show position ++ "/" ++ show (maxShortY rowLength 0 maxGap) ++ " rowLength " ++ show rowLength ++ " maxGap " ++ show maxGap ++ " lseq " ++ show lSeq)
    | position == rowLength  + 1 = V.empty
    | position == (maxGap + 1) = V.singleton (barrierCost, barrierBit, LeftDir) 
    | position == 0 = V.cons (0, inDelBit, DiagDir) (getFirstRowUkkonen indelCost rowLength (position + 1) 0 lSeq maxGap) 
    | otherwise = --trace ("FRC " ++ show newCost)
        let 
        y   | (newState /= inDelBit) = --if there was no inDel overlap between states
                V.cons (newCost, newState, LeftDir) (getFirstRowUkkonen  indelCost rowLength (position + 1) newCost lSeq maxGap)
            | otherwise = --indel in both states so no cost
                V.cons (prevCost, newState, LeftDir) (getFirstRowUkkonen  indelCost rowLength (position + 1) prevCost lSeq maxGap)
        in y
        where 
            newCost = prevCost + indelCost
            newState = getUnionIntersectionState inDelBit (lSeq V.! (position - 1))


-- |
-- getRowUkkonen starts at second row (=1) and creates each row in turn--Ukkonen
getRowsUkkonen :: BaseChar -> BaseChar -> Int -> Int -> Int -> V.Vector (Int, Int64, Direction) -> Int -> V.Vector (V.Vector (Int, Int64, Direction))
getRowsUkkonen lSeq rSeq indelCost subCost rowNum prevRow maxGap | trace "getRowsUkkonen" False = undefined
getRowsUkkonen lSeq rSeq indelCost subCost rowNum prevRow maxGap
    | rowNum == (length rSeq + 1) = V.empty
    | startPosition == 0 = --trace ("Row " ++ show rowNum ++ " of " ++ show (V.length rSeq) ++ " starts " ++ show startPosition ++ ":" ++ show thisRowZero) (
                V.cons thisRowZero (getRowsUkkonen lSeq rSeq indelCost subCost (rowNum + 1) thisRowZero maxGap) --)
    | otherwise = --trace ("Row " ++ show rowNum ++ " of " ++ show (V.length rSeq) ++" starts " ++ show startPosition ++ ":" ++ show thisRowNonZero) (
        V.cons thisRowNonZero (getRowsUkkonen lSeq rSeq indelCost subCost (rowNum + 1) thisRowNonZero maxGap) --)
        where 
            startPosition = max 0 (rowNum - maxGap) --check for left barriers 
            thisRowZero =  getThisRowUkkonen lSeq rSeq indelCost subCost rowNum prevRow startPosition (V.length lSeq) 0 maxGap
            thisRowNonZero = V.cons (barrierCost, barrierBit, DownDir) (getThisRowUkkonen lSeq rSeq indelCost subCost rowNum prevRow startPosition  (V.length lSeq) barrierCost maxGap )


-- |
-- getThisRowUkkonen takes sequences and parameters with row number and make a non-first
-- row--Ukkonen
getThisRowUkkonen :: BaseChar -> BaseChar -> Int -> Int -> Int ->  V.Vector (Int, Int64, Direction) -> Int -> Int -> Int -> Int -> V.Vector (Int, Int64, Direction)
getThisRowUkkonen lSeq rSeq indelCost subCost rowNum prevRow position rowLength prevCost maxGap
    | position ==  rowLength  + 1 = V.empty
    | position == (rowNum + maxGap + 1) = V.singleton (barrierCost, barrierBit, LeftDir)
    | position == 0 = let
        x   | (newState /= inDelBit) =
                V.cons (upValue + indelCost, newState, DownDir) (getThisRowUkkonen lSeq rSeq indelCost subCost rowNum prevRow (position + 1) rowLength (upValue + indelCost) maxGap)
            | otherwise = 
                V.cons (upValue, newState, DownDir) (getThisRowUkkonen lSeq rSeq indelCost subCost rowNum prevRow (position + 1) rowLength upValue maxGap)
                where
                    newState = getUnionIntersectionState inDelBit (rSeq V.! (rowNum - 1))
                    (upValue, _, _) = prevRow V.! position
        in x
    | otherwise = V.cons (minCost, minState, minDir) (getThisRowUkkonen lSeq rSeq indelCost subCost rowNum prevRow (position + 1) rowLength minCost maxGap)
        where
            lSeqPos = position - 1 --since first is '-' the index is row/pos - 1
            rSeqRow = rowNum - 1 --since first is '-' the index is row/pos - 1
            leftCost = getOverlapCost prevCost indelCost (lSeq V.! lSeqPos) --need to check for overlap
            (upValue, _, _) = prevRow V.! transformFullYShortY  position (rowNum - 1) maxGap
            downCost = getOverlapCost upValue indelCost (rSeq V.! rSeqRow) --need to check for overlap
            (diagValue, _, _) = prevRow V.! transformFullYShortY  (position - 1) (rowNum - 1) maxGap
            intersection = (lSeq V.! lSeqPos) .&. (rSeq V.! rSeqRow)
            union = (lSeq V.! lSeqPos) .|. (rSeq V.! rSeqRow)
            (diagCost, diagState) = getDiagDirCost diagValue intersection union subCost
            (minCost, minState, minDir) = getMinCostDir leftCost downCost diagCost diagState 
                (getUnionIntersectionState inDelBit (lSeq V.! lSeqPos)) (getUnionIntersectionState inDelBit (rSeq V.! rSeqRow)) 


-- |
-- firstOfThree takes a triple and returns first member
firstOfThree :: (a, b, c) -> a
firstOfThree (in1, in2, in3) = in1


-- |
-- secondOfThree takes a triple and returns second member
secondOfThree :: (a, b, c) -> b
secondOfThree (in1, in2, in3) = in2


-- |
-- thirdOfThree takes a triple and returns third member
thirdOfThree :: (a, b, c) -> c
thirdOfThree (in1, in2, in3) = in3
