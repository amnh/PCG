{- |
Module      :  Parsimony Optimization functions
Description :  Functions for parsimony cost and vertex optimizations
Copyright   :  (c) 2014 Ward C. Wheeler, Division of Invertebrate Zoology, AMNH. All rights reserved.
License     :  

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met: 

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer. 
2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution. 

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

The views and conclusions contained in the software and documentation are those
of the authors and should not be interpreted as representing official policies, 
either expressed or implied, of the FreeBSD Project.

Maintainer  :  Ward Wheeler <wheeler@amnh.org>
Stability   :  unstable
Portability :  portable (I hope)

-}

module Parsimony
( getPrelim
, getPrelimTriple
, ukkonenDO
) where

import Debug.Trace
import Data.Int
import Data.Bits
import Data.List
import Data.Maybe
import qualified Data.Vector.Storable as VS
import qualified Data.Vector as V
import ReadFiles
import CharacterData

data Direction = LeftDir | DownDir | DiagDir
    deriving (Read, Show, Eq)

--Move these to Character Data?
inDelBit = bit 63 :: Int64 --(bit 63) :: Int64 --set indelBit to 64th bit in Int64
barrierCost = bit 60 :: Int --really big Int--asssumes 64 bit at least, but can be added to without rolling over.
barrierBit = bit 63 :: Int64


-- | getPrelimTriple takes bit-coded states (as triple) and returns cost and prelim state
getPrelimTriple :: (BaseChar, BaseChar, CharInfo) -> (BaseChar, Float, BaseChar, BaseChar, BaseChar)
getPrelimTriple (lState, rState, charInfo)
    | not (activity charInfo) = (V.singleton (0 :: Int64), 0, V.singleton (0 :: Int64), lState, rState)
    | charType charInfo == NonAdd = let
        x   | intersection /= 0 = (V.singleton intersection, 0, V.singleton intersection, lState, rState)
            | otherwise = (V.singleton ((lS .|. rS) :: Int64), weight charInfo, V.singleton ((lS .|. rS) :: Int64), lState, rState)
        in x
    | charType charInfo == GenSeq = --trace ("lS " ++ show lState ++ " rs " ++ show rState ++ "median " ++ show median ++ " cost " ++ show cost ++ " mG " ++ show medianGap ++ " aL " ++ show alignLeft ++ " aR " ++ show alignRight) 
        (median, charWeight * cost, medianGap, alignLeft, alignRight)  --this should be based on min of inouts length
    | charType charInfo == NucSeq = (median2, charWeight * cost2, medianGap2, alignLeft2, alignRight2)
    | otherwise = error "Unrecognized/Not implemented character type"
        where
            (median, cost, medianGap, alignLeft, alignRight) = naiveDo lState rState charInfo
            charWeight = weight charInfo
            (median2, cost2, medianGap2, alignLeft2, alignRight2) = ukkonenDO lState rState charInfo
            lS = V.head lState
            rS = V.head rState
            intersection = (lS .&. rS) :: Int64


-- | getPrelim takes bit-coded states and returns cost and prelim state
-- depends entirely on getPrelimTriple, just joins together args
getPrelim :: BaseChar -> BaseChar -> CharInfo -> (BaseChar, Float, BaseChar, BaseChar, BaseChar)
getPrelim lState rState charInfo = getPrelimTriple (lState, rState, charInfo)


-- | transformFullYShortY take full Y value (if did entire NW matrix) and returns
--short (Ukkonnen Y) given Y, Y length and row number
--remove error when working--overhead
transformFullYShortY :: Int -> Int -> Int -> Int
transformFullYShortY currentY rowNumber maxGap 
    | transformY < 0 = error (show currentY ++ " " ++ show rowNumber ++ " " ++ show maxGap ++ " Impossible negative value for transfomred Y")
    | otherwise = transformY
        where transformY = currentY - max 0 (rowNumber - maxGap - 1)

-- | maxShortY is the maximum value for short Y in Ukkonen usually 2*maxGap
--untill near bottom of matrix when running up against the full Y length
--assumes length X /> length Y.
maxShortY :: Int -> Int -> Int -> Int
maxShortY maxFullY rowNumber maxGap =
    let leftPart = min rowNumber maxGap
        rightPart = min maxGap (maxFullY - rowNumber)
    in leftPart + rightPart


-- | setLeftRight returns sequence that is longer first,
--shorter second.  Handles equal length by returning max first.
setLeftRight :: BaseChar -> BaseChar -> (BaseChar, Int, BaseChar, Int)
setLeftRight inL inR
    | V.length inL < V.length inR = (inR, V.length inR, inL, V.length inL)
    | V.length inL > V.length inR = (inL, V.length inL, inR, V.length inR)
    | otherwise = (outL, V.length outL, outR, V.length outR)
        where
            outL = max inL inR
            outR = min inL inR 

-- | ukkonenCore core functions of Ukkonen to allow for recursing with maxGap
--doubled if not large enough (returns Nothing)  
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
-- | UkkonenDO takes two input sequences and returns median sequence and cost
--only 1:1 for now. Uses Ukkonen's space/time saving algorithm
--need to make sure Left/Right and diag/ins/del orders consistent and with
--POY4/5
--lseq > rseq appeard more efficient--could be wrong
--move to C via FFI
--Still occasional error in cost and median (disagreement) show in Chel.seq
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

-- | tracebackUkkonen creates REVERSE mediian from nwMatrix, reverse to make tail
--recusive, for Ukkonen space/time saving offsets
--need to count gaps in traceback for threshold/barrier stuff
--CHANGE TO MAYBE (V.Vector Int64) FOR BARRIER CHECK
tracebackUkkonen :: V.Vector (V.Vector (Int, Int64, Direction)) -> BaseChar -> BaseChar -> Int -> Int -> Int -> Int -> Int -> V.Vector (Int64, Int64, Int64)
tracebackUkkonen nwMatrix inlSeq inrSeq posR posL maxGap rInDel lInDel | trace ("tracebackUkkonen " ++ show posR ++ show posL ++ show inlSeq ++ show inrSeq) False = undefined
tracebackUkkonen nwMatrix inlSeq inrSeq posR posL maxGap rInDel lInDel
--trace ("psLR " ++ show posR ++ " " ++ show posL ++ " Left " ++ show lInDel ++ " Right " ++ show rInDel ++ " maxGap " ++ show maxGap) (
    | (rInDel  > (maxGap - 2)) || (lInDel > (maxGap - 2)) = V.singleton ((0 :: Int64), (0 :: Int64), (0 :: Int64))  
    | posL <= 0 && posR <= 0 = trace "not y" V.empty
    | otherwise = trace "y" $ let 
        y   | direction == LeftDir = V.cons (state, inDelBit, inrSeq V.! (posR - 1)) (tracebackUkkonen nwMatrix inlSeq inrSeq posR (posL - 1) maxGap rInDel (lInDel + 1))
            | direction == DownDir = V.cons (state, inlSeq V.! (posL - 1), inDelBit) (tracebackUkkonen nwMatrix inlSeq inrSeq (posR - 1) posL maxGap (rInDel + 1) lInDel)  
            | otherwise = V.cons (state, inlSeq V.! (posL - 1), inrSeq V.! (posR - 1)) (tracebackUkkonen nwMatrix inlSeq inrSeq (posR - 1) (posL - 1) maxGap rInDel lInDel)
        in trace (show y) y
        where (_, state, direction) = (nwMatrix V.! posR) V.! (transformFullYShortY posL posR  maxGap) --(transformFullYShortY posL posR maxGap)

-- | getFirstRowUkkonen initializes first row of NW-Ukkonen matrix
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


-- | getRowUkkonen starts at second row (=1) and creates each row in turn--Ukkonen
getRowsUkkonen :: BaseChar -> BaseChar -> Int -> Int -> Int -> V.Vector (Int, Int64, Direction) -> Int -> V.Vector (V.Vector (Int, Int64, Direction))
getRowsUkkonen lSeq rSeq indelCost subCost rowNum prevRow maxGap | trace "getRowsUkkonen" False = undefined
getRowsUkkonen lSeq rSeq indelCost subCost rowNum prevRow maxGap
    | rowNum == ((V.length rSeq) + 1) = V.empty
    | startPosition == 0 = --trace ("Row " ++ show rowNum ++ " of " ++ show (V.length rSeq) ++ " starts " ++ show startPosition ++ ":" ++ show thisRowZero) (
                V.cons thisRowZero (getRowsUkkonen lSeq rSeq indelCost subCost (rowNum + 1) thisRowZero maxGap) --)
    | otherwise = --trace ("Row " ++ show rowNum ++ " of " ++ show (V.length rSeq) ++" starts " ++ show startPosition ++ ":" ++ show thisRowNonZero) (
        V.cons thisRowNonZero (getRowsUkkonen lSeq rSeq indelCost subCost (rowNum + 1) thisRowNonZero maxGap) --)
        where 
            startPosition = max 0 (rowNum - maxGap) --check for left barriers 
            thisRowZero =  getThisRowUkkonen lSeq rSeq indelCost subCost rowNum prevRow startPosition (V.length lSeq) 0 maxGap
            thisRowNonZero = V.cons (barrierCost, barrierBit, DownDir) (getThisRowUkkonen lSeq rSeq indelCost subCost rowNum prevRow startPosition  (V.length lSeq) barrierCost maxGap )

-- | getThisRowUkkonen takes sequences and parameters with row number and make a non-first
--row--Ukkonen
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
            (upValue, _, _) = prevRow V.! (transformFullYShortY  position (rowNum - 1) maxGap)
            downCost = getOverlapCost upValue indelCost (rSeq V.! rSeqRow) --need to check for overlap
            (diagValue, _, _) = prevRow V.! (transformFullYShortY  (position - 1) (rowNum - 1) maxGap)
            intersection = (lSeq V.! lSeqPos) .&. (rSeq V.! rSeqRow)
            union = (lSeq V.! lSeqPos) .|. (rSeq V.! rSeqRow)
            (diagCost, diagState) = getDiagDirCost diagValue intersection union subCost
            (minCost, minState, minDir) = getMinCostDir leftCost downCost diagCost diagState 
                (getUnionIntersectionState inDelBit (lSeq V.! lSeqPos)) (getUnionIntersectionState inDelBit (rSeq V.! rSeqRow)) 

-- | firstOfThree takes a triple and returns first member
firstOfThree :: (a, b, c) -> a
firstOfThree (in1, in2, in3) = in1

-- | secondOfThree takes a triple and returns second member
secondOfThree :: (a, b, c) -> b
secondOfThree (in1, in2, in3) = in2

-- | thirdOfThree takes a triple and returns third member
thirdOfThree :: (a, b, c) -> c
thirdOfThree (in1, in2, in3) = in3

-- | naiveDo takes two input sequences and returns median sequence and cost 
--based on charInfo-1:1 for now
--to do:
--      different costs
--      left/right based on name
--      Ukkonnen
--      C via FFI
--      Affine
naiveDo :: BaseChar -> BaseChar -> CharInfo -> (BaseChar, Float, BaseChar, BaseChar, BaseChar)
naiveDo inlSeq inrSeq charInfo
    | V.null inlSeq = (inrSeq, 0, inrSeq, V.replicate (V.length inrSeq) (maxBound :: Int64), inrSeq)
    | V.null inrSeq = (inlSeq, 0, inlSeq, inlSeq, V.replicate (V.length inlSeq) (maxBound :: Int64))
    | otherwise = --trace ("NW: " ++ show nwMatrix ++ "\nCost/median " ++ show cost ++ "->" ++ show median)
        (median, fromIntegral cost, medianGap, alignLeft, alignRight)
        where
            indelCost = 1
            subCost = 1
            --this for left right constant--want longer in left for Ukkonnen
            (lSeq, lLength, rSeq, rLength) = setLeftRight inlSeq inrSeq
            --lSeq = max inlSeq inrSeq
            --rSeq = min inlSeq inrSeq
            --lLength = V.length lSeq
            --rLength = V.length rSeq
            firstRow = getFirstRow indelCost lLength 0 0 lSeq
            nwMatrix = V.cons firstRow (getRows lSeq rSeq indelCost subCost 1 firstRow)
            (cost, _, _) = (nwMatrix V.! rLength) V.! lLength
            (medianGap, alignLeft, alignRight) = V.unzip3 $ V.reverse (traceback nwMatrix rSeq lSeq (V.length rSeq) (V.length lSeq))
            median = V.filter (/= inDelBit) medianGap

-- | traceback creates REVERSE mediian from nwMatrix, reverse to make tail
--recusive
traceback :: V.Vector (V.Vector (Int, Int64, Direction)) -> BaseChar -> BaseChar -> Int -> Int -> V.Vector (Int64, Int64, Int64)
traceback nwMatrix inlSeq inrSeq posL posR
    | posL == 0 && posR == 0 = V.empty  
    | otherwise = let
        x   | direction == LeftDir = V.cons  (state, inDelBit, inrSeq V.! (posR - 1)) (traceback nwMatrix inlSeq inrSeq posL (posR - 1)) 
            | direction == DownDir = V.cons (state, inlSeq V.! (posL - 1), inDelBit) (traceback nwMatrix inlSeq inrSeq (posL - 1) posR )  
            | otherwise = V.cons (state, inlSeq V.! (posL - 1), inrSeq V.! (posR - 1)) (traceback nwMatrix inlSeq inrSeq (posL - 1) (posR - 1))
        in x
        where (_, state, direction) = (nwMatrix V.! posL ) V.! posR


-- | getFirstRow initializes foirst row of NW matrix
getFirstRow :: Int -> Int -> Int -> Int -> BaseChar -> V.Vector (Int, Int64, Direction)
getFirstRow indelCost rowLength position prevCost lSeq
    | position == (rowLength + 1) = V.empty
    | position == 0 = V.cons (0, inDelBit, DiagDir) (getFirstRow indelCost rowLength (position + 1) 0 lSeq) 
    | newState /= inDelBit = --if there was no inDel overlap between states
        V.cons (newCost, newState, LeftDir) (getFirstRow  indelCost rowLength (position + 1) newCost lSeq)
    | otherwise = --indel in both states so no cost
        V.cons (prevCost, newState, LeftDir) (getFirstRow  indelCost rowLength (position + 1) prevCost lSeq)
        where 
            newCost = prevCost + indelCost
            newState = getUnionIntersectionState inDelBit (lSeq V.! (position - 1))


-- | getRow starts at second row (=1) and cretes each row in turn
getRows :: BaseChar -> BaseChar -> Int -> Int -> Int -> V.Vector (Int, Int64, Direction) -> V.Vector (V.Vector (Int, Int64, Direction))
getRows lSeq rSeq indelCost subCost rowNum prevRow
    | rowNum == (V.length rSeq) + 1 = V.empty
    | otherwise = --trace ("Row " ++ show rowNum)
        V.cons thisRow (getRows lSeq rSeq indelCost subCost (rowNum + 1) thisRow) 
        where thisRow =  getThisRow lSeq rSeq indelCost subCost rowNum prevRow 0 (V.length lSeq) 0

-- | getDiagDirCost takes union intersection and state to get diagonla sub or no-sub
--cost
getDiagDirCost :: Int -> Int64 -> Int64 -> Int -> (Int, Int64)
getDiagDirCost upLeftDirCost intersection union subCost --trace ("DiagCost " ++ show upLeftDirCost ++ " int " ++ show intersection ++ " union " ++ show union) (
    | intersection /= 0 = (upLeftDirCost, intersection) 
    | otherwise = (upLeftDirCost + subCost, union)

-- | getUnionIntersection
getUnionIntersectionState :: Int64 -> Int64 -> Int64
getUnionIntersectionState lState rState 
    | intersection /= 0 = intersection
    | otherwise = (lState .|. rState) :: Int64
        where intersection = (lState .&. rState) :: Int64

-- | getMinCostDir takes costs and states of three directins and returns min cost,
--directin, and state
--ORDER diag, down, left so same as POY4-5.
getMinCostDir :: Int -> Int -> Int -> Int64 -> Int64 -> Int64 -> (Int, Int64, Direction)
getMinCostDir leftCost downCost diagCost diagState leftState downState
    | diagCost == minValue = (diagCost, diagState, DiagDir)
    | downCost == minValue = (downCost, downState, DownDir)
    | otherwise = (leftCost, leftState, LeftDir)
        where minValue = minimum [leftCost, downCost, diagCost]

-- | getOverlapCost cheks for ovelap in gap so if indel, but opossite a gap
--ambiguity--there is no cost
getOverlapCost :: Int -> Int -> Int64 -> Int
getOverlapCost preCost indelCost oppositeState
    --trace("bits " ++ show oppositeState ++ " overAND " ++ show ((.&.) oppositeState inDelBit) ++ " control " ++ show ((.&.) inDelBit inDelBit)) ( 
    | (.&.) oppositeState inDelBit == (0 :: Int64) =preCost + indelCost
    | otherwise =  preCost

-- | getThisRow takes sequences and parameters with row number and make a non-first
--row
getThisRow :: BaseChar -> BaseChar -> Int -> Int -> Int ->  V.Vector (Int, Int64, Direction) -> Int -> Int -> Int -> V.Vector (Int, Int64, Direction)
getThisRow lSeq rSeq indelCost subCost rowNum prevRow position rowLength prevCost
    | position == (rowLength + 1) = V.empty
    | (position == 0) && (newState /= inDelBit) = V.cons (upValue + indelCost, newState, DownDir) --following in case overlap of inDelBit in leading gaps
        (getThisRow lSeq rSeq indelCost subCost rowNum prevRow (position + 1) rowLength (upValue + indelCost))
    | position == 0 = V.cons (upValue, newState, DownDir)
        (getThisRow lSeq rSeq indelCost subCost rowNum prevRow (position + 1) rowLength upValue)
    | otherwise = --trace ("preRow " ++ show prevRow ++ "row " ++ show rowNum ++ " col " ++ show position 
        --    ++ " trip " ++ show minCost ++ " " ++ show minState ++ " " ++ show minDir)
        V.cons (minCost, minState, minDir) (getThisRow lSeq rSeq indelCost subCost rowNum prevRow (position + 1) rowLength minCost)
        where
            newState = getUnionIntersectionState inDelBit (rSeq V.! (rowNum - 1))
            (upValue, _, _) = prevRow V.! position

            lSeqPos = position - 1 --since first is '-' the index is row/pos - 1
            rSeqRow = rowNum - 1 --since first is '-' the index is row/pos - 1
            leftCost = getOverlapCost prevCost indelCost (lSeq V.! lSeqPos) --need to check for overlap
            downCost = getOverlapCost upValue indelCost (rSeq V.! rSeqRow) --need to check for overlap
            (diagValue, _, _) = prevRow V.! (position - 1)
            intersection = (lSeq V.! lSeqPos) .&. (rSeq V.! rSeqRow)
            union = (lSeq V.! lSeqPos) .|. (rSeq V.! rSeqRow)
            (diagCost, diagState) = getDiagDirCost diagValue intersection union subCost
            (minCost, minState, minDir) = getMinCostDir leftCost downCost diagCost diagState 
                (getUnionIntersectionState inDelBit (lSeq V.! lSeqPos)) (getUnionIntersectionState inDelBit (rSeq V.! rSeqRow)) 




