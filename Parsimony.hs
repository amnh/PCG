{- |
Module      :  Parsimony Optimization functions
Description :  Functions for parimsony cost and vertex optimizations
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
inDelBit = (bit 63) :: Int64 --(bit 63) :: Int64 --set indelBit to 64th bit in Int64
barrierCost = (bit 60) :: Int --really big Int--asssumes 64 bit at least, but can be added to without rolling over.
barrierBit = (bit 63) :: Int64

-- | getPrelim takes bit-coded states and returns cost and prelim state
getPrelim :: BaseChar -> BaseChar -> CharInfo -> (BaseChar, Float)
getPrelim lState rState charInfo =
    --trace ("\nlS " ++ show lState ++ " rS " ++ show rState) (
    if not (activity charInfo) then (VS.singleton (0 :: Int64), 0)
    else if charType charInfo == NonAdd then
        let lS = VS.head lState
            rS = VS.head rState
            intersection = (lS .&. rS) :: Int64 
        in
        if intersection /= 0 then (VS.singleton intersection, 0)
        else 
            let union = VS.singleton ((lS .|. rS) :: Int64)
                cost = (weight charInfo)
            in (union, cost)
    else if charType charInfo == GenSeq then
        let (median, cost) = naive_do lState rState charInfo
            charWeight = (weight charInfo)
        in
        (median, charWeight * cost) 
    else if charType charInfo == NucSeq then
        let (median2, cost2) = ukkonenDO lState rState charInfo
            charWeight = (weight charInfo)
        in
        {-let (lSeq, _, rSeq, _) = setLeftRight lState rState
            (median, cost) = naive_do lState rState charInfo
        in
        if (cost /= cost2) || (median /= median2) then 
            trace ("(" ++ show cost ++ " " ++ show cost2 ++ ")" ++ "\n" ++ show lSeq ++ "\n" ++ show rSeq ++ "\n" ++ show median ++ "\n" ++ show median2 ++ "\n") 
            (median2, charWeight * cost2)
        else-}  (median2, charWeight * cost2)
    else error "Unrecognized/Not implemented character type"

-- | transformFullYShortY take full Y value (if did entire NW matrix) and returns
--short (Ukkonnen Y) given Y, Y length and row numbera
--remove error when working--overhead
transformFullYShortY :: Int -> Int -> Int -> Int
transformFullYShortY currentY rowNumber maxGap =
    let transformY = currentY - (max 0 (rowNumber - maxGap - 1))
    in
    if (transformY < 0) then error (show currentY ++ " " ++ show rowNumber ++ " " ++ show maxGap ++ " Impossible negative value for transfomred Y")
    else transformY

-- | maxShortY is teh maximum value for short Y in Ukkonen usually 2*maxGap
--untill near bottom of matrix when running up against the full Y length
--assumes length X /> length Y.
maxShortY :: Int -> Int -> Int -> Int
maxShortY maxFullY rowNumber maxGap =
    let leftPart = min rowNumber maxGap
        rightPart = min maxGap (maxFullY - rowNumber)
    in
    leftPart + rightPart

-- | setLeftRight returns sequence that is longer first,
--shorter second
setLeftRight :: BaseChar -> BaseChar -> (BaseChar, Int, BaseChar, Int)
setLeftRight inL inR = 
        if VS.length inL < VS.length inR then (inR, VS.length inR, inL, VS.length inL)
        else if VS.length inL > VS.length inR then (inL, VS.length inL, inR, VS.length inR)
        else 
            let outL = max inL inR
                outR = min inL inR 
            in
            (outL, VS.length outL, outR, VS.length outR)

-- | ukkonenCore core functions of Ukkonen to allow for recursing with maxGap
--doubled if not large enough (returns Nothing)  
ukkonenCore :: BaseChar -> Int -> BaseChar -> Int -> Int -> Int -> Int -> (BaseChar, Float)
ukkonenCore lSeq lLength rSeq rLength maxGap indelCost subCost =  
    let firstRow = getFirstRowUkkonen indelCost lLength 0 0 lSeq maxGap
        nwMatrix = V.cons firstRow (getRowsUkkonen lSeq rSeq indelCost subCost 1 firstRow maxGap)
        (cost, _, _) = V.last (V.last nwMatrix) -- V.! rLength) --V.! (transformFullYShortY lLength rLength  maxGap) --fix for offset
        medianTest = tracebackUkkonen nwMatrix rLength lLength maxGap 0 0
    in
    --trace ("mh " ++ show (VS.head medianTest)) (
    if  (VS.last medianTest) /= (0 :: Int64) then 
        --trace (show nwMatrix) 
        (VS.reverse medianTest, fromIntegral cost)
    else --trace ("Going back!! " ++ show cost) 
        ukkonenCore lSeq lLength rSeq rLength (2 * maxGap) indelCost subCost
    

--FOR both DO's  lseq is a row, acrosss so num columns = length of lseq
--There are rseq rows
-- | UkkonenDO takes two input sequences and returns median sequence and cost
--only 1:1 for now. Uses Ukkonen's space/time saving algorithm
--need to make sure Left/Right and diag/ins/del orders consistent and with
--POY4/5
--lseq > rseq appeard more efficient--could be wrong
--move to C via FFI
--Still occasional error in cost and median (disagreement) show in Chel.seq
ukkonenDO :: BaseChar -> BaseChar -> CharInfo -> (BaseChar, Float)
ukkonenDO inlSeq inrSeq charInfo =
    if VS.null inlSeq then (inrSeq, 0)
    else if VS.null inrSeq then (inlSeq, 0)
    else 
        let indelCost = 1
            subCost = 1
            --this for left right constant--want longer in left for Ukkonnen
            (lSeq, lLength, rSeq, rLength) = setLeftRight inlSeq inrSeq
            maxGap = 1 + lLength - rLength  --10000 :: Int --holder lseq - rSeq + 1
            (median, cost) = ukkonenCore lSeq lLength rSeq rLength maxGap indelCost subCost
            --firstRow = getFirstRowUkkonen indelCost lLength 0 0 lSeq maxGap
            --nwMatrix = V.cons firstRow (getRowsUkkonen lSeq rSeq indelCost subCost 1 firstRow maxGap)
            --(cost, _, _) = (nwMatrix V.! rLength) V.! (transformFullYShortY lLength rLength  maxGap) --fix for offset
            --median = trace ("getting median " ++ show cost) VS.reverse (fromMaybe VS.empty (tracebackUkkonen nwMatrix rLength lLength maxGap 0 0))
        in
        --trace ("\nCost/median " ++ show cost ++ "->" ++ show median)
        --(median, fromIntegral cost)
        (median, cost)

-- | tracebackUkkonen creates REVERSE mediian from nwMatrix, reverse to make tail
--recusive, for Ukkonen space/time saving offsets
--need to count gaps in traceback for threshold/barrier stuff
--CHANGE TO MAYBE (VS.Vector Int64) FOR BARRIER CHECK
tracebackUkkonen :: V.Vector (V.Vector (Int, Int64, Direction)) -> Int -> Int -> Int -> Int -> Int -> VS.Vector Int64
tracebackUkkonen nwMatrix posR posL maxGap rInDel lInDel =
        --trace ("psLR " ++ show posR ++ " " ++ show posL ++ " Left " ++ show lInDel ++ " Right " ++ show rInDel ++ " maxGap " ++ show maxGap) (
        if (rInDel  > (maxGap - 2)) || (lInDel > (maxGap - 2)) then VS.singleton (0 :: Int64)
        else if posL == 0 && posR == 0 then VS.empty
        else 
            --trace ("TB " ++ show posL ++ " " ++ show posR) (
            let (_, state, direction) = (nwMatrix V.! posR) V.! (transformFullYShortY posL posR  maxGap) --(transformFullYShortY posL posR maxGap)
            in
                --trace ("state " ++ show state ++ " dir " ++ show direction) (
                if (state /= inDelBit) then
                    if direction == LeftDir then (VS.cons  state (tracebackUkkonen nwMatrix (posR) (posL - 1) maxGap rInDel (lInDel + 1))) 
                    else if direction == DownDir then (VS.cons state (tracebackUkkonen nwMatrix (posR - 1) (posL) maxGap (rInDel + 1) lInDel))  
                    else (VS.cons state (tracebackUkkonen nwMatrix (posR - 1) (posL - 1) maxGap rInDel lInDel))
                else 
                    if direction == LeftDir then (tracebackUkkonen nwMatrix (posR) (posL - 1) maxGap rInDel (lInDel + 1)) 
                    else if direction == DownDir then (tracebackUkkonen nwMatrix (posR - 1) (posL) maxGap (rInDel + 1) lInDel)  
                    else (tracebackUkkonen nwMatrix (posR - 1) (posL - 1) maxGap rInDel lInDel) 
            --)--)

-- | getFirstRowUkkonen initializes first row of NW-Ukkonen matrix
getFirstRowUkkonen :: Int -> Int -> Int -> Int -> BaseChar -> Int -> V.Vector (Int, Int64, Direction)
getFirstRowUkkonen indelCost rowLength position prevCost lSeq  maxGap = 
    --trace ("row 0 pos " ++ show position ++ "/" ++ show (maxShortY rowLength 0 maxGap) ++ " rowLength " ++ show rowLength ++ " maxGap " ++ show maxGap ++ " lseq " ++ show lSeq) (
    if position == rowLength  + 1 then V.empty
    else if position == (maxGap + 1) then V.singleton (barrierCost, barrierBit, LeftDir) 
    else
        if position == 0 then V.cons (0, inDelBit, DiagDir) (getFirstRowUkkonen indelCost rowLength (position + 1) 0 lSeq maxGap) 
        else
            let newCost = prevCost + indelCost
                newState = getUnionIntersectionState inDelBit (lSeq VS.! (position - 1))
            in
            --trace ("FRC " ++ show newCost)
            if (newState /= inDelBit) then --if there was no inDel overlap between states
                V.cons (newCost, newState, LeftDir) 
                    (getFirstRowUkkonen  indelCost rowLength (position + 1) newCost lSeq maxGap)
            else                           --indel in both states so no cost
                V.cons (prevCost, newState, LeftDir)
                    (getFirstRowUkkonen  indelCost rowLength (position + 1) prevCost lSeq maxGap)
   --) 

-- | getRowUkkonen starts at second row (=1) and creates each row in turn--Ukkonen
getRowsUkkonen :: BaseChar -> BaseChar -> Int -> Int -> Int -> V.Vector (Int, Int64, Direction) -> Int -> V.Vector (V.Vector (Int, Int64, Direction))
getRowsUkkonen lSeq rSeq indelCost subCost rowNum prevRow maxGap =
    if rowNum == ((VS.length rSeq) + 1) then V.empty
    else 
        let startPosition = max 0 (rowNum - maxGap) --check for left barriers 
            thisRowZero =  getThisRowUkkonen lSeq rSeq indelCost subCost rowNum prevRow startPosition (VS.length lSeq) 0 maxGap
            thisRowNonZero = V.cons (barrierCost, barrierBit, DownDir) (getThisRowUkkonen lSeq rSeq indelCost subCost rowNum prevRow startPosition  (VS.length lSeq) barrierCost maxGap )
        in
        if startPosition == 0 then 
            --trace ("Row " ++ show rowNum ++ " of " ++ show (VS.length rSeq) ++ " starts " ++ show startPosition ++ ":" ++ show thisRowZero) (
                V.cons thisRowZero (getRowsUkkonen lSeq rSeq indelCost subCost (rowNum + 1) thisRowZero maxGap) 
                --)
        else
            --trace ("Row " ++ show rowNum ++ " of " ++ show (VS.length rSeq) ++" starts " ++ show startPosition ++ ":" ++ show thisRowNonZero) (
            V.cons thisRowNonZero (getRowsUkkonen lSeq rSeq indelCost subCost (rowNum + 1) thisRowNonZero maxGap)
            --)

-- | getThisRowUkkonen takes sequences and parameters with row number and make a non-first
--row--Ukkonen
getThisRowUkkonen :: BaseChar -> BaseChar -> Int -> Int -> Int ->  V.Vector (Int, Int64, Direction) -> Int -> Int -> Int -> Int -> V.Vector (Int, Int64, Direction)
getThisRowUkkonen lSeq rSeq indelCost subCost rowNum prevRow position rowLength prevCost maxGap =
    if  position ==  rowLength  + 1 then V.empty
    else if position == (rowNum + maxGap + 1) then V.singleton (barrierCost, barrierBit, LeftDir)
    else if position == 0 then
        let newState = getUnionIntersectionState inDelBit (rSeq VS.! (rowNum - 1))
            (upValue, _, _) = prevRow V.! position 
        in --following in case overlap of inDelBit in leading gaps
        if (newState /= inDelBit) then
            V.cons (upValue + indelCost, newState, DownDir) 
                (getThisRowUkkonen lSeq rSeq indelCost subCost rowNum prevRow (position + 1) rowLength (upValue + indelCost) maxGap)
        else 
            V.cons (upValue, newState, DownDir)
                (getThisRowUkkonen lSeq rSeq indelCost subCost rowNum prevRow (position + 1) rowLength upValue maxGap)
    else 
        let lSeqPos = position - 1 --since first is '-' the index is row/pos - 1
            rSeqRow = rowNum - 1 --since first is '-' the index is row/pos - 1
            leftCost = getOverlapCost prevCost indelCost (lSeq VS.! lSeqPos) --need to check for overlap
            (upValue, _, _) = prevRow V.! (transformFullYShortY  position (rowNum - 1) maxGap)
            downCost = getOverlapCost upValue indelCost (rSeq VS.! rSeqRow) --need to check for overlap
            (diagValue, _, _) = prevRow V.! (transformFullYShortY  (position - 1) (rowNum - 1) maxGap)
            intersection = (lSeq VS.! lSeqPos) .&. (rSeq VS.! rSeqRow)
            union = (lSeq VS.! lSeqPos) .|. (rSeq VS.! rSeqRow)
            (diagCost, diagState) = getDiagDirCost diagValue intersection union subCost
            (minCost, minState, minDir) = getMinCostDir leftCost downCost diagCost diagState 
                (getUnionIntersectionState inDelBit (lSeq VS.! lSeqPos)) (getUnionIntersectionState inDelBit (rSeq VS.! rSeqRow)) 
        in
        --trace ("preRow " ++ show prevRow ) -- ++ "row " ++ show rowNum ++ " col " ++ show position 
        --    ++ " trip " ++ show minCost ++ " " ++ show minState ++ " " ++ show minDir)
        V.cons (minCost, minState, minDir) (getThisRowUkkonen lSeq rSeq indelCost subCost rowNum prevRow (position + 1) rowLength minCost maxGap)        

-- | naive_do takes two input sequences and returns median sequence and cost 
--based on charInfo-1:1 for now
--to do:
--      different costs
--      left/right based on name
--      Ukkonnen
--      C via FFI
--      Affine
naive_do :: BaseChar -> BaseChar -> CharInfo -> (BaseChar, Float)
naive_do inlSeq inrSeq charInfo =
    if VS.null inlSeq then (inrSeq, 0)
    else if VS.null inrSeq then (inlSeq, 0)
    else 
        let indelCost = 1
            subCost = 1
            --this for left right constant--want longer in left for Ukkonnen
            (lSeq, lLength, rSeq, rLength) = setLeftRight inlSeq inrSeq
            --lSeq = max inlSeq inrSeq
            --rSeq = min inlSeq inrSeq
            --lLength = VS.length lSeq
            --rLength = VS.length rSeq
            firstRow = getFirstRow indelCost lLength 0 0 lSeq
            nwMatrix = V.cons firstRow (getRows lSeq rSeq indelCost subCost 1 firstRow)
            (cost, _, _) = (nwMatrix V.! rLength) V.! lLength
            median = VS.reverse (traceback nwMatrix (VS.length rSeq) (VS.length lSeq))
        in
        --trace ("NW: " ++ show nwMatrix ++ "\nCost/median " ++ show cost ++ "->" ++ show median)
        (median, fromIntegral cost)

-- | traceback creates REVERSE mediian from nwMatrix, reverse to make tail
--recusive
traceback :: V.Vector (V.Vector (Int, Int64, Direction)) -> Int -> Int -> VS.Vector Int64
traceback nwMatrix posL posR =
        --trace ("psLR " ++ show posL ++ " " ++ show posR) (
        if posL == 0 && posR == 0 then VS.empty
        else 
            let (_, state, direction) = (nwMatrix V.! posL ) V.! posR
            in
                if (state /= inDelBit) then
                    if direction == LeftDir then VS.cons  state (traceback nwMatrix (posL) (posR - 1)) 
                    else if direction == DownDir then VS.cons state (traceback nwMatrix (posL - 1) (posR) )  
                    else VS.cons state (traceback nwMatrix (posL - 1) (posR - 1))
                else 
                    if direction == LeftDir then (traceback nwMatrix (posL) (posR - 1)) 
                    else if direction == DownDir then (traceback nwMatrix (posL - 1) (posR) )  
                    else (traceback nwMatrix (posL - 1) (posR - 1)) 

-- | getFirstRow initializes foirst row of NW matrix
getFirstRow :: Int -> Int -> Int -> Int -> BaseChar -> V.Vector (Int, Int64, Direction)
getFirstRow indelCost rowLength position prevCost lSeq  = 
    if position == (rowLength + 1) then V.empty
    else
        if position == 0 then V.cons (0, inDelBit, DiagDir) (getFirstRow indelCost rowLength (position + 1) 0 lSeq) 
        else
            let newCost = prevCost + indelCost
                newState = getUnionIntersectionState inDelBit (lSeq VS.! (position - 1))
            in
            --trace ("FRC " ++ show newCost)
            if (newState /= inDelBit) then --if there was no inDel overlap between states
                V.cons (newCost, newState, LeftDir) 
                    (getFirstRow  indelCost rowLength (position + 1) newCost lSeq)
            else                           --indel in both states so no cost
                V.cons (prevCost, newState, LeftDir)
                    (getFirstRow  indelCost rowLength (position + 1) prevCost lSeq)

-- | getRow starts at second row (=1) and cretes each row in turn
getRows :: BaseChar -> BaseChar -> Int -> Int -> Int -> V.Vector (Int, Int64, Direction) -> V.Vector (V.Vector (Int, Int64, Direction))
getRows lSeq rSeq indelCost subCost rowNum prevRow =
    if rowNum == ((VS.length rSeq) + 1) then V.empty
    else 
        let thisRow =  getThisRow lSeq rSeq indelCost subCost rowNum prevRow 0 (VS.length lSeq) 0 
        in
        --trace ("Row " ++ show rowNum)
        V.cons thisRow (getRows lSeq rSeq indelCost subCost (rowNum + 1) thisRow) 

-- | getDiagDirCost takes union intersection and state to get diagonla sub or no-sub
--cost
getDiagDirCost :: Int -> Int64 -> Int64 -> Int -> (Int, Int64)
getDiagDirCost upLeftDirCost intersection union subCost =
    --trace ("DiagCost " ++ show upLeftDirCost ++ " int " ++ show intersection ++ " union " ++ show union) (
    if intersection /= 0 then (upLeftDirCost, intersection)
    else (upLeftDirCost + subCost, union)

-- | getUnionIntersection
getUnionIntersectionState :: Int64 -> Int64 -> Int64
getUnionIntersectionState lState rState =
    let intersection = (lState .&. rState) :: Int64
    in
    if intersection /= 0 then intersection
    else (lState .|. rState) :: Int64

-- | getMinCostDir takes costs and states of three directins and returns min cost,
--directin, and state
--ORDER diag, down, left so same as POY4-5.
getMinCostDir :: Int -> Int -> Int -> Int64 -> Int64 -> Int64 -> (Int, Int64, Direction)
getMinCostDir leftCost downCost diagCost diagState leftState downState =
    let minValue = minimum [leftCost, downCost, diagCost]
    in
    --trace ("costs " ++ show leftCost ++ " " ++ show downCost ++ " " ++ show diagCost ++ " -> " ++ show minValue) (
    if diagCost == minValue then (diagCost, diagState, DiagDir)
    else if downCost == minValue then (downCost, downState, DownDir)
    else (leftCost, leftState, LeftDir)
    --else error "Error in getMinCost"
    {-else (downCost, downState, DownDir)
    else (leftCost, leftState, LeftDir)
    if leftCost == minValue then (leftCost, leftState, LeftDir)
    else if downCost == minValue then (downCost, downState, DownDir)
    else (diagCost, diagState, DiagDir)
    -}
    
-- | getOverlapCost cheks for ovelap in gap so if indel, but opossite a gap
--ambiguity--there is no cost
getOverlapCost :: Int -> Int -> Int64 -> Int
getOverlapCost preCost indelCost oppositeState =
    --trace("bits " ++ show oppositeState ++ " overAND " ++ show ((.&.) oppositeState inDelBit) ++ " control " ++ show ((.&.) inDelBit inDelBit)) ( 
    --if preCost == barrierCost then barrierCost 
    --else 
    if (.&.) oppositeState inDelBit == (0 :: Int64) then preCost + indelCost
    else preCost
    --)

-- | getThisRow takes sequences and parameters with row number and make a non-first
--row
getThisRow :: BaseChar -> BaseChar -> Int -> Int -> Int ->  V.Vector (Int, Int64, Direction) -> Int -> Int -> Int -> V.Vector (Int, Int64, Direction)
getThisRow lSeq rSeq indelCost subCost rowNum prevRow position rowLength prevCost =
    if position == (rowLength + 1) then V.empty
    else if position == 0 then
        let newState = getUnionIntersectionState inDelBit (rSeq VS.! (rowNum - 1))
            (upValue, _, _) = prevRow V.! position
        in --following in case overlap of inDelBit in leading gaps
        if (newState /= inDelBit) then
            V.cons (upValue + indelCost, newState, DownDir) 
                (getThisRow lSeq rSeq indelCost subCost rowNum prevRow (position + 1) rowLength (upValue + indelCost))
        else 
            V.cons (upValue, newState, DownDir)
                (getThisRow lSeq rSeq indelCost subCost rowNum prevRow (position + 1) rowLength upValue)
    else 
        let lSeqPos = position - 1 --since first is '-' the index is row/pos - 1
            rSeqRow = rowNum - 1 --since first is '-' the index is row/pos - 1
            leftCost = getOverlapCost prevCost indelCost (lSeq VS.! lSeqPos) --need to check for overlap
            (upValue, _, _) = prevRow V.! position 
            downCost = getOverlapCost upValue indelCost (rSeq VS.! rSeqRow) --need to check for overlap
            (diagValue, _, _) = prevRow V.! (position - 1)
            intersection = (lSeq VS.! lSeqPos) .&. (rSeq VS.! rSeqRow)
            union = (lSeq VS.! lSeqPos) .|. (rSeq VS.! rSeqRow)
            (diagCost, diagState) = getDiagDirCost diagValue intersection union subCost
            (minCost, minState, minDir) = getMinCostDir leftCost downCost diagCost diagState 
                (getUnionIntersectionState inDelBit (lSeq VS.! lSeqPos)) (getUnionIntersectionState inDelBit (rSeq VS.! rSeqRow)) 
        in
        --trace ("preRow " ++ show prevRow ++ "row " ++ show rowNum ++ " col " ++ show position 
        --    ++ " trip " ++ show minCost ++ " " ++ show minState ++ " " ++ show minDir)
        V.cons (minCost, minState, minDir) (getThisRow lSeq rSeq indelCost subCost rowNum prevRow (position + 1) rowLength minCost)        



