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


import           Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.Internal hiding (Direction)
import           Bio.Character.Encodable
import           Data.Bits
--import           Data.Foldable         (minimumBy)
--import           Data.Key              ((!))
--import           Data.Matrix.NotStupid (Matrix, matrix, nrows, ncols)
import           Data.MonoTraversable
import           Data.Ord
import           Data.Semigroup
import           Data.Vector           (Vector)
import qualified Data.Vector      as V
import           Data.Vector.Instances ()

--import Debug.Trace (trace)


data Ribbon a
   = Ribbon
   { height :: Word 
   , width  :: Word -- width >= height
   , radius :: Word
   , linear :: Vector a
   } deriving (Eq)


data Direction = LeftDir | DownDir | DiagDir
    deriving (Read, Show, Eq)


{-
inDelBit    = bit 63 :: Int64 --(bit 63) :: Int64 --set indelBit to 64th bit in Int64
barrierCost = bit 60 :: Int --really big Int--asssumes 64 bit at least, but can be added to without rolling over.
barrierBit  = bit 63 :: Int64
-}


{- |
 - The Ukkonen code from the prototype codebase
 -}


barrierCost :: Word
barrierCost = maxBound


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
ukkonenDO
  :: (DOCharConstraint s, Show s)
  => s
  -> s
  -> OverlapFunction (Element s)
  -> (Word, s, s, s, s)
--ukkonenDO inlSeq inrSeq _ | trace ("calling ukonnen DO with seqs " <> show inlSeq <> show inrSeq) False = undefined
ukkonenDO char1 char2 costStruct
  | lesserLen <= 4 = naiveDOMemo char1 char2 costStruct
  | otherwise      = handleMissingCharacter char1 char2 result
  where
    indelCost = 1
    subCost   = 1
    maxGap    = 1 + longerLen - lesserLen  --10000 :: Int --holder lseq - rSeq + 1
    longerLen = olength longer
    lesserLen = olength lesser
    (swapped, longer, lesser) = measureCharacters char1 char2
    --this for left right constant--want longer in left for Ukkonnen
    (median, cost, medGap, alignLeft, alignRight) = ukkonenCore longer longerLen lesser lesserLen maxGap indelCost subCost
    result
      | swapped   = (median, cost, medGap, alignRight, alignLeft )
      | otherwise = (median, cost, medGap, alignLeft , alignRight)

    -- |
    -- Returns sequence that is longer first, shorter second.
    -- Handles equal length by not swapping characters.
    measureCharacters :: MonoFoldable s => s -> s -> (Bool, s, s)
    measureCharacters lhs rhs =
        case comparing olength lhs rhs of
          EQ -> (False, lhs, rhs)
          GT -> (False, lhs, rhs)
          LT -> ( True, rhs, lhs)


-- |
-- ukkonenCore core functions of Ukkonen to allow for recursing with maxGap
-- doubled if not large enough (returns Nothing)  
ukkonenCore
  :: (DOCharConstraint s, Show s)
  => s
  -> Int
  -> s
  -> Int
  -> Int
  -> Word
  -> Word
  -> (Word, s, s, s, s)
--ukkonenCore _ _ _ _ _ _ _ | trace "ukkonenCore" False = undefined
ukkonenCore lSeq lLength rSeq rLength maxGap indelCost subCost
  | headEx gappedMedian /= 0 = (cost, ungappedMedian, gappedMedian, lhsAlignment, rhsAlignment)
  | otherwise                = --trace ("Going back!! " <> show cost) 
                        ukkonenCore lSeq lLength rSeq rLength (2 * maxGap) indelCost subCost
  where
    firstRow       = getFirstRowUkkonen indelCost lLength 0 0 lSeq maxGap
    nwMatrix       = V.cons firstRow (getRowsUkkonen lSeq rSeq indelCost subCost 1 firstRow maxGap)
--    median       = V.filter (/= gap) medianGap
    gap            = gapOfStream lSeq
    gappedMedian   = constructDynamic medianGap
    ungappedMedian = constructDynamic $ V.filter (/= gap) medianGap
    lhsAlignment   = constructDynamic alignLeft
    rhsAlignment   = constructDynamic alignRight
    (cost, _, _)   = V.last (V.last nwMatrix) -- V.! rLength) --V.! (transformFullYShortY lLength rLength  maxGap) --fix for offset
    (medianGap, alignLeft, alignRight) = V.unzip3 $ V.reverse (tracebackUkkonen nwMatrix lSeq rSeq rLength lLength maxGap 0 0)


-- |
-- transformFullYShortY take full Y value (if did entire NW matrix) and returns
-- short (Ukkonnen Y) given Y, Y length and row number
-- remove error when working--overhead
transformFullYShortY :: Int -> Int -> Int -> Int
transformFullYShortY currentY rowNumber maxGap
  | transformY < 0 = error (show currentY <> " " <> show rowNumber <> " " <> show maxGap <> " Impossible negative value for transfomred Y")
  | otherwise = transformY
  where
    transformY = currentY - max 0 (rowNumber - maxGap - 1)


-- |
-- tracebackUkkonen creates REVERSE mediian from nwMatrix, reverse to make tail
-- recusive, for Ukkonen space/time saving offsets
-- need to count gaps in traceback for threshold/barrier stuff
-- CHANGE TO MAYBE (V.Vector Int64) FOR BARRIER CHECK
tracebackUkkonen
  :: (DOCharConstraint s, Show s)
  => V.Vector (V.Vector (Word, Element s, Direction))
  -> s
  -> s
  -> Int
  -> Int
  -> Int
  -> Int
  -> Int
  -> V.Vector (Element s, Element s, Element s)
--tracebackUkkonen _nwMatrix inlSeq inrSeq posR posL _ _ _ | trace ("tracebackUkkonen " <> show posR <> show posL <> show inlSeq <> show inrSeq) False = undefined
tracebackUkkonen nwMatrix inlSeq inrSeq posR posL maxGap rInDel lInDel
--trace ("psLR " <> show posR <> " " <> show posL <> " Left " <> show lInDel <> " Right " <> show rInDel <> " maxGap " <> show maxGap) (
  | (rInDel  > (maxGap - 2)) || (lInDel > (maxGap - 2)) = V.singleton (0, 0, 0)
  | posL <= 0 && posR <= 0 = {- trace "not y" -} V.empty
  | otherwise = --trace "y" $
      let y | direction == LeftDir = V.cons (state,                             gap, inrSeq `indexStream` (posR - 1)) (tracebackUkkonen nwMatrix inlSeq inrSeq  posR      (posL - 1) maxGap  rInDel     (lInDel + 1))
            | direction == DownDir = V.cons (state, inlSeq `indexStream` (posL - 1),                             gap) (tracebackUkkonen nwMatrix inlSeq inrSeq (posR - 1)  posL      maxGap (rInDel + 1) lInDel     )  
            | otherwise            = V.cons (state, inlSeq `indexStream` (posL - 1), inrSeq `indexStream` (posR - 1)) (tracebackUkkonen nwMatrix inlSeq inrSeq (posR - 1) (posL - 1) maxGap  rInDel      lInDel     )
      in {- traceShowId -} y
  where
    gap = gapOfStream inlSeq
    (_, state, direction) = (nwMatrix V.! posR) V.! transformFullYShortY posL posR  maxGap --(transformFullYShortY posL posR maxGap)


-- |
-- getFirstRowUkkonen initializes first row of NW-Ukkonen matrix
getFirstRowUkkonen
  :: (DOCharConstraint s, Show s)
  => Word
  -> Int
  -> Int
  -> Word
  -> s
  -> Int
  -> V.Vector (Word, Element s, Direction)
--getFirstRowUkkonen _ rowLen position _ lSeq _ | trace ("getFirstRowUkkonen " <> show lSeq <> show position <> show rowLen) False = undefined
getFirstRowUkkonen indelCost rowLength position prevCost lSeq maxGap
 --trace ("row 0 pos " <> show position <> "/" <> show (maxShortY rowLength 0 maxGap) <> " rowLength " <> show rowLength <> " maxGap " <> show maxGap <> " lseq " <> show lSeq)
  | position == rowLength + 1 = V.empty
  | position == maxGap    + 1 = V.singleton (barrierCost, gap, LeftDir) 
  | position == 0             = V.cons (0, gap, DiagDir) (getFirstRowUkkonen indelCost rowLength (position + 1) 0 lSeq maxGap) 
  | otherwise                 = --trace ("FRC " <> show newCost)
      let y | (newState /= gap) = --if there was no inDel overlap between states
                V.cons ( newCost, newState, LeftDir) (getFirstRowUkkonen indelCost rowLength (position + 1)  newCost lSeq maxGap)
            | otherwise = --indel in both states so no cost
                V.cons (prevCost, newState, LeftDir) (getFirstRowUkkonen indelCost rowLength (position + 1) prevCost lSeq maxGap)
      in y
  where
    gap      = gapOfStream lSeq
    newCost  = prevCost + indelCost
    newState = getUnionIntersectionState gap $ lSeq `indexStream` (position - 1)


-- |
-- getRowUkkonen starts at second row (=1) and creates each row in turn--Ukkonen
getRowsUkkonen
  :: (DOCharConstraint s, Show s)
  => s
  -> s
  -> Word
  -> Word
  -> Int
  -> V.Vector (Word, Element s, Direction)
  -> Int
  -> V.Vector (V.Vector (Word, Element s, Direction))
--getRowsUkkonen _ _ _ _ _ _ _ | trace "getRowsUkkonen" False = undefined
getRowsUkkonen lSeq rSeq indelCost subCost rowNum prevRow maxGap
  | rowNum == (olength rSeq + 1) = V.empty
  | startPosition == 0 = --trace ("Row " <> show rowNum <> " of " <> show (V.length rSeq) <> " starts " <> show startPosition <> ":" <> show thisRowZero) (
                V.cons thisRowZero (getRowsUkkonen lSeq rSeq indelCost subCost (rowNum + 1) thisRowZero maxGap) --)
  | otherwise = --trace ("Row " <> show rowNum <> " of " <> show (V.length rSeq) <>" starts " <> show startPosition <> ":" <> show thisRowNonZero) (
        V.cons thisRowNonZero (getRowsUkkonen lSeq rSeq indelCost subCost (rowNum + 1) thisRowNonZero maxGap) --)
  where 
    gap            = gapOfStream lSeq
    startPosition  = max 0 (rowNum - maxGap) --check for left barriers 
    thisRowZero    = getThisRowUkkonen lSeq rSeq indelCost subCost rowNum prevRow startPosition (olength lSeq) 0 maxGap
    thisRowNonZero = V.cons (barrierCost, gap, DownDir) (getThisRowUkkonen lSeq rSeq indelCost subCost rowNum prevRow startPosition (olength lSeq) barrierCost maxGap)


-- |
-- getThisRowUkkonen takes sequences and parameters with row number and make a non-first
-- row--Ukkonen
getThisRowUkkonen
  :: (DOCharConstraint s, Show s)
  => s
  -> s
  -> Word
  -> Word
  -> Int
  -> V.Vector (Word, Element s, Direction)
  -> Int
  -> Int
  -> Word
  -> Int
  -> V.Vector (Word, Element s, Direction)
getThisRowUkkonen lSeq rSeq indelCost subCost rowNum prevRow position rowLength prevCost maxGap
  | position == rowLength  + 1      = V.empty
  | position == rowNum + maxGap + 1 = V.singleton (barrierCost, gap, LeftDir)
  | position == 0 =
      let x | (newState /= gap) =
                V.cons (upValue + indelCost, newState, DownDir) (getThisRowUkkonen lSeq rSeq indelCost subCost rowNum prevRow (position + 1) rowLength (upValue + indelCost) maxGap)
            | otherwise = 
                V.cons (upValue, newState, DownDir) (getThisRowUkkonen lSeq rSeq indelCost subCost rowNum prevRow (position + 1) rowLength upCost maxGap)
                where
                    newState = getUnionIntersectionState gap (rSeq `indexStream` (rowNum - 1))
                    (upCost, _, _) = prevRow V.! position
      in x
  | otherwise = V.cons (minCost, minState, minDir) (getThisRowUkkonen lSeq rSeq indelCost subCost rowNum prevRow (position + 1) rowLength minCost maxGap)
  where
    gap          = gapOfStream lSeq
    lSeqPos      = position - 1 --since first is '-' the index is row/pos - 1
    rSeqRow      = rowNum   - 1 --since first is '-' the index is row/pos - 1
    leftCost     = getOverlapCost prevCost indelCost gap (lSeq `indexStream` lSeqPos) --need to check for overlap
    downCost     = getOverlapCost upValue  indelCost gap (rSeq `indexStream` rSeqRow) --need to check for overlap
    intersection = (lSeq `indexStream` lSeqPos) .&. (rSeq `indexStream` rSeqRow)
    union        = (lSeq `indexStream` lSeqPos) .|. (rSeq `indexStream` rSeqRow)
    (diagCost, diagState) = getDiagDirCost diagValue intersection union subCost
    (  upValue,       _,      _) = prevRow V.! transformFullYShortY  position (rowNum - 1) maxGap
    (diagValue,       _,      _) = prevRow V.! transformFullYShortY  (position - 1) (rowNum - 1) maxGap
    ( minCost, minState, minDir) = getMinCostDir
                                     leftCost
                                     downCost
                                     diagCost
                                     diagState 
                                     (getUnionIntersectionState gap (lSeq `indexStream` lSeqPos))
                                     (getUnionIntersectionState gap (rSeq `indexStream` rSeqRow)) 


getUnionIntersectionState :: Bits b => b -> b -> b
getUnionIntersectionState lState rState
  | intersection /= zeroBits = intersection
  | otherwise                = lState .|. rState
  where
    intersection = lState .&. rState


-- |
-- getOverlapCost cheks for ovelap in gap so if indel, but opossite a gap
-- ambiguity--there is no cost
getOverlapCost :: (Bits b, Num n) => n -> n -> b -> b -> n
getOverlapCost preCost indelCost oppositeState gap
    --trace("bits " <> show oppositeState <> " overAND " <> show ((.&.) oppositeState inDelBit) <> " control " <> show ((.&.) inDelBit inDelBit))
  | oppositeState .&. gap == zeroBits = preCost + indelCost
  | otherwise                         = preCost


-- |
-- getDiagDirCost takes union intersection and state to get diagonla sub or no-sub
--cost
getDiagDirCost :: (Bits b, Num n) => n -> b -> b -> n -> (n, b)
getDiagDirCost upLeftDirCost intersection union subCost --trace ("DiagCost " <> show upLeftDirCost <> " int " <> show intersection <> " union " <> show union) (
  | intersection /= zeroBits = (upLeftDirCost          , intersection)
  | otherwise                = (upLeftDirCost + subCost,        union)


-- |
-- getMinCostDir takes costs and states of three directins and returns min cost,
-- directin, and state
-- ORDER diag, down, left so same as POY4-5.
getMinCostDir :: Ord v
              => v -> v -> v
              -> s -> s -> s
              -> (v, s, Direction)
getMinCostDir leftCost downCost diagCost diagState leftState downState
  | diagCost == minValue = (diagCost, diagState, DiagDir)
  | downCost == minValue = (downCost, downState, DownDir)
  | otherwise            = (leftCost, leftState, LeftDir)
  where
    minValue = minimum [leftCost, downCost, diagCost] 


-- |
-- firstOfThree takes a triple and returns first member
firstOfThree :: (a, b, c) -> a
firstOfThree  (x, _, _) = x


-- |
-- secondOfThree takes a triple and returns second member
secondOfThree :: (a, b, c) -> b
secondOfThree (_, x, _) = x


-- |
-- thirdOfThree takes a triple and returns third member
thirdOfThree :: (a, b, c) -> c
thirdOfThree  (_, _, x) = x
