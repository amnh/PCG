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


import           Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.Internal -- hiding (Direction)
import           Bio.Character.Encodable
import           Data.Bits
import           Data.Foldable
import           Data.Key                 ((!))
import           Data.List                (intercalate)
--import           Data.Matrix.NotStupid   (Matrix, matrix, nrows, ncols)
import           Data.MonoTraversable
--import           Data.Ord
import           Data.Semigroup
import           Data.Vector              (Vector)
import qualified Data.Vector         as V
import           Data.Vector.Instances    ()
import           Numeric.Extended.Natural
import Debug.Trace

{-
data Ribbon a
   = Ribbon
   { height :: Word 
   , width  :: Word -- width >= height
   , radius :: Word
   , linear :: Vector a
   } deriving (Eq)
-}


{-
data Direction = LeftDir | DownDir | DiagDir
    deriving (Read, Show, Eq)
-}


-- |
-- This internal type used for computing the alignment cost. This type has an
-- "infinity" value that is conviently used for the barrier costs. The cost is
-- strictly non-negative, and possibly infinite.
type Cost = ExtendedNatural


{- |
 - The Ukkonen code from the prototype codebase
 -}


barrierCost :: Cost
barrierCost = infinity


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
    -- TODO: Do not use indel/sub costs, use the supplied overlap function.
    indelCost = 1
    subCost   = 1
    
    maxGap    = 1 + longerLen - lesserLen  --10000 :: Int --holder lseq - rSeq + 1

    -- We determine which character is longer and whether or not the alignment
    -- results will later need to be swapped. This is necessary because we assume
    -- an invariant of the longer character being on the left column and the
    -- shorter character on the top row.
    (swapped, longer, lesser) = measureCharacters char1 char2
    longerLen = olength longer
    lesserLen = olength lesser

    -- This for left right constant--want longer in left for Ukkonnen
    -- Perform the Ukkonen work once ensuring invariants are applied
    (extendedCost, ungappedMedians, gappedMedian, alignLeft, alignRight) = ukkonenCore longer longerLen lesser lesserLen maxGap indelCost subCost
    -- Conditionally swap resulting alignments if the inputs were swapped
    (alignedChar1, alignedChar2)
      | swapped   = (alignRight, alignLeft )
      | otherwise = (alignLeft , alignRight)

    -- Extract the cost from the extended number range, removing the Infinity value.
    alignmentCost = unsafeToFinite extendedCost
    result = (alignmentCost, ungappedMedians, gappedMedian, alignedChar1, alignedChar2)


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
  -> Cost
  -> Cost
  -> (Cost, s, s, s, s)
--ukkonenCore _ _ _ _ _ _ _ | trace "ukkonenCore" False = undefined
ukkonenCore lSeq lLength rSeq rLength maxGap indelCost subCost
--  | headEx (trace renderedMatrix gappedMedian) /= 0 = (cost, ungappedMedian, gappedMedian, lhsAlignment, rhsAlignment)
  | headEx gappedMedian /= 0 = (cost, ungappedMedian, gappedMedian, lhsAlignment, rhsAlignment)
  | otherwise                = --trace ("Going back!! " <> show cost) $
                               ukkonenCore lSeq lLength rSeq rLength (2 * maxGap) indelCost subCost
  where
    firstRow       = getFirstRowUkkonen indelCost lLength 0 0 lSeq maxGap
    otherRows      = getRowsUkkonen lSeq rSeq indelCost subCost 1 firstRow maxGap
    nwMatrix       = firstRow `V.cons` otherRows
    renderedMatrix = renderUkkonenMatrix nwMatrix
    
    (medianGap, alignLeft, alignRight) = V.unzip3 . V.reverse $ tracebackUkkonen nwMatrix lSeq rSeq rLength lLength maxGap 0 0
    (cost, _, _)   = V.last (V.last nwMatrix)
    ungappedMedian = filterGaps gappedMedian
    gappedMedian   = constructDynamic medianGap
    lhsAlignment   = constructDynamic alignLeft
    rhsAlignment   = constructDynamic alignRight


-- |
-- getFirstRowUkkonen initializes first row of NW-Ukkonen matrix
getFirstRowUkkonen
  :: (DOCharConstraint s, Show s)
  => Cost
  -> Int
  -> Int
  -> Cost
  -> s
  -> Int
  -> V.Vector (Cost, Element s, Direction)
--getFirstRowUkkonen _ rowLen position _ lSeq _ | trace ("getFirstRowUkkonen " <> show lSeq <> show position <> show rowLen) False = undefined
getFirstRowUkkonen indelCost rowLength position prevCost lSeq maxGap
 --trace ("row 0 pos " <> show position <> "/" <> show (maxShortY rowLength 0 maxGap) <> " rowLength " <> show rowLength <> " maxGap " <> show maxGap <> " lseq " <> show lSeq)
  | position == rowLength + 1 = V.empty
  | position == maxGap    + 1 = V.singleton (barrierCost, gap, LeftArrow) 
  | position == 0             = V.cons (0, gap, DiagArrow) (getFirstRowUkkonen indelCost rowLength (position + 1) 0 lSeq maxGap) 
  | otherwise                 = --trace ("FRC " <> show newCost)
      let y | (newState /= gap) = --if there was no inDel overlap between states
                V.cons ( newCost, newState, LeftArrow) (getFirstRowUkkonen indelCost rowLength (position + 1)  newCost lSeq maxGap)
            | otherwise = --indel in both states so no cost
                V.cons (prevCost, newState, LeftArrow) (getFirstRowUkkonen indelCost rowLength (position + 1) prevCost lSeq maxGap)
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
  -> Cost
  -> Cost
  -> Int
  -> V.Vector (Cost, Element s, Direction)
  -> Int
  -> V.Vector (V.Vector (Cost, Element s, Direction))
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
    thisRowNonZero = V.cons (barrierCost, gap, UpArrow) (getThisRowUkkonen lSeq rSeq indelCost subCost rowNum prevRow startPosition (olength lSeq) barrierCost maxGap)


-- |
-- getThisRowUkkonen takes sequences and parameters with row number and make a non-first
-- row--Ukkonen
getThisRowUkkonen
  :: (DOCharConstraint s, Show s)
  => s
  -> s
  -> Cost
  -> Cost
  -> Int
  -> V.Vector (Cost, Element s, Direction)
  -> Int
  -> Int
  -> Cost
  -> Int
  -> V.Vector (Cost, Element s, Direction)
getThisRowUkkonen lSeq rSeq indelCost subCost rowNum prevRow position rowLength prevCost maxGap
  | position == rowLength  + 1      = V.empty
  | position == rowNum + maxGap + 1 = V.singleton (barrierCost, gap, LeftArrow)
  | position == 0 =
      let x | (newState /= gap) =
                V.cons (upValue + indelCost, newState, UpArrow) (getThisRowUkkonen lSeq rSeq indelCost subCost rowNum prevRow (position + 1) rowLength (upValue + indelCost) maxGap)
            | otherwise = 
                V.cons (upValue, newState, UpArrow) (getThisRowUkkonen lSeq rSeq indelCost subCost rowNum prevRow (position + 1) rowLength upCost maxGap)
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
    (diagCost, diagState) = getDiagArrowCost diagValue intersection union subCost
    (  upValue,       _,      _) = prevRow V.! transformFullYShortY  position (rowNum - 1) maxGap
    (diagValue,       _,      _) = prevRow V.! transformFullYShortY  (position - 1) (rowNum - 1) maxGap
    ( minCost, minState, minDir) =
        getMinCostDir
          leftCost
          downCost
          diagCost
          diagState 
          (getUnionIntersectionState gap (lSeq `indexStream` lSeqPos))
          (getUnionIntersectionState gap (rSeq `indexStream` rSeqRow)) 


-- |
-- tracebackUkkonen creates REVERSE mediian from nwMatrix, reverse to make tail
-- recusive, for Ukkonen space/time saving offsets
-- need to count gaps in traceback for threshold/barrier stuff
-- CHANGE TO MAYBE (V.Vector Int64) FOR BARRIER CHECK
tracebackUkkonen
  :: (DOCharConstraint s, Show s)
  => V.Vector (V.Vector (Cost, Element s, Direction))
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
  | (rInDel  > (maxGap - 2)) || (lInDel > (maxGap - 2)) = V.singleton (sentinalValue, sentinalValue, sentinalValue)
  | posL <= 0 && posR <= 0 = {- trace "not y" -} V.empty
  | otherwise =
      case direction of
        LeftArrow -> V.cons (state, inlSeq `indexStream` (posL - 1),                              gap) (tracebackUkkonen nwMatrix inlSeq inrSeq  posR      (posL - 1) maxGap (rInDel + 1) lInDel     )  
        UpArrow   -> V.cons (state,                             gap, inrSeq `indexStream` (posR - 1)) (tracebackUkkonen nwMatrix inlSeq inrSeq (posR - 1)  posL      maxGap  rInDel     (lInDel + 1))
        DiagArrow -> V.cons (state, inlSeq `indexStream` (posL - 1), inrSeq `indexStream` (posR - 1)) (tracebackUkkonen nwMatrix inlSeq inrSeq (posR - 1) (posL - 1) maxGap  rInDel      lInDel     )
  where
    gap           = gapOfStream inlSeq
    sentinalValue = gap `xor` gap -- a "0" value with the correct dimensionality.
    (_, state, direction) = (nwMatrix V.! posR) V.! transformFullYShortY posL posR  maxGap --(transformFullYShortY posL posR maxGap)
{-
    indexStream' s i = (trace (unwords [show direction, show (posL, posR), shownStreamPokes]) s) `indexStream` i
      where
        shownStreamPokes =
            case direction of
              LeftArrow -> unwords ["L @",             "X", "R @", show $ posR - 1]
              UpArrow   -> unwords ["L @", show $ posL - 1, "R @",             "X"]
              DiagArrow -> unwords ["L @", show $ posL - 1, "R @", show $ posR - 1]
-}

-- |
-- transformFullYShortY take full Y value (if did entire NW matrix) and returns
-- short (Ukkonnen Y) given Y, Y length and row number
-- remove error when working--overhead
transformFullYShortY :: Int -> Int -> Int -> Int
transformFullYShortY currentY rowNumber maxGap
  | transformY < 0 = error $ unwords [show currentY, show rowNumber, show maxGap, "Impossible negative value for transfomred Y"]
  | otherwise      = transformY
  where
    transformY = currentY - max 0 (rowNumber - maxGap - 1)


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
-- getDiagArrowCost takes union intersection and state to get diagonla sub or no-sub
--cost
getDiagArrowCost :: (Bits b, Num n) => n -> b -> b -> n -> (n, b)
getDiagArrowCost upLeftArrowCost intersection union subCost --trace ("DiagCost " <> show upLeftArrowCost <> " int " <> show intersection <> " union " <> show union) (
  | intersection /= zeroBits = (upLeftArrowCost          , intersection)
  | otherwise                = (upLeftArrowCost + subCost,        union)


-- |
-- getMinCostDir takes costs and states of three directins and returns min cost,
-- directin, and state
-- ORDER diag, down, left so same as POY4-5.
getMinCostDir :: Ord v
              => v -> v -> v
              -> s -> s -> s
              -> (v, s, Direction)
getMinCostDir leftCost downCost diagCost diagState leftState downState
  | diagCost == minValue = (diagCost, diagState, DiagArrow)
  | leftCost == minValue = (leftCost, leftState, LeftArrow)
  | otherwise            = (downCost, downState,   UpArrow)
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




renderUkkonenMatrix :: Vector (Vector (Cost, a, Direction)) -> String
renderUkkonenMatrix jaggedMatrix = unlines . toList $ V.generate rowCount g
  where
    g i = prefix <> rowStr
      where
        prefix = replicate (max 0 offset) '\t'
        rowStr = intercalate "\t" . fmap (show . contract) . toList $ jaggedMatrix ! i
        offset = i - searchRange + 1

    searchRange      = length $ jaggedMatrix ! 0
    rowCount         = length jaggedMatrix
    contract (x,_,y) = (x,y)
