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
import           Data.MonoTraversable
import           Data.Semigroup
import           Data.Vector              (Vector)
import qualified Data.Vector         as V
import           Data.Vector.Instances    ()
import           Numeric.Extended.Natural

import Debug.Trace


data Ribbon a
   = Ribbon
   { height   :: Int 
   , width    :: Int -- width >= height
   , diagonal :: Int
   , offset   :: Int
   , linear   :: Vector a
   } deriving (Eq)


ribbonIndex :: Ribbon a -> (Int, Int) -> Int
ribbonIndex r (i,j) = rowPrefix + colIndex
  where
    a = offset r
    colIndex  = j - max 0 (i - a)
    rowPrefix = (max 0 (i-1)) * (d + 2*a) - ( ((a-b)*(a+b+1)) `div` 2 )
      where
        d = diagonal r
        b = min 0 (a - i)
{-
    rowPrefix = (max 0 (i-1)) * (d + 2*a) - t a + t b
      where
        d = diagonal r
        b = min 0 (a - i)
        t k = (k*(k+1)) `div` 2
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


-- |
-- UkkonenDO takes two input sequences and returns median sequence and cost
-- only 1:1 for now. Uses Ukkonen's space/time saving algorithm
-- need to make sure Left/Right and diag/ins/del orders consistent and with
-- POY4/5
-- lseq > rseq appeard more efficient--could be wrong
-- move to C via FFI
-- Still occasional error in cost and median (disagreement) show in Chel.seq
ukkonenDO
  :: DOCharConstraint s
  => s
  -> s
  -> OverlapFunction (Element s)
  -> (Word, s, s, s, s)
ukkonenDO char1 char2 overlapFunction
  | noGainFromUkkonenMethod = naiveDOMemo char1 char2 overlapFunction
  | otherwise               = handleMissingCharacter char1 char2 result
  where
    
    -- If the longer character is 50% larger than the shorter character then
    -- there is no point in using the barriers. Rather, we fill the full matrix
    -- immediately.
    --
    -- Additionally, if the shorter sequence is of length 4 or less, then the
    -- inital barrier will be set at a adjacent to or beyond the lower left and
    -- upper right corners.
    --
    -- Lastly, a threshhold coeffcient is computed as the minimal indel cost from
    -- any symbol in the alphabet to gap. However, if the indel cost for any
    -- symbol is zero, the algorithm will hang and a naive approach must be taken.
    --
    -- Do not perform Ukkonen's algorithm if and only if:
    --
    -- > longerLen >= 1.5 * lesserLen
    --     OR
    -- > lesserLen <= 4
    --     OR
    -- > coefficient == 0
    noGainFromUkkonenMethod = or
        [     lesserLen <= 4
        , 2 * longerLen >= 3 * lesserLen
        ,   coefficient == 0
        ]
      where
        longerLen = olength longer
        lesserLen = olength lesser

    -- /O(2*(a - 1))/
    --
    -- This was taken from Ukkonen's original 1985 paper where the coeffcient
    -- delta (Δ) was defined by the minimum transition cost from any symbol in
    -- the alphabet (Σ) to the gap symbol (-).
    --
    -- This should only be zero if all transition costs to gap are zero.
    -- But if that is the case the algorithm will hang.
    coefficient = minimum $ indelCost <$> nonGapElements
      where
        gap            = gapOfStream char1
        alphabetSize   = symbolCount gap
        nonGapElements = [ 0 .. alphabetSize - 2 ]
        indelCost i    =  min (snd (overlapFunction (bit i)  gap    ))
                              (snd (overlapFunction  gap    (bit i) ))

    -- We determine which character is longer and whether or not the alignment
    -- results will later need to be swapped. This is necessary because we assume
    -- an invariant of the longer character being on the left column and the
    -- shorter character on the top row.
    (swapped, longer, lesser) = measureCharacters char1 char2

    -- This for left right constant--want longer in left for Ukkonnen
    -- Perform the Ukkonen work once ensuring invariants are applied
    (alignmentCost, ungappedMedians, gappedMedian, alignLeft, alignRight) = ukkonenInternal longer lesser overlapFunction coefficient

    -- Conditionally swap resulting alignments if the inputs were swapped
    (alignedChar1, alignedChar2)
      | swapped   = (alignRight, alignLeft )
      | otherwise = (alignLeft , alignRight)

    -- Construct the resulting 5-tuple.
    result = (alignmentCost, ungappedMedians, gappedMedian, alignedChar1, alignedChar2)


-- |
-- ukkonenInternal core functions of Ukkonen to allow for recursing with maxGap
-- doubled if not large enough (returns Nothing)  
ukkonenInternal
  :: DOCharConstraint s
  => s
  -> s
  -> OverlapFunction (Element s)
  -> Word
  -> (Word, s, s, s, s)
ukkonenInternal longerTop lesserLeft overlapFunction minimumIndelCost = ukkonenUntilOptimal startOffset
  where
    -- General values that need to be in scope for the recursive computations.
    longerLen   = olength longerTop
    lesserLen   = olength lesserLeft
    coefficient = fromEnum minimumIndelCost
    
    -- We start the offset at two rather than at one so that the first doubling
    -- isn't trivially small.
    startOffset = 2

    -- /O(1)/
    --
    -- Necessary to compute the width of a row in the barrier constrained matrix.
    quasiDiagonalWidth = differenceInLength + 1
      where
        differenceInLength = longerLen - lesserLen

    -- /O(n + m)/
    --
    -- This is important to decrement the threshhold value to account for
    -- diagonal directions in the matrix having an "indel" cost because one or
    -- more of the aligned character elements was a gap.
    gapsPresentInInputs = longerGaps + lesserGaps
      where
        longerGaps = countGaps longerTop
        lesserGaps = countGaps lesserLeft
        countGaps  = length . filter (== gap) . otoList
        gap        = gapOfStream longerTop

    ukkonenUntilOptimal offset
--      | threshhold <= trace (renderedBounds <> renderedMatrix) alignmentCost = ukkonenUntilOptimal (2 * offset)
      | threshhold <= alignmentCost = ukkonenUntilOptimal (2 * offset)
      | otherwise                   = (alignmentCost, ungappedMedian, gappedMedian, lhsAlignment, rhsAlignment)
      where
        nwMatrix       = generateUkkonenBand longerTop lesserLeft overlapFunction maxGap
        renderedMatrix = renderUkkonenMatrix longerTop lesserLeft maxGap nwMatrix
        renderedBounds = unlines
            [ "Diag Width : " <> show quasiDiagonalWidth
            , "Input Gaps : " <> show gapsPresentInInputs
            , "Offset     : " <> show offset
            , "Coefficient: " <> show coefficient
            , "Threshhold : " <> show threshhold
            , "Total Cost : " <> show alignmentCost
            ]
    
        (medianGap, alignLeft, alignRight) = unzip3 . reverse $ tracebackUkkonen nwMatrix longerTop lesserLeft lesserLen longerLen maxGap
        (nwCost, _, _) = V.last $ V.last nwMatrix
        alignmentCost  = unsafeToFinite nwCost
        ungappedMedian = filterGaps gappedMedian
        gappedMedian   = constructDynamic medianGap
        lhsAlignment   = constructDynamic alignLeft
        rhsAlignment   = constructDynamic alignRight

        maxGap         = quasiDiagonalWidth + offset

        computedValue  = coefficient * (quasiDiagonalWidth + offset - gapsPresentInInputs)
        threshhold     = toEnum $ max 0 computedValue -- The threshhold value must be non-negative


generateUkkonenBand
  :: DOCharConstraint s
  => s
  -> s
  -> OverlapFunction (Element s)
  -> Int
  -> Vector (Vector (Cost, Element s, Direction))
generateUkkonenBand longerTop lesserLeft overlapFunction maxGap = nwMatrix
  where
    nwMatrix = V.fromList $ headRow : tailRows 
    headRow  = V.fromList $ generateHeadRowUkkonen 0 0
    tailRows = generateTailRowsUkkonen headRow 1

    gap      = gapOfStream longerTop
    rowCount = olength lesserLeft
    colCount = olength longerTop

    generateHeadRowUkkonen position prevCost
      | position == colCount + 1 = []
      | position == maxGap   + 1 = [(barrierCost,      gap, LeftArrow) ]
      | position == 0            =  (          0,      gap, DiagArrow) : (generateHeadRowUkkonen (position + 1)         0) 
      | newState /= gap          =  (  addedCost, newState, LeftArrow) : (generateHeadRowUkkonen (position + 1) addedCost)
      | otherwise                =  (   prevCost, newState, LeftArrow) : (generateHeadRowUkkonen (position + 1)  prevCost)
      where
        topElement          = longerTop `indexStream` (position - 1)
        addedCost           = prevCost + newCost
        (newState, newCost) = fromFinite <$> overlapFunction topElement gap

    generateTailRowsUkkonen prevRow rowNum
      | rowNum == rowCount + 1 = []
      | otherwise              = thisRow : generateTailRowsUkkonen thisRow (rowNum + 1)
      where
        thisRow = getThisRowUkkonen longerTop lesserLeft overlapFunction maxGap prevRow colCount rowNum


-- |
-- getThisRowUkkonen takes sequences and parameters with row number and make a non-first
-- row--Ukkonen
getThisRowUkkonen
  :: DOCharConstraint s
  => s
  -> s
  -> OverlapFunction (Element s)
  -> Int
  -> Vector (Cost, Element s, Direction)
  -> Int
  -> Int
  -> Vector (Cost, Element s, Direction)
getThisRowUkkonen longerTop lesserLeft overlapFunction maxGap prevRow rowLength rowNum = V.fromList resultRow
  where
    resultRow
      | startPosition == 0 =                               getThisRowUkkonen' startPosition 0
      | otherwise          = (barrierCost, gap, UpArrow) : getThisRowUkkonen' startPosition barrierCost
    
    gap            = gapOfStream longerTop
    startPosition  = max 0 (rowNum - maxGap) --check for left barrier
    getThisRowUkkonen' position leftwardValue
      | position == rowLength  + 1      = []
      | position == rowNum + maxGap + 1 = [(barrierCost, gap, LeftArrow)]
      | position == 0 =
          let (newState, newCost) = fromFinite <$> overlapFunction gap (lesserLeft `indexStream` (rowNum - 1))
              (  upCost, _, _) = prevRow ! position
              extraUpCost = upCost + newCost
          in  if    (newState /= gap)
              then  (extraUpCost, newState, UpArrow) : (getThisRowUkkonen' (position + 1)  extraUpCost)
              else  (     upCost, newState, UpArrow) : (getThisRowUkkonen' (position + 1)       upCost)
      | otherwise = (    minCost, minState, minDir ) : (getThisRowUkkonen' (position + 1)      minCost)
      where
        longerPos   = position - 1 --since first is '-' the index is row/pos - 1
        lesserPos   = rowNum   - 1 --since first is '-' the index is row/pos - 1
        leftElement = lesserLeft `indexStream` lesserPos
        topElement  = longerTop  `indexStream` longerPos

        rightCost                     = rightOverlapCost + leftwardValue
        diagCost                      =  diagOverlapCost + diagonalValue
        downCost                      =  downOverlapCost +   upwardValue 
        (rightChar, rightOverlapCost) = fromFinite <$> overlapFunction topElement  gap
        ( diagChar,  diagOverlapCost) = fromFinite <$> overlapFunction topElement  leftElement
        ( downChar,  downOverlapCost) = fromFinite <$> overlapFunction gap         leftElement
        (minCost, minState, minDir)   = getMinimalCostDirection
                                          ( diagCost,  diagChar)
                                          (rightCost, rightChar)
                                          ( downCost,  downChar)
        (  upwardValue,  _,      _) = prevRow ! transformFullYShortY  position      (rowNum - 1) maxGap
        (diagonalValue,  _,      _) = prevRow ! transformFullYShortY (position - 1) (rowNum - 1) maxGap


-- |
-- tracebackUkkonen creates REVERSE mediian from nwMatrix, reverse to make tail
-- recusive, for Ukkonen space/time saving offsets
-- need to count gaps in traceback for threshold/barrier stuff
-- CHANGE TO MAYBE (Vector Int64) FOR BARRIER CHECK
tracebackUkkonen
  :: DOCharConstraint s
  => Vector (Vector (Cost, Element s, Direction))
  -> s
  -> s
  -> Int
  -> Int
  -> Int
  -> [(Element s, Element s, Element s)]
tracebackUkkonen nwMatrix longerTop lesserLeft posR posL maxGap
  | any (== 0) [posL, posR] = []
  | otherwise =
      case direction of
        LeftArrow -> (state, longerTop `indexStream` (posL - 1),                                 gap) : (tracebackUkkonen nwMatrix longerTop lesserLeft  posR      (posL - 1) maxGap)  
        UpArrow   -> (state,                                gap, lesserLeft `indexStream` (posR - 1)) : (tracebackUkkonen nwMatrix longerTop lesserLeft (posR - 1)  posL      maxGap)
        DiagArrow -> (state, longerTop `indexStream` (posL - 1), lesserLeft `indexStream` (posR - 1)) : (tracebackUkkonen nwMatrix longerTop lesserLeft (posR - 1) (posL - 1) maxGap)
  where
    gap = gapOfStream longerTop
    (_, state, direction) = (nwMatrix ! posR) ! transformFullYShortY posL posR maxGap
--    sentinalValue = gap `xor` gap -- a "0" value with the correct dimensionality.

{--
    indexStream' s i = (trace (unwords [show direction, show (posL, posR), shownStreamPokes]) s) `indexStream` i
      where
        shownStreamPokes =
            case direction of
              LeftArrow -> unwords ["L @",             "X", "R @", show $ posR - 1]
              UpArrow   -> unwords ["L @", show $ posL - 1, "R @",             "X"]
              DiagArrow -> unwords ["L @", show $ posL - 1, "R @", show $ posR - 1]
--}


-- |
-- transformFullYShortY take full Y value (if did entire NW matrix) and returns
-- short (Ukkonnen Y) given Y, Y length and row number
-- remove error when working--overhead
transformFullYShortY :: Int -> Int -> Int -> Int
transformFullYShortY currentY rowNumber maxGap
  | transformY < 0 = error $ unwords [show currentY, show rowNumber, show maxGap, "Impossible negative value for transformed Y"]
  | otherwise      = transformY
  where
    transformY = currentY - max 0 (rowNumber - maxGap - 1)


renderUkkonenMatrix :: DOCharConstraint s => s -> s -> Int -> Vector (Vector (Cost, a, Direction)) -> String
renderUkkonenMatrix lhs rhs maxGap jaggedMatrix = unlines
    [ dimensionPrefix
    , barrierPrefix
    , headerRow
    , barRow
    , renderedRows
    ]
  where
    lhsLen = olength lhs
    rhsLen = olength rhs
    (longer, lesser)
      | olength lhs >= olength rhs = (lhs, rhs)
      | otherwise                  = (rhs, lhs)
    searchRange      = length $ jaggedMatrix ! 0
    longerTokens     = toShownIntegers longer
    lesserTokens     = toShownIntegers lesser
    toShownIntegers  = fmap (show . (fromIntegral :: Integral a => a -> Integer)) . otoList
    matrixTokens     = concatMap (fmap showCell . toList) $ toList jaggedMatrix
    showCell (c,_,d) = show c <> show d
    maxPrefixWidth   = maxLengthOf lesserTokens
    maxColumnWidth   = max (maxLengthOf longerTokens) . maxLengthOf $ toList matrixTokens
    maxLengthOf      = maximum . fmap length
    showPaddedCell   = pad maxColumnWidth . showCell

    pad :: Int -> String -> String
    pad n e = replicate (n - length e) ' ' <> e <> " "

    dimensionPrefix = " " <> unwords
        [ "Dimensions:"
        , show $ olength longer + 1
        , "X"
        , show $ olength lesser + 1
        ]

    barrierPrefix = " " <> unwords
        [ "Difference:"
        , show difference
        , ","
        , "Barrier:"
        , show $ maxGap - difference
        , "Threshhold:"
        , show maxGap
        ]
      where
        difference = max lhsLen rhsLen - min lhsLen rhsLen

    headerRow = mconcat
        [ " "
        , pad maxPrefixWidth "\\"
        , "| "
        , pad maxColumnWidth "*"
        , concatMap (pad maxColumnWidth) longerTokens
        ]

    barRow = mconcat
        [ " "
        , bar maxPrefixWidth
        , "+"
        , concatMap (const (bar maxColumnWidth)) $ undefined : longerTokens
        ]
      where
        bar n = replicate (n+1) '-'

    renderedRows = unlines . zipWith3 renderRow [0..] ("*":lesserTokens) $ toList jaggedMatrix
      where
        renderRow k e vs = mconcat [rowHeaderCell, uncomputedPrefix, computedRegion]
          where
            rowHeaderCell    = " " <> pad maxPrefixWidth e <> "| "
            uncomputedPrefix = mconcat $ replicate (max 0 offset) (pad maxColumnWidth "")
            computedRegion   = concatMap showPaddedCell $ toList vs
            offset           = k - searchRange + 1
