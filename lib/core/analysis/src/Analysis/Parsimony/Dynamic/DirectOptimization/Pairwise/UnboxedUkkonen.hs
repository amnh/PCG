-----------------------------------------------------------------------------
-- |
-- Module      :  Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.NeedlemanWunsch
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Direct optimization pairwise alignment using the Needleman-Wunsch algorithm.
-- These functions will allocate an M * N matrix.
--
-----------------------------------------------------------------------------

{-# LANGUAGE ApplicativeDo    #-}
{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE TypeFamilies       #-}

module Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.UnboxedUkkonen
  ( unboxedUkkonenDO
  ) where

import           Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.Internal (Direction(..), DOCharConstraint, OverlapFunction, handleMissingCharacter, measureCharacters)
import           Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.UnboxedSwapping (unboxedSwappingDO)
import           Bio.Character.Encodable
import           Control.Monad.ST
import           Data.Bits
import           Data.DList                  (snoc)
import           Data.Foldable
import qualified Data.List.NonEmpty          as NE
import           Data.Matrix.Unboxed         (Matrix, unsafeFreeze, unsafeIndex)
import qualified Data.Matrix.Unboxed.Mutable as M
import           Data.Maybe                  (fromMaybe)
import           Data.MonoTraversable
import qualified Data.Vector.Unboxed.Mutable as V


-- |
-- Performs a naive direct optimization.
-- Takes in two characters to run DO on and an overlap function
-- Returns an assignment character, the cost of that assignment, the assignment
-- character with gaps included, the aligned version of the first input character,
-- and the aligned version of the second input character. The process for this
-- algorithm is to generate a traversal matrix, then perform a traceback.
{-# SCC unboxedUkkonenDO #-}
{-# INLINE unboxedUkkonenDO #-}
{-# SPECIALISE unboxedUkkonenDO :: OverlapFunction AmbiguityGroup -> DynamicCharacter -> DynamicCharacter -> (Word, DynamicCharacter) #-}
unboxedUkkonenDO
  :: DOCharConstraint s
  => OverlapFunction (Subcomponent (Element s))
  -> s
  -> s
  -> (Word, s)
unboxedUkkonenDO overlapFunction char1 char2
  | noGainFromUkkonenMethod = buildFullMatrix
  | otherwise               =
    case directOptimization overlapFunction buildPartialMatrixMaybe char1 char2 of
      Nothing  -> buildFullMatrix
      Just res -> res
  where
    (_, longer, lesser) = measureCharacters char1 char2

    gap = getMedian med $ gapOfStream char1
    med  x y = fst $ overlapFunction x y
    cost x y = snd $ overlapFunction x y
    
    buildFullMatrix = unboxedSwappingDO overlapFunction char1 char2

    buildPartialMatrixMaybe = createUkkonenMethodMatrix coefficient gapsPresentInInputs $
                                  buildDirectionMatrix med cost longer lesser

    -- /O(1)/
    --
    -- If the longer character is 50% larger than the shorter character, then
    -- there is no point in using the barriers. Rather, we fill the full matrix
    -- immediately.
    --
    -- Additionally, if the shorter sequence is of length 4 or less, then the
    -- initial barrier will be set adjacent to or beyond the lower left and
    -- upper right corners.
    --
    -- Lastly, a threshold coeffcient is computed as the minimal indel cost from
    -- any symbol in the alphabet to gap. However, if the indel cost for any
    -- symbol is zero, the algorithm will hang, and a naive approach must be taken.
    --
    -- Do not perform Ukkonen's algorithm if and only if:
    --
    -- > longerLen >= 1.5 * lesserLen
    --     OR
    -- > lesserLen <= 4
    --     OR
    -- > coefficient == 0
    noGainFromUkkonenMethod =     lesserLen <= 4
                           || 2 * longerLen >= 3 * lesserLen
                           || coefficient == 0
      where
        longerLen = olength longer
        lesserLen = olength lesser

    -- /O(2*(a - 1))/
    --
    -- This was taken from Ukkonen's original 1985 paper wherein the coeffcient
    -- delta @(Δ)@ was defined by the minimum transition cost from any symbol in
    -- the alphabet @(Σ)@ to the gap symbol @'-'@.
    --
    -- If there is any transition to a gap from a non-gap for which the cost is
    -- zero, then this coefficient will be zero. This leaves us with no way to
    -- determine if optimality is preserved, and the Ukkonen algorithm will hang.
    -- Consequently, we do not perform Ukkonen's algorithm if the coefficient is
    -- zero.
    coefficient = minimum $ indelCost <$> nonGapElements
      where
        alphabetSize   = fromEnum $ symbolCount gap
        nonGapElements = [ 0 .. alphabetSize - 2 ]
        indelCost i    = min (cost (bit i)  gap   )
                             (cost  gap    (bit i))

    -- /O(n + m)/
    --
    -- If one or more of the aligned character elements contained a gap, diagonal
    -- directions in the matrix have an "indel" cost. 'gapsPresentInInputs' is
    -- necessary in order to decrement the threshold value to account for this.
    -- This was not described in Ukkonen's original paper, as the inputs were assumed
    -- not to contain any gaps.
    gapsPresentInInputs = char1Gaps + char2Gaps
      where
        char1Gaps = toEnum $ countGaps char1
        char2Gaps = toEnum $ countGaps char2
        countGaps = length . filter (hasGap . getMedian med) . otoList
        hasGap b  = popCount (b .&. gap) > 0


-- |
-- /O( (n - m + 1 ) * log(n - m + 1) )/, /n/ >= /m/
--
-- Generates an /optimal/, partially-filled-in matrix using Ukkonen's string
-- edit distance algorithm.
--
-- Note that the threshold value is lowered more than described in Ukkonen's
-- paper. This is to handle input elements that contain a gap. In Ukkonen's
-- original description of the algorithm, there was a subtle assumption that
-- input did not contain any gap symbols.
{-# SCC createUkkonenMethodMatrix #-}
{-# INLINE createUkkonenMethodMatrix #-}
{-# SPECIALISE createUkkonenMethodMatrix :: Word -> Word -> (Word -> Maybe (Word, Matrix Direction)) -> DynamicCharacter -> DynamicCharacter -> Maybe (Word, Word, Matrix Direction) #-}
createUkkonenMethodMatrix
  :: DOCharConstraint s
  => Word -- ^ Coefficient value, representing the /minimum/ transition cost from a state to gap
  -> Word -- ^ Gaps present in input
  -> (Word -> Maybe (Word, Matrix Direction))
  -> s    -- ^ Longer dynamic character
  -> s    -- ^ Shorter dynamic character
  -> Maybe (Word, Word, Matrix Direction)
createUkkonenMethodMatrix minimumIndelCost gapsPresentInInputs matrixBuilder longerTop lesserLeft = ukkonenUntilOptimal startOffset
  where
    -- General values that need to be in scope for the recursive computations.
    longerLen   = olength longerTop
    lesserLen   = olength lesserLeft
    coefficient = fromEnum minimumIndelCost

    -- We start the offset at two rather than at one so that the first doubling
    -- isn't trivially small.
    startOffset = 2 + gapsPresentInInputs

    -- /O(1)/
    --
    -- Necessary to compute the width of a row in the barrier-constrained matrix.
    quasiDiagonalWidth = differenceInLength + 1
      where
        differenceInLength = longerLen - lesserLen

    ukkonenUntilOptimal offset = matrixBuilder offset >>= considerRecursing offset

    considerRecursing offset (alignmentCost, dirMatrix)
      | threshold <= alignmentCost = ukkonenUntilOptimal $ 2 * offset
      | otherwise                  = Just (offset, alignmentCost, dirMatrix)
      where
        computedValue = coefficient * (quasiDiagonalWidth + fromEnum offset - fromEnum gapsPresentInInputs)
        threshold     = toEnum $ max 0 computedValue -- The threshold value must be non-negative

               
{-# SCC buildDirectionMatrix #-}
buildDirectionMatrix
  :: DOCharConstraint s
  => (Subcomponent (Element s) -> Subcomponent (Element s) -> Subcomponent (Element s))
  -> (Subcomponent (Element s) -> Subcomponent (Element s) -> Word)
  -> s
  -> s
  -> Word
  -> Maybe (Word, Matrix Direction)
buildDirectionMatrix med cost longerTop lesserLeft o
  | cols <= longerLen + 1 = Just fullMatrix
  | otherwise             = Nothing
  where
    offset      = fromEnum o    
    gap         = gapOfStream longerTop
    gapGroup    = getMedian med gap
    longerLen   = olength longerTop
    lesserLen   = olength lesserLeft
    rows        = olength lesserLeft + 1
    cols        = quasiDiagonalWidth + (2 * offset)
    quasiDiagonalWidth = differenceInLength + 1
      where
        differenceInLength = longerLen - lesserLen

    fullMatrix = runST $ do
      
      ---------------------------------------
      -- Allocate required space          --
      ---------------------------------------

      mDir <- M.new (rows, cols)
      vOne <- V.new cols
      vTwo <- V.new cols

      ---------------------------------------
      -- Define some generalized functions --
      ---------------------------------------
      
      -- Write to a single cell of the current vector and directional matrix simultaneously
      let write v !p@(~(_,!j)) ~(!c, !d) = V.write v j c *> M.unsafeWrite mDir p d

      -- Define how to compute values to an entire row of the Ukkonen matrix
      -- Takes parameterized functions which describe
      -- how to compute the first and last cells of the row.
      let writeRow firstCell lastCell i =
            -- Precomute some values that will be used for the whole row
            let (prev, curr)
                  | odd i     = (vOne, vTwo)
                  | otherwise = (vTwo, vOne)
                start = max 0 $ offset - i
                stop  = (cols - 1) - max 0 (offset - ((rows - 1) - i))
                leftElement = getMedian med . fromMaybe gap $ lesserLeft `lookupStream` (i - 1)
                insertCost  = cost gapGroup leftElement
            in  do -- Write to the first cell of the Ukkonen band
                   firstCell prev curr leftElement insertCost i start >>= write curr (i, start)
                   -- Write to the all the intermediary cells of the Ukkonen band
                   for_ [start + 1 .. stop - 1] $ \j -> {-# SCC internalCell_work #-} do
                     let topElement = getMedian med . fromMaybe gap $ longerTop `lookupStream` (i + j - offset - 1) 
                     let deleteCost = cost topElement    gapGroup
                     let alignCost  = cost topElement leftElement
                     diagCost <- V.unsafeRead prev $ j
                     topCost  <- V.unsafeRead prev $ j + 1
                     leftCost <- V.unsafeRead curr $ j - 1
                     write curr (i,j) $ minimum
                             [ ( alignCost + diagCost, DiagArrow)
                             , (deleteCost + leftCost, LeftArrow)
                             , (insertCost +  topCost, UpArrow  )
                             ]
                   -- Write to the last cell of the Ukkonen band
                   lastCell prev curr leftElement insertCost i stop >>= write curr (i, stop)

      -- Define how to compute the first cell of the first "offest" rows.
      -- We need to ensure that there are only Up Arrow values in the directional matrix.
      -- We can also reduce the number of comparisons the first row makes from 3 to 1,
      -- since the diagonal and leftward values are "out of bounds."
      let leftColumn prev _curr _leftElement insertCost _i j = {-# SCC leftColumn #-} do
            firstPrevCost <- V.unsafeRead prev (j + 1)
            pure (insertCost + firstPrevCost, UpArrow)

      -- Define how to compute the first cell of the remaining rows.
      -- We need to ensure that there are no Left Arrow values in the directional matrix.
      -- We can also reduce the number of comparisons the first row makes from 3 to 2,
      -- since the leftward values are "out of bounds."
      let leftBoundary prev _curr leftElement insertCost i j = {-# SCC leftBoundary #-}
            let topElement = getMedian med . fromMaybe gap $ longerTop  `lookupStream` (i - offset - 1)
                alignCost  = cost topElement leftElement
            in  do diagCost <- V.unsafeRead prev $ j
                   topCost  <- V.unsafeRead prev $ j + 1
                   pure $  minimum
                       [ ( alignCost + diagCost, DiagArrow)
                       , (insertCost +  topCost, UpArrow  )
                       ]

      -- Define how to compute the last cell of the first "rows - offest" rows.
      -- We need to ensure that there are only Left Arrow values in the directional matrix.
      -- We can also reduce the number of comparisons the first row makes from 3 to 1,
      -- since the diagonal and upward values are "out of bounds."
      let rightBoundary prev curr leftElement _insertCost i j = {-# SCC rightBoundary #-}
            let topElement = getMedian med . fromMaybe gap $ longerTop  `lookupStream` (i + j - offset - 1)
                deleteCost = cost topElement    gapGroup
                alignCost  = cost topElement leftElement
            in  do diagCost <- V.unsafeRead prev $ j
                   leftCost <- V.unsafeRead curr $ j - 1
                   pure $ minimum
                       [ ( alignCost + diagCost, DiagArrow)
                       , (deleteCost + leftCost, LeftArrow)
                       ]

      -- Define how to compute the last cell of the last "offest" rows.
      -- We need to ensure that there are no Up Arrow values in the directional matrix.
      -- We can also reduce the number of comparisons the first row makes from 3 to 2,
      -- since the upward values are "out of bounds."
      let rightColumn prev curr leftElement insertCost i j = {-# SCC rightColumn #-}  do
            let topElement  = getMedian med . fromMaybe gap $  longerTop `lookupStream` (i + j - offset - 1)
            let deleteCost  = cost topElement    gapGroup
            let alignCost   = cost topElement leftElement
            diagCost <- V.unsafeRead prev $ j
            topCost  <- V.unsafeRead prev $ j + 1
            leftCost <- V.unsafeRead curr $ j - 1
            pure $ minimum
                [ ( alignCost + diagCost, DiagArrow)
                , (deleteCost + leftCost, LeftArrow)
                , (insertCost +  topCost, UpArrow  )
                ]

      ---------------------------------------
      -- Compute all values of the matrix  --
      ---------------------------------------

      -- Some entries in the vectors and matrix are invalid.
      --
      -- Specifically, all entries (i,j) in the space such that:
      --  |(0 + 0) - (i + j)| < offset || |(rows + cols) - (i + j)| < offset
      --
      -- This is the first valid entry in the matrix.
      -- It is isomorphic to the origin of a full Needleman-Wunsch matrix.
      write vOne (0, offset) (0, DiagArrow)

      -- Special case the first row
      -- We need to ensure that there are only Left Arrow values in the directional matrix.
      -- We can also reduce the number of comparisons the first row makes from 3 to 1,
      -- since the diagonal and upward values are "out of bounds."
      for_ [offset+1 .. cols - 1] $ \j ->
        let topElement    = getMedian med . fromMaybe gap $ longerTop `lookupStream` (j - offset - 1)
            firstCellCost = cost gapGroup topElement
        in  do firstPrevCost <- V.unsafeRead vOne (j - 1)
               write vOne (0,j) (firstCellCost + firstPrevCost, LeftArrow)

      -- Loop through the next "offset" number of rows
      for_ [1 .. offset] $
         writeRow leftColumn   rightBoundary

      -- Loop through the "middle section" of rows
      for_ [offset + 1 .. rows - offset - 1] $
         writeRow leftBoundary rightBoundary

      -- Loop through the last "offset" number of rows
      for_ [rows - offset .. rows - 1] $
         writeRow leftBoundary rightColumn

      let v | odd  rows = vOne
            | otherwise = vTwo
      c <- V.unsafeRead v (cols - offset - 1)
      m <-   unsafeFreeze mDir
      pure (c, m)


directOptimization
  :: ( DOCharConstraint s
     )
  => OverlapFunction (Subcomponent (Element s))
  -> (s -> s -> Maybe (Word, Word, Matrix Direction))
  -> s
  -> s
  -> Maybe (Word, s)
directOptimization overlapλ matrixFunction char1 char2 =
    let (swapped, longerChar, shorterChar)       = measureCharacters char1 char2
    in  case matrixFunction longerChar shorterChar of
          Nothing -> Nothing
          Just (offset, alignmentCost, traversalMatrix) ->
            let alignmentContext = traceback overlapλ offset traversalMatrix longerChar shorterChar
                alignment        = (alignmentCost, transformation alignmentContext)
                transformation
                  | swapped   = omap swapContext
                  | otherwise = id
            in  Just $ handleMissingCharacter char1 char2 alignment


{-# SCC traceback #-}
traceback
  :: DOCharConstraint s
  => OverlapFunction (Subcomponent (Element s))
  -> Word
  -> Matrix Direction
  -> s
  -> s
  -> s
traceback overlapFunction o alignMatrix longerChar lesserChar = alignmentContext
  where
    f x y = fst $ overlapFunction x y

    offset = fromEnum o

    alignmentContext = dlistToDynamic $ go startPoint
    dlistToDynamic = constructDynamic . NE.fromList . toList

    longerLen = olength longerChar
    lesserLen = olength lesserChar
    rows      = lesserLen + 1
    cols      = quasiDiagonalWidth + (2 * offset)
    quasiDiagonalWidth = differenceInLength + 1
      where
        differenceInLength = longerLen - lesserLen

    startPoint = (rows - 1, cols - 1 - offset)
    finalPoint = (0, offset)

    go p@(i, j)
      | p == finalPoint = mempty
      | otherwise       =
        let previousSequence = go (row', col')

            directionArrow = unsafeIndex alignMatrix p

            (row', col', localContext) =
                case directionArrow of
                  LeftArrow -> let j' = j-1
                                   te = longerChar `indexStream` (i + j - offset - 1)
                                   e  = deleteElement $ getMedian f te
                               in (i , j', e)
                  UpArrow   -> let i' = i-1
                                   le = lesserChar `indexStream` i'
                                   e  = insertElement $ getMedian f le
                               in (i', j+1, e)
                  DiagArrow -> let i' = i-1
                                   j' = j
                                   te = longerChar `indexStream` (i + j - offset - 1)
                                   le = lesserChar `indexStream` i'
                                   e  = alignElement (getMedian f le) $ getMedian f te
                               in (i', j', e)

        in  previousSequence `snoc` localContext


{--
renderMatricies :: (Show a, Show b, V.Unbox a, V.Unbox b) => Word -> Matrix a -> Matrix b -> String
renderMatricies o mDir mCost = unlines [ x, y ]
  where
    offset    = fromEnum o
    (r,c)     = dim mDir
    x         = unlines $ foldMap (\(a,b) -> " " <> show a <> show b) <$> toLists (MZ.zip mCost mDir)
    y         = foldMapWithKey (\i -> (<>"\n") . (grabPad i <>) . foldMap (\(a,b) -> " " <> show a <> show b) . grabRow i) $ toLists (MZ.zip mCost mDir)
    grabRow i = take (c - (max 0 (offset - (r - 1 - i)))) . drop (max 0 (offset - i))
    grabPad i = fold $ replicate (max 0 (i - offset)) "   "

renderMatrix :: (Show a, V.Unbox a) => Word -> Matrix a -> String
renderMatrix o mDir = unlines [ x, y ]
  where
    offset    = fromEnum o
    (r,c)     = dim mDir
    x         = unlines $ foldMap (\a -> " " <> show a) <$> toLists mDir
    y         = foldMapWithKey (\i -> (<>"\n") . (grabPad i <>) . foldMap (\a -> " " <> show a) . grabRow i) $ toLists mDir
    grabRow i = take (c - (max 0 (offset - (r - 1 - i)))) . drop (max 0 (offset - i))
    grabPad i = fold $ replicate (max 0 (i - offset)) "   "
--}
