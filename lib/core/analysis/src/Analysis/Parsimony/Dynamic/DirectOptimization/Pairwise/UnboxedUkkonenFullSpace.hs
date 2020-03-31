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

{-# LANGUAGE ApplicativeDo       #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE UnboxedTuples       #-}

module Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.UnboxedUkkonenFullSpace
  ( unboxedUkkonenFullSpaceDO
  ) where

import           Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.Internal (Direction(..), DOCharConstraint, OverlapFunction, handleMissingCharacter, measureCharacters)
import           Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.UnboxedSwapping (unboxedSwappingDO)
import           Bio.Character.Encodable
import           Control.Monad.Loops         (iterateUntilM, whileM_)
import           Control.Monad.ST
import           Data.Bits
import           Data.DList                  (snoc)
import           Data.Foldable
import qualified Data.List.NonEmpty          as NE
import           Data.Matrix.Unboxed         (Matrix, unsafeFreeze, unsafeIndex)
import           Data.Matrix.Unboxed.Mutable (MMatrix)
import qualified Data.Matrix.Unboxed.Mutable as M
import           Data.MonoTraversable
import           Data.STRef


-- |
-- Performs a naive direct optimization.
-- Takes in two characters to run DO on and an overlap function
-- Returns an assignment character, the cost of that assignment, the assignment
-- character with gaps included, the aligned version of the first input character,
-- and the aligned version of the second input character. The process for this
-- algorithm is to generate a traversal matrix, then perform a traceback.
{-# SCC unboxedUkkonenFullSpaceDO #-}
{-# INLINE unboxedUkkonenFullSpaceDO #-}
{-# SPECIALISE unboxedUkkonenFullSpaceDO :: OverlapFunction AmbiguityGroup -> DynamicCharacter -> DynamicCharacter -> (Word, DynamicCharacter) #-}
unboxedUkkonenFullSpaceDO
  :: DOCharConstraint s
  => OverlapFunction (Subcomponent (Element s))
  -> s
  -> s
  -> (Word, s)
unboxedUkkonenFullSpaceDO overlapFunction char1 char2
  | noGainFromUkkonenMethod = buildFullMatrix
  | otherwise               = directOptimization overlapFunction buildPartialMatrixMaybe char1 char2
  where
    (_, longer, lesser) = measureCharacters char1 char2

    gap = getMedian $ gapOfStream char1
    cost x y = snd $ overlapFunction x y
    
    buildFullMatrix = unboxedSwappingDO overlapFunction char1 char2

    buildPartialMatrixMaybe = createUkkonenMethodMatrix coefficient gapsPresentInInputs cost

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
    -- Also, a threshold coeffcient is computed as the minimal indel cost from
    -- any symbol in the alphabet to gap. However, if the indel cost for any
    -- symbol is zero, the algorithm will hang, and a naive approach must be taken.
    --
    -- Lastly, if the sum of the gaps in both strings is equal to or exceeds the
    -- length of the longer string, then the threshold criteria will never be met
    -- by definition.
    --
    -- Do not perform Ukkonen's algorithm if and only if:
    --
    -- > longerLen >= 1.5 * lesserLen
    --     OR
    -- > lesserLen <= 4
    --     OR
    -- > coefficient == 0
    --     OR
    -- > gapsPresentInInputs >= longerLen
    noGainFromUkkonenMethod =     lesserLen <= 4
                           || 2 * longerLen >= 3 * lesserLen
                           || coefficient == 0
                           || gapsPresentInInputs >= longerLen
      where
        longerLen = toEnum $ olength longer
        lesserLen = toEnum $ olength lesser

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
        countGaps = length . filter (hasGap . getMedian) . otoList
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
{-# SPECIALISE createUkkonenMethodMatrix :: Word -> Word -> (AmbiguityGroup -> AmbiguityGroup -> Word) -> DynamicCharacter -> DynamicCharacter -> (Word, Matrix Direction) #-}
createUkkonenMethodMatrix
  :: DOCharConstraint a
  => Word -- ^ Coefficient value, representing the /minimum/ transition cost from a state to gap
  -> Word -- ^ Gaps present in input
  -> (Subcomponent (Element a) -> Subcomponent (Element a) -> Word)
  -> a    -- ^ Longer dynamic character
  -> a    -- ^ Shorter dynamic character
  -> (Word, Matrix Direction)
createUkkonenMethodMatrix minimumIndelCost gapsPresentInInputs cost longerTop lesserLeft = finalMatrix
  where
    -- General values that need to be in scope for the recursive computations.
    longerLen   = olength longerTop
    lesserLen   = olength lesserLeft

    -- We start the offset at two rather than at one so that the first doubling
    -- isn't trivially small.
    startOffset = 2 + gapsPresentInInputs

    -- /O(1)/
    --
    -- Necessary to compute the width of a row in the barrier-constrained matrix.
    quasiDiagonalWidth = toEnum $ differenceInLength + 1
      where
        differenceInLength = longerLen - lesserLen

    needToResizeBand :: forall s. MMatrix s Word -> STRef s Word -> ST s Bool
    needToResizeBand mCost offsetRef = do
        offset        <- readSTRef offsetRef
        if   quasiDiagonalWidth + offset > toEnum longerLen
        then pure False
        else do
                alignmentCost <- M.unsafeRead mCost (lesserLen, longerLen)
                let threshold -- The threshold value must be non-negative
                      | quasiDiagonalWidth + offset <= gapsPresentInInputs = 0
                      | otherwise = minimumIndelCost * (quasiDiagonalWidth + offset - gapsPresentInInputs)
                pure $ threshold <= alignmentCost
      
    finalMatrix = runST $ do
        (mCost, mDir) <- buildInitialBandedMatrix cost longerTop lesserLeft startOffset
        offsetRef <- newSTRef startOffset
        whileM_ (needToResizeBand mCost offsetRef) $ do
          previousOffset <- readSTRef offsetRef
          let currentOffset = previousOffset `shiftL` 1 -- Multiply by 2
          writeSTRef offsetRef currentOffset
          expandBandedMatrix cost longerTop lesserLeft mCost mDir previousOffset currentOffset

        c <- M.unsafeRead mCost (lesserLen, longerLen)
        m <- unsafeFreeze mDir
        pure (c, m)

               
{-# SCC buildInitialBandedMatrix #-}
buildInitialBandedMatrix
  :: forall s a. DOCharConstraint a
  => (Subcomponent (Element a) -> Subcomponent (Element a) -> Word)
  -> a
  -> a
  -> Word
  -> ST s (MMatrix s Word, MMatrix s Direction)
buildInitialBandedMatrix cost longerTop lesserLeft o = fullMatrix
  where
    -- Note: "offset" cannot cause "width" to exceed "cols"
    offset      = let o' = fromEnum o in  min o' $ cols - quasiDiagonalWidth
    gap         = gapOfStream longerTop
    gapGroup    = getMedian gap
    longerLen   = olength longerTop
    lesserLen   = olength lesserLeft
    rows        = olength lesserLeft + 1
    cols        = olength longerTop  + 1
    width       = quasiDiagonalWidth + (offset `shiftL` 1) -- Multiply by 2
    quasiDiagonalWidth = differenceInLength + 1
      where
        differenceInLength = longerLen - lesserLen

    fullMatrix = do
      
      ---------------------------------------
      -- Allocate required space           --
      ---------------------------------------

      mCost <- M.new (rows, cols)
      mDir  <- M.new (rows, cols)

      ---------------------------------------
      -- Define some generalized functions --
      ---------------------------------------

      -- Write to a single cell of the current vector and directional matrix simultaneously
      let write !p ~(!c, !d) = M.unsafeWrite mCost p c *> M.unsafeWrite mDir p d

      -- Define how to compute the first cell of the first "offest" rows.
      -- We need to ensure that there are only Up Arrow values in the directional matrix.
      -- We can also reduce the number of comparisons the first row makes from 3 to 1,
      -- since the diagonal and leftward values are "out of bounds."
      let leftColumn _leftElement insertCost i j = {-# SCC leftColumn #-} do
            firstPrevCost <- M.unsafeRead mCost (i - 1, j)
            pure (insertCost + firstPrevCost, UpArrow)

      -- Define how to compute the first cell of the remaining rows.
      -- We need to ensure that there are no Left Arrow values in the directional matrix.
      -- We can also reduce the number of comparisons the first row makes from 3 to 2,
      -- since the leftward values are "out of bounds."
      let leftBoundary leftElement insertCost i j = {-# SCC leftBoundary #-}
            let topElement = getMedian $ longerTop `indexStream` (j - 1)
                alignCost  = cost topElement leftElement
            in  do diagCost <- M.unsafeRead mCost (i - 1, j - 1)
                   topCost  <- M.unsafeRead mCost (i - 1, j    )
                   pure $ minimum
                       [ ( alignCost + diagCost, DiagArrow)
                       , (insertCost +  topCost, UpArrow  )
                       ]

      -- Define how to compute the last cell of the first "rows - offest" rows.
      -- We need to ensure that there are only Left Arrow values in the directional matrix.
      -- We can also reduce the number of comparisons the first row makes from 3 to 1,
      -- since the diagonal and upward values are "out of bounds."
      let rightBoundary leftElement _insertCost i j = {-# SCC rightBoundary #-}
            let topElement = getMedian $ longerTop `indexStream` (j - 1)
                deleteCost = cost topElement    gapGroup
                alignCost  = cost topElement leftElement
            in  do diagCost <- M.unsafeRead mCost (i - 1, j - 1)
                   leftCost <- M.unsafeRead mCost (i    , j - 1)
                   pure $ minimum
                       [ ( alignCost + diagCost, DiagArrow)
                       , (deleteCost + leftCost, LeftArrow)
                       ]

      -- Define how to compute the last cell of the last "offest" rows.
      -- We need to ensure that there are no Up Arrow values in the directional matrix.
      -- We can also reduce the number of comparisons the first row makes from 3 to 2,
      -- since the upward values are "out of bounds."
      let rightColumn leftElement insertCost i j = {-# SCC rightColumn #-}  do
            let topElement  = getMedian $ longerTop `indexStream` (j - 1)
            let deleteCost  = cost topElement    gapGroup
            let alignCost   = cost topElement leftElement
            diagCost <- M.unsafeRead mCost (i - 1, j - 1)
            topCost  <- M.unsafeRead mCost (i - 1, j    )
            leftCost <- M.unsafeRead mCost (i    , j - 1)
            pure $ minimum
                [ ( alignCost + diagCost, DiagArrow)
                , (deleteCost + leftCost, LeftArrow)
                , (insertCost +  topCost, UpArrow  )
                ]

      -- Define how to compute values to an entire row of the Ukkonen matrix.
      let writeRow i =
            -- Precomute some values that will be used for the whole row
            let start = max  0         $ i - offset
                stop  = min (cols - 1) $ i - offset + width - 1
                leftElement = getMedian $ lesserLeft `indexStream` (i - 1)
                insertCost  = cost gapGroup leftElement
                firstCell
                  | i <= offset = leftColumn
                  | otherwise   = leftBoundary

                lastCell
                  | i <= cols - quasiDiagonalWidth - offset = rightBoundary
                  | otherwise = rightColumn
            in  do -- Write to the first cell of the Ukkonen band
                   firstCell leftElement insertCost i start >>= write (i, start)
                   -- Write to the all the intermediary cells of the Ukkonen band
                   for_ [start + 1 .. stop - 1] $ \j -> {-# SCC internalCell_work #-}
                     let topElement = getMedian $ longerTop `indexStream` (j - 1) 
                         deleteCost = cost topElement    gapGroup
                         alignCost  = cost topElement leftElement
                     in  do
                           diagCost <- M.unsafeRead mCost (i - 1, j - 1)
                           topCost  <- M.unsafeRead mCost (i - 1, j    )
                           leftCost <- M.unsafeRead mCost (i    , j - 1)
                           write (i,j) $ minimum
                               [ ( alignCost + diagCost, DiagArrow)
                               , (deleteCost + leftCost, LeftArrow)
                               , (insertCost +  topCost, UpArrow  )
                               ]
                           -- Write to the last cell of the Ukkonen band
                           lastCell leftElement insertCost i stop >>= write (i, stop)

      ---------------------------------------
      -- Compute all values of the matrix  --
      ---------------------------------------

      -- Write to the origin to seed the first row.
      write (0, 0) (0, DiagArrow)

      -- Write the first row to seed subsequent rows.
      for_ [1 .. min (cols - 1) (width - offset - 1)] $ \j ->
        let topElement    = getMedian $ longerTop `indexStream` (j - 1)
            firstCellCost = cost gapGroup topElement
        in  do firstPrevCost <- M.unsafeRead mCost (0, j - 1)
               write (0,j) (firstCellCost + firstPrevCost, LeftArrow)

      -- Loop through the remaining rows.
      for_ [1 .. rows - 1] writeRow

      -- Return the matricies for possible expansion
      pure (mCost, mDir)


{-# SCC expandBandedMatrix #-}
expandBandedMatrix
  :: forall s a. DOCharConstraint a
  => (Subcomponent (Element a) -> Subcomponent (Element a) -> Word)
  -> a
  -> a
  -> MMatrix s Word
  -> MMatrix s Direction
  -> Word
  -> Word
  -> ST s ()
expandBandedMatrix cost longerTop lesserLeft mCost mDir po co = updateBand
  where
    
    -- Note: "offset" cannot cause "width + quasiDiagonalWidth" to exceed "2 * cols"
    offset      = let o' = fromEnum co in  min o' $ cols - quasiDiagonalWidth
    prevOffset  = fromEnum po
    gap         = gapOfStream longerTop
    gapGroup    = getMedian gap
    longerLen   = olength longerTop
    lesserLen   = olength lesserLeft
    rows        = olength lesserLeft + 1
    cols        = olength longerTop  + 1
    width       = quasiDiagonalWidth + (offset `shiftL` 1) -- Multiply by 2
    quasiDiagonalWidth = differenceInLength + 1
      where
        differenceInLength = longerLen - lesserLen

    updateBand = do

      ---------------------------------------
      -- Allocate mutable state variables  --
      ---------------------------------------

      headStop  <- newSTRef $ cols
      tailStart <- newSTRef $ cols

      ---------------------------------------
      -- Define some generalized functions --
      ---------------------------------------

      -- Write to a single cell of the current vector and directional matrix simultaneously
      let write !p ~(!c, !d) = M.unsafeWrite mCost p c *> M.unsafeWrite mDir p d

      -- Write to an internal cell (not on a boundary) of the matrix.
      let internalCell leftElement insertCost i j = {-# SCC internalCell_expanding #-}
            let topElement  = getMedian $ longerTop `indexStream` (j - 1)
                deleteCost  = cost topElement    gapGroup
                alignCost   = cost topElement leftElement
            in do
                  diagCost <- M.unsafeRead mCost (i - 1, j - 1)
                  topCost  <- M.unsafeRead mCost (i - 1, j    )
                  leftCost <- M.unsafeRead mCost (i    , j - 1)
                  pure $ minimum
                      [ ( alignCost + diagCost, DiagArrow)
                      , (deleteCost + leftCost, LeftArrow)
                      , (insertCost +  topCost, UpArrow  )
                      ]
      
      -- Define how to compute the first cell of the first "offest" rows.
      -- We need to ensure that there are only Up Arrow values in the directional matrix.
      -- We can also reduce the number of comparisons the first row makes from 3 to 1,
      -- since the diagonal and leftward values are "out of bounds."
      let leftColumn _leftElement insertCost i j = {-# SCC leftColumn #-} do
            firstPrevCost <- M.unsafeRead mCost (i - 1, j)
            pure (insertCost + firstPrevCost, UpArrow)

      -- Define how to compute the first cell of the remaining rows.
      -- We need to ensure that there are no Left Arrow values in the directional matrix.
      -- We can also reduce the number of comparisons the first row makes from 3 to 2,
      -- since the leftward values are "out of bounds."
      let leftBoundary leftElement insertCost i j = {-# SCC leftBoundary #-}
            let topElement = getMedian $ longerTop `indexStream` (j - 1)
                alignCost  = cost topElement leftElement
            in  do diagCost <- M.unsafeRead mCost (i - 1, j - 1)
                   topCost  <- M.unsafeRead mCost (i - 1, j    )
                   pure $ minimum
                       [ ( alignCost + diagCost, DiagArrow)
                       , (insertCost +  topCost, UpArrow  )
                       ]

      -- Define how to compute the last cell of the first "rows - offest" rows.
      -- We need to ensure that there are only Left Arrow values in the directional matrix.
      -- We can also reduce the number of comparisons the first row makes from 3 to 1,
      -- since the diagonal and upward values are "out of bounds."
      let rightBoundary leftElement _insertCost i j = {-# SCC rightBoundary #-}
            let topElement = getMedian $ longerTop `indexStream` (j - 1)
                deleteCost = cost topElement    gapGroup
                alignCost  = cost topElement leftElement
            in  do diagCost <- M.unsafeRead mCost (i - 1, j - 1)
                   leftCost <- M.unsafeRead mCost (i    , j - 1)
                   pure $ minimum
                       [ ( alignCost + diagCost, DiagArrow)
                       , (deleteCost + leftCost, LeftArrow)
                       ]

      let rightColumn = {-# SCC rightColumn #-} internalCell
      
      let computeCell leftElement insertCost i j = {-# SCC recomputeCell #-}
            let topElement = getMedian $ longerTop `indexStream` (j - 1) 
                deleteCost = cost topElement    gapGroup
                alignCost  = cost topElement leftElement
            in do
                  diagCost <- M.unsafeRead mCost (i - 1, j - 1)
                  topCost  <- M.unsafeRead mCost (i - 1, j    )
                  leftCost <- M.unsafeRead mCost (i    , j - 1)
                  oldCost  <- M.unsafeRead mCost (i    , j    )
                  let e@(c,_) = minimum
                                  [ ( alignCost + diagCost, DiagArrow)
                                  , (deleteCost + leftCost, LeftArrow)
                                  , (insertCost +  topCost, UpArrow  )
                                  ]
                  write (i,j) e
                  pure (c == oldCost, j)

      -- Define how to compute values to an entire row of the Ukkonen matrix.
      let extendRow i =
            -- Precomute some values that will be used for the whole row
            let start0 =  max 0          $ i - offset
                start3 =  min (cols - 1) $ i + width - offset - prevOffset
                goUpTo = (max 0          $ i - prevOffset) - 1
                stop   =  min (cols - 1) $ i + width - offset - 1
                leftElement = getMedian $ lesserLeft `indexStream` (i - 1)
                insertCost  = cost gapGroup leftElement
                firstCell
                  | i <= offset = leftColumn
                  | otherwise   = leftBoundary

                lastCell
                  | i <= cols - quasiDiagonalWidth - offset = rightBoundary
                  | otherwise = rightColumn

                continueRecomputing (changed, j) = changed || j >= stop - 1
                computeCell' ~(_,j) = computeCell leftElement insertCost i j
                internalCell' j = internalCell leftElement insertCost i j >>= write (i,j)
                recomputeUntilSame j = snd <$> iterateUntilM continueRecomputing computeCell' (False, j)
            in  do -- Get the starts from the previous iteration
                   start1 <- readSTRef headStop
                   start2 <- readSTRef tailStart

                   -- Conditionally write to the first cell of the Ukkonen band
                   if   i > prevOffset
                   then firstCell leftElement insertCost i start0 >>= write (i, start0)
                   else pure ()

                   for_ [start0+1 .. goUpTo] internalCell'
                   leadStop  <- if goUpTo < start0
                                then pure start1
                                else recomputeUntilSame $ goUpTo+1
                   headStop' <- if   leadStop >= start1
                                then pure leadStop
                                else recomputeUntilSame start1
                   tailStop' <- recomputeUntilSame start2
                   for_ [max (tailStop'+1) start3 .. stop-1] internalCell'

                   -- Conditionally write to the last cell of the Ukkonen band
                   if   tailStop' <= stop - 1
                   then lastCell leftElement insertCost i stop >>= write (i, stop)
                   else pure ()

                   -- Update references for the next row
                   writeSTRef headStop headStop'
                   writeSTRef tailStart $ if tailStop' /= start2 then tailStop' else start3

      ---------------------------------------
      -- Compute all values of the matrix  --
      ---------------------------------------

      let start = quasiDiagonalWidth + prevOffset
      -- Extend the first row to seed subsequent rows.
      for_ [start .. min (cols - 1) (width - offset - 1)] $ \j ->
        let topElement    = getMedian $ longerTop `indexStream` (j - 1)
            firstCellCost = cost gapGroup topElement
        in  do firstPrevCost <- M.unsafeRead mCost (0, j - 1)
               write (0,j) (firstCellCost + firstPrevCost, LeftArrow)
      writeSTRef tailStart start

      -- Loop through the remaining rows.
      for_ [1 .. rows - 1] extendRow


directOptimization
  :: ( DOCharConstraint s
     )
  => OverlapFunction (Subcomponent (Element s))
  -> (s -> s -> (Word, Matrix Direction))
  -> s
  -> s
  -> (Word, s)
directOptimization overlapλ matrixFunction char1 char2 =
    let ~(swapped, longerChar, shorterChar) = measureCharacters char1 char2
        ~(alignmentCost, traversalMatrix)   = matrixFunction longerChar shorterChar
        transformation   = if swapped then omap swapContext else id
        alignmentContext = traceback overlapλ traversalMatrix longerChar shorterChar
        alignment        = (alignmentCost, transformation alignmentContext)
    in  handleMissingCharacter char1 char2 alignment


{-# SCC traceback #-}
traceback
  :: DOCharConstraint s
  => OverlapFunction (Subcomponent (Element s))
  -> Matrix Direction
  -> s
  -> s
  -> s
traceback overlapFunction alignMatrix longerChar lesserChar = alignmentContext
  where
    f x y = fst $ overlapFunction x y
    gap = getMedian $ gapOfStream longerChar

    alignmentContext = dlistToDynamic $ go startPoint
    dlistToDynamic = constructDynamic . NE.fromList . toList

    longerLen = olength longerChar
    lesserLen = olength lesserChar
    rows      = lesserLen + 1
    cols      = longerLen + 1

    startPoint = (rows - 1, cols - 1)

    go !p@(~(!i, !j))
      | p == (0,0) = mempty
      | i < 0 = error $ "i is out of range: " <> show i
      | j < 0 = error $ "j is out of range: " <> show j
      | otherwise  =
        let previousSequence = go (row', col')

            directionArrow = unsafeIndex alignMatrix p

            (# !row', !col', !localContext #) =
                case directionArrow of
                  LeftArrow -> let j' = j - 1
                                   te = getMedian $ longerChar `indexStream` j'
                                   e  = deleteElement (f gap te) te
                               in (# i , j', e #)
                  UpArrow   -> let i' = i - 1
                                   le = getMedian $ lesserChar `indexStream` i'
                                   e  = insertElement (f le gap) le
                               in (# i', j, e #)
                  DiagArrow -> let i' = i - 1
                                   j' = j - 1
                                   te = getMedian $ longerChar `indexStream` j'
                                   le = getMedian $ lesserChar `indexStream` i'
                                   e  = alignElement (f le te) le te
                               in (# i', j', e #)

        in  previousSequence `snoc` localContext
