-----------------------------------------------------------------------------
-- |
-- Module      :  Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.Ukkonen.Internal
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

{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE TypeFamilies       #-}

module Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.Ukkonen.Internal
  ( ukkonenDO
  ) where


import           Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.Internal
import           Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.NeedlemanWunsch (naiveDOMemo)
import           Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.Ukkonen.Ribbon  (Ribbon)
import qualified Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.Ukkonen.Ribbon  as Ribbon
import           Bio.Character.Encodable
import           Data.Bits
import           Data.Foldable
import           Data.Key
import           Data.MonoTraversable
import           Data.Vector.Instances                                                  ()
import           Numeric.Extended.Natural
import           Prelude                                                                hiding (lookup)


-- |
-- Time & space saving data structure for computing only a central "ribbon" of
-- a two-dimensional matrix.
--
-- Allocates a "ribbon" down the diagonal plus an offset of the matrix rather
-- than the entire matrix. The computed ribbon of the matrix is expanded until
-- optimality of the the result can be guaranteed. The ribbon is expanded at most
-- a logrithmic number of times in terms of the matrix dimensions.
--
-- Use the 'createUkkonenMethodMatrix' function to create this effcient structure.
newtype UkkonenMethodMatrix a = U (Ribbon a)
  deriving stock (Eq, Foldable, Functor)


type instance Key UkkonenMethodMatrix = (Int, Int)


instance Indexable UkkonenMethodMatrix where

    index (U r) k = r ! k


instance Lookup UkkonenMethodMatrix where

      k `lookup` (U x) = k `lookup` x


-- |
-- /O( (n - m + 1 ) * log(n - m + 1) )/, /n/ >= /m/
--
-- Compute the alignment of two dynamic characters and the median states by
-- using Ukkonen's string edit distance algorthim to improve space and time
-- complexity.
{-# INLINE ukkonenDO #-}
{-# SPECIALISE ukkonenDO :: DynamicCharacter -> DynamicCharacter -> OverlapFunction AmbiguityGroup -> (Word, DynamicCharacter) #-}
ukkonenDO
  :: DOCharConstraint s
  => s
  -> s
  -> OverlapFunction (Subcomponent (Element s))
  -> (Word, s)
ukkonenDO char1 char2 overlapFunction
  | noGainFromUkkonenMethod = naiveDOMemo char1 char2 overlapFunction
  | otherwise               = directOptimization char1 char2 overlapFunction $ createUkkonenMethodMatrix coefficient
  where
    (_, longer, lesser) = measureCharacters char1 char2
    
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
        gap            = getMedian (\x y -> fst $ overlapFunction x y) $ gapOfStream char1
        alphabetSize   = fromEnum $ symbolCount gap
        nonGapElements = [ 0 .. alphabetSize - 2 ]
        indelCost i    = min (snd (overlapFunction (bit i)  gap    ))
                             (snd (overlapFunction  gap    (bit i) ))


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
{-# INLINE createUkkonenMethodMatrix #-}
{-# SPECIALISE createUkkonenMethodMatrix :: Word -> DynamicCharacter -> DynamicCharacter -> OverlapFunction AmbiguityGroup -> UkkonenMethodMatrix (Cost, Direction) #-}
createUkkonenMethodMatrix
  :: DOCharConstraint s
  => Word -- ^ Coefficient value, representing the /minimum/ transition cost from a state to gap
  -> s    -- ^ Longer dynamic character
  -> s    -- ^ Shorter dynamic character
  -> OverlapFunction (Subcomponent (Element s)) -- ^ Function to determine the cost a median state between two other states.
  -> UkkonenMethodMatrix (Cost, Direction)
createUkkonenMethodMatrix minimumIndelCost longerTop lesserLeft overlapFunction = U $ ukkonenUntilOptimal startOffset
  where
    -- General values that need to be in scope for the recursive computations.
    longerLen   = olength longerTop
    lesserLen   = olength lesserLeft
    coefficient = fromEnum minimumIndelCost
    rows        = toEnum lesserLen + 1
    cols        = toEnum longerLen + 1

    -- We start the offset at two rather than at one so that the first doubling
    -- isn't trivially small.
    startOffset = 2

    -- /O(1)/
    --
    -- Necessary to compute the width of a row in the barrier-constrained matrix.
    quasiDiagonalWidth = differenceInLength + 1
      where
        differenceInLength = longerLen - lesserLen

    -- /O(n + m)/
    --
    -- If one or more of the aligned character elements contained a gap, diagonal
    -- directions in the matrix have an "indel" cost. 'gapsPresentInInputs' is
    -- necessary in order to decrement the threshold value to account for this.
    -- This was not described in Ukkonen's original paper, as the inputs were assumed
    -- not to contain any gaps.
    gapsPresentInInputs = longerGaps + lesserGaps
      where
        longerGaps = countGaps longerTop
        lesserGaps = countGaps lesserLeft
        countGaps  = length . filter (hasGap . getMedian med) . otoList
        hasGap b   = popCount (b .&. gap) > 0
        med x y    = fst $ overlapFunction x y
        gap = getMedian med $ gapOfStream longerTop

    ukkonenUntilOptimal offset
      | threshold <= alignmentCost = ukkonenUntilOptimal $ 2 * offset
      | otherwise                  = ukkonenMatrix
      where
        ukkonenMatrix      = Ribbon.generate rows cols generatingFunction $ toEnum offset
        generatingFunction = needlemanWunschDefinition longerTop lesserLeft overlapFunction ukkonenMatrix
        (cost, _)          = ukkonenMatrix ! (lesserLen, longerLen)
        alignmentCost      = unsafeToFinite cost
        computedValue      = coefficient * (quasiDiagonalWidth + offset - gapsPresentInInputs)
        threshold          = toEnum $ max 0 computedValue -- The threshold value must be non-negative

{--
        renderedMatrix = renderCostMatrix longerTop lesserLeft ukkonenMatrix

        renderedBounds = unlines
            [ "Diag Width : " <> show quasiDiagonalWidth
            , "Input Gaps : " <> show gapsPresentInInputs
            , "Offset     : " <> show offset
            , "Coefficient: " <> show coefficient
            , "Threshold  : " <> show threshold
            , "Total Cost : " <> show alignmentCost
            ]
--}
