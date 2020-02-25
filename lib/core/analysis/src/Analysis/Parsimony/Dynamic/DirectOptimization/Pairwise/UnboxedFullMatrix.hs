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

{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
--{-# LANGUAGE Strict           #-}
{-# LANGUAGE TypeFamilies     #-}

module Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.UnboxedFullMatrix
  ( unboxedFullMatrixDO
  ) where

import           Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.Internal (Cost, Direction(..), DOCharConstraint, OverlapFunction, handleMissingCharacter, measureCharacters)
import           Bio.Character.Encodable
import           Data.DList                  (snoc)
import           Data.Foldable
import qualified Data.List.NonEmpty          as NE
import           Data.Matrix.Unboxed         (Matrix, (!), create, unsafeIndex)
import qualified Data.Matrix.Unboxed.Mutable as M
import           Data.Maybe                  (fromMaybe)
import           Data.MonoTraversable
import           Numeric.Extended.Natural

-- |
-- Performs a naive direct optimization.
-- Takes in two characters to run DO on and an overlap function
-- Returns an assignment character, the cost of that assignment, the assignment
-- character with gaps included, the aligned version of the first input character,
-- and the aligned version of the second input character. The process for this
-- algorithm is to generate a traversal matrix, then perform a traceback.
{-# INLINE unboxedFullMatrixDO #-}
{-# SPECIALISE unboxedFullMatrixDO :: DynamicCharacter -> DynamicCharacter -> OverlapFunction AmbiguityGroup -> (Word, DynamicCharacter) #-}
unboxedFullMatrixDO
  :: DOCharConstraint s
  => s
  -> s
  -> OverlapFunction (Subcomponent (Element s))
  -> (Word, s)
unboxedFullMatrixDO char1 char2 tcm = directOptimization char1 char2 tcm buildFullUnboxedMatrix


buildFullUnboxedMatrix
  :: DOCharConstraint s
  => s
  -> s
  -> OverlapFunction (Subcomponent (Element s))
  -> Matrix (Cost, Direction)
buildFullUnboxedMatrix topChar leftChar overlapFunction = fullMatrix
  where
    med  x y   = fst $ overlapFunction x y
    cost x y   = snd $ overlapFunction x y
    gap        = gapOfStream topChar
    gapGroup   = getMedian med gap
    rows       = olength leftChar + 1
    cols       = olength topChar  + 1

    fullMatrix = create $ do
      m <- M.new (rows, cols)
      let ref k@(i,j)
            | i <  0 || j <  0 = pure infinity
            | i == 0 && j == 0 = pure 0
            | otherwise      = fst <$> M.unsafeRead m k
      for_ [(r,c) | r <- [0 .. rows - 1], c <- [0 .. cols - 1]] $ \ ~(i,j) ->
        let
          topElement  = getMedian med . fromMaybe gap $  topChar `lookupStream` (j - 1)
          leftElement = getMedian med . fromMaybe gap $ leftChar `lookupStream` (i - 1)
          deleteCost  = fromFinite $ cost topElement    gapGroup
          alignCost   = fromFinite $ cost topElement leftElement
          insertCost  = fromFinite $ cost   gapGroup leftElement
        in do diagCost <- ref (i - 1, j - 1)
              topCost  <- ref (i - 1, j    )
              leftCost <- ref (i    , j - 1)
              M.unsafeWrite m (i, j) $ minimum
                  [ ( alignCost + diagCost, DiagArrow)
                  , (deleteCost + leftCost, LeftArrow)
                  , (insertCost +  topCost, UpArrow  )
                  ]
      pure m


directOptimization
  :: ( DOCharConstraint s
     )
  => s
  -> s
  -> OverlapFunction (Subcomponent (Element s))
  -> (s -> s -> OverlapFunction (Subcomponent (Element s)) -> Matrix (Cost, Direction))
  -> (Word, s)
directOptimization char1 char2 overlapλ matrixFunction =
    handleMissingCharacter char1 char2 alignment
  where
    (swapped, longerChar, shorterChar) = measureCharacters char1 char2
    traversalMatrix                    = matrixFunction longerChar shorterChar overlapλ
    (alignmentCost, alignmentContext)  = traceback overlapλ traversalMatrix longerChar shorterChar
    alignment                          = (alignmentCost, transformation alignmentContext)
    transformation
      | swapped   = omap swapContext
      | otherwise = id


traceback
  :: DOCharConstraint s
  => OverlapFunction (Subcomponent (Element s))
  -> Matrix (Cost, Direction)
  -> s
  -> s
  -> (Word, s)
traceback overlapFunction alignMatrix longerChar lesserChar = (finalCost, alignmentContext)
  where
    f x y = fst $ overlapFunction x y
    finalCost = unsafeToFinite cost

    alignmentContext = dlistToDynamic $ go lastCell
    lastCell  = (row, col)
    (cost, _) = alignMatrix ! lastCell

    dlistToDynamic = constructDynamic . NE.fromList . toList

    col = olength longerChar
    row = olength lesserChar

    go p@(i, j)
      | p == (0,0) = mempty
      | otherwise  = previousSequence `snoc` localContext
      where
        previousSequence = go (row', col')

        (_, directionArrow) = unsafeIndex alignMatrix p

        (row', col', localContext) =
            case directionArrow of
              LeftArrow -> let j' = j-1
                               te = longerChar `indexStream` j'
                               e  = deleteElement $ getMedian f te
                           in (i , j', e)
              UpArrow   -> let i' = i-1
                               le = lesserChar `indexStream` i'
                               e  = insertElement $ getMedian f le
                           in (i', j , e)
              DiagArrow -> let i' = i-1
                               j' = j-1
                               te = longerChar `indexStream` j'
                               le = lesserChar `indexStream` i'
                               e  = alignElement (getMedian f le) $ getMedian f te
                           in (i', j', e)
