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

{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
--{-# LANGUAGE Strict           #-}
{-# LANGUAGE TypeFamilies     #-}

module Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.UnboxedSwapping
  ( unboxedSwappingDO
  ) where

import           Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.Internal (Direction(..), DOCharConstraint, OverlapFunction, handleMissingCharacter, measureCharacters)
import           Bio.Character.Encodable
import           Control.Monad.ST
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
{-# INLINE unboxedSwappingDO #-}
{-# SPECIALISE unboxedSwappingDO :: DynamicCharacter -> DynamicCharacter -> OverlapFunction AmbiguityGroup -> (Word, DynamicCharacter) #-}
unboxedSwappingDO
  :: DOCharConstraint s
  => s
  -> s
  -> OverlapFunction (Subcomponent (Element s))
  -> (Word, s)
unboxedSwappingDO char1 char2 tcm = directOptimization char1 char2 tcm buildDirectionMatrix


buildDirectionMatrix
  :: DOCharConstraint s
  => s
  -> s
  -> OverlapFunction (Subcomponent (Element s))
  -> (Word, Matrix Direction)
buildDirectionMatrix topChar leftChar overlapFunction = fullMatrix
  where
    med  x y   = fst $ overlapFunction x y
    cost x y   = snd $ overlapFunction x y
    gap        = gapOfStream topChar
    gapGroup   = getMedian med gap
    rows       = olength leftChar + 1
    cols       = olength topChar  + 1

    fullMatrix = runST $ do
      mDir <- M.new (rows, cols)
      vOne <- V.new cols
      vTwo <- V.new cols

      let write v !p@(_,!j) !c !d = V.unsafeWrite v j c *> M.unsafeWrite mDir p d

      write vOne (0,0) 0 DiagArrow

      -- Special case the first row
      -- We need to ensure that there are only Left Arrow values in the directional matrix.
      -- We can also reduce the number of comparisons the first row makes from 3 to 1,
      -- since the diagonal and upward values are "out of bounds."
      for_ [1 .. cols - 1] $ \j ->
        let topElement    = getMedian med . fromMaybe gap $ topChar `lookupStream` (j - 1)
            firstCellCost = cost gapGroup topElement
        in  do firstPrevCost <- V.unsafeRead vOne (j - 1)
               write vOne (0,j) (firstCellCost + firstPrevCost) LeftArrow

      for_ [1 .. rows - 1] $ \i ->
        let (prev, curr)
              | odd i     = (vOne, vTwo)
              | otherwise = (vTwo, vOne)
            leftElement   = getMedian med . fromMaybe gap $ leftChar `lookupStream` (i - 1)
            -- Special case the first cell of each row
            -- We need to ensure that there are only Up Arrow values in the directional matrix.
            -- We can also reduce the number of comparisons the first row makes from 3 to 1,
            -- since the diagonal and leftward values are "out of bounds."
            firstCellCost = cost leftElement gapGroup 
        in  do firstPrevCost <- V.unsafeRead prev 0
               write curr (i,0) (firstCellCost + firstPrevCost) UpArrow
               -- Finish special case for first cell of each row
               -- Begin processing all other cells in the curr vector
               for_ [1 .. cols - 1] $ \j ->
                 let topElement  = getMedian med . fromMaybe gap $  topChar `lookupStream` (j - 1)
                     deleteCost  = cost topElement    gapGroup
                     alignCost   = cost topElement leftElement
                     insertCost  = cost gapGroup   leftElement
                 in  do diagCost <- V.unsafeRead prev $ j - 1
                        topCost  <- V.unsafeRead prev $ j
                        leftCost <- V.unsafeRead curr $ j - 1
                        let xs = [ ( alignCost + diagCost, DiagArrow)
                                 , (deleteCost + leftCost, LeftArrow)
                                 , (insertCost +  topCost, UpArrow  )
                                 ]
                        let (c,d) = minimum xs
                        write curr (i,j) c d

      let v | odd  rows = vOne
            | otherwise = vTwo
      c <- V.unsafeRead v (cols - 1)
      m <-   unsafeFreeze mDir
      pure (c, m)


directOptimization
  :: ( DOCharConstraint s
     )
  => s
  -> s
  -> OverlapFunction (Subcomponent (Element s))
  -> (s -> s -> OverlapFunction (Subcomponent (Element s)) -> (Word, Matrix Direction))
  -> (Word, s)
directOptimization char1 char2 overlapλ matrixFunction =
    handleMissingCharacter char1 char2 alignment
  where
    (swapped, longerChar, shorterChar) = measureCharacters char1 char2
    (alignmentCost, traversalMatrix)   = matrixFunction longerChar shorterChar overlapλ
    alignmentContext                   = traceback overlapλ traversalMatrix longerChar shorterChar
    alignment                          = (alignmentCost, transformation alignmentContext)
    transformation
      | swapped   = omap swapContext
      | otherwise = id


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

    alignmentContext = dlistToDynamic $ go lastCell
    lastCell  = (row, col)

    dlistToDynamic = constructDynamic . NE.fromList . toList

    col = olength longerChar
    row = olength lesserChar

    go p@(i, j)
      | p == (0,0) = mempty
      | otherwise  = previousSequence `snoc` localContext
      where
        previousSequence = go (row', col')

        directionArrow = unsafeIndex alignMatrix p

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
