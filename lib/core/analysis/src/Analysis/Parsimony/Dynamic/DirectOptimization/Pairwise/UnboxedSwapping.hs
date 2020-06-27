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
{-# LANGUAGE Strict           #-} -- This makes things a little bit faster
{-# LANGUAGE TypeFamilies     #-}

module Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.UnboxedSwapping
  ( unboxedSwappingDO
  ) where

import           Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.Internal (Direction(..), DOCharConstraint, OverlapFunction, measureAndUngapCharacters, measureCharacters)
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

--import Debug.Trace
trace = const id
traceShowId = id


-- |
-- Performs a naive direct optimization.
-- Takes in two characters to run DO on and an overlap function
-- Returns an assignment character, the cost of that assignment, the assignment
-- character with gaps included, the aligned version of the first input character,
-- and the aligned version of the second input character. The process for this
-- algorithm is to generate a traversal matrix, then perform a traceback.
{-# SCC unboxedSwappingDO #-}
{-# INLINE unboxedSwappingDO #-}
{-# SPECIALISE unboxedSwappingDO :: OverlapFunction AmbiguityGroup -> DynamicCharacter -> DynamicCharacter -> (Word, DynamicCharacter) #-}
unboxedSwappingDO
  :: DOCharConstraint s
  => OverlapFunction (Subcomponent (Element s))
  -> s
  -> s
  -> (Word, s)
unboxedSwappingDO tcm = directOptimization tcm buildDirectionMatrix


buildDirectionMatrix
  :: DOCharConstraint s
  => OverlapFunction (Subcomponent (Element s))
  -> s
  -> s
  -> (Word, Matrix Direction)
buildDirectionMatrix _ topChar leftChar | trace (unlines ["Top char: " <> show topChar, "Left char: " <> show leftChar]) False = undefined
buildDirectionMatrix overlapFunction topChar leftChar = fullMatrix
  where
    cost x y   = snd $ overlapFunction x y
    gap        = gapOfStream topChar
    gapGroup   = getMedian gap
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
        let topElement    = getMedian . fromMaybe gap $ topChar `lookupStream` (j - 1)
            firstCellCost = cost gapGroup topElement
        in  do firstPrevCost <- V.unsafeRead vOne (j - 1)
               write vOne (0,j) (firstCellCost + firstPrevCost) LeftArrow

      for_ [1 .. rows - 1] $ \i ->
        let (prev, curr)
              | odd i     = (vOne, vTwo)
              | otherwise = (vTwo, vOne)
            leftElement   = getMedian . fromMaybe gap $ leftChar `lookupStream` (i - 1)
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
                 let topElement  = getMedian . fromMaybe gap $  topChar `lookupStream` (j - 1)
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


{-# SCC directOptimization #-}
directOptimization
  :: DOCharConstraint s
  => OverlapFunction (Subcomponent (Element s))
  -> (OverlapFunction (Subcomponent (Element s)) -> s -> s -> (Word, Matrix Direction))
  -> s
  -> s
  -> (Word, s)
directOptimization overlapλ matrixFunction char1 char2
  | isMissing char1 = (0, char2)
  | isMissing char2 = (0, char1)
  | otherwise =
      let (swapped, gapsLesser, gapsLonger, shorterChar, longerChar) = measureAndUngapCharacters char1 char2
          (alignmentCost, ungappedAlignment) =
              if      olength shorterChar == 0
              then if olength  longerChar == 0
                   -- Niether character was Missing, but both are empty when gaps are removed
                   then (0, toMissing char1)
                   -- Niether character was Missing, but one of them is empty when gaps are removed
                   else let gap = getMedian $ gapOfStream char1
                            f x = let m = getMedian x in deleteElement (fst $ overlapλ m gap) m
                        in  (0, trace "Just one is gapped" $ omap f longerChar)
                   -- Both have some non-gap elements, perform string alignment
              else let (cost, dirMatrix) = matrixFunction overlapλ longerChar shorterChar
                   in  (cost, traceback overlapλ dirMatrix longerChar shorterChar)
          transformation    = if swapped then  trace "SWAPPING" $ omap swapContext else trace "NO SWAP" id
          regappedAlignment = insertGaps gapsLesser gapsLonger shorterChar longerChar ungappedAlignment
          alignmentContext  = transformation regappedAlignment
      in (alignmentCost, alignmentContext)


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
    gap = getMedian $ gapOfStream longerChar

    go p@(i, j)
      | p == (0,0) = mempty
      | otherwise  = 
        let previousSequence = go (row', col')
            directionArrow   = unsafeIndex alignMatrix p

            (row', col', localContext) =
                case directionArrow of
                  LeftArrow -> let j' = j-1
                                   te = getMedian $ longerChar `indexStream` j'
                                   e  = deleteElement (f gap te) te
                               in  (i , j', e)
                  UpArrow   -> let i' = i-1
                                   le = getMedian $ lesserChar `indexStream` i'
                                   e  = insertElement (f le gap) le
                               in  (i', j , e)
                  DiagArrow -> let i' = i-1
                                   j' = j-1
                                   te = getMedian $ longerChar `indexStream` j'
                                   le = getMedian $ lesserChar `indexStream` i'
                                   e  = alignElement (f le te) le te
                               in  (i', j', e)
        in previousSequence `snoc` localContext
