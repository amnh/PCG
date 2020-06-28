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
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Strict           #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE UnboxedTuples    #-}

module Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.UnboxedFullMatrix
  ( unboxedFullMatrixDO
  ) where

import           Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.Internal (Direction(..), DOCharConstraint, OverlapFunction, measureAndUngapCharacters)
import           Bio.Character.Encodable
import           Data.DList                  (snoc)
import           Data.Foldable
import qualified Data.List.NonEmpty          as NE
import           Data.Matrix.Unboxed         (Matrix, (!), create, unsafeIndex)
import qualified Data.Matrix.Unboxed.Mutable as M
import           Data.MonoTraversable
import           Numeric.Extended.Natural


-- |
-- Performs a naive direct optimization.
-- Takes in two characters to run DO on and an overlap function
-- Returns an assignment character, the cost of that assignment, the assignment
-- character with gaps included, the aligned version of the first input character,
-- and the aligned version of the second input character. The process for this
-- algorithm is to generate a traversal matrix, then perform a traceback.
{-# SCC unboxedFullMatrixDO #-}
{-# INLINE unboxedFullMatrixDO #-}
{-# SPECIALISE unboxedFullMatrixDO :: OverlapFunction AmbiguityGroup -> DynamicCharacter -> DynamicCharacter -> (Word, DynamicCharacter) #-}
unboxedFullMatrixDO
  :: DOCharConstraint s
  => OverlapFunction (Subcomponent (Element s))
  -> s
  -> s
  -> (Word, s)
unboxedFullMatrixDO tcm = directOptimization tcm buildFullUnboxedMatrix


buildFullUnboxedMatrix
  :: DOCharConstraint s
  => OverlapFunction (Subcomponent (Element s))
  -> s
  -> s
  -> Matrix (Word, Direction)
buildFullUnboxedMatrix overlapFunction topChar leftChar = fullMatrix
  where
    cost x y   = snd $ overlapFunction x y
    gap        = getMedian $ gapOfStream topChar
    rows       = olength leftChar + 1
    cols       = olength topChar  + 1

    fullMatrix = create $ do
      m <- M.new (rows, cols)
      
      let ref = fmap fst . M.unsafeRead m
      
      -- Write to the origin to seed the first row.
      M.unsafeWrite m (0,0) (0, DiagArrow)

      -- Write the first row to seed subsequent rows.
      for_ [1 .. cols - 1] $ \j ->
        let topElement    = getMedian $ topChar `indexStream` (j - 1)
            firstCellCost = cost gap topElement
        in  do firstPrevCost <- ref (0, j - 1)
               M.unsafeWrite m (0,j) (firstCellCost + firstPrevCost, LeftArrow)

      -- Write the first column to seed subsequent coulmns.
      for_ [1 .. rows - 1] $ \i ->
        let leftElement   = getMedian $ leftChar `indexStream` (i - 1)
            firstCellCost = cost leftElement gap
        in  do firstPrevCost <- ref (0, i - 1)
               M.unsafeWrite m (i,0) (firstCellCost + firstPrevCost, UpArrow)

      -- Write the remaining rows
      for_ [ (r ,c) | r <- [1 .. rows - 1], c <- [1 .. cols - 1]] $ \ ~(i, j) ->
        let topElement             = getMedian   $  topChar `indexStream` (j - 1)
            leftElement            = getMedian   $ leftChar `indexStream` (i - 1)
            deleteCost             = fromFinite  $ cost            topElement         gap
            (alignElem, alignCost) = fromFinite <$> overlapFunction topElement leftElement
            insertCost             = fromFinite  $ cost            gap        leftElement
        in  do diagCost <- ref (i - 1, j - 1)
               topCost  <- ref (i - 1, j    )
               leftCost <- ref (i    , j - 1)
               M.unsafeWrite m (i, j) $ getMinimalResult gap alignElem
                   [ ( alignCost + diagCost, DiagArrow)
                   , (deleteCost + leftCost, LeftArrow)
                   , (insertCost +  topCost, UpArrow  )
                   ]
      pure m
{-
        let leftElement = maybe gap getMedian $ leftChar `lookupStream` (i - 1)
            topElement  = maybe gap getMedian $  topChar `lookupStream` (j - 1)
        in  -- Preserve the gap in the left (lesser) string
            if      leftElement == gap 
            then ref (i - 1, j    ) >>= (\c -> M.unsafeWrite m (i, j) (c,   UpArrow))
            -- Preserve the gap in the top  (longer) string
            else if topElement == gap 
            then ref (i    , j - 1) >>= (\c -> M.unsafeWrite m (i, j) (c, LeftArrow))
            else let deleteCost  = fromFinite $ cost topElement         gap
                     alignCost   = fromFinite $ cost topElement leftElement
                     insertCost  = fromFinite $ cost        gap leftElement
                 in  do diagCost <- ref (i - 1, j - 1)
                        topCost  <- ref (i - 1, j    )
                        leftCost <- ref (i    , j - 1)
                        M.unsafeWrite m (i, j) $ minimum
                            [ ( alignCost + diagCost, DiagArrow)
                            , (deleteCost + leftCost, LeftArrow)
                            , (insertCost +  topCost, UpArrow  )
                            ]
      pure m
-}


getMinimalResult
  :: ( Eq a
     , Foldable f
     )
  => a
  -> a
  -> f (Word, Direction)
  -> (Word, Direction)
getMinimalResult gap alignElem opts =
    let v@(~(c,d)) = minimum opts
    in  if   d == DiagArrow && alignElem == gap
        then (c, LeftArrow)
        else v


directOptimization
  :: ( DOCharConstraint s
     )
  => OverlapFunction (Subcomponent (Element s))
  -> (OverlapFunction (Subcomponent (Element s)) -> s -> s -> Matrix (Word, Direction))
  -> s
  -> s
  -> (Word, s)
directOptimization overlapλ matrixFunction char1 char2
  | isMissing char1 = (0, char2)
  | isMissing char2 = (0, char1)
  | otherwise =
      let ~(swapped, gapsLesser, gapsLonger, shorterChar, longerChar) = measureAndUngapCharacters char1 char2
          ~(alignmentCost, ungappedAlignment) =
              if      olength shorterChar == 0
              then if olength  longerChar == 0
                   -- Niether character was Missing, but both are empty when gaps are removed
                   then (0, toMissing char1)
                   -- Niether character was Missing, but one of them is empty when gaps are removed
                   else let gap = getMedian $ gapOfStream char1
                            f x = let m = getMedian x in deleteElement (fst $ overlapλ m gap) m
                        in  (0, omap f longerChar)
                   -- Both have some non-gap elements, perform string alignment
              else let traversalMatrix = matrixFunction overlapλ longerChar shorterChar
                   in  traceback overlapλ traversalMatrix longerChar shorterChar
          transformation    = if swapped then omap swapContext else id
          regappedAlignment = insertGaps gapsLesser gapsLonger shorterChar longerChar ungappedAlignment
          alignmentContext  = transformation regappedAlignment
      in  (alignmentCost, alignmentContext)


traceback
  :: DOCharConstraint s
  => OverlapFunction (Subcomponent (Element s))
  -> Matrix (Word, Direction)
  -> s
  -> s
  -> (Word, s)
traceback overlapFunction alignMatrix longerChar lesserChar = (cost, alignmentContext)
  where
    f x y = fst $ overlapFunction x y

    alignmentContext = dlistToDynamic $ go lastCell
    lastCell  = (row, col)
    (cost, _) = alignMatrix ! lastCell

    dlistToDynamic = constructDynamic . NE.fromList . toList

    col = olength longerChar
    row = olength lesserChar
    gap = getMedian $ gapOfStream longerChar

    go p@(~(i, j))
      | p == (0,0) = mempty
      | otherwise  = 
        let previousSequence = go (row', col')

            (_, directionArrow) = unsafeIndex alignMatrix p

            (# row', col', localContext #) =
                case directionArrow of
                  LeftArrow -> let j' = j - 1
                                   te = getMedian $ longerChar `indexStream` j'
                                   e  = deleteElement (f gap te) te
                               in (# i , j', e #)
                  UpArrow   -> let i' = i - 1
                                   le = getMedian $ lesserChar `indexStream` i'
                                   e  = insertElement (f le gap) le
                               in (# i', j , e #)
                  DiagArrow -> let i' = i - 1
                                   j' = j - 1
                                   te = getMedian $ longerChar `indexStream` j'
                                   le = getMedian $ lesserChar `indexStream` i'
                                   e  = alignElement (f le te) le te
                               in (# i', j', e #)
        in  previousSequence `snoc` localContext


{-
-- |
-- Serializes an alignment matrix to a 'String'. Uses input characters for row
-- and column labelings.
--
-- Useful for debugging purposes.
renderCostMatrix
  :: DOCharConstraint s
  => Subcomponent (Element s)
  -> s
  -> s
  -> Matrix (Word, Direction)
  -> String
renderCostMatrix gapGroup lhs rhs mat = unlines
    [ dimensionPrefix
    , headerRow
    , barRow
    , renderedRows
    ]
  where
    (_,longer,lesser) = measureCharacters lhs rhs
    longerTokens      = toShownIntegers longer
    lesserTokens      = toShownIntegers lesser
    toShownIntegers   = fmap renderContext . otoList
    matrixTokens      = fmap (fmap showCell) $ toLists mat
    showCell (0,DiagArrow) = ""
    showCell (c,d)    = show c <> show d
    maxPrefixWidth    = maxLengthOf lesserTokens
    maxColumnWidth    = max (maxLengthOf longerTokens) . maxLengthOf $ fold matrixTokens
    maxLengthOf       = maximum . fmap length

    colCount = olength longer + 1
    rowCount = olength lesser + 1

    dimensionPrefix  = " " <> unwords
        [ "Dimensions:"
        , show rowCount
        , "⨉"
        , show colCount
        ]

    headerRow = fold
        [ " "
        , pad maxPrefixWidth "⊗"
        , "┃ "
        , pad maxColumnWidth "⁎"
        , concatMap (pad maxColumnWidth) longerTokens
        ]

    barRow    = fold
        [ " "
        , bar maxPrefixWidth
        , "╋"
        , concatMap (const (bar maxColumnWidth)) $ undefined : longerTokens
        ]
      where
        bar n = replicate (n+1) '━'

    renderedRows = unlines . zipWith renderRow ("⁎":lesserTokens) $ matrixTokens
      where
        renderRow e vs = " " <> pad maxPrefixWidth e <> "┃ " <> concatMap (pad maxColumnWidth) vs

    renderContext v
      | isAlign  v = if getMedian v == gapGroup then "—" else "α"
      | isDelete v = if getMedian v == gapGroup then "—" else "δ"
      | isInsert v = if getMedian v == gapGroup then "—" else "ι"
      | otherwise  = "—" -- isGap v

    pad :: Int -> String -> String
    pad n e = replicate (n - len) ' ' <> e <> " "
      where
        len = length e
-}
