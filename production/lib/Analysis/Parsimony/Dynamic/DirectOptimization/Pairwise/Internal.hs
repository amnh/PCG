-----------------------------------------------------------------------------
-- |
-- Module      :  Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.Internal
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Direct optimization functionality for binary trees
--
-----------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds, FlexibleContexts, TypeFamilies #-}

module Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.Internal where

import Bio.Character.Encodable
import Data.Bits
import Data.DList            (snoc)
import Data.Foldable
import Data.Key
import Data.Matrix.NotStupid (Matrix, getRow, matrix, nrows, ncols, toLists)
import Data.Maybe            (fromMaybe)
import Data.MonoTraversable
import Data.Ord
import Data.Semigroup
import Numeric.Extended.Natural
import Prelude        hiding (lookup, zipWith)

import Debug.Trace


-- |
-- The direction to align the character at a given matrix point.
--
-- It should be noted that the ordering of the three arrow types are important
-- as it guarantees that the derived Ord instance will have the following
-- property:
--
-- DiagArrow < LeftArrow < UpArrow
--
-- This means:
--
--   - DiagArrow is biased towards most   when one or more costs are equal
--
--   - LeftArrow is biased towards second when one or more costs are equal
--
--   -   UpArrow is biased towards least  when one or more costs are equal
--
-- Using this Ord instance, we can resolve ambiguous transformations in a
-- deterministic way. Without loss of generality in determining the ordering,
-- we choose the same biasing as the C code called from the FFI for consistency.
data Direction = DiagArrow | LeftArrow | UpArrow
  deriving (Eq, Ord)


-- | (✔)
instance Show Direction where

    show DiagArrow = "↖"
    show LeftArrow = "←"
    show UpArrow   = "↑"


-- |
-- This internal type used for computing the alignment cost. This type has an
-- "infinity" value that is conviently used for the barrier costs. The cost is
-- strictly non-negative, and possibly infinite.
type Cost = ExtendedNatural


-- |
-- A representation of an alignment matrix for DO.
-- The matrix itself stores tuples of the cost and direction at that position.
-- We also store a vector of characters that are generated.
type NeedlemanWunchMatrix s = Matrix (Cost, Direction, s)


-- |
-- Constraints on the input dynamic characters that direct optiomization operates on.
type DOCharConstraint c = (EncodableDynamicCharacter c, {- Show c, -} Show (Element c), Integral (Element c))


type MatrixConstraint m = (Indexable m, Key m ~ (Int, Int))


-- |
-- A generalized function represention the "overlap" between dynamic character
-- elements, supplying the corresponding median and cost to align the two
-- characters.
type OverlapFunction c = c -> c -> (c, Word)



type MatrixFunction m s = s -> s -> OverlapFunction (Element s) -> m (Cost, Direction, Element s)


-- |
-- Wraps the primative operations in this module to a cohesive operation that is
-- parameterized by an 'OverlapFunction'.
--
-- Reused internally by different implementations.
directOptimization
  :: ( DOCharConstraint s
     , MatrixConstraint m
     )
  => s
  -> s
  -> OverlapFunction (Element s)
  -> MatrixFunction m s
  -> (Word, s, s, s, s)
directOptimization char1 char2 overlapFunction matrixFunction =
    handleMissingCharacter char1 char2 alignment
  where
    (swapped, longerChar, shorterChar)   = measureCharacters char1 char2
    traversalMat                         = matrixFunction longerChar shorterChar overlapFunction
    (alignmentCost, gapped, left, right) = traceback traversalMat longerChar shorterChar
    alignment = (alignmentCost, ungapped, gapped, alignedChar1, alignedChar2)
{-      
    (gapped', left', right') = (\(x,y,z) -> (constructDynamic x, constructDynamic y, constructDynamic z)) 
                               $ correctBiasing (getGapElement $ gapped `indexStream` 0) (otoList gapped, otoList left, otoList right)
-}
    ungapped = filterGaps gapped
    (alignedChar1, alignedChar2)
      | swapped   = (right, left )
      | otherwise = (left , right)


-- |
-- A generalized function to handle missing dynamic characters.
--
-- Intended to be resued by multiple, differing implementations.
handleMissingCharacter
  :: PossiblyMissingCharacter s
  => s
  -> s
  -> (Word, s, s, s, s)
  -> (Word, s, s, s, s) 
handleMissingCharacter lhs rhs v =
    -- Appropriately handle missing data:
    case (isMissing lhs, isMissing rhs) of
      (True , True ) -> (0, lhs, lhs, lhs, rhs) --WLOG
      (True , False) -> (0, rhs, rhs, rhs, rhs)
      (False, True ) -> (0, lhs, lhs, lhs, lhs)
      (False, False) -> v


-- |
-- Performs a naive direct optimization.
-- Takes in two characters to run DO on and a metadata object
-- Returns an assignment character, the cost of that assignment, the assignment
-- character with gaps included, the aligned version of the first input character,
-- and the aligned version of the second input character. The process for this
-- algorithm is to generate a traversal matrix, then perform a traceback.
naiveDO :: DOCharConstraint s
        => s                       -- ^ First  dynamic character
        -> s                       -- ^ Second dynamic character
        -> (Word -> Word -> Word)  -- ^ Structure defining the transition costs between character states
        -> (Word, s, s, s, s)      -- ^ The cost of the alignment
                                   -- 
                                   --   The /ungapped/ character derived from the the input characters' N-W-esque matrix traceback
                                   -- 
                                   --   The /gapped/ character derived from the the input characters' N-W-esque matrix traceback
                                   -- 
                                   --   The gapped alignment of the /first/ input character when aligned with the second character
                                   -- 
                                   --   The gapped alignment of the /second/ input character when aligned with the first character

-- naiveDO char1 char2 costStruct = handleMissingCharacter char1 char2 $ naiveDOInternal char1 char2 (overlap costStruct)
naiveDO char1 char2 costStruct = directOptimization char1 char2 (overlap costStruct) createNeedlemanWunchMatrix


-- |
-- The same as 'naiveDO' except that the "cost structure" parameter is ignored.
-- Instead a constant cost is used.
naiveDOConst :: DOCharConstraint s => s -> s -> (Word -> Word -> Word) -> (Word, s, s, s, s)
-- naiveDOConst char1 char2 _ = handleMissingCharacter char1 char2 $ naiveDOInternal char1 char2 overlapConst
naiveDOConst char1 char2 _ = directOptimization char1 char2 overlapConst createNeedlemanWunchMatrix


-- |
-- The same as 'naiveDO' except that the "cost structure" parameter is assumed to
-- be a memoized overlap function.
naiveDOMemo :: DOCharConstraint s
            => s
            -> s
            -> OverlapFunction (Element s)
            -> (Word, s, s, s, s)
naiveDOMemo char1 char2 tcm = directOptimization char1 char2 tcm createNeedlemanWunchMatrix


-- |
-- Wrapper function to do an enhanced Needleman-Wunsch algorithm.
-- Calls naiveDO, but only returns the last two fields (gapped alignments of inputs)
doAlignment :: DOCharConstraint s => s -> s -> (Word -> Word -> Word) -> (s, s)
doAlignment char1 char2 costStruct = (char1Align, char2Align)
    where
        (_, _, _, char1Align, char2Align) = naiveDO char1 char2 costStruct
        

-- |
-- Strips the gap elements from the supplied character.
-- 
-- If the character contains /only/ gaps, a missing character is returned.
filterGaps :: EncodableDynamicCharacter c => c -> c
filterGaps char =
    case filter (/= gap) $ otoList char of
      [] -> toMissing char
      xs -> constructDynamic xs
  where
    gap = gapOfStream char


-- |
-- Main function to generate an 'NeedlemanWunchMatrix'. Works as in Needleman-Wunsch,
-- but allows for multiple indel/replacement costs, depending on the symbol change
-- cost function. Also, returns the aligned parent characters, with appropriate
-- ambiguities, as the third of each tuple in the matrix.
--
-- Takes in two 'EncodableDynamicCharacter's and a 'CostStructure'. The first
-- character must be the longer of the two and is the top labeling of the matrix.
-- Returns an 'NeedlemanWunchMatrix'.
createNeedlemanWunchMatrix :: DOCharConstraint s => s -> s -> OverlapFunction (Element s) -> NeedlemanWunchMatrix (Element s)
--createNeedlemanWunchMatrix topChar leftChar overlapFunction = trace renderedMatrix result
createNeedlemanWunchMatrix topChar leftChar overlapFunction = result
  where
    result             = matrix rows cols generatingFunction
    rows               = olength leftChar + 1
    cols               = olength topChar  + 1
    generatingFunction = needlemanWunschDefinition topChar leftChar overlapFunction result
    renderedMatrix     = renderCostMatrix topChar leftChar result


-- Internal generator function for the matrix
-- Deals with both first row and other cases.
needlemanWunschDefinition
  :: (DOCharConstraint s, Indexable f, Key f ~ (Int,Int))
  => s
  -> s
  -> OverlapFunction (Element s)
  -> f (Cost, Direction, Element s)
  -> (Int, Int)
  -> (Cost, Direction, Element s)
needlemanWunschDefinition topChar leftChar overlapFunction memo (row, col) | trace ("nw ?? "<> show (row,col)) False = undefined
needlemanWunschDefinition topChar leftChar overlapFunction memo (row, col)
      -- :)
      | row == 0 && col == 0         = (0                               , DiagArrow,         gap)
      | row == 0 && rightChar /= gap = (leftwardValue + rightOverlapCost, LeftArrow,   rightChar)
      | row == 0                     = (leftwardValue                   , LeftArrow,   rightChar)
      | col == 0 &&  downChar /= gap = (  upwardValue +  downOverlapCost,   UpArrow,    downChar)
      | col == 0                     = (  upwardValue                   ,   UpArrow,    downChar)
      | leftElement == gap &&
         topElement == gap           = (diagCost                        , DiagArrow,         gap)
      | otherwise                    = (minCost                         , minDir   ,    minState)
      where
        gap                           = gapOfStream topChar
        (!?) m k =
          case k `lookup` m of
            Just  v -> v
            Nothing -> (infinity, DiagArrow, gap)
        
        topElement                    =  topChar `indexStream` (col - 1)
        leftElement                   = leftChar `indexStream` (row - 1)
        (leftwardValue, _, _)         = memo !? (row    , col - 1)
        (diagonalValue, _, _)         = memo !? (row - 1, col - 1)
        (  upwardValue, _, _)         = memo !? (row - 1, col    )
        (rightChar, rightOverlapCost) = fromFinite <$> overlapFunction topElement  gap
        ( diagChar,  diagOverlapCost) = fromFinite <$> overlapFunction topElement  leftElement 
        ( downChar,  downOverlapCost) = fromFinite <$> overlapFunction gap         leftElement
        rightCost                     = rightOverlapCost + leftwardValue
        diagCost                      =  diagOverlapCost + diagonalValue
        downCost                      =  downOverlapCost +   upwardValue
        (minCost, minState, minDir)   = getMinimalCostDirection
                                          ( diagCost,  diagChar)
                                          (rightCost, rightChar)
                                          ( downCost,  downChar)
{-                                        
        err = unlines
          [ show (row, col)
          , "  right: " <> show (fromIntegral rightChar, rightOverlapCost, leftwardValue, rightCost)
          , "   down: " <> show (fromIntegral  downChar,  downOverlapCost,   upwardValue,  downCost)
          , "   diag: " <> show (fromIntegral  diagChar,  diagOverlapCost, diagonalValue,  diagCost)
          , "Chosen:"
          , "  " <> show (minCost, fromIntegral minState, minDir) 
          ]            
-}


-- |
-- Serializes an alignment matrix to a 'String'. Omits the median characters in
-- the matrix.
--
-- Useful for debugging purposes.
renderMatrix :: NeedlemanWunchMatrix a -> String
renderMatrix mat = unlines . fmap unwords . toLists $ showCell <$> mat
  where
    showCell (c,d,_) = show (c, d)


-- |
-- Performs the traceback of an 'NeedlemanWunchMatrix'.
--
-- Takes in an 'NeedlemanWunchMatrix', two 'EncodableDynamicCharacter's and returns an
-- aligned 'EncodableDynamicCharacter', as well as the aligned versions of the
-- two inputs. Essentially does the second step of Needleman-Wunsch, following
-- the arrows from the bottom right corner, accumulating the sequences as it goes,
-- but returns three alignments: the left character, the right character, and the
-- parent. The child alignments *should* be biased toward the shorter of the two
-- dynamic characters.
traceback :: ( DOCharConstraint s
             , Indexable f
             , Key f ~ (Int,Int)
             )
          => f (Cost, Direction, Element s)
          -> s
          -> s
          -> (Word, s, s, s)
traceback alignMatrix longerChar lesserChar =
    ( unsafeToFinite cost
    , constructDynamic medianStates
    , constructDynamic alignedLongerChar
    , constructDynamic alignedLesserChar
    )
  where
      (medianStates, alignedLongerChar, alignedLesserChar) = go lastCell
      lastCell   = (row, col)
      (cost,_,_) = alignMatrix ! lastCell

      col = olength longerChar
      row = olength lesserChar
      gap = gapOfStream longerChar

      go p | traceShow p False = undefined
      go p@(row, col)
        | p == (0,0) = (mempty, mempty, mempty)
        | otherwise  = ( previousMedianCharElements `snoc` medianElement
                       , previousLongerCharElements `snoc` longerElement
                       , previousLesserCharElements `snoc` lesserElement
                       )
        where
          (previousMedianCharElements, previousLongerCharElements, previousLesserCharElements) = go (row', col')
              
          (_, directionArrow, medianElement) = alignMatrix ! p

          (row', col', longerElement, lesserElement) =
              case directionArrow of
                LeftArrow -> (row    , col - 1, longerChar `indexStream` (col - 1),                               gap )
                UpArrow   -> (row - 1, col    ,                               gap , lesserChar `indexStream` (row - 1))
                DiagArrow -> (row - 1, col - 1, longerChar `indexStream` (col - 1), lesserChar `indexStream` (row - 1))


-- |
-- Simple function to get the cost from an alignment matrix
getTotalAlignmentCost :: Matrix (a, b, c) -> a
getTotalAlignmentCost alignmentMatrix = cost
  where
    (cost, _, _) = alignmentMatrix ! (nrows alignmentMatrix - 1, ncols alignmentMatrix - 1) 


-- |
-- Memoized wrapper of the overlap function
getOverlap :: (EncodableStreamElement c {- , Memoizable c, -}) => c -> c -> (Word -> Word -> Word) -> (c, Word)
getOverlap inChar1 inChar2 costStruct = result
    where
        result = {- memoize2 -} overlap costStruct inChar1 inChar2

        
-- |
-- Takes two 'EncodableStreamElement' and a symbol change cost function and
-- returns a tuple of a new character, along with the cost of obtaining that
-- character. The return character may be (or is even likely to be) ambiguous.
-- Will attempt to intersect the two characters, but will union them if that is
-- not possible, based on the symbol change cost function. 
--
-- To clarify, the return character is an intersection of all possible least-cost
-- combinations, so for instance, if @ char1 == A,T @ and @ char2 == G,C @, and
-- the two (non-overlapping) least cost pairs are A,C and T,G, then the return
-- value is A,C,G,T.
overlap :: (EncodableStreamElement c {- , Show c -}) => (Word -> Word -> Word) -> c -> c -> (c, Word)
overlap costStruct char1 char2
    | intersectionStates == zeroBits = minimalChoice $ allPossibleBaseCombosCosts costStruct char1 char2
    | otherwise                      = (intersectionStates, 0)
    where
      intersectionStates = char1 .&. char2


-- |
-- Given a structure of character elements and costs, calculates the least
-- costly intersection of character elements and the cost of that intersection
-- of chaarcter elements.
minimalChoice :: (Bits c, Foldable t, Ord n) => t (c, n) -> (c, n)
minimalChoice = foldr1 f
  where
    f (val1, cost1) (val2, cost2)
      | cost1 == cost2 = (val1 .|. val2, cost1)
      | cost1 < cost2  = (val1         , cost1)
      | otherwise      = (val2         , cost2)


-- |
-- Finds the cost of a pairing of two static characters.
--
-- Takes in a symbol change cost function and two ambiguous elements of a dynamic
-- character and returns a list of tuples of all possible unambiguous pairings,
-- along with the cost of each pairing. The resulting elements each have exactly
-- two bits set. 
allPossibleBaseCombosCosts :: EncodableStreamElement s => (Word -> Word -> Word) -> s -> s -> [(s, Word)]
allPossibleBaseCombosCosts costStruct char1 char2 =
    [ (x .|. y, costStruct i j)
    | (i, x) <- getSubChars char1
    , (j, y) <- getSubChars char2
    ]


-- |
-- Given a symbol change cost function and two tuples of an 'Word' and an
-- unambiguous 'EncodableStreamElement', determines the cost of a pairing
-- (intersection) of those characters into an ambiguous character. The 'Word's
-- are the set bits in each character and are used as lookup into the symbol
-- change cost function. 
getCost :: EncodableStreamElement s => (Word -> Word -> Word) -> (Word, s) -> (Word, s) -> (s, Word)
getCost costStruct seqTup1 seqTup2 = 
    case (seqTup1, seqTup2) of
        ((pos1, c1), (pos2, c2)) -> (c1 .|. c2, costStruct pos1 pos2)


-- |
-- Takes in a 'EncodableStreamElement', possibly with more than one bit set, and
-- returns a list of tuples of 'Word's and 'EncodableStreamElement's, such that,
-- for each set bit in the input, there is one element in the output list, a
-- tuple with an 'Word', @ x @, giving the location of the set bit, as well as an
-- 'EncodableStreamElement' of the same length as the input, but with only the
-- bit at location @ x @ set.
getSubChars :: EncodableStreamElement s => s -> [(Word, s)]
getSubChars fullChar = foldMap f [0 .. symbolCount fullChar - 1]
  where
    f i
      | fullChar `testBit` i = pure (toEnum i,  z `setBit` i)
      | otherwise            = mempty
    z = fullChar `xor` fullChar


-- |
-- Transformation should no longer be nescissary
-- Replaced definition with the identiy function over two values.
correctBiasing :: a -> ([a], [a], [a]) -> ([a], [a], [a])
correctBiasing = const id
{-
correctBiasing   _ ( [], [], []) = ( [],  [],  [])
correctBiasing   _ ([x],[y],[z]) = ([x], [y], [z])
correctBiasing gap (x1:x2:xs, y1:y2:ys, z1:z2:zs)
  | y1 == gap && z1 /= gap && z1 == y2 && z1 == z2 = (x2:xs'', y2:ys'', z2:zs'')
  | y1 /= gap && z1 == gap && y1 == y2 && y1 == z2 = (x2:xs'', y2:ys'', z2:zs'')
  | otherwise                                      = (x1:xs' , y1:ys' , z1:zs' )
  where
    (xs' , ys' , zs' ) = correctBiasing gap ( x2:xs, y2:ys, z2:zs )
    (xs'', ys'', zs'') = correctBiasing gap ( x1:xs, y1:ys, z1:zs )
-}


-- |
-- An overlap function that applies the discrete metric to aligning two elements.
overlapConst :: (EncodableStreamElement c {- , Show c -}) => c -> c -> (c, Word)
overlapConst lhs rhs
  | intersect == zeroBits = (lhs .|. rhs, 1)
  | otherwise             = (intersect  , 0)
  where
    intersect = lhs .&. rhs


-- |
-- Wraps the primative operations in this module to a cohesive operation that is
-- parameterized by an 'OverlapFunction'.
--
-- Reused internally by different implementations.
naiveDOInternal
  :: DOCharConstraint s
  => s
  -> s
  -> OverlapFunction (Element s)
  -> (Word, s, s, s, s)
naiveDOInternal char1 char2 overlapFunction = (alignmentCost, ungapped, gapped', alignedChar1, alignedChar2)
    where
      (swapped, longerChar, shorterChar)   = measureCharacters char1 char2
      traversalMat                         = createNeedlemanWunchMatrix longerChar shorterChar overlapFunction
      (alignmentCost, gapped, left, right) = traceback traversalMat longerChar shorterChar
      (gapped', left', right') = (\(x,y,z) -> (constructDynamic x, constructDynamic y, constructDynamic z)) 
                               $ correctBiasing (getGapElement $ gapped `indexStream` 0) (otoList gapped, otoList left, otoList right)
      ungapped = filterGaps gapped'
      (alignedChar1, alignedChar2)
        | swapped   = (right', left' )
        | otherwise = (left' , right')


-- |
-- Returns the dynamic character that is longer first, shorter second, and notes
-- whether or not the inputs were swapped to place the characters in this ordering.
--
-- Handles equal length characters by /not/ swapping characters.
measureCharacters :: MonoFoldable s => s -> s -> (Bool, s, s)
measureCharacters lhs rhs =
    case comparing olength lhs rhs of
      EQ -> (False, lhs, rhs)
      GT -> (False, lhs, rhs)
      LT -> ( True, rhs, lhs)


getMinimalCostDirection :: (EncodableStreamElement e, Ord c) => (c, e) -> (c, e) -> (c, e) -> (c, e, Direction)
getMinimalCostDirection (diagCost, diagChar) (rightCost, rightChar) (downCost,  downChar) = 
    minimumBy (comparing (\(c,_,d) -> (c,d)))
      [ (diagCost ,  diagChar        , DiagArrow)
      , (rightCost, rightChar .|. gap, LeftArrow)
      , (downCost ,  downChar .|. gap, UpArrow  )
      ]
  where
    gap = getGapElement diagChar


-- |
-- Serializes an alignment matrix to a 'String'. Uses input characters for row
-- and column labelings.
--
-- Useful for debugging purposes.
renderCostMatrix
  :: ( DOCharConstraint s
     , Foldable f
     , Functor f
     , Indexable f
     , Key f ~ (Int,Int)
     , Show a
     , Show b
     )
  => s
  -> s
  -> f (a, b, c) -- ^ The Needleman-Wunsch alignment matrix
  -> String
renderCostMatrix lhs rhs mtx = unlines
    [ dimensionPrefix
    , headerRow
    , barRow
    , renderedRows
    ]
  where
    (_,longer,lesser) = measureCharacters lhs rhs
    longerTokens      = toShownIntegers longer
    lesserTokens      = toShownIntegers lesser
    toShownIntegers   = fmap (show . (fromIntegral :: Integral a => a -> Integer)) . otoList
    matrixTokens      = showCell <$> mtx
    showCell (c,d,_)  = show c <> show d
    maxPrefixWidth    = maxLengthOf lesserTokens
    maxColumnWidth    = max (maxLengthOf longerTokens) . maxLengthOf $ toList matrixTokens
    maxLengthOf       = maximum . fmap length

    colCount = olength longer + 1
    rowCount = olength lesser + 1

    dimensionPrefix  = " " <> unwords
        [ "Dimensions:"
        , show colCount
        , "X"
        , show $ olength lesser + 1
        ]
    
    headerRow = mconcat
        [ " "
        , pad maxPrefixWidth "\\"
        , "| "
        , pad maxColumnWidth "*"
        , concatMap (pad maxColumnWidth) longerTokens
        ]

    barRow    = mconcat
        [ " "
        , bar maxPrefixWidth
        , "+"
        , concatMap (const (bar maxColumnWidth)) $ undefined : longerTokens
        ]
      where
        bar n = replicate (n+1) '-'

    renderedRows = unlines . zipWith renderRow ("*":lesserTokens) $ getRows matrixTokens
      where
        renderRow e vs = " " <> pad maxPrefixWidth e <> "| " <> concatMap (pad maxColumnWidth) vs

        getRows m = (`getRow'` m) <$> [0 .. rowCount - 1]
        getRow' i m = g <$> [0 .. colCount - 1]
          where
            g j = fromMaybe "" $ (i,j) `lookup` m


    pad :: Int -> String -> String
    pad n e = replicate (n - len) ' ' <> e <> " "
      where
        len = length e
