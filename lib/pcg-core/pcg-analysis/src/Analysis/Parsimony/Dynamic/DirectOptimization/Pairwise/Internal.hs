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
-- Defines the primative operations for standard Needleman-Wunsch and Ukkonen
-- algorithms for performing a direct optimization heuristic alignmnet between
-- two dynamic characters.
--
-----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE TypeFamilies       #-}

module Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.Internal
  ( Cost
  , Direction()
  , DOCharConstraint
  , MatrixConstraint
  , MatrixFunction
  , NeedlemanWunchMatrix
  , OverlapFunction
  -- * Direct Optimization primative construction functions
  , directOptimization
  , filterGaps
  , handleMissingCharacter
  , handleMissingCharacterThreeway
  , measureCharacters
  , needlemanWunschDefinition
--  , renderCostMatrix
--  , traceback
  -- * Probably removable
  , overlap
  , overlapConst
  , getOverlap
  , minimalChoice
  ) where

import           Bio.Character.Encodable
import           Control.Foldl              (Fold (..))
import qualified Control.Foldl              as F
import           Control.Monad.State.Strict
import           Data.Bits
import           Data.DList                 (snoc)
import           Data.Foldable
import           Data.Key
import           Data.List.NonEmpty         (NonEmpty (..))
import qualified Data.List.NonEmpty         as NE
import           Data.Matrix.NotStupid      (Matrix)
import           Data.Maybe                 (fromMaybe)
import           Data.MonoTraversable
import           Data.Ord
import           Data.Semigroup.Foldable
import           Numeric.Extended.Natural
import           Prelude                    hiding (lookup)


-- |
-- Which direction to align the character at a given matrix point.
--
-- It should be noted that the ordering of the three arrow types are important,
-- as it guarantees that the derived 'Ord' instance will have the following
-- property:
--
-- DiagArrow < LeftArrow < UpArrow
--
-- This means:
--
--   - DiagArrow has highest precedence when one or more costs are equal
--
--   - LeftArrow has second highest precedence when one or more costs are equal
--
--   -   UpArrow has lowest precedence when one or more costs are equal
--
-- Using this 'Ord' instance, we can resolve ambiguous transformations in a
-- deterministic way. Without loss of generality in determining the ordering,
-- we choose the same biasing as the C code called from the FFI for consistency.
data Direction = DiagArrow | LeftArrow | UpArrow
  deriving stock (Eq, Ord)


-- | (✔)
instance Show Direction where

    show DiagArrow = "↖"
    show LeftArrow = "←"
    show UpArrow   = "↑"


-- |
-- This internal type used for computing the alignment cost. This type has an
-- "infinity" value that is conveniently used for the barrier costs. The cost is
-- strictly non-negative, and possibly infinite.
type Cost = ExtendedNatural


-- |
-- A representation of an alignment matrix for DO.
-- The matrix itself stores tuples of the cost and direction at that position.
-- We also store a vector of characters that are generated.
type NeedlemanWunchMatrix e = Matrix (Cost, Direction, e)


-- |
-- Constraints on the input dynamic characters that direct optimization requires
-- to operate.
type DOCharConstraint s = (EncodableDynamicCharacter s, Ord (Element s) {- , Show s, Show (Element s), Integral (Element s) -})


-- |
-- Constraints on the type of structure a "matrix" exposes to be used in rendering
-- and traceback functions.
type MatrixConstraint m = (Indexable m, Key m ~ (Int, Int))


-- |
-- A parameterized function to generate an alignment matrix.
type MatrixFunction m s = s -> s -> OverlapFunction (Element s) -> m (Cost, Direction, Element s)


-- |
-- A generalized function representation: the "overlap" between dynamic character
-- elements, supplying the corresponding median and cost to align the two
-- characters.
type OverlapFunction e = e -> e -> (e, Word)


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
    (swapped, longerChar, shorterChar) = measureCharacters char1 char2
    traversalMatrix = matrixFunction longerChar shorterChar overlapFunction
    (alignmentCost, ungapped, gapped, left, right) = traceback traversalMatrix longerChar shorterChar
    alignment = (alignmentCost, ungapped, gapped, alignedChar1, alignedChar2)
--    ungapped  = filterGaps gapped
    (alignedChar1, alignedChar2)
      | swapped   = (right, left )
      | otherwise = (left , right)


-- |
-- Strips the gap elements from the supplied character.
--
-- If the character contains /only/ gaps, a missing character is returned.
filterGaps :: EncodableDynamicCharacter s => s -> s
filterGaps char =
    case filter (/= gap) $ otoList char of
      []   -> toMissing char
      x:xs -> constructDynamic $ x:|xs
  where
    gap = gapOfStream char


-- |
-- A generalized function to handle missing dynamic characters.
--
-- Intended to be reused by multiple, differing implementations.
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
-- As `handleMissingCharacter`, but with three inputs.
--
-- For use in FFI 3D calls to C.
handleMissingCharacterThreeway
  :: PossiblyMissingCharacter s
  => (s -> s -> (Word, s, s, s, s)) -- fn takes two inputs, gives back cost, ungapped, gapped, two alignments
  -> s
  -> s
  -> s
  -> (Word, s, s, s, s, s)
  -> (Word, s, s, s, s, s)
handleMissingCharacterThreeway f a b c v =
    -- Appropriately handle missing data:
    case (isMissing a, isMissing b, isMissing c) of
      (True , True , True ) -> (0, a, a, a, b, c) --WLOG. return cost = 0
      (True , True , False) -> (0, c, c, c, c, c)
      (True , False, True ) -> (0, b, b, b, b, b)
      (True , False, False) -> let (cost, ungapd, gapd, lhs, rhs) = f b c
                               in  (cost, ungapd, gapd, undefined, lhs, rhs)
      (False, True , True ) -> (0, a, a, a, a, a)
      (False, True , False) -> let (cost, ungapd, gapd, lhs, rhs) = f a c
                               in  (cost, ungapd, gapd, lhs, undefined, rhs)
      (False, False, True ) -> let (cost, ungapd, gapd, lhs, rhs) = f a b
                               in  (cost, ungapd, gapd, lhs, rhs, undefined)
      (False, False, False) -> v


-- |
-- /O(1)/ for input characters of differing lengths
--
-- /O(k)/ for input characters of equal length, where /k/ is the shared prefix of
-- both characters.
--
-- Returns the dynamic character that is longer first, shorter second, and notes
-- whether or not the inputs were swapped to place the characters in this ordering.
--
-- Handles equal length characters by considering the lexicographically larger
-- character as longer.
--
-- Handles equality of inputs by /not/ swapping.
measureCharacters :: (MonoFoldable s, Ord (Element s)) => s -> s -> (Bool, s, s)
measureCharacters lhs rhs
  | lhsOrdering == LT = ( True, rhs, lhs)
  | otherwise         = (False, lhs, rhs)
  where
    lhsOrdering =
        case comparing olength lhs rhs of
          EQ -> otoList lhs `compare` otoList rhs
          x  -> x


-- |
-- Internal generator function for the matrices based on the Needleman-Wunsch
-- definition described in their paper.
needlemanWunschDefinition
  :: ( DOCharConstraint s
     , Indexable f
     , Key f ~ (Int, Int)
     )
  => s
  -> s
  -> OverlapFunction (Element s)
  -> f (Cost, Direction, Element s)
  -> (Int, Int)
  -> (Cost, Direction, Element s)
needlemanWunschDefinition topChar leftChar overlapFunction memo p@(row, col)
  | p == (0,0) = (      0, DiagArrow,      gap)
  | otherwise  = (minCost,    minDir, minState)
  where
    -- | Lookup with a default value of infinite cost.
    {-# INLINE (!?) #-}
    (!?) m k = fromMaybe (infinity, DiagArrow, gap) $ k `lookup` m

    gap                           = gapOfStream topChar
    topElement                    = fromMaybe gap $  topChar `lookupStream` (col - 1)
    leftElement                   = fromMaybe gap $ leftChar `lookupStream` (row - 1)
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
-- |
-- Serializes an alignment matrix to a 'String'. Uses input characters for row
-- and column labelings.
--
-- Useful for debugging purposes.
renderCostMatrix
  :: ( DOCharConstraint s
     , Enum (Element s)
     , Foldable f
     , Functor f
     , Indexable f
     , Key f ~ (Int, Int)
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
    toShownIntegers   = fmap (show . fromEnum) . otoList
    matrixTokens      = showCell <$> mtx
    showCell (c,d,_)  = show c <> show d
    maxPrefixWidth    = maxLengthOf lesserTokens
    maxColumnWidth    = max (maxLengthOf longerTokens) . maxLengthOf $ toList matrixTokens
    maxLengthOf       = maximum . fmap length

    colCount = olength longer + 1
    rowCount = olength lesser + 1

    dimensionPrefix  = " " <> unwords
        [ "Dimensions:"
        , show rowCount
        , "X"
        , show colCount
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
-}


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
             , Key f ~ (Int, Int)
             )
          => f (Cost, Direction, Element s)
          -> s
          -> s
          -> (Word, s, s, s, s)
traceback alignMatrix longerChar lesserChar =
    ( unsafeToFinite cost
    , constructDynamic . NE.fromList $ toList ungappedMedianStates
    , constructDynamic . NE.fromList $ toList medianStates
    , constructDynamic . NE.fromList $ toList alignedLongerChar
    , constructDynamic . NE.fromList $ toList alignedLesserChar
    )
  where
      (ungappedMedianStates, medianStates, alignedLongerChar, alignedLesserChar) = go lastCell
      lastCell     = (row, col)
      (cost, _, _) = alignMatrix ! lastCell

      col = olength longerChar
      row = olength lesserChar
      gap = gapOfStream longerChar

      go p@(i, j)
        | p == (0,0) = (mempty, mempty, mempty, mempty)
        | otherwise  = ( if   medianElement == gap
                         then previousUngapped
                         else previousUngapped      `snoc` medianElement
                       , previousMedianCharElements `snoc` medianElement
                       , previousLongerCharElements `snoc` longerElement
                       , previousLesserCharElements `snoc` lesserElement
                       )
        where
          (previousUngapped, previousMedianCharElements, previousLongerCharElements, previousLesserCharElements) = go (row', col')

          (_, directionArrow, medianElement) = alignMatrix ! p

          (row', col', longerElement, lesserElement) =
              case directionArrow of
                LeftArrow -> (i    , j - 1, longerChar `indexStream` (j - 1),                             gap )
                UpArrow   -> (i - 1, j    ,                             gap , lesserChar `indexStream` (i - 1))
                DiagArrow -> (i - 1, j - 1, longerChar `indexStream` (j - 1), lesserChar `indexStream` (i - 1))


{--
 - Internal computations
 -}


-- |
-- Memoized wrapper of the overlap function
getOverlap :: EncodableStreamElement e => e -> e -> (Word -> Word -> Word) -> (e, Word)
getOverlap inChar1 inChar2 costStruct = result
  where
    result = overlap costStruct inChar1 inChar2


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
overlap :: (EncodableStreamElement e {- , Show e -}) => (Word -> Word -> Word) -> e -> e -> (e, Word)
overlap costStruct char1 char2 = F.impurely ofoldMUnwrap (F.premapM g outerFold) char1 `evalState` 0
  where
    !zero     = char1 `xor` char1

    outerFold = F.generalize $ Fold f (char1 `xor` char1, maxBound) id

    costAndSymbol (i, x) = (x, cost1 + cost2)
      where
        !cost1 = getDistance3 costStruct i char1
        !cost2 = getDistance3 costStruct i char2

    f (!symbol1, !cost1) (!symbol2, !cost2) =
        case cost1 `compare` cost2 of
          EQ -> (symbol1 .|. symbol2, cost1)
          LT -> (symbol1            , cost1)
          GT -> (symbol2            , cost2)

    g = const $ do
        j <- get
        modify' (+1)
        let !v = toEnum j
        let !b = zero `setBit` j
        pure $ costAndSymbol (v, b)


getDistance3 :: (MonoFoldable b, Element b ~ Bool) => (Word -> Word -> Word) -> Word -> b -> Word
getDistance3 costStruct i b = fromMaybe errMsg $
    F.impurely ofoldMUnwrap (F.premapM f (F.generalize F.minimum)) b `evalState` 0
  where
    errMsg = error "There were no bits set in the character!"

    f e = do
        j <- get
        modify' (+1)
        pure $ if e
               then costStruct i j
               else maxBound
{-
    getDistance2 :: FiniteBits b => Word -> b -> Word
    getDistance2 i b =
        case F.fold (F.prefilter (b `testBit`) (F.premap (costStruct i . toEnum) F.minimum)) indices of
          Just x  -> x
          Nothing -> error $ "There were no bits set in the character!"
      where
        indices = [0 .. finiteBitSize b - 1]
-}

-- |
-- Given a structure of unambiguous character elements and costs, calculates the
-- least costly intersection of unambiguous character elements and the cost of
-- that intersection.
minimalChoice :: (Bits b, Foldable1 t, Ord c) => t (b, c) -> (b, c)
minimalChoice = foldl1 f
  where
    f (!symbol1, !cost1) (!symbol2, !cost2) =
        case cost1 `compare` cost2 of
          EQ -> (symbol1 .|. symbol2, cost1)
          LT -> (symbol1            , cost1)
          GT -> (symbol2            , cost2)


{-
-- |
-- Finds the cost between all single, unambiguous symbols and two dynamic
-- character elements (ambiguity groups of symbols).
--
-- Takes in a symbol change cost function and two ambiguous elements of a dynamic
-- character and returns a list of tuples of all possible unambiguous pairings,
-- along with the cost of each pairing. The resulting elements each have exactly
-- two bits set.
symbolDistances
  :: EncodableStreamElement e
  => (Word -> Word -> Word)
  -> e
  -> e
  -> NonEmpty (e, Word)
symbolDistances costStruct char1 char2 = costAndSymbol <$> allSymbols
  where
    costAndSymbol (i, x) = (x, cost1 + cost2)
      where
        !cost1 = getDistance3 costStruct i char1
        !cost2 = getDistance3 costStruct i char2

    symbolIndices = NE.fromList [0 .. finiteBitSize char1 - 1]
    allSymbols    = (toEnum &&& setBit zero) <$> symbolIndices
    zero          = char1 `xor` char1
-}

{-
    getDistance3 :: (MonoFoldable b, Element b ~ Bool) => Word -> b -> Word
    getDistance3 i b =
        case F.impurely ofoldMUnwrap (F.prefilterM pure (F.premapM f (F.generalize F.minimum))) b `evalState` 0 of
          Just x  -> x
          Nothing -> error $ "There were no bits set in the character!"
      where
        f _ = do
            j <- get
            modify' (+1)
            pure $ costStruct i j
-}

{-
    getDistance2 :: FiniteBits b => Word -> b -> Word
    getDistance2 i b =
        case F.fold (F.prefilter (b `testBit`) (F.premap (costStruct i . toEnum) F.minimum)) indices of
          Just x  -> x
          Nothing -> error $ "There were no bits set in the character!"
      where
        indices = [0 .. finiteBitSize b - 1]
-}
{-
    getDistance :: FiniteBits b => Word -> b -> Word
    getDistance i e = F.minimum $ costStruct i <$> getSetBits e

    getSetBits :: FiniteBits b => b -> NonEmpty Word
    getSetBits b =
        case fold (F.prefilter (b `testBit`) (costStruct i <$> F.minimum)) indices of
          x:xs -> x:|xs
          []   -> error $ "There were no bits set in the character: " <>
                    show (foldMap (\i -> if b `testBit` i then "1" else "0") indices)
      where
        indices = [0 .. finiteBitSize b - 1]
-}
{-
        go  0 xs = if b `testBit` 0 then 0:xs else xs
        go !n xs
          | b `testBit` n = go (n-1) $ toEnum n:xs
          | otherwise     = go (n-1) xs
-}


-- |
-- An overlap function that applies the discrete metric to aligning two elements.
overlapConst :: (EncodableStreamElement e {- , Show e -}) => e -> e -> (e, Word)
overlapConst lhs rhs
  | intersect == zeroBits = (lhs .|. rhs, 1)
  | otherwise             = (intersect  , 0)
  where
    intersect = lhs .&. rhs


getMinimalCostDirection :: (EncodableStreamElement e, Ord c) => (c, e) -> (c, e) -> (c, e) -> (c, e, Direction)
getMinimalCostDirection (diagCost, diagChar) (rightCost, rightChar) (downCost,  downChar) =
    minimumBy (comparing (\(c,_,d) -> (c,d)))
      [ (diagCost ,  diagChar        , DiagArrow)
      , (rightCost, rightChar .|. gap, LeftArrow)
      , (downCost ,  downChar .|. gap, UpArrow  )
      ]
  where
    gap = getGapElement diagChar


{-
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
-}


{-
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
-}
