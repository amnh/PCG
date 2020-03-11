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
-- Defines the primitive operations for standard Needleman-Wunsch and Ukkonen
-- algorithms for performing a direct optimization heuristic alignmnet between
-- two dynamic characters.
--
-----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.Internal
  ( Cost
  , Direction(..)
  , DOCharConstraint
  , MatrixConstraint
  , MatrixFunction
  , NeedlemanWunchMatrix
  , OverlapFunction
  -- * Direct Optimization primitive construction functions
  , directOptimization
  , filterGaps
  , handleMissingCharacter
  , handleMissingCharacterThreeway
  , measureCharacters
  , needlemanWunschDefinition
--  , renderCostMatrix
--  , traceback
  ) where

import           Bio.Character.Encodable
import           Control.Monad.State.Strict
import           Data.DList                  (snoc)
import           Data.Foldable
import           Data.Key
import           Data.List.NonEmpty          (NonEmpty (..))
import qualified Data.List.NonEmpty          as NE
import           Data.Matrix.NotStupid       (Matrix)
import           Data.Maybe                  (fromMaybe)
import           Data.MonoTraversable
import           Data.Ord
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Primitive       as P
import qualified Data.Vector.Unboxed         as U
import           Data.Word                   (Word8)
import           Numeric.Extended.Natural
import           Prelude                     hiding (lookup, zipWith)


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


-- |
-- This internal type used for computing the alignment cost. This type has an
-- "infinity" value that is conveniently used for the barrier costs. The cost is
-- strictly non-negative, and possibly infinite.
type Cost = ExtendedNatural


-- |
-- A representation of an alignment matrix for DO.
-- The matrix itself stores tuples of the cost and direction at that position.
-- We also store a vector of characters that are generated.
type NeedlemanWunchMatrix = Matrix (Cost, Direction)


-- |
-- Constraints on the input dynamic characters that direct optimization requires
-- to operate.
type DOCharConstraint s = (EncodableDynamicCharacter s, Ord (Element s), EncodableDynamicCharacterElement (Element s), Show (Element s) {- , Show s, Show (Element s), Integral (Element s) -})


-- |
-- Constraints on the type of structure a "matrix" exposes to be used in rendering
-- and traceback functions.
type MatrixConstraint m = (Foldable m, Functor m, Indexable m, Key m ~ (Int, Int))


-- |
-- A parameterized function to generate an alignment matrix.
type MatrixFunction m s = s -> s -> OverlapFunction (Subcomponent (Element s)) -> m (Cost, Direction)


-- |
-- A generalized function representation: the "overlap" between dynamic character
-- elements, supplying the corresponding median and cost to align the two
-- characters.
type OverlapFunction e = e -> e -> (e, Word)


data instance U.MVector s Direction = MV_Direction (P.MVector s Word8)


data instance U.Vector   Direction  = V_Direction  (P.Vector    Word8)


instance U.Unbox Direction


instance M.MVector U.MVector Direction where

    {-# INLINE basicLength #-}
    basicLength (MV_Direction v) = M.basicLength v

    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeSlice i n (MV_Direction v) = MV_Direction $ M.basicUnsafeSlice i n v

    {-# INLINE basicOverlaps #-}
    basicOverlaps (MV_Direction v1) (MV_Direction v2) = M.basicOverlaps v1 v2

    {-# INLINE basicUnsafeNew #-}
    basicUnsafeNew n = MV_Direction `liftM` M.basicUnsafeNew n

    {-# INLINE basicInitialize #-}
    basicInitialize (MV_Direction v) = M.basicInitialize v

    {-# INLINE basicUnsafeReplicate #-}
    basicUnsafeReplicate n x = MV_Direction `liftM` M.basicUnsafeReplicate n (fromDirection x)

    {-# INLINE basicUnsafeRead #-}
    basicUnsafeRead (MV_Direction v) i = toDirection `liftM` M.basicUnsafeRead v i

    {-# INLINE basicUnsafeWrite #-}
    basicUnsafeWrite (MV_Direction v) i x = M.basicUnsafeWrite v i (fromDirection x)

    {-# INLINE basicClear #-}
    basicClear (MV_Direction v) = M.basicClear v

    {-# INLINE basicSet #-}
    basicSet (MV_Direction v) x = M.basicSet v (fromDirection x)

    {-# INLINE basicUnsafeCopy #-}
    basicUnsafeCopy (MV_Direction v1) (MV_Direction v2) = M.basicUnsafeCopy v1 v2

    basicUnsafeMove (MV_Direction v1) (MV_Direction v2) = M.basicUnsafeMove v1 v2

    {-# INLINE basicUnsafeGrow #-}
    basicUnsafeGrow (MV_Direction v) n = MV_Direction `liftM` M.basicUnsafeGrow v n


instance G.Vector U.Vector Direction where

    {-# INLINE basicUnsafeFreeze #-}
    basicUnsafeFreeze (MV_Direction v) = V_Direction `liftM` G.basicUnsafeFreeze v

    {-# INLINE basicUnsafeThaw #-}
    basicUnsafeThaw (V_Direction v) = MV_Direction `liftM` G.basicUnsafeThaw v

    {-# INLINE basicLength #-}
    basicLength (V_Direction v) = G.basicLength v

    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeSlice i n (V_Direction v) = V_Direction $ G.basicUnsafeSlice i n v

    {-# INLINE basicUnsafeIndexM #-}
    basicUnsafeIndexM (V_Direction v) i = toDirection `liftM` G.basicUnsafeIndexM v i

    basicUnsafeCopy (MV_Direction mv) (V_Direction v) = G.basicUnsafeCopy mv v

    {-# INLINE elemseq #-}
    elemseq _ = seq


instance Show Direction where

    show DiagArrow = "↖"
    show LeftArrow = "←"
    show UpArrow   = "↑"


-- |
-- Wraps the primitive operations in this module to a cohesive operation that is
-- parameterized by an 'OverlapFunction'.
--
-- Reused internally by different implementations.
{-# INLINE directOptimization #-}
-- {-# SPECIALISE directOptimization :: MatrixConstraint m => DynamicCharacter -> DynamicCharacter -> OverlapFunction DynamicCharacterElement -> MatrixFunction m DynamicCharacter -> (Word, DynamicCharacter, DynamicCharacter, DynamicCharacter, DynamicCharacter) #-}
directOptimization
  :: ( DOCharConstraint s
     , MatrixConstraint m
     )
  => s
  -> s
  -> OverlapFunction (Subcomponent (Element s))
  -> MatrixFunction m s
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


-- |
-- Strips the gap elements from the supplied character.
--
-- If the character contains /only/ gaps, a missing character is returned.
{-# INLINE filterGaps #-}
{-# SPECIALISE filterGaps ::  DynamicCharacter -> DynamicCharacter #-}
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
{-# INLINE handleMissingCharacter #-}
{-# SPECIALISE handleMissingCharacter :: DynamicCharacter -> DynamicCharacter -> (Word, DynamicCharacter) -> (Word, DynamicCharacter) #-}
handleMissingCharacter
  :: PossiblyMissingCharacter s
  => s
  -> s
  -> (Word, s)
  -> (Word, s)
handleMissingCharacter lhs rhs v =
    -- Appropriately handle missing data:
    case (isMissing lhs, isMissing rhs) of
      (True , True ) -> (0, lhs)
      (True , False) -> (0, rhs)
      (False, True ) -> (0, lhs)
      (False, False) -> v


-- |
-- As `handleMissingCharacter`, but with three inputs.
--
-- For use in FFI 3D calls to C.
{-# INLINE handleMissingCharacterThreeway #-}
{-# SPECIALISE handleMissingCharacterThreeway :: (DynamicCharacter -> DynamicCharacter -> (Word, DynamicCharacter, DynamicCharacter, DynamicCharacter, DynamicCharacter)) -> DynamicCharacter -> DynamicCharacter -> DynamicCharacter -> (Word, DynamicCharacter, DynamicCharacter, DynamicCharacter, DynamicCharacter, DynamicCharacter) -> (Word, DynamicCharacter, DynamicCharacter, DynamicCharacter, DynamicCharacter, DynamicCharacter) #-}
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
{-# INLINE measureCharacters #-}
{-# SPECIALISE measureCharacters :: DynamicCharacter -> DynamicCharacter -> (Bool, DynamicCharacter, DynamicCharacter) #-}
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
{-# INLINE needlemanWunschDefinition #-}
-- {-# SPECIALISE needlemanWunschDefinition :: (Indexable f, Key f ~ (Int, Int)) => DynamicCharacter -> DynamicCharacter -> OverlapFunction DynamicCharacterElement -> f (Cost, Direction, DynamicCharacterElement) -> (Int, Int) -> (Cost, Direction, DynamicCharacterElement) #-}
needlemanWunschDefinition
  :: ( DOCharConstraint s
     , Indexable f
     , Key f ~ (Int, Int)
     )
  => s
  -> s
  -> OverlapFunction (Subcomponent (Element s))
  -> f (Cost, Direction)
  -> (Int, Int)
  -> (Cost, Direction)
needlemanWunschDefinition topChar leftChar overlapFunction memo p@(row, col)
  | p == (0,0) = (      0, DiagArrow)
  | otherwise  = (minCost,    minDir)
  where
    -- | Lookup with a default value of infinite cost.
    {-# INLINE (!?) #-}
    (!?) m k = fromMaybe (infinity, DiagArrow) $ k `lookup` m

    gap                   = gapOfStream topChar
    gapGroup              = getMedian gap
    topElement            = getMedian . fromMaybe gap $  topChar `lookupStream` (col - 1)
    leftElement           = getMedian . fromMaybe gap $ leftChar `lookupStream` (row - 1)
    (leftwardValue, _)    = memo !? (row    , col - 1)
    (diagonalValue, _)    = memo !? (row - 1, col - 1)
    (  upwardValue, _)    = memo !? (row - 1, col    )
    (_, rightOverlapCost) = fromFinite <$> overlapFunction topElement  gapGroup
    (_,  diagOverlapCost) = fromFinite <$> overlapFunction topElement  leftElement
    (_,  downOverlapCost) = fromFinite <$> overlapFunction gapGroup    leftElement
    rightCost             = rightOverlapCost + leftwardValue
    diagCost              =  diagOverlapCost + diagonalValue
    downCost              =  downOverlapCost +   upwardValue
    (minCost, minDir)     = minimum [ (diagCost , DiagArrow)
                                    , (rightCost, LeftArrow)
                                    , (downCost , UpArrow  )
                                    ]


{--
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
     , Key f ~ (Int, Int)
     , Show a
     , Show b
     )
  => s
  -> s
  -> f (a, b) -- ^ The Needleman-Wunsch alignment matrix
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
--    toShownIntegers   = fmap (show . showBitsValue) . otoList
    toShownIntegers   = fmap (const "#") . otoList
    matrixTokens      = showCell <$> mtx
    showCell (c,d)    = show c <> show d
    maxPrefixWidth    = maxLengthOf lesserTokens
    maxColumnWidth    = max (maxLengthOf longerTokens) . maxLengthOf $ toList matrixTokens
    maxLengthOf       = maximum . fmap length

{-
    showBitsValue :: FiniteBits b => b -> Word
    showBitsValue b = go (finiteBitSize b) 0
      where
        go 0 v = v
        go i v = let i' = i-1
                     v' | b `testBit` i' = v + bit i'
                        | otherwise      = v
                 in  go i' v'
-}

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
--}


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
{-# INLINE traceback #-}
-- {-# SPECIALISE traceback :: (Indexable f, Key f ~ (Int, Int)) => f (Cost, Direction, DynamicCharacterElement) -> DynamicCharacter -> DynamicCharacter -> (Word, DynamicCharacter, DynamicCharacter, DynamicCharacter, DynamicCharacter) #-}
traceback :: ( DOCharConstraint s
             , Indexable f
             , Key f ~ (Int, Int)
             )
          => OverlapFunction (Subcomponent (Element s))
          -> f (Cost, Direction)
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
    gap = getMedian $ gapOfStream longerChar

    go p@(i, j)
      | p == (0,0) = mempty
      | otherwise  = previousSequence `snoc` localContext
      where
        previousSequence = go (row', col')

        (_, directionArrow) = alignMatrix ! p

        (row', col', localContext) =
            case directionArrow of
              LeftArrow -> let j' = j-1
                               te = getMedian $ longerChar `indexStream` j'
                               e  = deleteElement (f gap te) te
                           in (i , j', e)
              UpArrow   -> let i' = i-1
                               le = getMedian $ lesserChar `indexStream` i'
                               e  = insertElement (f le gap) le
                           in (i', j , e)
              DiagArrow -> let i' = i-1
                               j' = j-1
                               te = getMedian $ longerChar `indexStream` j'
                               le = getMedian $ lesserChar `indexStream` i'
                               e  = alignElement (f le te) le te
                           in (i', j', e)


{-
{-# INLINE getMinimalCostDirection #-}
{-# SPECIALISE getMinimalCostDirection :: (Cost, DynamicCharacterElement) -> (Cost, DynamicCharacterElement) -> (Cost, DynamicCharacterElement) -> (Cost, DynamicCharacterElement, Direction) #-}
getMinimalCostDirection :: (EncodableStreamElement e, Ord c) => (c, e) -> (c, e) -> (c, e) -> (c, e, Direction)
getMinimalCostDirection (diagCost, diagChar) (rightCost, rightChar) (downCost,  downChar) =
    minimumBy (comparing (\(c,d) -> (c,d)))
      [ (diagCost , DiagArrow)
      , (rightCost, LeftArrow)
      , (downCost , UpArrow  )
      ]
  where
    gap = getGapElement diagChar
-}


{-# INLINE fromDirection #-}
fromDirection :: Direction -> Word8
fromDirection DiagArrow = 0
fromDirection LeftArrow = 1
fromDirection UpArrow   = 2


{-# INLINE toDirection #-}
toDirection :: Word8 -> Direction
toDirection 0 = DiagArrow
toDirection 1 = LeftArrow
toDirection _ = UpArrow
