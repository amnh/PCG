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
  , measureAndUngapCharacters
  , needlemanWunschDefinition
--  , renderCostMatrix
--  , traceback
  ) where

import           Bio.Character.Encodable
import           Control.Monad.State.Strict
import           Data.DList                  (snoc)
import           Data.Foldable
import           Data.IntMap                 (IntMap)
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

--import Debug.Trace
trace = const id
traceShowId = id


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
type DOCharConstraint s = (EncodableDynamicCharacter s, EncodableDynamicCharacterElement (Element s), Ord (Element s), Ord (Subcomponent (Element s)), Show s {- , Show (Element s) , Show s, Show (Element s), Integral (Element s) -})


-- |
-- Constraints on the type of structure a "matrix" exposes to be used in rendering
-- and traceback functions.
type MatrixConstraint m = (Foldable m, Functor m, Indexable m, Key m ~ (Int, Int))


-- |
-- A parameterized function to generate an alignment matrix.
type MatrixFunction m s = OverlapFunction (Subcomponent (Element s)) -> s -> s -> m (Cost, Direction)


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
{-# SCC directOptimization #-}
{-# INLINE directOptimization #-}
-- {-# SPECIALISE directOptimization :: MatrixConstraint m => DynamicCharacter -> DynamicCharacter -> OverlapFunction DynamicCharacterElement -> MatrixFunction m DynamicCharacter -> (Word, DynamicCharacter, DynamicCharacter, DynamicCharacter, DynamicCharacter) #-}
directOptimization
  :: ( DOCharConstraint s
     , MatrixConstraint m
     )
  => OverlapFunction (Subcomponent (Element s))
  -> s
  -> s
  -> MatrixFunction m s
  -> (Word, s)
directOptimization overlapλ char1 char2 matrixFunction =
    let (swapped, gapsLesser, gapsLonger, shorterChar, longerChar) = measureAndUngapCharacters char1 char2
        (alignmentCost, ungappedAlignment) =
          if      olength shorterChar == 0
          then if olength  longerChar == 0
               -- Neither character was Missing, but both are empty when gaps are removed
               then (0, toMissing char1)
               -- Neither character was Missing, but one of them is empty when gaps are removed
               else let gap = getMedian $ gapOfStream char1
                        f x = let m = getMedian x in deleteElement (fst $ overlapλ m gap) m
                    in  (0, (\x -> trace ("One char all gaps, non-gapped char: " <> show x) x) $ omap f longerChar)
               -- Both have some non-gap elements, perform string alignment
          else let traversalMatrix = matrixFunction overlapλ longerChar $ trace "Neither Empty" shorterChar
               in  traceback overlapλ traversalMatrix longerChar shorterChar
        transformation    = if swapped then omap swapContext else id
        regappedAlignment = insertGaps gapsLesser gapsLonger shorterChar longerChar ungappedAlignment
        alignmentContext  = transformation regappedAlignment
    in  handleMissingCharacter char1 char2 (alignmentCost, alignmentContext)
    

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
-- Returns the dynamic character that is shorter first, longer second, and notes
-- whether or not the inputs were swapped to place the characters in this ordering.
--
-- Handles equal length characters by considering the lexicographically larger
-- character as longer.
--
-- Handles equality of inputs by /not/ swapping.
{-# INLINE measureCharacters #-}
{-# SPECIALISE measureCharacters :: DynamicCharacter -> DynamicCharacter -> (Bool, DynamicCharacter, DynamicCharacter) #-}
measureCharacters
  :: ( EncodableDynamicCharacterElement (Element s)
     , MonoFoldable s
     , Ord (Element s)
     , Ord (Subcomponent (Element s))
     , Show s
     )
  => s
  -> s
  -> (Bool, s, s)
measureCharacters lhs rhs
  | lhsOrdering == GT = traceShowId ( True, rhs, lhs)
  | otherwise         = traceShowId (False, lhs, rhs)
  where
    lhsOrdering =
        -- First, compare inputs by length.
        case trace "Comparing lengths" $ comparing olength lhs rhs of
          -- If the inputs are equal length,
          -- Then compare by the (arbitary) lexicographical ordering of the median states.
          EQ -> let x = otoList lhs
                    y = otoList rhs
                    f = fmap getMedian
                in  case trace "comparing medians" $ f x `compare` f y of
                      -- If the input median states have the same ordering,
                      -- Lastly, we compare by the lexicographic ordering of the "tagged triples."
                      --
                      -- If they are equal after this step,
                      -- Then the inputs are representationally equal.
                      -- Actually, honest to goodness 100% equal!
                      EQ -> trace "Comparing representations" $ x `compare` y
                      v  -> v
          v  -> v


-- |
-- /O(1)/ for input characters of differing lengths
--
-- /O(k)/ for input characters of equal length, where /k/ is the shared prefix of
-- both characters.
--
-- Considers the median values of the characters, ignores the left/right tagging.
--
-- First remove the gaps from the input characters.
--
-- If both "ungapped" inputs are empty, we measure the original "gapped" inputs to
-- determine if the inputs need to be swapped. This is requried to ensure comutativity
-- of subsequent operations which use this method.
--
-- Returns the "ungapped" dynamic character that is "shorter" first, "longer" second,
-- the removed gap mappings (in the same order), and notes whether or not the inputs
-- were swapped to place the characters in this ordering.
--
-- Handles equal length characters by considering the lexicographically larger
-- character as longer.
--
-- Handles equality of inputs by /not/ swapping.
{-# INLINE measureAndUngapCharacters #-}
{-# SPECIALISE measureAndUngapCharacters :: DynamicCharacter -> DynamicCharacter -> (Bool, IntMap Word, IntMap Word, DynamicCharacter, DynamicCharacter) #-}
measureAndUngapCharacters
  :: ( EncodableDynamicCharacter s
     , EncodableDynamicCharacterElement (Element s)
     , MonoFoldable s
     , Ord (Subcomponent (Element s))
     , Show s
     )
  => s
  -> s
  -> (Bool, IntMap Word, IntMap Word, s, s)
measureAndUngapCharacters char1 char2
  | swapInputs = (True , gapsChar2, gapsChar1, ungappedChar2, ungappedChar1)
  | otherwise  = (False, gapsChar1, gapsChar2, ungappedChar1, ungappedChar2)
  where
    (gapsChar1, ungappedChar1) = deleteGaps char1
    (gapsChar2, ungappedChar2) = deleteGaps char2
    swapInputs =
      let needToSwap (x,_,_) = x
          ungappedLen1 = olength ungappedChar1
          ungappedLen2 = olength ungappedChar2
      in  case ungappedLen1 `compare` ungappedLen2 of
            EQ | ungappedLen1 == 0 -> needToSwap $ trace "Comparing gapped inputs" $ measureCharacters char1 char2
            _                      -> needToSwap $ trace "Comparing ungapped inputs" $ measureCharacters ungappedChar1 ungappedChar2


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
  => OverlapFunction (Subcomponent (Element s))
  -> s
  -> s
  -> f (Cost, Direction)
  -> (Int, Int)
  -> (Cost, Direction)
needlemanWunschDefinition overlapFunction topChar leftChar memo p@(row, col)
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
--             , Foldable f
--             , Functor f
             , Indexable f
             , Key f ~ (Int, Int)
             )
          => OverlapFunction (Subcomponent (Element s))
          -> f (Cost, Direction)
          -> s
          -> s
          -> (Word, s)
--traceback _ alignMatrix longerChar lesserChar | trace (renderCostMatrix longerChar lesserChar alignMatrix) False = undefined
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
