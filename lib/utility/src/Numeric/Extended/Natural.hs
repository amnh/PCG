-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Extended.Natural
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}

module Numeric.Extended.Natural
  ( ExtendedNatural()
  , ExtendedNumber(..)
  , Finite
  ) where

import           Control.DeepSeq
import           Data.Binary
import           Data.Bits
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Primitive       as P
import qualified Data.Vector.Unboxed         as U
import           GHC.Exts
import           GHC.Generics
import           GHC.Integer.Logarithms
import           Numeric.Extended.Internal
import           Test.QuickCheck


-- |
-- A type that extends the Natural numbers to include an infinity value.
--
-- This is a newtyped 'Word' for efficiency purposes.
--
-- The following behavior should be noted:
--
--   - 'ExtendedNatural' has a domain of @[0 .. (maxBound :: Word) - 1] <> infinity@
--
--   - @unsafeToFinite (maxBound :: ExtendedNatural) == (maxBound :: Word) - 1@
--
--   - @unsafeToFinite (minBound :: ExtendedNatural) == (minBound :: Word)@
--
--   - @unsafeToFinite (infinity :: ExtendedNatural) == (maxBound :: Word)@
--
--   - 'ExtendedNatural' does not throw an error on arithmetic underflows
--     caused by subtracting a larger value from a smaller value. Instead,
--     the result is floored at zero.
--
--   - 'ExtendedNatural' does not throw an error on arithmetic overflows caused
--     by adding or multiplying two numbers that would produce a result larger
--     than @(maxBound :: ExtendedNatural)@. Instead, the result is capped at
--     @(maxBound :: ExtendedNatural)@.
--
--   - An @infinity@ value can only result from it's use as a constant, addition
--     with @infinity@ as an operand, multiplication with @infinity@ as an
--     operand subtraction with @infinity@ as the minuend, or division with
--     @infinity@ as the denominator.
--
newtype ExtendedNatural = Cost Word
    deriving anyclass (Binary, NFData)
    deriving stock    (Eq, Generic, Ord)


type instance Finite ExtendedNatural = Word


instance Arbitrary ExtendedNatural where

    arbitrary = frequency [ (1, pure infinity), (19, Cost <$> finiteValue)]
      where
        finiteValue = arbitrary `suchThat` (/= maxBound)


instance Bounded ExtendedNatural where

    {-# INLINE maxBound #-}
    maxBound = Cost $ maxBound - 1

    {-# INLINE minBound #-}
    minBound = Cost minBound


instance Enum ExtendedNatural where

    fromEnum = fromEnum . toWord

    toEnum   = Cost . toEnum

    {-# INLINE succ #-}
    succ val@(Cost x)
      | val == infinity = infinity
      | val == maxBound = maxBound
      | otherwise       = Cost $ x + 1

    {-# INLINE pred #-}
    pred  val@(Cost x)
      | val == infinity = infinity
      | val == minBound = minBound
      | otherwise       = Cost $ x - 1



instance ExtendedNumber ExtendedNatural where

    unsafeToFinite = toWord

    fromFinite = fromWord

    infinity = Cost maxBound


instance Num ExtendedNatural where

    lhs@(Cost x) + rhs@(Cost y)
      | lhs    == infinity  = infinity
      | rhs    == infinity  = infinity
      | result >= infinity  = maxBound
      | result <  maxima    = maxBound
      | otherwise           = Cost result
      where
        maxima = max x y
        result = x + y

    lhs@(Cost x) - rhs@(Cost y)
      | lhs == infinity = infinity
      | lhs <= rhs      = minBound
      | otherwise       = Cost $ x - y

    lhs@(Cost x) * rhs@(Cost y)
      -- If either value is infinite,
      -- then the product is infinite
      | lhs    == infinity  = infinity
      | rhs    == infinity  = infinity

      -- If both values are finite,
      -- then we consider the minimum number
      -- of possible bits in the product,
      -- and compare that to the number
      -- of bits in this machine's Word width.
      | otherwise =
          let minProductBits = bitsUsed x + bitsUsed y - 1
          in  case minProductBits `compare` wordWidth of

                -- If the minimum possible number of bits to
                -- represent the product is less than the Word width,
                -- then we compute the product directly.
                LT -> Cost $ x * y

                -- If the minimum possible number of bits to
                -- represent the product is exceeds the Word width,
                -- then an overflow definitely occurred and
                -- the product is the upper finite bound.
                GT -> maxBound

                -- If the minimum possible number of bits to
                -- represent the product equals the Word width,
                -- then great care must be taken!
                -- First we compute the product directly.
                -- Then we perform an expensive division operation
                -- to determine if overflow occurred.
                EQ -> let result = x * y
                      in  if   result `quotRem` x /= (y,0)
                          then maxBound
                          else Cost result

    {-# INLINE abs #-}
    abs = id

    {-# INLINE signum #-}
    signum (Cost 0) = 0
    signum        _ = 1

    fromInteger = Cost . fromInteger

    {-# INLINE negate #-}
    negate = const 0


instance Integral ExtendedNatural where

    toInteger = toInteger . toWord

    quotRem lhs@(Cost x) rhs@(Cost y)
      | lhs == infinity = (infinity, minBound)
      | rhs == infinity = (minBound, minBound)
      | rhs == minBound = (infinity, minBound)
      | otherwise       = let (q,r) = x `quotRem` y
                          in  (Cost q, Cost r)

    divMod = quotRem


instance Real ExtendedNatural where

    toRational = toRational . toInteger


instance Show ExtendedNatural where

    show (Cost input)
      | input == maxBound = "∞"
      | otherwise         = show input


newtype instance U.MVector s ExtendedNatural = MV_ExtendedNatural (P.MVector s Word)


newtype instance U.Vector ExtendedNatural    = V_ExtendedNatural  (P.Vector    Word)


instance U.Unbox ExtendedNatural


instance M.MVector U.MVector ExtendedNatural where

    {-# INLINE basicLength #-}
    basicLength (MV_ExtendedNatural v) = M.basicLength v

    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeSlice i n (MV_ExtendedNatural v) = MV_ExtendedNatural $ M.basicUnsafeSlice i n v

    {-# INLINE basicOverlaps #-}
    basicOverlaps (MV_ExtendedNatural v1) (MV_ExtendedNatural v2) = M.basicOverlaps v1 v2

    {-# INLINE basicUnsafeNew #-}
    basicUnsafeNew n = MV_ExtendedNatural <$> M.basicUnsafeNew n

    {-# INLINE basicInitialize #-}
    basicInitialize (MV_ExtendedNatural v) = M.basicInitialize v

    {-# INLINE basicUnsafeReplicate #-}
    basicUnsafeReplicate n x = MV_ExtendedNatural <$> M.basicUnsafeReplicate n (toWord x)

    {-# INLINE basicUnsafeRead #-}
    basicUnsafeRead (MV_ExtendedNatural v) i = fromWord <$> M.basicUnsafeRead v i

    {-# INLINE basicUnsafeWrite #-}
    basicUnsafeWrite (MV_ExtendedNatural v) i x = M.basicUnsafeWrite v i (toWord x)

    {-# INLINE basicClear #-}
    basicClear (MV_ExtendedNatural v) = M.basicClear v

    {-# INLINE basicSet #-}
    basicSet (MV_ExtendedNatural v) x = M.basicSet v (toWord x)

    {-# INLINE basicUnsafeCopy #-}
    basicUnsafeCopy (MV_ExtendedNatural v1) (MV_ExtendedNatural v2) = M.basicUnsafeCopy v1 v2

    {-# INLINE basicUnsafeGrow #-}
    basicUnsafeMove (MV_ExtendedNatural v1) (MV_ExtendedNatural v2) = M.basicUnsafeMove v1 v2

    basicUnsafeGrow (MV_ExtendedNatural v) n = MV_ExtendedNatural <$> M.basicUnsafeGrow v n


instance G.Vector U.Vector ExtendedNatural where

    {-# INLINE basicUnsafeFreeze #-}
    basicUnsafeFreeze (MV_ExtendedNatural v) = V_ExtendedNatural <$> G.basicUnsafeFreeze v

    {-# INLINE basicUnsafeThaw #-}
    basicUnsafeThaw (V_ExtendedNatural v) = MV_ExtendedNatural <$> G.basicUnsafeThaw v

    {-# INLINE basicLength #-}
    basicLength (V_ExtendedNatural v) = G.basicLength v

    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeSlice i n (V_ExtendedNatural v) = V_ExtendedNatural $ G.basicUnsafeSlice i n v

    {-# INLINE basicUnsafeIndexM #-}
    basicUnsafeIndexM (V_ExtendedNatural v) i = fromWord <$> G.basicUnsafeIndexM v i

    basicUnsafeCopy (MV_ExtendedNatural mv) (V_ExtendedNatural v) = G.basicUnsafeCopy mv v

    {-# INLINE elemseq #-}
    elemseq = const seq


{-# INLINE toWord #-}
toWord :: ExtendedNatural -> Word
toWord (Cost x)
  | maxBound == x = x - 1
  | otherwise     = x


{-# INLINE fromWord #-}
fromWord :: Word -> ExtendedNatural
fromWord = Cost


{-# INLINE wordWidth #-}
wordWidth :: Int
wordWidth = finiteBitSize (minBound :: Word)


-- |
-- Calculate the integer logarithm of a 'Word' to base 2 using efficient compiler
-- builtins. This should translate into an assembly primitive on CISC chipsets.
-- Might be slightly more expensive on RISC chipsets.
{-# INLINE bitsUsed  #-}
bitsUsed :: Word -> Int
bitsUsed (W# w#)
  |  isTrue# (w# `eqWord#` 0##) || isTrue# (w# `eqWord#` 1##) = 0 -- technically incorrect, but useful for us.
  | otherwise = I# (wordLog2# w#) + 1
