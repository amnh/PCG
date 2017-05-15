-----------------------------------------------------------------------------
-- |
-- Module      :  Data.ExtendedNatural
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
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
--   - 'ExtendedNatural' does not throw an error on arithmetic underflows
--     caused by subtracting a larger value from a smaller value. Instead,
--     the result is floored at zero.
--
--   - 'ExtendedNatural' does not throw an error on arithmetic overflows
--     caused by adding or multiplying two numbers that would produce a
--     result larger than @(maxBound :: ExtendedNatural)@. Instead, the result is
--     capped at @(maxBound :: ExtendedNatural)@.
--
--   - An @infinity@ value can only result from it's use as a constant, addition
--     with @infinity@ as an operand, multiplication with @infinity@ as an
--     operand subtraction with @infinity@ as the minuend, or division with
--     @infinity@ as the0 denominator.
-- 
-----------------------------------------------------------------------------

{-# LANGUAGE MagicHash, TypeFamilies #-}

module Data.ExtendedNatural
  ( ExtendedNatural()
  , ExtendedNumber(..)
  , Finite
  ) where

import Data.Bits
import Data.ExtendedFinite
import GHC.Exts
import GHC.Integer.Logarithms


-- |
-- A natural number extended to include infinity.
newtype ExtendedNatural = Cost Word
  deriving (Eq, Ord)


type instance Finite ExtendedNatural = Word


instance Bounded ExtendedNatural where

    maxBound = Cost $ maxBound - 1

    minBound = Cost minBound


instance Enum ExtendedNatural where

    fromEnum = fromEnum . toWord

    toEnum   = Cost . toEnum


instance ExtendedNumber ExtendedNatural where

    unsafeToFinite = toWord

    fromFinite = fromWord

    infinity = maxBound


instance Num ExtendedNatural where

    lhs@(Cost x) + rhs@(Cost y)
      | lhs    == infinity   = infinity
      | rhs    == infinity   = infinity
      | result >= infinity   = maxBound
      | result <  maxima     = maxBound
      | otherwise            = Cost result
      where
        maxima = max x y 
        result = x + y

    lhs@(Cost x) - rhs@(Cost y)
      | lhs == infinity = infinity
      | lhs <= rhs      = minBound 
      | otherwise       = Cost $ x - y

    lhs@(Cost x) * rhs@ (Cost y)
      | lhs    == infinity   = infinity
      | rhs    == infinity   = infinity
      | zBits  >= wordWidth  = maxBound
      | otherwise            = Cost $ x * y
      where
        wordWidth = finiteBitSize (minBound :: Word)
        zBits = xBits + yBits
        xBits = wordLog2 x
        yBits = wordLog2 y

    abs = id

    signum (Cost 0) = 0
    signum        _ = 1

    fromInteger = Cost . fromInteger

    negate = const 0


instance Integral ExtendedNatural where

    toInteger = toInteger . toWord

    quotRem lhs@(Cost x) rhs@(Cost y)
      | lhs == infinity = (infinity, minBound)
      | rhs == infinity = (minBound, minBound)
      | rhs == minBound = (infinity, minBound)
      | otherwise       = let (q,r) = x `quotRem` y
                          in  (Cost q, Cost r)


instance Real ExtendedNatural where

    toRational = toRational . toInteger


instance Show ExtendedNatural where

    show (Cost input)
      | input == maxBound     = "∞"
      | otherwise             = show input


{-# INLINE toWord #-}
toWord :: ExtendedNatural -> Word
toWord (Cost x)
  | maxBound == x = x - 1
  | otherwise     = x


{-# INLINE fromWord #-}
fromWord :: Word -> ExtendedNatural
fromWord = Cost


-- |
-- Calculate the integer logarithm of a 'Word' to base 2 using efficient
-- compiler builtins. 
{-# INLINE wordLog2  #-}
wordLog2 :: Word -> Int
wordLog2 (W# w#)
  | isTrue# (w# `eqWord#` 0##) = 0 -- technically incorrect, but useful for us.
  | otherwise                  = I# (wordLog2# w#)
