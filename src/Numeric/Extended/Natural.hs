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

{-# LANGUAGE DeriveGeneric, MagicHash, TypeFamilies #-}

module Numeric.Extended.Natural
  ( ExtendedNatural()
  , ExtendedNumber(..)
  , Finite
  ) where

import Control.DeepSeq
import Data.Bits
import GHC.Exts
import GHC.Integer.Logarithms
import GHC.Generics
import Numeric.Extended.Internal
import Test.QuickCheck


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
  deriving (Eq, Generic, Ord)


type instance Finite ExtendedNatural = Word


instance Arbitrary ExtendedNatural where

    arbitrary = do
        n <- choose weightForInfinity
        if n == 1
        then pure infinity
        else Cost <$> arbitrary
      where
        -- We generate the 'infinity' value 1 in 20 times.
        weightForInfinity = (1, 20) :: (Int, Int)


instance Bounded ExtendedNatural where

    maxBound = Cost $ maxBound - 1

    minBound = Cost minBound


instance Enum ExtendedNatural where

    fromEnum = fromEnum . toWord

    toEnum   = Cost . toEnum

    succ     = (+ 1)

    pred x   = x - 1


instance ExtendedNumber ExtendedNatural where

    unsafeToFinite = toWord

    fromFinite = fromWord

    infinity = Cost maxBound


instance NFData ExtendedNatural


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
      | zBits  >  wordWidth  = maxBound
      | otherwise            = Cost $ x * y
      where
        wordWidth = finiteBitSize (minBound :: Word)
        zBits = xBits + yBits
        xBits = bitsUsed x
        yBits = bitsUsed y

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

    divMod = quotRem


instance Real ExtendedNatural where

    toRational = toRational . toInteger


instance Show ExtendedNatural where

    show (Cost input)
      | input == maxBound = "∞"
      | otherwise         = show input


{-# INLINE toWord #-}
toWord :: ExtendedNatural -> Word
toWord (Cost x)
  | maxBound == x = x - 1
  | otherwise     = x


{-# INLINE fromWord #-}
fromWord :: Word -> ExtendedNatural
fromWord = Cost



-- |
-- Calculate the integer logarithm of a 'Word' to base 2 using efficient compiler
-- builtins. This should translate into an assembly primative on CISC chipsets.
-- Might be slightly more expensive on RISC chipsets.
{-# INLINE bitsUsed  #-}
bitsUsed :: Word -> Int
bitsUsed (W# w#)
  |  isTrue# (w# `eqWord#` 0##) || isTrue# (w# `eqWord#` 1##) = 0 -- technically incorrect, but useful for us.
  | otherwise = I# (wordLog2# w#) + 1
