-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Extended.Real
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- A type that extends the Real numbers to include an infinity value.
--
-- This is a newtyped @Maybe Double@ for efficiency purposes.
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies  #-}

module Numeric.Extended.Real
  ( ExtendedReal()
  , ExtendedNumber(..)
  , Finite
  , (~==)
  ) where

import           Control.DeepSeq
import           GHC.Generics
import           Numeric.Extended.Internal
import           Test.QuickCheck


-- |
-- A /non-negative/, real number extended to include infinity where:
--
-- > infinity > maxBound
--
-- @maxBound@ is defined as the largest representable finite value.
newtype ExtendedReal = Cost Double
    deriving (Eq, Generic, Ord)


type instance Finite ExtendedReal = Double


instance Arbitrary ExtendedReal where

    arbitrary = do
        n <- choose weightForInfinity
        if n == 1
        then pure infinity
        else Cost <$> (arbitrary `suchThat` (>= 0))
      where
        -- We generate the 'infinity' value 1 in 20 times.
        weightForInfinity = (1, 20) :: (Int, Int)


instance Bounded ExtendedReal where

    maxBound = Cost 1.7976931348623157e+308

    minBound = Cost 0.0


instance Enum ExtendedReal where

    fromEnum (Cost x)
      | x >= v    = maxBound :: Int
      | otherwise = fromEnum x
      where
        v = toEnum (maxBound :: Int)

    toEnum = Cost . toEnum

    succ   = (+ 1)

    pred   = subtract 1


instance ExtendedNumber ExtendedReal where

    unsafeToFinite = toDouble

    fromFinite = fromDouble

    infinity   = Cost $ 1 / 0


instance Fractional ExtendedReal where

    lhs@(Cost x) / rhs@(Cost y) =
        case (lhs == infinity, rhs == infinity) of
          ( True,    _) -> infinity
          (False, True) -> minBound
          (False,False) -> Cost $ x / y

    recip (Cost x) = Cost $ recip x

    fromRational = Cost . fromRational


instance NFData ExtendedReal


-- |
-- Subtraction will never result in a negative value:
-- @x - y@ where @y > x@ results in @0@
instance Num ExtendedReal where

    lhs@(Cost x) - (Cost y)
      | lhs == infinity = infinity
      | otherwise       = max (Cost (x - y)) minBound

    lhs@(Cost x) + rhs@(Cost y) =
        case (lhs == infinity, rhs == infinity) of
          ( True,    _) -> infinity
          (    _, True) -> infinity
          (    _,    _) -> let v = Cost $ x + y
                           in if   v == infinity
                              then maxBound
                              else v

    lhs@(Cost x) * rhs@(Cost y) =
        case (lhs == infinity, rhs == infinity) of
          ( True,    _) -> infinity
          (    _, True) -> infinity
          (    _,    _) -> let v = Cost $ x * y
                           in if   v == infinity
                              then maxBound
                              else v

    abs = id

    signum (Cost x) = Cost $ signum x -- the second signum is Double.signum

    fromInteger = Cost . fromInteger

    negate = id


instance Real ExtendedReal where

    toRational (Cost x) = toRational x


instance Show ExtendedReal where

    show value@(Cost x)
      | value == infinity = "∞"
      | otherwise         = show x


{-# INLINE toDouble #-}
toDouble :: ExtendedReal -> Double
toDouble (Cost x) = x


{-# INLINE fromDouble #-}
fromDouble :: Double -> ExtendedReal
fromDouble x
  | x == nan  = infinity
  | x <    0  = minBound
  | otherwise = Cost x


nan :: Double
nan = 0 / 0


epsilon :: ExtendedReal
epsilon = Cost 2.2204460492503131e-16


infix 4 ~==


-- |
-- Approximate equality.
--
-- Two ExtendedNatural values \(x\) & \(y\) are /approximately/ equal iff:
--
-- \(\left( x \leq y \land x + \varepsilon \geq y \right) \lor \left( x \geq y \land x - \varepsilon \leq y \right) \)
--
-- Where \(\varepsilon\) is defined as the smallest IEEE representable positive value \(x\) such that \(1 + x \not = 1\).
--
(~==) :: ExtendedReal -> ExtendedReal -> Bool
(~==) lhs rhs =
    case lhs `compare` rhs  of
      EQ -> True
      LT -> lhs + epsilon >= rhs
      GT -> lhs - epsilon <= rhs

