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
-- A /non-negative/, rational number extended to include infinity where:
--
--   * Non-negative
--   * Monotonically non-decreasing "addition"
--   * Equatable
--   * Total ordering
--   * An "infinite value" representation
--   * Perfect precision for rational numbers
--   * Reasonable efficiency
--   * Monoid under addition
--                     
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}


module Numeric.Cost
  ( ExtendedReal()
  , ExtendedNumber(..)
  , Finite
  , (~==)
  ) where

import Control.DeepSeq
import Data.Binary
import Data.Ratio
import Numeric.Natural
import GHC.Generics
import Numeric.Extended.Internal
import Test.QuickCheck
import TextShow                  (TextShow (showb))


-- |
-- A /non-negative/, rational number extended to include infinity where:
newtype Cost = Cost { unwrap :: Ratio Natural }
    deriving (Eq, Generic)


type instance Finite Cost = Ratio Natural


instance Arbitrary Cost where

    arbitrary = do
        n <- choose weightForInfinity
        if n == 0
        then pure infinity
        else do
           num <- arbitrary
           dem <- arbitrary `suchThat` (>= 0))
           pure . Cost $ num % dem
      where
        -- We generate the 'infinity' value 1 in 20 times.
        weightForInfinity = (0, 19) :: (Word, Word)

instance Binary Cost


instance Eq Cost where

    (Cost x) == (Cost y) =
       (0 == denominator x && 0 == denominator y) || x == y
  

instance ExtendedNumber Cost where

    unsafeToFinite = unwrap

    fromFinite = Cost

    infinity   = Cost $ 1 % 0


instance Fractional Cost where

    lhs@(Cost x) / rhs@(Cost y) =
        case (lhs == infinity, rhs == infinity) of
          ( True,     _) -> infinity
          (False,  True) -> minBound
          (False, False) -> if   y == 0
                            then infinity
                            else Cost $ x / y

    recip (Cost x) = Cost $ recip x

    fromRational = Cost . fromRational


instance Monoid Cost where

    mempty = Cost $ 0 % 1


instance NFData Cost


instance Ord Cost where

   (Cost x) `compare` (Cost y) =
       -- Special case the infinity values
       case (0 == denominator x, 0 == denominator y) of
         (True , True ) -> EQ
         (True , False) -> GT
         (False, True ) -> LT
         (False, False) -> x `compare` y


instance Real Cost where

    toRational (Cost x) = toRational x


instance Semigroup where

    (<>) (Cost x) (Cost y) = Cost $ x + y 


-- TODO make this nice with repeating fractions.
instance Show Cost where

    show value@(Cost x)
      | value == infinity = "∞"
      | otherwise         = show x


instance TextShow ExtendedReal where

    showb value@(Cost x)
      | value == infinity = "∞"
      | otherwise         = showb x


renderCost :: Cost -> String
renderCost (Cost v) 
  | den == 0  = "∞"
  | otherwise = renderRational den num
  where
    den = denominator v
    num = numerator   v

renderRational :: Natural -> Natural -> String
renderRational den num = show q : "." : (show <$> go r [])
  where
    (q,r) = num `divRem` den

    go  0 xs = reverse xs
    go !v xs = let (!q,!r) = (v*10) `divRem` den
               in  go r (q:xs)
