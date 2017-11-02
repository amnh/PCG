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

{-# LANGUAGE DeriveGeneric, TypeFamilies #-}

module Numeric.Extended.Real
  ( ExtendedReal()
  , ExtendedNumber(..)
  , Finite
  ) where

import Control.DeepSeq
import Control.Applicative       (liftA2)
import Data.Ratio
import Data.Maybe                (fromMaybe)
import GHC.Generics
import Numeric.Extended.Internal


-- |
-- A non-negative real number extended to include infinity, where:
--
-- > infinity == maxBound
--
newtype ExtendedReal = Cost (Maybe Double)
    deriving (Eq, Generic)


type instance Finite ExtendedReal = Double


instance Bounded ExtendedReal where

    maxBound = Cost Nothing

    minBound = Cost $ Just 0.0


instance Enum ExtendedReal where

    fromEnum (Cost x) = maybe (maxBound :: Int) fromEnum x

    toEnum = Cost . Just . toEnum


instance ExtendedNumber ExtendedReal where

    unsafeToFinite = toDouble

    fromFinite = fromDouble

    infinity = maxBound


instance Fractional ExtendedReal where

    (Cost lhs) / (Cost rhs) =
        case (lhs, rhs) of
          (Nothing,       _) -> Cost Nothing
          (Just _ , Nothing) -> Cost $ Just 0.0
          (Just x , Just y ) -> Cost . Just $ x / y

    recip (Cost x) = Cost $ recip <$> x
    
    fromRational = Cost . Just . fromRational


instance NFData ExtendedReal


instance Num ExtendedReal where

    (Cost lhs) + (Cost rhs) = Cost $ liftA2 (+) lhs rhs

    (Cost lhs) - (Cost rhs) = Cost $ liftA2 (-) lhs rhs

    (Cost lhs) * (Cost rhs) = Cost $ liftA2 (*) lhs rhs

    abs = id

    signum (Cost (Just x)) = Cost . Just $ signum x -- the second signum is Double.signum
    signum               _ = 1

    fromInteger = Cost . Just . fromInteger

    negate = id


instance Ord ExtendedReal where

    (Cost lhs) <= (Cost rhs) =
        case (lhs, rhs) of
            (Nothing, Nothing) -> True
            (Nothing, Just _ ) -> False
            (Just _,  Nothing) -> True
            (Just x,  Just y ) -> x <= y

    (Cost lhs) < (Cost rhs) =
        case lhs of
            Nothing -> False
            Just x  ->
                case rhs of
                    Nothing -> True
                    Just y  -> x < y

    (Cost lhs) > (Cost rhs) =
        case rhs of
            Nothing -> False
            Just x  ->
                case lhs of
                    Nothing -> True
                    Just y  -> x > y


instance Real ExtendedReal where

    toRational (Cost x) = maybe (1%0) toRational x


instance Show ExtendedReal where

    show (Cost input) = maybe "∞" show input


{-# INLINE toDouble #-}
toDouble :: ExtendedReal -> Double
toDouble (Cost x) = fromMaybe (read "infinity" :: Double) x


{-# INLINE fromDouble #-}
fromDouble :: Double -> ExtendedReal
fromDouble = Cost . Just


