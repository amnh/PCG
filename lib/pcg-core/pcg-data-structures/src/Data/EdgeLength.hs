-----------------------------------------------------------------------------
-- |
-- Module      :  Data.EdgeLength
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- A possible present, real-valued edge length with a 'Monoid' instance defined
-- in terms of addition.
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Data.EdgeLength
  ( EdgeLength()
  , fromDouble
  , fromDoubleMay
  , fromRationalMay
  ) where


import Control.Applicative (liftA2)
import Control.DeepSeq
import Data.Binary         (Binary)
import Data.Default
import Data.Monoid         (Sum (..))
import GHC.Generics
import TextShow            (TextShow (showb))


-- |
-- A monoidal edge annotation for representing an edge length.
--
-- The identity element represents an *unspecified* edge length, which is
-- distinct from a specified edge length of zero.
--
-- The associative semigroup operation is isomorphic to addition over the real
-- numbers when both operands have specified edge lengths. When one or more of
-- the edge lengths are unspecified (the identity element), the semigroup
-- operation returns a specified edge length if possible, satifying the monoid
-- laws regarding the identity element.
newtype EdgeLength = C (Maybe (Sum Rational))
    deriving stock    (Eq, Generic, Ord)
    deriving anyclass (NFData)
    deriving newtype  (Default, Semigroup, Monoid)


instance Binary EdgeLength


instance Num EdgeLength where

    (C lhs) + (C rhs) = C $ liftAlt2 (+) lhs rhs

    (C lhs) - (C rhs) = C $ liftAlt2 (-) lhs rhs

    (C lhs) * (C rhs) = C $ liftAlt2 (*) lhs rhs

    abs = C . fmap abs . unwrap

    signum (C Nothing) = C . Just $ Sum 0
    signum (C       x) = C $ signum <$> x

    fromInteger = C . Just . fromInteger

    negate = C . fmap negate . unwrap


instance Show EdgeLength where

    show (C  Nothing) = "{?}"
    show (C (Just x)) = "{" <> show (getSum x) <> "}"

instance TextShow EdgeLength where

    showb (C  Nothing) = "{?}"
    showb (C (Just x)) = "{" <> showb (getSum x) <> "}"


-- |
-- Construct an 'EdgeLength' from a 'Double' value.
{-# INLINE fromDouble #-}
fromDouble :: Double -> EdgeLength
fromDouble = C . Just . Sum . toRational


-- |
-- Construct an 'EdgeLength' from a 'Maybe Double' value.
-- A @Nothing@ value indicates a missing edge length.
{-# INLINE fromDoubleMay #-}
fromDoubleMay :: Maybe Double -> EdgeLength
fromDoubleMay = C . fmap (Sum . toRational)


-- |
-- Construct an 'EdgeLength' from a 'Maybe Rational' value.
-- A @Nothing@ value indicates a missing edge length.
{-# INLINE fromRationalMay #-}
fromRationalMay :: Maybe Rational -> EdgeLength
fromRationalMay = C . fmap Sum


{-# INLINE liftAlt2 #-}
liftAlt2 :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
liftAlt2 _ Nothing  Nothing = Nothing
liftAlt2 _ Nothing  rhs     = rhs
liftAlt2 _ lhs      Nothing = lhs
liftAlt2 f lhs      rhs     = liftA2 f lhs rhs


{-# INLINE unwrap #-}
unwrap :: EdgeLength -> Maybe (Sum Rational)
unwrap (C x) = x
