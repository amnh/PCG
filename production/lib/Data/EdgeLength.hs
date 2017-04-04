
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.EdgeLength
  ( EdgeLength()
  , fromDouble
  , fromDoubleMay
  ) where


import Control.Applicative (liftA2)
import Data.Monoid hiding ((<>))
import Data.Semigroup


-- |
-- A monoidal edge annotation for representing an edge length.
--
-- The identity element represents an *unspecified* edge length, which is
-- distinct from a specified edge length of length zero.
--
-- The assoiative semigroup opperation is isomorphic to addition over the real
-- numbers when both operands are specified edge length. When one or more of
-- the edge lengths are unspecified (the identity element), the semigroup
-- operation returns a specified edge length if possible, satifying the monoid
-- laws regarding the identity element.
newtype EdgeLength = C (Maybe (Sum Double))
  deriving(Eq, Ord, Semigroup, Monoid)


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
            

{-# INLINE fromDouble #-}
fromDouble :: Double -> EdgeLength
fromDouble = C . Just . Sum


{-# INLINE fromDoubleMay #-}
fromDoubleMay :: Maybe Double -> EdgeLength
fromDoubleMay = C . fmap Sum


{-# INLINE liftAlt2 #-}
liftAlt2 :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
liftAlt2 _ Nothing  Nothing = Nothing
liftAlt2 _ Nothing  rhs     = rhs
liftAlt2 _ lhs      Nothing = lhs
liftAlt2 f lhs      rhs     = liftA2 f lhs rhs


{-# INLINE unwrap #-}
unwrap :: EdgeLength -> Maybe (Sum Double)
unwrap (C x) = x
