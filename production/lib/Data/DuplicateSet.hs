-----------------------------------------------------------------------------
-- |
-- Module      :  Data.DuplicateSet
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- An unordered container that uses an Ordering constraint to store duplciate
-- elements and reduce duplicate work.
--
-- By construction, all 'DuplicateSet' values are "non-empty sets".
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveFunctor, TypeFamilies #-}

module Data.DuplicateSet
  ( DuplicateSet()
  , replicate
  , singleton
  ) where


import           Data.Bifunctor            (first)
import           Data.Foldable
import qualified Data.List.NonEmpty as NE
import           Data.Map                  (Map)
import qualified Data.Map           as Map
import           Data.Semigroup
import           Data.Semigroup.Foldable
import           Data.Set                  (Set)
import           Prelude            hiding (lookup, replicate)
import qualified Prelude            as P   (replicate)
import           Test.QuickCheck


-- |
-- A sequence of values that are repeated multiple times in contiguous blocks.
newtype DuplicateSet a = DSet (Map a Int)
   deriving (Eq, Ord)


-- |
-- Generation biases towards medium length
instance (Arbitrary a, Ord a) => Arbitrary (DuplicateSet a) where

    arbitrary = DSet . Map.fromList <$> f 0
      where
        f n = do
          b <- (arbitrary :: Gen Int) `suchThat` (\x -> 1 < x && x <= 6)
          if b - n <= 0
          then pure []
          else do
            i <- (arbitrary :: Gen Int) `suchThat` (\x -> 0 < x && x <= 12)
            v <-  arbitrary
            ((v,i):) <$> f (n+1)



instance Foldable DuplicateSet where

    {-# INLINE fold #-}
    fold      = fold . expand

    {-# INLINE foldMap #-}
    foldMap f = fold . foldMap (uncurry (flip P.replicate)) . fmap (first f) . Map.toAscList . unwrap

    {-# INLINE foldr #-}
    foldr f e = foldr f e . expand

    {-# INLINE toList #-}
    toList    = expand

    {-# INLINE null #-}
    null      = null . unwrap

    {-# INLINE length #-}
    length    = sum . unwrap
    
    {-# INLINE elem #-}
    elem e    = elem e . uniqueElems

    -- | /O(log(n))/
    {-# INLINE maximum #-}
    maximum   = fst . Map.findMax . unwrap 
  

    -- | /O(log(n))/
    {-# INLINE minimum #-}
    minimum   = fst . Map.findMin . unwrap

    
instance Foldable1 DuplicateSet where

    {-# INLINE fold1 #-}
    fold1      = fold1 . NE.fromList . expand

    {-# INLINE foldMap1 #-}
    foldMap1 f = foldMap1 f . NE.fromList . expand

    
instance Ord a => Semigroup (DuplicateSet a) where

    {-# INLINE (<>) #-}
    lhs <> rhs = DSet $ Map.unionWith (+) (unwrap lhs) (unwrap rhs)


instance Show a => Show (DuplicateSet a) where

    show = show . expand


-- |
-- /O(1)/
--
-- @replicate n x@ is a 'DuplicateSet' of the supplied element value repeated @n@ times.
{-# INLINE replicate #-}
replicate :: Ord a => Int -> a -> DuplicateSet a
replicate i e
  | i < 1     = error $ "Call to replicate with a non-positve value: " <> show i
  | otherwise = DSet $ Map.singleton e i


-- |
-- /O(1)/
--
-- Create a singleton 'DuplicateSet' with one element value occuring one time.
{-# INLINE singleton #-}
singleton :: Ord a => a -> DuplicateSet a
singleton = DSet . (`Map.singleton` 1)


{-# INLINE expand #-}
expand :: DuplicateSet a -> [a]
expand =  foldMap (uncurry (flip P.replicate)) . Map.toAscList . unwrap


-- | /O(n)/
{-# INLINE uniqueElems #-}
uniqueElems :: DuplicateSet a -> Set a
uniqueElems = Map.keysSet . unwrap


{-# INLINE unwrap #-}
unwrap :: DuplicateSet a -> Map a Int
unwrap (DSet xs) = xs

