-----------------------------------------------------------------------------
-- |
-- Module      :  Data.ReplicatedSequence
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable, DeriveFunctor, DeriveTraversable, GeneralizedNewtypeDeriving, TypeFamilies #-}

module Data.Vector.NonEmpty
  ( Vector()
  , fromNonEmpty
  , singleton
  -- * Useful stuff
  , uncons
  , unfoldr
  ) where


import           Control.DeepSeq
import           Data.Data
import           Data.Foldable
import           Data.Functor.Alt
import           Data.Functor.Classes
import           Data.Functor.Bind
import           Data.Functor.Extend
import           Data.Hashable
import           Data.Key
import qualified Data.List.NonEmpty         as NE
import           Data.Pointed
import           Data.Semigroup
import           Data.Semigroup.Foldable
import           Data.Semigroup.Traversable
import qualified Data.Vector                as V
import           Data.Vector.Instances             ()
import           Prelude                    hiding (lookup)
import           Test.QuickCheck


-- |
-- A sequence of values that are repeated multiple times in contiguous blocks.
newtype Vector a = NEV { unwrap :: V.Vector a }
   deriving ( Adjustable
            , Applicative
            , Apply
            , Bind
            , Data
            , Eq
            , Eq1
            , Extend
            , Foldable
            , FoldableWithKey
            , Functor
            , Hashable
            , Indexable
            , Keyed
            , Lookup
            , Monad
            , NFData
            , Ord
            , Ord1
            , Pointed
            , Semigroup
            , Traversable
            , Zip
            , ZipWithKey
            )


-- |
-- Generation biases towards medium length
instance Arbitrary a => Arbitrary (Vector a) where

    arbitrary = do
      list   <- arbitrary
      values <- case list of
                  [] -> pure <$> arbitrary
                  xs -> pure xs
      pure . NEV $ V.fromList values


instance Alt Vector where

    (<!>) = (<>)


instance Foldable1 Vector where

    fold1 = fold1 . toNonEmpty

    foldMap1 f = foldMap1 f . toNonEmpty

    toNonEmpty = NE.fromList . toList . unwrap


instance FoldableWithKey1 Vector where

    foldMapWithKey1 f = foldMapWithKey1 f . toNonEmpty


type instance Key Vector = Int


instance Traversable1 Vector where

    traverse1 f = fmap fromNonEmpty . traverse1 f . toNonEmpty


instance TraversableWithKey Vector where

    traverseWithKey f = fmap NEV . traverseWithKey f . unwrap
  

instance TraversableWithKey1 Vector where

    traverseWithKey1 f = fmap fromNonEmpty . traverseWithKey1 f . toNonEmpty
  

instance Show a => Show (Vector a) where

    show = show . unwrap


-- |
-- /O(1)/
--
-- A synomym for 'point'.
{-# INLINE singleton #-}
singleton :: a -> Vector a
singleton = NEV . V.singleton


-- |
-- /On)/
--
-- Construct a 'Vector' from a non-empty structure.
{-# INLINE fromNonEmpty #-}
fromNonEmpty :: Foldable1 f => f a -> Vector a
fromNonEmpty = NEV . V.fromList . toList . toNonEmpty


-- | /O(n)/
--
-- Construct a vector by repeatedly applying the generator function
-- to a seed. The generator function yields 'Just' the next element and the
-- new seed or 'Nothing' if there are no more elements.
--
-- > unfoldr (\n -> if n == 0 then Nothing else Just (n,n-1)) 10
-- >  = <10,9,8,7,6,5,4,3,2,1>
{-# INLINE unfoldr #-}
unfoldr :: (b -> Maybe (a, b)) -> b -> Vector a
unfoldr f = NEV . V.unfoldr f


-- | /O(n)/
--
-- 'uncons' produces the first element of the 'Vector', and a 'Vector' of the remaining elements, if any.
uncons :: Vector a -> (a, Maybe (Vector a))
uncons (NEV v) = (first, stream)
  where
    stream
      | len == 1  = Nothing
      | otherwise = Just . NEV $ V.slice 1 (len-1) v
    first = v ! 0
    len   = length v
