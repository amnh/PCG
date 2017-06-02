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
-- By construction, all values of type 'DuplicateSet' are "non-empty sets".
--
-- The main utility of the DuplicateSet data structure comes from the
-- 'cartesianProductWith' function, which takes the cartesian product of two
-- 'DuplicaterSet's elements withouf repeating work on duplciate elements.
-----------------------------------------------------------------------------

module Data.DuplicateSet
  ( DuplicateSet()
  , cartesianProduct
  , cartesianProductWith
  , cartesianProductWithFilter
  , expand
  , map
  , replicate
  , singleton
  ) where


import           Data.Bifunctor            (first)
import           Data.Foldable
import           Data.List.NonEmpty        (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Map                  (Map)
import qualified Data.Map           as Map
import           Data.Maybe                (fromJust)
import           Data.Semigroup
import           Data.Semigroup.Foldable
import           Data.Set                  (Set)
import           Prelude            hiding (lookup, replicate, map)
import qualified Prelude            as P   (replicate)
import           Test.QuickCheck


-- |
-- A sequence of values that are repeated multiple times in contiguous blocks.
newtype DuplicateSet a = DSet (Map a Int)
   deriving (Eq)


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
    toList    = toList . expand

    {-# INLINE null #-}
    null      = const False

    {-# INLINE length #-}
    length    = sum . unwrap
    
    {-# INLINE elem #-}
    elem e    = elem e . uniqueElems

    -- | /O(log(m))/ where /m/ is the number of unique elements.
    {-# INLINE maximum #-}
    maximum   = fst . Map.findMax . unwrap 
  

    -- | /O(log(m))/ where /m/ is the number of unique elements.
    {-# INLINE minimum #-}
    minimum   = fst . Map.findMin . unwrap

    
instance Foldable1 DuplicateSet where

    {-# INLINE fold1 #-}
    fold1      = fold1 . expand

    {-# INLINE foldMap1 #-}
    foldMap1 f = foldMap1 f . expand


instance Ord a => Ord (DuplicateSet a) where

    compare lhs rhs = uniqueElems lhs `compare` uniqueElems rhs

    
instance Ord a => Semigroup (DuplicateSet a) where

    {-# INLINE (<>) #-}
    lhs <> rhs = DSet $ Map.unionWith (+) (unwrap lhs) (unwrap rhs)


instance Show a => Show (DuplicateSet a) where

    show = show . expand


-- |
-- /O(1)/
--
-- @replicate n x@ is a 'DuplicateSet' of the supplied element value repeated
-- @n@ times.
{-# INLINE replicate #-}
replicate :: Int -> a -> DuplicateSet a
replicate i e
  | i < 1     = error $ "Call to replicate with a non-positve value: " <> show i
  | otherwise = DSet $ Map.singleton e i


-- |
-- /O(1)/
--
-- Create a singleton 'DuplicateSet' with one element value occuring one time.
{-# INLINE singleton #-}
singleton :: a -> DuplicateSet a
singleton = DSet . (`Map.singleton` 1)

-- |
-- /O(n)/ where /n/ is the total number of duplicated elements.
--

-- Note that the call to NE.fromList is safe here as the DuplicateSet must be
-- non-empty by construction.
{-# INLINE expand #-}
expand :: DuplicateSet a -> NonEmpty a
expand = NE.fromList . foldMap (uncurry (flip P.replicate)) . Map.toAscList . unwrap


{-# INLINE map #-}
-- |
-- /O(n*log(n))/
--
-- Map a function over the 'DuplicateSet'.
-- The function is not required to be monotonic and consequently the 'DuplicateSet'
-- must be re-sorted after the map.
map :: Ord b => (a -> b) -> DuplicateSet a -> DuplicateSet b
map f = DSet . Map.mapKeys f . unwrap


-- |
-- /O(m)/ where /m/ is the number of unique elements.
{-# INLINE uniqueElems #-}
uniqueElems :: DuplicateSet a -> Set a
uniqueElems = Map.keysSet . unwrap


{-# INLINE unwrap #-}
unwrap :: DuplicateSet a -> Map a Int
unwrap (DSet xs) = xs


-- |
-- |
-- /O(x*y)/ where /x/ is the number of unique elements in the left-hand side
-- and /y/ is the number of unique elements in the right-hand side.
--
-- An alias for @cartesianProductWith (,)@.
cartesianProduct :: (Eq a, Eq b) => DuplicateSet a -> DuplicateSet b -> DuplicateSet (a, b)
cartesianProduct = cartesianProductWith (,)


-- |
-- /O(x*y)/ where /x/ is the number of unique elements in the left-hand side
-- and /y/ is the number of unique elements in the right-hand side.
cartesianProductWith :: (Eq c) => (a -> b -> c) -> DuplicateSet a -> DuplicateSet b -> DuplicateSet c
cartesianProductWith f lhs rhs = fromJust $ cartesianProductWithFilter (\x y -> Just $ f x y) lhs rhs


-- |
-- /O(x*y)/ where /x/ is the number of unique elements in the left-hand side
-- and /y/ is the number of unique elements in the right-hand side.
--
-- Pairs of elements for which for which a @Nothing@ value is returned are
-- filtered from the resulting 'Maybe DuplicateSet'. If all pairs of elements
-- return a @Nothing@ value, then the result of the product is a @Nothing@ value.
-- Otherwise a the resulting 'Maybe DuplicateSet' has a @Just@ value. This is to
-- preserve the invariant that a 'DuplicateSet is always "non-empty".

-- I think that the use of Map.fromAscList is safe here.
-- If not we can change the call to Map.fromList as the cartesian product is /O(x*y)/.
cartesianProductWithFilter :: Eq c => (a -> b -> Maybe c) -> DuplicateSet a -> DuplicateSet b -> Maybe (DuplicateSet c)
cartesianProductWithFilter f lhs rhs =
    case resultList of
      [] -> Nothing
      xs -> Just . DSet $ Map.fromAscList xs
  where
    lhs' = Map.toAscList $ unwrap lhs
    rhs' = Map.toAscList $ unwrap rhs
    resultList = do
        (x,m) <- lhs'
        (y,n) <- rhs'
        case f x y of
           Just v  -> [(v, m*n)]
           Nothing -> []
