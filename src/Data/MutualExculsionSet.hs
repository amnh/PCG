-----------------------------------------------------------------------------
-- |
-- Module      :  Data.MutualExculsionSet
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Set-like structures for collection of edges.
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric #-}

module Data.MutualExculsionSet
  ( MutualExculsionSet
  -- * Construction
  , singleton
  -- * Deconstruct
  , excludedSet
  , includedSet
  , mutuallyExclusivePairs
  -- * Manipulation
  , invert
  -- * Comparison / Queries
  , excludedLookup
  , includedLookup
  , isExcluded
  , isIncluded
  , isPermissible
  ) where


import           Control.DeepSeq
import           Data.Foldable
import           Data.Functor.Classes
import           Data.Hashable
import           Data.Key
import           Data.List                (nub)
import qualified Data.Map          as M
import qualified Data.Map.Internal as M
import           Data.Monoid       hiding ((<>))
import           Data.Semigroup
import           Data.Set                 (Set)
import qualified Data.Set          as S
import           Data.Tuple
import           GHC.Generics             (Generic)
import           Prelude           hiding (lookup, zip)
import           Test.QuickCheck


-- |
-- Set of elements which are mutually exclusive.
--
-- Each element in the set implies that another element cannot be included in the
-- set.
--
-- This is an efficient representation for construction and manipulation of a
-- collection of mutually exclusive elements using an newtyped 'Bimap' with some
-- typeclass instance modifications.
data  MutualExculsionSet a
    = MES
    { includedKeyedMap :: !(M.Map a a)
    , excludedKeyedMap :: !(M.Map a a)
    }
    deriving (Eq, Generic, Ord)


instance (Arbitrary a, Ord a) => Arbitrary (MutualExculsionSet a) where

    arbitrary = do
        -- Generate some number of unique elements
        is  <- S.fromList <$> arbitrary
        let n = length is
        -- Try our best to generate the same number of new unique elements
        es  <- S.fromList <$> vectorOf n (arbitrary `suchThat` (`notElem` is))
        -- Randomize the ordering of the second collection
        es' <- shuffle $ toList es
        -- Zip the two collections of universally unique elements together
        -- to create a unique bijection between mutually exclusive pairs.
        let tuples   = zip (S.toAscList is) es'
        let included = M.fromDistinctAscList tuples
        let excluded = M.fromList $ swap <$> tuples
        -- Build the MutualExculsionSet from the two maps
        pure $ MES included excluded


instance Eq1 MutualExculsionSet where

    liftEq eq (MES a b) (MES c d) =
        length a == length c && liftEq eq (M.keysSet a) (M.keysSet c)


instance Ord1 MutualExculsionSet where

    liftCompare cmp (MES a b) (MES c d) =
        liftCompare cmp (M.keysSet a) (M.keysSet c)


-- |
-- Fold over the /included/ elements of the mutual exclusion set.
--
-- To fold over the /excluded/ elements of the set, first call 'invert'.
instance Foldable MutualExculsionSet where

    {-# INLINE elem #-}
    elem x     = elem x . excludedKeyedMap
  
    {-# INLINABLE fold #-}
    fold       = fold . excludedKeyedMap

    {-# INLINE foldMap #-}
    foldMap f  = foldMap f . excludedKeyedMap

    {-# INLINE foldl #-}
    foldl   f x = foldl  f x . excludedKeyedMap

    {-# INLINE foldr #-}
    foldr   f x = foldr  f x . excludedKeyedMap

    {-# INLINE foldl' #-}
    foldl'  f x = foldl' f x . excludedKeyedMap

    {-# INLINE foldr' #-}
    foldr'  f x = foldr' f x . excludedKeyedMap

    {-# INLINE length #-}
    length      = length . excludedKeyedMap

    {-# INLINABLE maximum #-}
    maximum     = maximum . excludedKeyedMap

    {-# INLINABLE minimum #-}
    minimum     = minimum . excludedKeyedMap

    {-# INLINE null #-}
    null        = null . excludedKeyedMap

    {-# INLINABLE product #-}
    product     = product . excludedKeyedMap

    {-# INLINABLE sum #-}
    sum         = sum . excludedKeyedMap

    {-# INLINE toList #-}
    toList      = toList . excludedKeyedMap
    

instance Hashable a => Hashable (MutualExculsionSet a) where

    hashWithSalt salt = foldl' hashWithSalt salt . mutuallyExclusivePairs
  

instance Ord a => Monoid (MutualExculsionSet a) where

    mappend = (<>)

    mempty  = MES mempty mempty
    

instance NFData a => NFData (MutualExculsionSet a)


-- |
-- \( \mathcal{O} \left( m * \log_2 ( \frac {n}{m + 1} ) + n + m \right), m \leq n \)
--
-- Perfoms an "inner union."
instance Ord a => Semigroup (MutualExculsionSet a) where

    (MES lhsIKS lhsEKS) <> (MES rhsIKS rhsEKS) = MES iks' eks'
      where
        -- /O(n + m)/
        iks = mergeLogic lhsIKS rhsIKS

        -- /O(n + m)/
        eks = mergeLogic lhsEKS rhsEKS

        -- /O(m*log(n/m + 1) + n + m), m <= n/
        iks' = M.withoutKeys iks $ M.keysSet lhsEKS <> M.keysSet rhsEKS

        -- /O(m*log(n/m + 1) + n + m), m <= n/
        eks' = M.withoutKeys eks $ M.keysSet lhsIKS <> M.keysSet rhsIKS

        -- When a key *is not* present in both maps,
        -- preserve the key and it's corresponding value.
        --
        -- When a key *is* present in both maps but the values are not equal,
        -- discard the key and both of the values. Otherwise preserve the key
        -- and the shared value.
        mergeLogic = M.merge M.preserveMissing M.preserveMissing discardDifferentValues
          where
            discardDifferentValues = M.zipWithMaybeMatched conditionalUnion
              where
                conditionalUnion _ v1 v2
                  | v1 == v2  = Just v1
                  | otherwise = Nothing


instance Show a => Show (MutualExculsionSet a) where

    show x = unwords
        [ "MutualExclusionSet"
        , shownIncluded
        , "|"
        , shownExcluded
        ]
      where
        shownIncluded = show included
        shownExcluded = show excluded
        (included, excluded) = unzip . M.toAscList $ includedKeyedMap x


-- |
-- \( \mathcal{O} \left( 1 \right) \)
--
-- Construct a singleton 'MutualExculsionSet' value by supplying an included
-- element and the corresponding, mutually exclusive element.
--
-- Use the semigroup operator '(<>)' to merge singleton contexts into
-- a larger 'MutualExculsionSet'.
singleton
  :: a -- ^ Included element
  -> a -- ^ Excluded element
  -> MutualExculsionSet a
singleton x y = MES (M.singleton x y) (M.singleton y x)


-- |
-- \( \mathcal{O} \left( 1 \right) \)
--
-- inverts the included and excluded elements of the 'MutualExculsionSet'. The
-- previously included elements become the excluded elements and the previously
-- excluded elements become included elements.
--
-- This inversion function preserves the bijective relationship of the elements
-- within 'MutualExculsionSet' so that the following will always hold:
--
-- > invert . invert === id
invert :: MutualExculsionSet a -> MutualExculsionSet a 
invert (MES i e) = MES e i


-- |
-- \( \mathcal{O} \left( n \right) \)
--
-- Retreive the list of included elements in the 'MutualExculsionSet'.
includedSet :: MutualExculsionSet a -> Set a
includedSet = M.keysSet . includedKeyedMap


-- |
-- \( \mathcal{O} \left( n \right) \)
--
-- Retreive the list of excluded elements in the 'MutualExculsionSet'.
excludedSet :: MutualExculsionSet a -> Set a
excludedSet = M.keysSet . excludedKeyedMap


-- |
-- \( \mathcal{O} \left( \log_2 n \right) \)
--
-- Lookup an /included/ key in the 'MutualExculsionSet'.
--
--  * If the provided element *is not* included, the result will be @Nothing@.
--
--  * If the provided element *is* included, the result will be @Just value@,
--    where @value@ is corresponding excluded element.
includedLookup :: Ord a => a -> MutualExculsionSet a -> Maybe a
includedLookup k = lookup k . includedKeyedMap

  
-- |
-- \( \mathcal{O} \left( \log_2 n \right) \)
--
-- Lookup an /excluded/ key in the 'MutualExculsionSet'.
--
--  * If the provided element *is not* excluded, the result will be @Nothing@.
--
--  * If the provided element *is* excluded, the result will be @Just value@,
--    where @value@ is corresponding included element.
excludedLookup :: Ord a => a -> MutualExculsionSet a -> Maybe a
excludedLookup k = lookup k . excludedKeyedMap


-- |
-- \( \mathcal{O} \left( \log_2 n \right) \)
--
-- Query the 'MutualExculsionSet' to determine if the provided element is /included./
isIncluded :: Ord a => a -> MutualExculsionSet a -> Bool
isIncluded k = M.member k . includedKeyedMap

  
-- |
-- \( \mathcal{O} \left( \log_2 n \right) \)
--
-- Query the 'MutualExculsionSet' to determine if the provided element is /excluded./
isExcluded :: Ord a => a -> MutualExculsionSet a -> Bool
isExcluded k = M.member k . excludedKeyedMap


-- |
-- \( \mathcal{O} \left( n \right) \)
--
-- Retreive the list of mutually exclusive elements stored in the
-- 'MutualExculsionSet'.
--
-- The first element of the pair is the included element and the second element
-- of pair is the excluded element.
mutuallyExclusivePairs :: MutualExculsionSet a -> Set (a, a)
mutuallyExclusivePairs = S.fromDistinctAscList . M.toAscList . includedKeyedMap


-- |
-- \( \mathcal{O} \left( n + m \right) \)
--
-- Perform an operation to determine if a collection of elements is "permitted"
-- by 'MutualExculsionSet', ie that the collection does not contain any elements
-- which are excluded by the 'MutualExculsionSet'.
isPermissible :: (Foldable f, Ord a) => f a -> MutualExculsionSet a -> Bool
isPermissible xs mes = getAll $ foldMap f xs
  where
    f x = All $ x `notElem` badElems
    badElems = excludedSet mes
