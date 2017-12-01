-----------------------------------------------------------------------------
-- |
-- Module      :  Data.MutualExculsionSet.Internal
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Set-like structure for a collection of elements where each included element
-- implies the exclusion of another element.
--
-- The MutualExclusionSet holds a /bijective/ mapping between a set of included
-- elements and their corresponding mutually exclusive element. The construction
-- allows for efficient access to included and excluded elements.
--
-- The following properties will always hold:
--
--  * > invert . invert === id
--
--  * @isIncluded ==> not . isExcluded@
--
--  * @isExcluded ==> not . isIncluded@
--
--  * @isIncluded e === isExcluded e . invert@
--
--  * @isExcluded e === isIncluded e . invert@
--
--  * > includedSet === excludedSet . invert
--
--  * > excludedSet === includedSet . invert
--
--  * toList === toList . includedSet
--
--  * toList . invert === toList . excludedSet
--
--  * \(\exists k\) isIncluded e ==> excludedLookup k == Just e
--
--  * \(\exists k\) isExcluded e ==> includedLookup k == Just e
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveAnyClass, DeriveGeneric, FlexibleContexts #-}

module Data.MutualExculsionSet.Internal where


import           Control.DeepSeq
import qualified Data.DList        as DL
import           Data.Foldable
import           Data.Functor.Classes
import           Data.Hashable
import           Data.Key
import           Data.Ord
import           Data.Map                 (Map)
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
    { includedKeyedMap    :: !(Map a a)
    , excludedKeyedMap    :: !(Map a a)
    , includedFullMap     :: !(Map a (Set a))
    , excludedFullMap     :: !(Map a (Set a))
    , includedAndExcluded :: !(Set a)
    } deriving (Eq, Eq1, Generic, Ord1)


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
        pure $ unsafeFromList tuples


{-
-- Faster Eq when interface is stable
instance Eq a => Eq (MutualExculsionSet a) where

    (MES _ _ a b _) == (MES _ _ c d _) = [ a == c && b == d ]
-}

{-
instance Eq1 MutualExculsionSet where

    liftEq eq (MES a _ x) (MES c _ y) = and
        [ length a == length c
        , liftEq eq a c 
        , x == y
        ]
-}


-- |
-- Fold over the /included/ elements of the mutual exclusion set.
--
-- To fold over the /excluded/ elements of the set, first call 'invert'.
instance Foldable MutualExculsionSet where

    {-# INLINE elem #-}
    elem x     = elem x . toList
  
    {-# INLINABLE fold #-}
    fold       = fold . toList

    {-# INLINE foldMap #-}
    foldMap f  = foldMap f . toList

    {-# INLINE foldl #-}
    foldl   f x = foldl  f x . toList

    {-# INLINE foldr #-}
    foldr   f x = foldr  f x . toList

    {-# INLINE foldl' #-}
    foldl'  f x = foldl' f x . toList

    {-# INLINE foldr' #-}
    foldr'  f x = foldr' f x . toList

    {-# INLINE length #-}
    length      = length . toList

    {-# INLINABLE maximum #-}
    maximum     = maximum . toList

    {-# INLINABLE minimum #-}
    minimum     = minimum . toList

    {-# INLINE null #-}
    null        = null . toList

    {-# INLINABLE product #-}
    product     = product . toList

    {-# INLINABLE sum #-}
    sum         = sum . toList

    {-# INLINE toList #-}
    toList      = M.keys . includedKeyedMap
    

instance Hashable a => Hashable (MutualExculsionSet a) where

    hashWithSalt salt = foldl' hashWithSalt salt . mutuallyExclusivePairs
  

instance Ord a => Monoid (MutualExculsionSet a) where

    mappend = (<>)

    mempty  = MES mempty mempty mempty mempty mempty
    

instance NFData a => NFData (MutualExculsionSet a)


instance Ord a => Ord (MutualExculsionSet a) where

    x `compare` y =
        case comparing includedFullMap x y of
          EQ -> comparing excludedFullMap x y
          v  -> v

-- |
-- See 'merge' for behavior
instance Ord a => Semigroup (MutualExculsionSet a) where

    (<>) = merge

    {-# INLINE stimes #-}
    stimes _ x = x


instance Show a => Show (MutualExculsionSet a) where

    show x = unlines
        [ "MutualExclusionSet"
        , render $ includedKeyedMap x
        , render $ excludedKeyedMap x 
        , show . M.assocs $ includedFullMap x
        , show . M.assocs $ excludedFullMap x
        , show $ includedAndExcluded x
        ]
      where
        render a = unwords
            [ shownIncluded
            , "|"
            , shownExcluded
            ]
          where
            shownIncluded = show included
            shownExcluded = show excluded
            (included, excluded) = unzip $ M.toAscList a


-- |
-- \( \mathcal{O} \left( 1 \right) \)
--
-- Construct a singleton 'MutualExculsionSet' value by supplying an included
-- element and the corresponding, mutually exclusive element.
--
-- Use the semigroup operator '(<>)' to merge singleton contexts into
-- a larger 'MutualExculsionSet'.
singleton
  :: Eq a
  => a -- ^ Included element
  -> a -- ^ Excluded element
  -> MutualExculsionSet a
singleton x y =
    MES
    { includedKeyedMap    = included
    , excludedKeyedMap    = excluded
    , includedFullMap     = M.singleton x (S.singleton y)
    , excludedFullMap     = M.singleton y (S.singleton x)
    , includedAndExcluded = both
    }
  where
    (included, excluded, both)
      | x == y    = (M.empty        , M.empty        , S.singleton x)
      | otherwise = (M.singleton x y, M.singleton y x, S.empty      )


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
invert (MES i e x y b) = MES e i y x b


-- |
-- Merge Sets
-- |
-- \( \mathcal{O} \left( m * \log_2 ( \frac {n}{m + 1} ) + n + m \right), m \leq n \)
--
-- Perfoms an "union-like" operation.
merge :: Ord a => MutualExculsionSet a -> MutualExculsionSet a -> MutualExculsionSet a
merge (MES _ _ lhsIFM lhsEFM _) (MES _ _ rhsIFM rhsEFM _) =
    MES ikmBi' ekmBi' ikmFull ekmFull both
  where
    -- /O( m + n )/
    (ikmBi, ikmFull) = mergeLogic lhsIFM rhsIFM
    -- /O( m + n )/
    (ekmBi, ekmFull) = mergeLogic lhsEFM rhsEFM

    both = M.keysSet ikmFull `S.intersection` M.keysSet ekmFull

    ikmBi' = M.withoutKeys ikmBi both

    ekmBi' = M.withoutKeys ekmBi both

    -- When a key *is not* present in both maps,
    -- preserve the key and it's corresponding value.
    --
    -- When a key *is* present in both maps but the values are not equal,
    -- discard the key and both of the values. Otherwise preserve the key
    -- and the shared value.
    mergeLogic :: Ord a => Map a (Set a) -> Map a (Set a) -> (Map a a, Map a (Set a))
    mergeLogic lhs rhs = (M.fromDistinctAscList $ toList bijectives, full)
      where
        (bijectives, full) = go lhs rhs

        go = M.mergeA preserveMissingValues preserveMissingValues accumulateDifferentValues
        
        isBijective x y =
          case (toList x, toList y) of
            ([a], [b]) -> if a == b then Just a else Nothing
            _          -> Nothing
        
        preserveMissingValues = M.traverseMaybeMissing conditionalPreservation
          where
            conditionalPreservation k v =
              case toList v of
                [x] -> (DL.singleton (k, x), Just v)
                _   -> pure $ Just v

        accumulateDifferentValues = M.zipWithMaybeAMatched conditionalUnion
          where
            conditionalUnion k v1 v2 =
                case isBijective v1 v2 of
                  Just v  -> (DL.singleton (k, v), Just v1)
                  Nothing -> pure . Just $ v1 <> v2


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


-- |
-- \( \mathcal{O} \left( n * \log_2 n \right) \)
--
-- Assumed to be well constructed. No validation is performed.
unsafeFromList :: (Foldable f, Ord a) => f (a, a) -> MutualExculsionSet a
unsafeFromList xs = MES incMap' excMap' (S.singleton <$> incMap) (S.singleton <$> excMap) both
  where
    incMap  = M.fromList inc
    excMap  = M.fromList exc
    incMap' = M.withoutKeys incMap both
    excMap' = M.withoutKeys excMap both
    exc     = swap <$> inc
    inc     = toList xs
    both    = M.keysSet incMap `S.intersection` M.keysSet excMap
