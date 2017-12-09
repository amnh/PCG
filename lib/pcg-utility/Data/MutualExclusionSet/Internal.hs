-----------------------------------------------------------------------------
-- |
-- Module      :  Data.MutualExclusionSet.Internal
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Internal functions for constucting, amnipulating, and deconstructing a
-- 'MutualExclusionSet'.
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric, FlexibleContexts #-}

module Data.MutualExclusionSet.Internal where


import           Control.DeepSeq
import           Data.Foldable
import           Data.Functor.Classes
import           Data.Hashable
import           Data.Key
import           Data.List.NonEmpty       (NonEmpty(..))
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
import           Prelude           hiding (zip)
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
data  MutualExclusionSet a
    = MES
    { includedElemMap :: !(Map a (Set a))
    , excludedElemMap :: !(Map a (Set a))
    , includedFullMap :: !(Map a (Set a))
    , excludedFullMap :: !(Map a (Set a))
    } deriving (Generic)


-- | (✔)
instance (Arbitrary a, Ord a) => Arbitrary (MutualExclusionSet a) where

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


-- | (✔)
instance Eq a => Eq (MutualExclusionSet a) where

    (MES _ _ a b) == (MES _ _ c d) = a == c && b == d


-- | (✔)
instance Eq1 MutualExclusionSet where

    liftEq eq (MES _ _ a b) (MES _ _ c d) = and
        [ length a == length c
        , length b == length d
        , liftEq2 eq (liftEq eq) a c 
        , liftEq2 eq (liftEq eq) b d 
        ]


-- |
-- Fold over the /included/ elements of the mutual exclusion set.
--
-- To fold over the /excluded/ elements of the set, first call 'invert'.
instance Foldable MutualExclusionSet where

    -- We don't use isIncluded here because that would force an Ord constraint.
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
    length      = M.size . includedElemMap

    {-# INLINABLE maximum #-}
    maximum mes  =
        case M.lookupMax $ includedElemMap mes of
          Just (k,_) -> k
          Nothing    -> error "maximum called on empty MutualExclusionSet"

    {-# INLINABLE minimum #-}
    minimum mes  =
        case M.lookupMin $ includedElemMap mes of
          Just (k,_) -> k
          Nothing    -> error "minimum called on empty MutualExclusionSet"

    {-# INLINE null #-}
    null        = null . includedElemMap

    {-# INLINABLE product #-}
    product     = product . toList

    {-# INLINABLE sum #-}
    sum         = sum . toList

    {-# INLINE toList #-}
    toList      = M.keys . includedElemMap


-- | (✔)
instance Hashable a => Hashable (MutualExclusionSet a) where

    hashWithSalt salt (MES _ _ inc exc) = foldl' hashWithSalt (foldl' hashWithSalt salt (f inc)) $ f exc
      where
        f = toList . fmap toList
  

-- | (✔)
instance Ord a => Monoid (MutualExclusionSet a) where

    mappend = (<>)

    {-# INLINABLE mconcat #-}
    mconcat v =
      case v of
        []   -> mempty
        x:xs -> sconcat $ x:|xs

    mempty  = MES mempty mempty mempty mempty


-- | (✔)
instance NFData a => NFData (MutualExclusionSet a)


-- | (✔)
instance Ord a => Ord (MutualExclusionSet a) where

    x `compare` y =
        case comparing includedFullMap x y of
          EQ -> comparing excludedFullMap x y
          v  -> v


-- | (✔)
instance Ord1 MutualExclusionSet where

    liftCompare cmp (MES _ _ a b) (MES _ _ c d) =
        case liftCompare2 cmp (liftCompare cmp) a c of
          EQ -> liftCompare2 cmp (liftCompare cmp) b d
          v  -> v


-- | (✔)
instance Ord a => Semigroup (MutualExclusionSet a) where

    -- | Alias for 'merge'
    (<>) = merge

    {-# INLINE stimes #-}
    stimes = stimesIdempotentMonoid

    {-# INLINABLE sconcat #-}
    sconcat = mergeMany


-- | (✔)
instance Show a => Show (MutualExclusionSet a) where

    show = ("MutualExclusionSet " <>) . show . S.toAscList . mutuallyExclusivePairs


-- |
-- \( \mathcal{O} \left( 1 \right) \)
--
-- Construct a singleton 'MutualExclusionSet' value by supplying an included
-- element and the corresponding, mutually exclusive element.
--
-- Use the semigroup operator '(<>)' to merge singleton contexts into
-- a larger 'MutualExclusionSet'.
singleton
  :: Eq a
  => a -- ^ Included element
  -> a -- ^ Excluded element
  -> MutualExclusionSet a
singleton x y =
    MES
    { includedElemMap = inc
    , excludedElemMap = exc
    , includedFullMap = a 
    , excludedFullMap = b
    }
  where
    a = M.singleton x (S.singleton y)
    b = M.singleton y (S.singleton x)
    (inc, exc)
      | x == y    = (M.empty, M.empty)
      | otherwise = (a, b)


-- |
-- \( \mathcal{O} \left( 1 \right) \)
--
-- inverts the included and excluded elements of the 'MutualExclusionSet'. The
-- previously included elements become the excluded elements and the previously
-- excluded elements become included elements.
--
-- This inversion function preserves the bijective relationship of the elements
-- within 'MutualExclusionSet' so that the following will always hold:
--
-- > invert . invert === id
invert :: MutualExclusionSet a -> MutualExclusionSet a 
invert (MES a b x y) = MES b a y x


-- |
-- \( \mathcal{O} \left( n \right) \)
--
-- Retreive the list of included elements in the 'MutualExclusionSet'.
includedSet :: MutualExclusionSet a -> Set a
includedSet = M.keysSet . includedElemMap


-- |
-- \( \mathcal{O} \left( n \right) \)
--
-- Retreive the list of excluded elements in the 'MutualExclusionSet'.
excludedSet :: MutualExclusionSet a -> Set a
excludedSet = M.keysSet . excludedElemMap


-- |
-- \( \mathcal{O} \left( \log_2 n \right) \)
--
-- Lookup an /included/ key in the 'MutualExclusionSet'.
--
--  * If the provided element *is not* included, the result will be @Nothing@.
--
--  * If the provided element *is* included, the result will be @Just value@,
--    where @value@ is corresponding excluded element.
includedLookup :: Ord a => a -> MutualExclusionSet a -> Maybe a
includedLookup k = valueLookup k . includedElemMap

  
-- |
-- \( \mathcal{O} \left( \log_2 n \right) \)
--
-- Lookup an /excluded/ key in the 'MutualExclusionSet'.
--
--  * If the provided element *is not* excluded, the result will be @Nothing@.
--
--  * If the provided element *is* excluded, the result will be @Just value@,
--    where @value@ is corresponding included element.
excludedLookup :: Ord a => a -> MutualExclusionSet a -> Maybe a
excludedLookup k = valueLookup k . excludedElemMap


-- |
-- \( \mathcal{O} \left( \log_2 n \right) \)
--
-- Query the 'MutualExclusionSet' to determine if the provided element is /included./
isIncluded :: Ord a => a -> MutualExclusionSet a -> Bool
isIncluded k = M.member k . includedElemMap

  
-- |
-- \( \mathcal{O} \left( \log_2 n \right) \)
--
-- Query the 'MutualExclusionSet' to determine if the provided element is /excluded./
isExcluded :: Ord a => a -> MutualExclusionSet a -> Bool
isExcluded k = M.member k . excludedElemMap


-- |
-- \( \mathcal{O} \left( n \right) \)
--
-- Retreive the list of mutually exclusive elements stored in the
-- 'MutualExclusionSet'.
--
-- The first element of the pair is the included element and the second element
-- of pair is the excluded element.
mutuallyExclusivePairs :: MutualExclusionSet a -> Set (a, a)
mutuallyExclusivePairs = S.fromDistinctAscList . M.toAscList . fmap S.findMax . includedElemMap


-- |
-- \( \mathcal{O} \left( m * \log_2 ( \frac {n}{m + 1} ) \right), m \leq n \)
--
-- Perform an operation to determine if a collection of elements is "permitted"
-- by 'MutualExclusionSet', ie that the collection does not contain any elements
-- which are excluded by the 'MutualExclusionSet'.
isPermissible :: Ord a => MutualExclusionSet a -> MutualExclusionSet a -> Bool
isPermissible lhs rhs = null (includedElemMap lhs `M.intersection` excludedElemMap rhs)
                     && null (includedElemMap rhs `M.intersection` excludedElemMap lhs)


-- |
-- \( \mathcal{O} \left( 1 \right) \)
--
-- Determines if the MutualExclusionSet has a coherent construction.
--
-- * There must be a bijective mapping between included and excluded elements.
--
-- * No included element is also excluded
{-# INLINE isCoherent #-}
isCoherent :: MutualExclusionSet a -> Bool
isCoherent (MES a _ b _) = M.size a == M.size b


-- |
-- \( \mathcal{O} \left( n * \log_2 n \right) \)
--
-- Assumed to be well constructed. No validation is performed.
unsafeFromList :: (Foldable f, Ord a) => f (a, a) -> MutualExclusionSet a
unsafeFromList xs = MES incMap' excMap' incMap excMap
  where
    incMap' = M.withoutKeys incMap both
    excMap' = M.withoutKeys excMap both

    incMap  = S.singleton <$> M.fromList inc
    excMap  = S.singleton <$> M.fromList exc

    exc     = swap <$> inc
    inc     = toList xs

    both    = M.keysSet incMap `S.intersection` M.keysSet excMap


-- |
-- Nicely render the 'Data.MutualExclusionSet' in a multi-line 'String'.
--
-- Shows the internal state inluding bijectivity and mutual exclusivity
-- violations.
prettyPrintMutualExclusionSet :: (Ord a, Show a) => MutualExclusionSet a -> String
prettyPrintMutualExclusionSet mes = mconcat
    [ "MutualExclusionSet\n"
    , bijectiveValues
    , violationValues
    ]
  where
    bijectiveValues = unlines . fmap indent . ("Bijective map":)
                    $ bijectiveRender <$> toList (mutuallyExclusivePairs mes)
      where
        bijectiveRender (k,v) = unwords [ " ", show k, "<-->", show v ]

    violationValues = unlines . fmap indent . ("Violations":)
                    $ mconcat [ tooManyExcluded, {- inBoth, -} tooManyIncluded ]

    tooManyExcluded = foldMapWithKey renderTooManyExcluded
                    . M.withoutKeys (includedFullMap mes)
                    $ M.keysSet (includedElemMap mes) -- <> bothSet
      where
        renderTooManyExcluded k v = [unwords [ " ", show k, "--->", show (toList v) ]]

    tooManyIncluded = foldMapWithKey renderTooManyIncluded
                    . M.withoutKeys (excludedFullMap mes)
                    $ M.keysSet (excludedElemMap mes) -- <> bothSet
      where
        renderTooManyIncluded k v = [unwords [ " ", show (toList v), "<---", show k ]]

{-
    inBoth = rendingInBoth <$> toList bothSet
      where
        rendingInBoth x = unwords
            [ " "
            , show . toList $ excludedFullMap mes ! x
            , "<-->"
            , show . toList $ includedFullMap mes ! x
            ]

    bothSet = nonBijective mes
-}
    indent = ("  " <>)


-- |
-- This should only be called on singleton sets.
{-# INLINE valueLookup #-}
valueLookup :: Ord a => a -> Map a (Set a) -> Maybe a
valueLookup key space = key `M.lookup` space >>= S.lookupMax


-- |
-- \( \mathcal{O} \left( m * \log_2 ( \frac {n}{m + 1} ) + n + m \right), m \leq n \)
--
-- Merge two mutual exclusion sets.
--
-- Perfoms an "union-like" operation.
merge :: Ord a => MutualExclusionSet a -> MutualExclusionSet a -> MutualExclusionSet a
merge (MES _ _ lhsIFM lhsEFM) (MES _ _ rhsIFM rhsEFM) = mergePostProcess (inc, exc)
  where
    -- /O( m + n )/
    inc = mergeLogic lhsIFM rhsIFM
    -- /O( m + n )/
    exc = mergeLogic lhsEFM rhsEFM


-- |
-- Efficiently merge multiple mutual exclusion sets.
mergeMany :: (Foldable f, Ord a) => f (MutualExclusionSet a) -> MutualExclusionSet a
mergeMany = mergePostProcess . foldr f mempty
  where
    f :: Ord a
      => MutualExclusionSet a
      -> ( (Set a, Map a (Set a)), (Set a, Map a (Set a)) )
      -> ( (Set a, Map a (Set a)), (Set a, Map a (Set a)) )
    f mes (lhs, rhs) = (lhs >>= mergeLogic inc, rhs >>= mergeLogic exc)
      where
        inc = includedFullMap mes 
        exc = excludedFullMap mes 


-- |
-- When a key *is not* present in both maps,
-- preserve the key and it's corresponding value.
--
-- When a key *is* present in both maps but the values are not equal,
-- discard the key and both of the values. Otherwise preserve the key
-- and the shared value.
mergeLogic :: Ord a => Map a (Set a) -> Map a (Set a) -> (Set a, Map a (Set a))
mergeLogic = M.mergeA preserveMissingValues preserveMissingValues accumulateDifferentValues
  where
    isBijective x y =
      case (toList x, toList y) of
        ([a], [b]) -> a == b
        _          -> False
        
    preserveMissingValues = M.traverseMaybeMissing conditionalPreservation
      where
        conditionalPreservation _ v =
          case toList v of
            [_] -> pure $ Just v
            _   -> (v, Just v)

    accumulateDifferentValues = M.zipWithMaybeAMatched conditionalUnion
      where
        conditionalUnion _ v1 v2
          | isBijective v1 v2 = pure $ Just v1
          | otherwise         = let vs = v1 <> v2
                                in  (vs, Just vs)


-- |
-- Record non-bijective elements in a set.
--
-- Construct the the mutual exclusion set from the context.
mergePostProcess :: Ord a => ( (Set a, Map a (Set a)), (Set a, Map a (Set a)) ) -> MutualExclusionSet a
mergePostProcess ((incNotBi, incFull), (excNotBi, excFull)) = MES incElem excElem incFull excFull
  where
    incElem = M.withoutKeys incFull notBijective
    excElem = M.withoutKeys excFull notBijective
    
    includedAndExcluded = M.keysSet keyIntersection
    notBijective        = includedAndExcluded <> fold keyIntersection <> incNotBi <> excNotBi
    keyIntersection     = M.intersectionWith (<>) incFull excFull
