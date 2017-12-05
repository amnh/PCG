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


import           Control.Arrow
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
data  MutualExclusionSet a
    = MES
    { includedFullMap :: !(Map a (Set a))
    , excludedFullMap :: !(Map a (Set a))
    , nonBijective    :: !(Set a)
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

    (MES a b _) == (MES c d _) = a == c && b == d


-- | (✔)
instance Eq1 MutualExclusionSet where

    liftEq eq (MES a b _) (MES c d _) = and
        [ length a == length c
        , length b == length d
        , liftEq2 eq (liftEq eq) a c 
        , liftEq2 eq (liftEq eq) b d 
        ]


{-
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
    length x     = M.size (includedFullMap x) - S.size (nonBijective x)

    {-# INLINABLE maximum #-}
    maximum mes  =
        case S.lookupMax $ includedSet mes of
            Just k  -> k
            Nothing -> error "maximum called on empty MutualExclusionSet"

    {-# INLINABLE minimum #-}
    minimum mes  =
        case S.lookupMin $ includedSet mes of
            Just k  -> k
            Nothing -> error "minimum called on empty MutualExclusionSet"

    {-# INLINE null #-}
    null        = (0 ==) . length

    {-# INLINABLE product #-}
    product     = product . toList

    {-# INLINABLE sum #-}
    sum         = sum . toList

    {-# INLINE toList #-}
    toList      = toIncludedList
-}


-- | (✔)
instance Hashable a => Hashable (MutualExclusionSet a) where

    hashWithSalt salt (MES inc exc _) = foldl' hashWithSalt (foldl' hashWithSalt salt (f inc)) $ f exc
      where
        f = toList . fmap toList
  

-- | (✔)
instance Ord a => Monoid (MutualExclusionSet a) where

    mappend = (<>)

    mempty  = MES mempty mempty mempty
    

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

    liftCompare cmp (MES a b _) (MES c d _) =
        case liftCompare2 cmp (liftCompare cmp) a c of
          EQ -> liftCompare2 cmp (liftCompare cmp) b d
          v  -> v


-- | (✔)
instance Ord a => Semigroup (MutualExclusionSet a) where

    -- | Alias for 'merge'
    (<>) = merge

    {-# INLINE stimes #-}
    stimes _ x = x

    {-# INLINABLE sconcat #-}
    sconcat = mergeMany


-- | (✔)
instance (Ord a, Show a) => Show (MutualExclusionSet a) where

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
    { includedFullMap = M.singleton x (S.singleton y)
    , excludedFullMap = M.singleton y (S.singleton x)
    , nonBijective    = if x == y then S.singleton x else S.empty
    }


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
invert (MES x y b) = MES y x b


-- |
-- \( \mathcal{O} \left( n \right) \)
--
-- Retreive the list of included elements in the 'MutualExclusionSet'.
includedSet :: Ord a => MutualExclusionSet a -> Set a
includedSet x = allIncluded `S.difference` badElements
  where
    badElements = nonBijective x
    allIncluded = M.keysSet $ includedFullMap x


-- |
-- \( \mathcal{O} \left( n \right) \)
--
-- Retreive the list of excluded elements in the 'MutualExclusionSet'.
excludedSet :: Ord a => MutualExclusionSet a -> Set a
excludedSet x = allExcluded `S.difference` badElements
  where
    badElements = nonBijective x
    allExcluded = M.keysSet $ excludedFullMap x


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
includedLookup k = valueLookup k <$> nonBijective <*> includedFullMap

  
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
excludedLookup k = valueLookup k <$> nonBijective <*> excludedFullMap


-- |
-- \( \mathcal{O} \left( \log_2 n \right) \)
--
-- Query the 'MutualExclusionSet' to determine if the provided element is /included./
isIncluded :: Ord a => a -> MutualExclusionSet a -> Bool
isIncluded k = valueMember k <$> nonBijective <*> includedFullMap

  
-- |
-- \( \mathcal{O} \left( \log_2 n \right) \)
--
-- Query the 'MutualExclusionSet' to determine if the provided element is /excluded./
isExcluded :: Ord a => a -> MutualExclusionSet a -> Bool
isExcluded k = valueMember k <$> nonBijective <*> excludedFullMap


-- |
-- \( \mathcal{O} \left( n \right) \)
--
-- Retreive the list of mutually exclusive elements stored in the
-- 'MutualExclusionSet'.
--
-- The first element of the pair is the included element and the second element
-- of pair is the excluded element.
mutuallyExclusivePairs :: Ord a => MutualExclusionSet a -> Set (a, a)
mutuallyExclusivePairs x = S.fromDistinctAscList $ second S.findMax <$> M.toAscList goodMap
  where
    goodMap      = M.withoutKeys allIncluded badElements
    isSingleton  = (1 ==) . S.size
    badElements  = nonBijective x
    allIncluded  = includedFullMap x


-- |
-- \( \mathcal{O} \left( m * \log_2 ( \frac {n}{m + 1} ) \right), m \leq n \)
--
-- Perform an operation to determine if a collection of elements is "permitted"
-- by 'MutualExclusionSet', ie that the collection does not contain any elements
-- which are excluded by the 'MutualExclusionSet'.
isPermissible :: Ord a => MutualExclusionSet a -> MutualExclusionSet a -> Bool
isPermissible (MES lhsInc lhsExc x) (MES rhsInc rhsExc y) =
    null (a `M.intersection` d) && null (b `M.intersection` c)
  where
    a = M.withoutKeys lhsInc x
    b = M.withoutKeys lhsExc x
    c = M.withoutKeys rhsInc y
    d = M.withoutKeys rhsExc y


-- |
-- \( \mathcal{O} \left( 1\right) \)
--
-- Determines if the MutualExclusionSet has a coherent construction.
--
-- * There must be a bijective mapping between included and excluded elements.
--
-- * No included element is also excluded
{-# INLINE isCoherent #-}
isCoherent :: MutualExclusionSet a -> Bool
isCoherent (MES _ _ x) = null x


-- |
-- \( \mathcal{O} \left( n * \log_2 n \right) \)
--
-- Assumed to be well constructed. No validation is performed.
unsafeFromList :: (Foldable f, Ord a) => f (a, a) -> MutualExclusionSet a
unsafeFromList xs = MES (S.singleton <$> incMap) (S.singleton <$> excMap) both
  where
    incMap  = M.fromList inc
    excMap  = M.fromList exc
    exc     = swap <$> inc
    inc     = toList xs
    both    = M.keysSet incMap `S.intersection` M.keysSet excMap


-- |
-- Nicely render the 'Data.MutualExclusionSet' in a multi-line 'String'.
--
-- Shows the internal state inluding bijectivity and mutual exclusivity
-- violations.
prettyPrintMutualExclusionSet :: (Ord a, Show a) => MutualExclusionSet a -> String
prettyPrintMutualExclusionSet = undefined
{-
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
                    $ mconcat [ tooManyExcluded, inBoth, tooManyIncluded ]

    tooManyExcluded = foldMapWithKey renderTooManyExcluded
                    . M.withoutKeys (includedFullMap mes)
                    $ M.keysSet (includedKeyedMap mes) <> bothSet
      where
        renderTooManyExcluded k v = [unwords [ " ", show k, "--->", show (toList v) ]]

    tooManyIncluded = foldMapWithKey renderTooManyIncluded
                    . M.withoutKeys (excludedFullMap mes)
                    $ M.keysSet (excludedKeyedMap mes) <> bothSet
      where
        renderTooManyIncluded k v = [unwords [ " ", show (toList v), "<---", show k ]]

    inBoth = rendingInBoth <$> toList bothSet
      where
        rendingInBoth x = unwords
            [ " "
            , show . toList $ excludedFullMap mes ! x
            , "<-->"
            , show . toList $ includedFullMap mes ! x
            ]

    bothSet = nonBijective mes

    indent = ("  " <>)
-}


{-# INLINE valueLookup #-}
valueLookup :: Ord a => a -> Set a -> Map a (Set a) -> Maybe a
valueLookup key notThese space
  | key `S.member` notThese = Nothing
  | otherwise               = key `M.lookup` space >>= S.lookupMax


{-# INLINE valueMember #-}
valueMember :: Ord a => a -> Set a -> Map a b -> Bool
valueMember key notThese space = key `S.notMember` notThese && key `M.member` space


toIncludedList :: Eq a => MutualExclusionSet a -> [a]
toIncludedList x = go allIncluded badElements
  where
    badElements = toList $ nonBijective x
    allIncluded = M.keys $ includedFullMap x
    go    []  [] = []
    go    []  ys = []
    go    xs  [] = xs
    go (x:xs) ys =
      case go' x ys of
        Nothing -> x : go xs ys
        Just zs ->     go xs zs
      where
        go' e    []  = Nothing
        go' e (z:zs)
          | e == z    = Just zs
          | otherwise = go' e zs


-- |
-- \( \mathcal{O} \left( m * \log_2 ( \frac {n}{m + 1} ) + n + m \right), m \leq n \)
--
-- Merge two mutual exclusion sets.
--
-- Perfoms an "union-like" operation.
merge :: Ord a => MutualExclusionSet a -> MutualExclusionSet a -> MutualExclusionSet a
merge (MES lhsIFM lhsEFM _) (MES rhsIFM rhsEFM _) = mergePostProcess (inc, exc)
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
        ([a], [b]) -> if a == b then Just a else Nothing
        _          -> Nothing
        
    preserveMissingValues = M.traverseMaybeMissing conditionalPreservation
      where
        conditionalPreservation k v =
          case toList v of
            [x] -> pure $ Just v
            _   -> (v, Just v)

    accumulateDifferentValues = M.zipWithMaybeAMatched conditionalUnion
      where
        conditionalUnion k v1 v2 =
            case isBijective v1 v2 of
              Just v  -> pure $ Just v1
              Nothing -> let vs = v1 <> v2
                         in  (vs, Just vs)


-- |
-- Record non-bijective elements in a set.
--
-- Construct the the mutual exclusion set from the context.
mergePostProcess :: Ord a => ( (Set a, Map a (Set a)), (Set a, Map a (Set a)) ) -> MutualExclusionSet a
mergePostProcess ((incNotBi, incFull), (excNotBi, excFull)) = MES incFull excFull notBi
  where
    both   = M.keysSet keyIntersection
    notBi  = both <> fold keyIntersection <> incNotBi <> excNotBi
    keyIntersection  = M.intersectionWith (<>) incFull excFull



