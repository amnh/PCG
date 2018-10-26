module Data.SymmetricPair.Internal
  ( SymmPair (..)
  , symmPair
  , generateOrderedPairs
  , orderedPairsFromUnorderedList
  ) where

import Data.Set

newtype SymmPair a = SymmPair {getSymmPair :: (a, a)}

-- |
-- A smart constructor for a symmetric pair.
symmPair :: Ord a => (a, a) -> SymmPair a
symmPair = SymmPair

-- |
-- From a symmetric pair generate a set of ordered pairs.
--
-- Note: the set constructed is of size one if the pair of elements is equal
-- and two otherwise.
generateOrderedPairs :: Ord a => SymmPair a -> Set ((a, a))
generateOrderedPairs (SymmPair (a1, a2)) = (singleton (a1, a2)) <> (singleton (a2, a1))
-- SymmPair (a,a) -> Set ((a,a), (a,a))

-- |
-- Take a list of unordered pairs and generate all pairs with both orders.
-- To be used to take a collection of undirected edges and generate add
-- all directions.
orderedPairsFromUnorderedList :: (Ord a) => [(a, a)] -> Set (a, a)
orderedPairsFromUnorderedList = foldMap (generateOrderedPairs . symmPair)
