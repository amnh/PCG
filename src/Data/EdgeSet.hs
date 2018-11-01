-----------------------------------------------------------------------------
-- |
-- Module      :  Data.EdgeSet
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

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.EdgeSet
  ( EdgeSet()
--  , NetworkDisplayEdgeSet(..)
--  , SetLike(difference, union, intersection)
--  , collapseToEdgeSet
--  , fromEdgeSets
  , disjoint
  , makeParentEdgeSet
  , member
  , singletonEdgeSet
  , toIntSet
  ) where


import           Control.DeepSeq
import           Data.Foldable
import           Data.Foldable.Custom (sum')
import           Data.IntSet          (IntSet, singleton)
import           Data.Key
import           Data.List.NonEmpty   (NonEmpty (..))
import           Data.MonoTraversable (MonoFoldable (..))
import           Data.Semigroup
import           Data.Set             (Set)
import qualified Data.Set             as Set
import           GHC.Generics         (Generic)
import           Prelude              hiding (zipWith)


-- |
-- Represents a collection of edges.
--
-- Often used to represent a spanning tree in a DAG.
newtype EdgeSet e = ES (Set e)
  deriving (Eq, Foldable, Generic, Monoid, Ord, Semigroup)

{--
-- |
-- Represents multiple disconnected collections of edges.
--
-- Often used to represent a spanning forest in a multi-rooted DAG.
newtype NetworkDisplayEdgeSet e = NDES (NonEmpty (EdgeSet e))
  deriving (Generic, Show)


-- |
-- Set operations that can be performed on set-like structures.
class SetLike s where

    cardinality  :: s -> Word

    difference   :: s -> s -> s

    intersection :: s -> s -> s

    isSubsetOf   :: s -> s -> Bool

    union        :: s -> s -> s


instance NFData e => NFData (EdgeSet e) where

    rnf (ES set) = rnf set


instance NFData e => NFData (NetworkDisplayEdgeSet e) where

    rnf (NDES sets) = rnf sets


instance Ord a => Semigroup (NetworkDisplayEdgeSet a) where

    (NDES x) <> (NDES y) = NDES $ zipWith (<>) x y


instance Ord a => SetLike (EdgeSet a) where


    cardinality  (ES x) = cardinality x

    difference   (ES x) (ES y) = ES $ difference x y

    intersection (ES x) (ES y) = ES $ intersection x y

    isSubsetOf   (ES x) (ES y) = isSubsetOf x y

    union        (ES x) (ES y) = ES $ union x y


instance Ord a => SetLike (NetworkDisplayEdgeSet a) where

    cardinality  (NDES x) = sum' $ cardinality <$> x

    difference   (NDES x) (NDES y) = NDES $ zipWith difference x y

    intersection (NDES x) (NDES y) = NDES $ zipWith intersection x y

    isSubsetOf   (NDES x) (NDES y) = and  $ zipWith isSubsetOf x y

    union        (NDES x) (NDES y) = NDES $ zipWith union x y


instance Ord a => SetLike (Set a) where

    cardinality  = toEnum . length

    difference   = Set.difference

    intersection = Set.intersection

    isSubsetOf   = Set.isSubsetOf

    union        = Set.union

--}

instance NFData e => NFData (EdgeSet e) where

    rnf (ES set) = rnf set


instance Show a => Show (EdgeSet a) where

  show (ES xs) = show (toList xs)

{-
-- |
-- Coalesce the disconnected edge sets of the 'NetworkDisplayEdgeSet' to a
-- single 'EdgeSet'.
collapseToEdgeSet :: Ord e => NetworkDisplayEdgeSet e -> EdgeSet e
collapseToEdgeSet (NDES x) = fold1 x


-- |
-- Construct a 'NetworkDisplayEdgeSet' from a non-empty collection of edge sets.
fromEdgeSets :: NonEmpty (EdgeSet e) -> NetworkDisplayEdgeSet e
fromEdgeSets = NDES
-}

-- |
-- Get 'IntSet' from all nodes in an 'EdgeSet'
toIntSet :: EdgeSet (Int, Int) -> IntSet
toIntSet = foldMap edgeToIntSet
  where
    edgeToIntSet :: (Int, Int) -> IntSet
    edgeToIntSet (ind1, ind2) = singleton ind1 <> singleton ind2

-- |
-- Determine if a term is a member of an 'EdgeSet'
member :: Ord e => e -> EdgeSet e -> Bool
member e (ES edgeSet) = e `Set.member` edgeSet


-- |
-- Check if two edge sets have any intersection.
disjoint :: Ord e => EdgeSet e -> EdgeSet e -> Bool
disjoint (ES edgeSet1) (ES edgeSet2) = edgeSet1 `Set.disjoint` edgeSet2


-- |
-- Construct a singleton 'EdgeSet' value. Use the semigroup operator '(<>)' to
-- construct a larger 'EdgeSet'. This enforces the non-empty invariant of the
-- 'EdgeSet' data structure.
singletonEdgeSet :: e -> EdgeSet e
singletonEdgeSet = ES . Set.singleton


-- |
-- Take node index and the parent indices and add the edge from parent to node.
-- This is only intended to be used on non-root nodes.
makeParentEdgeSet
  :: Int                -- ^ Current node index
  -> IntSet             -- ^ Parent indices
  -> EdgeSet (Int, Int)
makeParentEdgeSet currInd = ofoldMap (\parInd -> singletonEdgeSet (parInd, currInd))
