-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Graph.EdgeSet
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

{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}

module Data.EdgeSet
  ( EdgeSet()
  , NetworkDisplayEdgeSet(..)
  , SetLike(..)
  , collapseToEdgeSet
  , fromEdgeSets
  , singletonEdgeSet
  ) where


import           Control.DeepSeq
import           Data.Foldable
import           Data.Key
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Semigroup
import           Data.Semigroup.Foldable
import           Data.Set           (Set)
import qualified Data.Set    as Set
import           GHC.Generics       (Generic)
import           Prelude     hiding (zipWith)


-- |
-- Represents a collection of edges.
--
-- Often used to represent a spanning tree in a DAG.
newtype EdgeSet e = ES (Set e)
  deriving (Eq, Foldable, Generic, Monoid, Ord, Semigroup)


-- |
-- Represents a multiple disconnected collections of edges.
--
-- Often used to represent a spanning forest in a multi-rooted DAG.
newtype NetworkDisplayEdgeSet e = NDES (NonEmpty (EdgeSet e))
  deriving (Generic, Show)


-- |
-- Set operations that can be perfomred on set-like structures.
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

    cardinality  (NDES x) = sum $ cardinality <$> x

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


instance Show a => Show (EdgeSet a) where

  show (ES xs) = show (toList xs)


-- |
-- Coalesce the disconnected edge sets of the 'NetworkDisplayEdgeSet' to a
-- single 'EdgeSet'.
collapseToEdgeSet :: Ord e => NetworkDisplayEdgeSet e -> EdgeSet e
collapseToEdgeSet (NDES x) = fold1 x


-- |
-- Construct a 'NetworkDisplayEdgeSet' from a non-empty collection of edge sets.
fromEdgeSets :: NonEmpty (EdgeSet e) -> NetworkDisplayEdgeSet e
fromEdgeSets = NDES


-- |
-- Construct a singleton 'EdgeSet' value. Use the semigroup operator '(<>)' to
-- construct larger a 'EdgeSet'. This enforces the non-empty invariant of the
-- 'EdgeSet' data-structure.
singletonEdgeSet :: e -> EdgeSet e
singletonEdgeSet = ES . Set.singleton
