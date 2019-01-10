-----------------------------------------------------------------------------
-- |
-- Module      :  Data.TopologyRepresentation
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
{-# LANGUAGE OverloadedStrings          #-}


module Data.TopologyRepresentation
  ( TopologyRepresentation
  -- * Construction
  , isolatedNetworkEdgeContext
  -- * Deconstruct
  , excludedNetworkEdges
  , includedNetworkEdges
  , mutuallyExclusivePairs
  -- * Comparison
  , isCompatableWithTopology
  , isEdgePermissibleWith
  ) where


import           Control.DeepSeq
import           Data.Foldable
import           Data.Functor.Classes
import           Data.Hashable
import           Data.MutualExclusionSet (MutualExclusionSet)
import qualified Data.MutualExclusionSet as MES
import           Data.Set                (Set)
import           GHC.Generics
import TextShow (TextShow(showb), unwordsB)

{-
isCompatableSubtopologyOf :: Ord a => TopologyRepresentation a -> TopologyRepresentation a -> Bool
isCompatableSubtopologyOf (TR x) (TR y) = isSubsetOf x y
-}


-- |
-- Represents a collection of network edges and their mutually-exclusive edges.
--
-- Often used to represent a unique spanning tree in a phylogenetic DAG.
newtype TopologyRepresentation a = TR { unwrap :: MutualExclusionSet a }
    deriving (Eq, Eq1, Hashable, Generic, Monoid, NFData, Ord, Ord1, Semigroup)

{-
instance Foldable TopologyRepresentation where

    fold = fold . toList

    foldMap f = foldMap f . toList

    toList = BM.keysR . unwrap


instance Hashable a => Hashable (TopologyRepresentation a) where

    hashWithSalt salt = foldl' hashWithSalt salt . mutuallyExclusivePairs


instance Ord a => Monoid (TopologyRepresentation a) where

    mappend = (<>)

    mempty  = TR BM.empty


instance NFData a => NFData (TopologyRepresentation a) where

    rnf (TR bm) = BM.fold f () bm `seq` ()
      where
        f x y t = t `seq` rnf x `seq` rnf y


-- |
-- /O(n + m)/
instance Ord a => Semigroup (TopologyRepresentation a) where

    (TR lhs) <> (TR rhs) = TR . BM.fromAscPairListUnchecked . M.toAscList $ M.unionWith const (BM.toMap lhs) (BM.toMap rhs)
-}

instance (Ord a, Show a) => Show (TopologyRepresentation a) where

    show x = unwords
        [ "Network Edges of Topology:"
        , showIncluded x
        , "|"
        , showExcluded x
        ]
      where
        showIncluded = show . toList . includedNetworkEdges
        showExcluded = show . toList . excludedNetworkEdges


instance (Ord a, TextShow a) => TextShow (TopologyRepresentation a) where

    showb x = unwordsB
        [ "Network Edges of Topology:"
        , showIncluded x
        , "|"
        , showExcluded x
        ]
      where
        showIncluded = showb . toList . includedNetworkEdges
        showExcluded = showb . toList . excludedNetworkEdges


-- |
-- \( \mathcal{O} \left( 1 \right) \)
--
-- Construct a singleton 'TopologyRepresentation' value by supplying two network edge identifiers: one
-- that represents an edge contained in the topology and a
-- corresponding one that represents the mutually-exclusive
-- incident edge.
--
-- Use the semigroup operator '(<>)' to merge isolated network edge contexts into
-- a larger 'TopologyRepresentation'.
{-# INLINE isolatedNetworkEdgeContext #-}
isolatedNetworkEdgeContext
  :: Eq a
  => a -- ^ Included network edge
  -> a -- ^ Excluded incident network edge
  -> TopologyRepresentation a
isolatedNetworkEdgeContext x y = TR $ MES.singleton x y


-- |
-- \( \mathcal{O} \left( n \right) \)
--
-- Retrieve the list of network edge identifiers present in the topology.
{-# INLINE includedNetworkEdges #-}
includedNetworkEdges :: TopologyRepresentation a -> Set a
includedNetworkEdges = MES.includedSet . unwrap


-- |
-- \( \mathcal{O} \left( n \right) \)
--
-- Retrieve the list of network edge identifiers excluded from the topology.
{-# INLINE excludedNetworkEdges #-}
excludedNetworkEdges :: TopologyRepresentation a -> Set a
excludedNetworkEdges = MES.excludedSet . unwrap


-- |
-- \( \mathcal{O} \left( n \right) \)
--
-- Retrieve the list of network edge identifiers stored in the topology
-- representation.
{-# INLINE mutuallyExclusivePairs #-}
mutuallyExclusivePairs :: TopologyRepresentation a -> Set (a,a)
mutuallyExclusivePairs = MES.mutuallyExclusivePairs . unwrap


-- |
-- \( \mathcal{O} \left( m * \log_2 ( \frac {n}{m + 1} ) \right), m \leq n \)
--
-- Perform a subsetting operation to determine is a sub-topology is compatible
-- with another topology.
{-# INLINE isCompatableWithTopology #-}
isCompatableWithTopology :: Ord a => TopologyRepresentation a -> TopologyRepresentation a -> Bool
isCompatableWithTopology ts = MES.isPermissible (unwrap ts) . unwrap


-- |
-- \( \mathcal{O} \left( \log_2 ( n ) \right) \)
--
-- Determine if an edge is compatible with a topology.
isEdgePermissibleWith :: Ord a => a -> TopologyRepresentation a -> Bool
isEdgePermissibleWith e = not . (e `MES.isExcluded`) . unwrap
