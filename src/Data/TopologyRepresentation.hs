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

{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}

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
  ) where


import           Control.DeepSeq
import           Data.Foldable
import           Data.Functor.Classes
import           Data.Hashable
import           Data.MutualExclusionSet        (MutualExclusionSet)
import qualified Data.MutualExclusionSet as MES
import           Data.Semigroup
import           Data.Set                       (Set)
import           GHC.Generics

{-
isCompatableSubtopologyOf :: Ord a => TopologyRepresentation a -> TopologyRepresentation a -> Bool
isCompatableSubtopologyOf (TR x) (TR y) = isSubsetOf x y
-}


-- |
-- Represents a collection of network edges and their mutually exclusive edges.
--
-- Often used to represent a unique spanning tree in a phylogenetic DAG.
newtype TopologyRepresentation a = TR { unwrap :: MutualExclusionSet a }
  deriving (Eq, Eq1, Foldable, Hashable, Generic, Monoid, NFData, Ord, Ord1, Semigroup)

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

instance Show a => Show (TopologyRepresentation a) where

    show x = unwords
        [ "Network Edges of Topology:"
        , showIncluded x
        , "|"
        , showExcluded x
        ]
      where
        showIncluded = show . toList . includedNetworkEdges
        showExcluded = show . toList . excludedNetworkEdges


-- |
-- \( \mathcal{O} \left( 1 \right) \)
--
-- Construct a singleton 'TopologyRepresentation' value by supplying a network
-- edge identifier representing an edge contained in the topology and a
-- corresponding network edge identifier representing the mutually exclusive
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
-- Retreive the list of network edge identifiers present in the topology.
{-# INLINE includedNetworkEdges #-}
includedNetworkEdges :: TopologyRepresentation a -> Set a
includedNetworkEdges = MES.includedSet . unwrap


-- |
-- \( \mathcal{O} \left( n \right) \)
--
-- Retreive the list of network edge identifiers excluded from the topology.
{-# INLINE excludedNetworkEdges #-}
excludedNetworkEdges :: TopologyRepresentation a -> Set a
excludedNetworkEdges = MES.excludedSet . unwrap


-- |
-- \( \mathcal{O} \left( n \right) \)
--
-- Retreive the list of network edge identifiers stored in the topology
-- representation.
{-# INLINE mutuallyExclusivePairs #-}
mutuallyExclusivePairs :: TopologyRepresentation a -> Set (a,a)
mutuallyExclusivePairs = MES.mutuallyExclusivePairs . unwrap


-- |
-- \( \mathcal{O} \left( m + n * \log_2 m \right) \)
--
-- Perform a subsetting operation to determine is a sub-topology is compatable
-- with another topology.
{-# INLINE isCompatableWithTopology #-}
isCompatableWithTopology :: (Foldable f, Ord a) => f a -> TopologyRepresentation a -> Bool
isCompatableWithTopology ts = MES.isPermissible ts . unwrap
