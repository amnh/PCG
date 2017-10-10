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
import           Data.Bimap             (Bimap)
import qualified Data.Bimap      as BM
--import           Data.EdgeSet
import           Data.Foldable
import           Data.Hashable
import qualified Data.Map        as M
import           Data.Monoid     hiding ((<>))
import           Data.Semigroup
import           Data.Set               (Set)
import qualified Data.Set        as S   (fromDistinctAscList)
import           GHC.Generics           (Generic)


{-
-- |
-- Represents a collection of network edges.
--
-- Often used to represent a unique spanning tree in a phylogenetic DAG.
newtype TopologyRepresentation a = TR (EdgeSet a)
  deriving (Eq, Foldable, Generic, Monoid, Ord, Semigroup)


instance NFData a => NFData (TopologyRepresentation a) where

    rnf (TR x) = rnf x


instance Show a => Show (TopologyRepresentation a) where

    show (TR es) = "Network Edges of Topology: " <> show es


-- |
-- /O(1)/
--
-- Construct a singleton 'TopologyRepresentation' value. Use the semigroup operator '(<>)' to
-- construct larger a 'TopologyRepresentation'.
singleNetworkEdge :: a -> TopologyRepresentation a
singleNetworkEdge = TR . singletonEdgeSet


-- |
-- /O(n + m)/
--
-- Perform a subsetting operation to determine is a sub-topology is compatable
-- with another topology. 
isCompatableSubtopologyOf :: Ord a => TopologyRepresentation a -> TopologyRepresentation a -> Bool
isCompatableSubtopologyOf (TR x) (TR y) = isSubsetOf x y
-}


-- |
-- Represents a collection of network edges and their mutually exclusive edges.
--
-- Often used to represent a unique spanning tree in a phylogenetic DAG.
newtype TopologyRepresentation a = TR { unwrap :: Bimap a a }
  deriving (Eq, Generic, Ord)


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
-- /O(1)/
--
-- Construct a singleton 'TopologyRepresentation' value by supplying a network
-- edge identifier representing an edge contained in the topology and a
-- corresponding network edge identifier representing the mutually exclusive
-- incident edge.
--
-- Use the semigroup operator '(<>)' to merge isolated network edge contexts into
-- a larger 'TopologyRepresentation'.
isolatedNetworkEdgeContext
  :: a -- ^ Included network edge
  -> a -- ^ Excluded incident network edge
  -> TopologyRepresentation a
isolatedNetworkEdgeContext x y = TR $ BM.singleton y x


-- |
-- /O(n)/
--
-- Retreive the list of network edge identifiers present in the topology.
includedNetworkEdges :: TopologyRepresentation a -> Set a
includedNetworkEdges = S.fromDistinctAscList . BM.keysR . unwrap


-- |
-- /O(n)/
--
-- Retreive the list of network edge identifiers excluded from the topology.
excludedNetworkEdges :: TopologyRepresentation a -> Set a
excludedNetworkEdges = M.keysSet . BM.toMap . unwrap


-- |
-- /O(n)/
--
-- Retreive the list of network edge identifiers stored in the topology
-- representation.
mutuallyExclusivePairs :: TopologyRepresentation a -> [(a,a)]
mutuallyExclusivePairs = BM.assocs . BM.twist . unwrap


-- |
-- /O(n + m)/
--
-- Perform a subsetting operation to determine is a sub-topology is compatable
-- with another topology.
isCompatableWithTopology :: (Foldable f, Ord a) => f a -> TopologyRepresentation a -> Bool
isCompatableWithTopology ts topo = getAll $ foldMap f ts
  where
    f x = All $ x `notElem` badElems
    badElems = excludedNetworkEdges topo
