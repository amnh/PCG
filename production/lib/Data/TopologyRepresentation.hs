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
  , singleNetworkEdge
  , isCompatableSubtopologyOf
  ) where


import Control.DeepSeq
import Data.EdgeSet
import Data.Semigroup
import GHC.Generics    (Generic)


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
