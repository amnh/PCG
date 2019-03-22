-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Graph.ReferenceDAG
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Bio.Graph.ReferenceDAG
  ( -- * Efficient access
    ReferenceDAG(graphData)
  , GraphData(..)
  , NodeRef()
  , nodeFoldMap
  , nodePostOrder
  , nodePreOrder
  -- * Lenses
  , HasNodeDecoration(..)
  , HasParentRefs(..)
  , HasChildRefs(..)
  , HasGraphData(..)
  , HasReferenceVector(..)
  , HasRootReferences(..)
  , HasDagCost(..)
  , HasNetworkEdgeCost(..)
  , HasRootingCost(..)
  , HasTotalBlockCost(..)
  , HasGraphMetadata(..)
  -- * Effciently construct a DAG
  , fromList
  , unfoldDAG
  -- * Remove metadata information and replace with a default
  , defaultGraphMetadata
  , defaultMetadata
  -- * Edgeset query
  , candidateNetworkEdges
  , referenceEdgeSet
  , referenceTreeEdgeSet
  , referenceNetworkEdgeSet
  , undirectedRootEdgeSet
  -- * Edgeset manipulation
  , connectEdge
  , invadeEdge
  -- * Rendering
  , referenceRendering
  , topologyRendering
  , getDotContext
  ) where

import Bio.Graph.ReferenceDAG.Internal
import Bio.Graph.ReferenceDAG.Network
