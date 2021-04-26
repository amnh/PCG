-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Graph.ReferenceDAG
-- Copyright   :  (c) 2015-2021 Ward Wheeler
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
  , IndexData(..)
  , GraphData(..)
  , NodeRef()
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
  , trivialRefDAG
  -- * Remove metadata information and replace with a default
  , defaultGraphMetadata
  , defaultMetadata
  -- * Edgeset query
  , candidateNetworkEdges
  , referenceEdgeSet
  , referenceNetworkEdgeSet
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
