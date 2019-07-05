-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.PhyloGraphPrime.Node
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Bio.Graph.Node
  ( EdgeSet
  , NewickSerialization()
  , PhylogeneticFreeNode (..)
  , PhylogeneticNode(..)
  , ResolutionCache
  , ResolutionInformation(..)
  , ResolutionMetadata(..)
  , HasNodeDecorationDatum(..)
  , HasSequenceDecoration(..)
  , HasResolutions(..)
  , HasTotalSubtreeCost(..)
  , HasLocalSequenceCost(..)
  , HasLeafSetRepresentation(..)
  , HasSubtreeRepresentation(..)
  , HasSubtreeEdgeSet(..)
  , HasTopologyRepresentation(..)
  , HasCharacterSequence(..)
  , addEdgeToEdgeSet
  , addNetworkEdgeToTopology
  , singletonEdgeSet
  , singletonNewickSerialization
  , singletonSubtreeLeafSet
  , pNode2
  ) where


-- import Bio.Graph.Node.Class
import Bio.Graph.Node.Internal
