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
  , PhylogeneticNode (..)
  , PhylogeneticNode2(..)
  , ResolutionCache
  , ResolutionInformation(..)
  , HasNodeDecorationDatum(..)
  , HasResolutions(..)
  , HasTotalSubtreeCost(..)
  , HasLocalSequenceCost(..)
  , HasLeafSetRepresentation(..)
  , HasSubtreeRepresentation(..)
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
