-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Metadata.DiscreteWithTCM
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Bio.Metadata.Dynamic
    ( DenseTransitionCostMatrix
    , DynamicCharacterMetadata(..)
    , DynamicCharacterMetadataDec()
    , GetDenseTransitionCostMatrix(..)
    , GetSymbolChangeMatrix(..)
    , GetPairwiseTransitionCostMatrix(..)
    , GetThreewayTransitionCostMatrix(..)
    , HasCharacterAlphabet(..)
    , HasCharacterName(..)
    , HasCharacterWeight(..)
    , HasTraversalFoci(..)
    , MemoizedCostMatrix()
    , TraversalFoci
    , TraversalFocusEdge
    , TraversalTopology
    , dynamicMetadata
    , dynamicMetadataWithSCM
    , overlap
    , overlap2
    , overlap3
    ) where

import Bio.Metadata.Dynamic.Class    hiding (DenseTransitionCostMatrix)
import Bio.Metadata.Dynamic.Internal
