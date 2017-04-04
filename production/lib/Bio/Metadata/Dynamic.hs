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
    , HasCharacterAlphabet(..)
    , HasCharacterName(..)
    , HasCharacterWeight(..)
    , HasDenseTransitionCostMatrix(..)
    , HasSymbolChangeMatrix(..)
    , HasTransitionCostMatrix(..)
    , HasTraversalLoci(..)
    , MemoizedCostMatrix()
    , dynamicMetadata
    , dynamicMetadataFromTCM
    , maybeConstructDenseTransitionCostMatrix
    ) where

import Bio.Metadata.Dynamic.Class
import Bio.Metadata.Dynamic.Internal
