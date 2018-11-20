-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Character.Decoration.Metric
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Bio.Character.Decoration.Metric
  ( DiscreteExtensionSankoffDecoration(..)
  , DiscreteCharacterMetadata()
  , DiscreteCharacterDecoration()
  , GeneralCharacterMetadata()
  , MetricDecorationInitial()
  , MetricCharacterDecoration()
  , SankoffDecoration()
  , SankoffOptimizationDecoration()
  , StateContributionList
  -- * Lenses
  , GetSymbolChangeMatrix(..)
  , GetPairwiseTransitionCostMatrix(..)
  , HasBeta(..)
  , HasCharacterAlphabet(..)
  , HasCharacterCost(..)
  , HasCharacterCostVector(..)
  , HasCharacterName(..)
  , HasCharacterWeight(..)
  , HasDiscreteCharacter(..)
  , HasFinalExtraCost(..)
  , HasIsLeaf(..)
  , HasPreliminaryExtraCost(..)
  , HasStateMinTuple(..)
  ) where

import Bio.Character.Decoration.Discrete
import Bio.Character.Decoration.Metric.Class
import Bio.Character.Decoration.Metric.Internal
import Bio.Character.Decoration.Shared

