-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Character.Decoration.MetricOptimization
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Secondary Sankoff model for speeding up search using techniques from
-- Goloboff 1998, "Tree Search ... Sankoff". Based on Metric, but additional
-- fields added to deal with equations presented in Goloboff paper.
--
-----------------------------------------------------------------------------

module Bio.Character.Decoration.MetricOptimization
  ( MetricOptimizationDecorationInitial()
  , MetricOptimizationCharacterDecoration()
  , SankoffOptimizationDecoration()
  , SankoffDecoration()
  , StateContributionList
  , DiscreteExtensionSankoffDecoration(..)
  , GeneralCharacterMetadata()
  , DiscreteCharacterMetadata()
  , DiscreteCharacterDecoration()
  , HasCharacterAlphabet(..)
  , HasCharacterCost(..)
  , HasCharacterCostVector(..)
  , HasCharacterName(..)
  , HasCharacterWeight(..)
  , HasStateMinTuple(..)
  , HasDiscreteCharacter(..)
  , HasSymbolChangeMatrix(..)
  , HasTransitionCostMatrix(..)
  , HasIsLeaf(..)
  ) where

import Bio.Character.Decoration.Discrete
import Bio.Character.Decoration.MetricOptimization.Class
import Bio.Character.Decoration.MetricOptimization.Internal
import Bio.Character.Decoration.Shared

