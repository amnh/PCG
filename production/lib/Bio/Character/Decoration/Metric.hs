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
  ( MetricDecorationInitial()
  , MetricCharacterDecoration()
  , SankoffOptimizationDecoration()
  , SankoffDecoration()
  , DiscreteExtensionSankoffDecoration(..)
  , GeneralCharacterMetadata()
  , DiscreteCharacterMetadata()
  , DiscreteCharacterDecoration()
  , HasCharacterAlphabet(..)
  , HasCharacterName(..)
  , HasCharacterSymbolTransitionCostMatrixGenerator(..)
  , HasCharacterTransitionCostMatrix(..)
  , HasCharacterWeight(..)
  , HasDiscreteCharacter(..)
  , HasMinCostVector(..)
  , HasDirectionalMinVector(..)
  , HasMinCost(..)
  ) where

import Bio.Character.Decoration.Discrete
import Bio.Character.Decoration.Metric.Class
import Bio.Character.Decoration.Metric.Internal

