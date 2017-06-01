-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Character.Decoration.NonMetric
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Bio.Character.Decoration.NonMetric
  ( NonMetricDecorationInitial()
  , NonMetricCharacterDecoration()
  , GeneralCharacterMetadata()
  , DiscreteCharacterMetadata()
  , DiscreteCharacterDecoration()
  , HasCharacterAlphabet(..)
  , HasCharacterName(..)
  , HasCharacterWeight(..)
  , HasDiscreteCharacter(..)
  , HasSymbolChangeMatrix(..)
  , HasTransitionCostMatrix(..)
  ) where

import Bio.Character.Decoration.Discrete
import Bio.Character.Decoration.NonMetric.Class
import Bio.Character.Decoration.NonMetric.Internal

