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
  ( DiscreteCharacterMetadata()
  , DiscreteCharacterDecoration()
  , GeneralCharacterMetadata()
  , NonMetricDecorationInitial()
  , NonMetricCharacterDecoration()
  -- * Lenses
  , GetSymbolChangeMatrix(..)
  , GetPairwiseTransitionCostMatrix(..)
  , HasCharacterAlphabet(..)
  , HasCharacterName(..)
  , HasCharacterWeight(..)
  , HasDiscreteCharacter(..)
  ) where

import Bio.Character.Decoration.Discrete
import Bio.Character.Decoration.NonMetric.Class
import Bio.Character.Decoration.NonMetric.Internal

