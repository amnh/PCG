-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Character.Decoration.Fitch
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Bio.Character.Decoration.Fitch
  ( DynamicDecorationInitial(..)
  , DynamicDecorationDirectOptimization(..)
  , DynamicDecorationImpliedAlignment(..)
  , GeneralCharacterMetadata()
  , HasCharacterAlphabet(..)
  , HasCharacterName(..)
  , HasCharacterSymbolTransitionCostMatrixGenerator(..)
  , HasCharacterTransitionCostMatrix(..)
  , HasCharacterWeight(..)
  , HasEncoded(..)
  , HasFinalGapped(..)
  , HasFinalUngapped(..)
  , HasPreliminaryGapped(..)
  , HasPreliminaryUngapped(..)
  , HasImpliedAlignment(..)
  ) where

import Bio.Character.Decoration.Fitch.Class
import Bio.Character.Decoration.Fitch.Internal
import Bio.Metadata.Discrete

