-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Character.Decoration.Dynamic
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Bio.Character.Decoration.Dynamic
  ( DynamicDecorationInitial()
  , DirectOptimizationPostOrderDecoration()
  , DynamicDecorationDirectOptimization()
  , DynamicDecorationImpliedAlignment()
  , SimpleDynamicDecoration()
  , DirectOptimizationDecoration()
  , ImpliedAlignmentDecoration()
  , DynamicCharacterDecoration(..)
  , DynamicDecorationDirectOptimizationPostOrderResult()
  , GeneralCharacterMetadata()
  , SimpleDynamicExtensionPostOrderDecoration(..)
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
  , PossiblyMissingCharacter(..)
  ) where

import Bio.Character.Decoration.Dynamic.Class
import Bio.Character.Decoration.Dynamic.Internal
import Bio.Character.Encodable
import Bio.Metadata.Discrete

