-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Character.Dynamic.Decoration
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Bio.Character.Dynamic.Decoration
  ( -- * Polymorphic Types
    DynamicDecorationInitial()
  , DynamicDecorationDirectOptimization()
  , DynamicDecorationImpliedAlignment()
    -- * Constraint Classes
  , DynamicDecoration()
  , DirectOptimizationDecoration()
  , ImpliedAlignmentDecoration()
    -- * Lenses
  , HasEncoded(..)
  , HasFinalGapped(..)
  , HasFinalUngapped(..)
  , HasPreliminaryGapped(..)
  , HasPreliminaryUngapped(..)
  , HasImpliedAlignment(..)
  ) where


import Bio.Character.Dynamic.Decoration.Class
import Bio.Character.Dynamic.Decoration.Internal
