-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Character.Decoration.Additive.Class
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts, FunctionalDependencies, MultiParamTypeClasses, TypeFamilies #-}

module Bio.Character.Decoration.Additive.Class where


import Bio.Character.Decoration.Discrete
import Bio.Character.Decoration.Shared
import Control.Lens
import Data.Range


-- |
-- An abstract initial additive character decoration with a polymorphic character
-- type.
class DiscreteCharacterDecoration s a => AdditiveCharacterDecoration s a | s -> a where

-- TODO: Make these range generalized
-- |
-- A decoration containing a character that has been scored using Additive's algorithm.
class ( DiscreteCharacterDecoration s c
      , HasChildPrelimIntervals s (Range (Bound c), Range (Bound c))
      , HasIsLeaf s Bool
      , HasCharacterCost s (Bound c)
      , HasPreliminaryInterval s (Range (Bound c))
      , HasFinalInterval s (Range (Bound c))
      ) => AdditiveDecoration s c | s -> c where


-- |
-- A decoration that can be constructed from a 'DiscreteCharacterDecoration' by
-- extending the decoration to contain the requisite fields for performing
-- Additive's algorithm.
class ( AdditiveDecoration s c
      ) => DiscreteExtensionAdditiveDecoration s c | s -> c where

    extendDiscreteToAdditive :: ( DiscreteCharacterDecoration x c
                                , Bound c ~ r
                                )
                             => x
                             -> r
                             -> Range r
                             -> Range r
                             -> (Range r, Range r)
                             -> Bool
                             -> s

-- |
-- A 'Lens' for the 'additiveChildPrelimIntervals' field.
class HasChildPrelimIntervals s a | s -> a where

    childPrelimIntervals :: Lens' s a
    {-# MINIMAL childPrelimIntervals #-}


-- |
-- A 'Lens' for the 'additivePreliminaryInterval' field.
class HasPreliminaryInterval s a | s -> a where

    preliminaryInterval :: Lens' s a
    {-# MINIMAL preliminaryInterval #-}


-- |
-- A 'Lens' for the 'additiveFinalInterval' field.
class HasFinalInterval s a | s -> a where

    finalInterval :: Lens' s a
    {-# MINIMAL finalInterval #-}
