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

{-# LANGUAGE FlexibleContexts, FunctionalDependencies, MultiParamTypeClasses #-}

module Bio.Character.Decoration.Additive.Class where


import Bio.Character.Decoration.Discrete
import Bio.Character.Decoration.Shared
import Control.Lens


-- |
-- An abstract initial additive character decoration with a polymorphic character
-- type.
class DiscreteCharacterDecoration s a => AdditiveCharacterDecoration s a | s -> a where


-- |
-- A decoration containing a character that has been scored using Additive's algorithm.
class ( DiscreteCharacterDecoration s c
      , HasChildPrelimIntervals s ((Word, Word), (Word, Word))
      , HasIsLeaf s Bool
      , HasCharacterCost s Word
      , HasPreliminaryInterval s (Word, Word)
      ) => AdditiveDecoration s c | s -> c where


-- |
-- A decoration that can be constructed from a 'DiscreteCharacterDecoration' by
-- extending the decoration to contain the requisite fields for performing
-- Additive's algorithm.
class ( AdditiveDecoration s c
      ) => DiscreteExtensionAdditiveDecoration s c | s -> c where

    extendDiscreteToAdditive :: DiscreteCharacterDecoration x c
                             => x
                             -> Word
                             -> (Word, Word)
                             -> ((Word, Word), (Word, Word))
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
