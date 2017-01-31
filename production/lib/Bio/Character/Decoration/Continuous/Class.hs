-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Character.Decoration.Continuous.Class
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts, FunctionalDependencies, MultiParamTypeClasses #-}

module Bio.Character.Decoration.Continuous.Class where

import Bio.Character.Decoration.Discrete
import Bio.Character.Decoration.Shared
import Control.Lens


-- |
-- A decoration of an initial encoding of a dynamic character which has the
-- appropriate 'Lens' & character class constraints.
class ( ContinuousCharacter c
      , GeneralCharacterMetadata s
      , HasContinuousCharacter s c
      ) => ContinuousDecoration s c | s -> c where

  
-- |
-- A character class for continuous characters.
class Ord c => ContinuousCharacter c where

    toContinuousCharacter :: Real r => Maybe r -> c 


-- |
-- A 'Lens' for the 'continuousCharacter' field
class HasContinuousCharacter s a | s -> a where

    continuousCharacter :: Lens' s a
    {-# MINIMAL continuousCharacter #-} 




-- |
-- An abstract initial additive character decoration with a polymorphic character
-- type.
class DiscreteCharacterDecoration s a => ContinuousCharacterDecoration s a | s -> a where


-- |
-- A decoration containing a character that has been scored using Continuous's algorithm.
class ( DiscreteCharacterDecoration s c
      , HasChildPrelimIntervals s ((Double, Double), (Double, Double))
      , HasIsLeaf s Bool
      , HasCharacterCost s Double
      , HasPreliminaryInterval s (Double, Double)
      ) => ContinuousAdditiveHybridDecoration s c | s -> c where


-- |
-- A decoration that can be constructed from a 'DiscreteCharacterDecoration' by
-- extending the decoration to contain the requisite fields for performing
-- Continuous's algorithm.
class ( ContinuousAdditiveHybridDecoration s c
      ) => DiscreteExtensionContinuousDecoration s c | s -> c where

    extendDiscreteToContinuous :: DiscreteCharacterDecoration x c
                             => x
                             -> Double
                             -> (Double, Double)
                             -> ((Double, Double), (Double, Double))
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
