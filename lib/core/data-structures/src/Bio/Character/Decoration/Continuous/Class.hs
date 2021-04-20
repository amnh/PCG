-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Character.Decoration.Continuous.Class
-- Copyright   :  (c) 2015-2021 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Bio.Character.Decoration.Continuous.Class
  (-- ContinuousCharacter(..)
    ContinuousCharacterDecoration
  , DiscreteExtensionContinuousDecoration(..)
  , HasChildPrelimIntervals(..)
  , HasContinuousCharacter(..)
  , HasPreliminaryInterval(..)
  ) where

import Bio.Character.Decoration.Discrete
import Bio.Character.Decoration.Shared
import Control.Lens.Type                 (Lens')


-- |
-- A decoration of an initial encoding of a dynamic character which has the
-- appropriate 'Control.Lens.Type.Lens' and character class constraints.
class ( -- ContinuousCharacter c
        HasContinuousCharacter s c
      ) => ContinuousDecoration s c | s -> c where


{-
-- |
-- A character class for continuous characters.
class Ord c => ContinuousCharacter c where

    toContinuousCharacter :: Real r => Maybe r -> c
-}


-- |
-- A 'Control.Lens.Type.Lens' for the 'continuousCharacter' field
class HasContinuousCharacter s a | s -> a where

    continuousCharacter :: Lens' s a
    {-# MINIMAL continuousCharacter #-}


-- |
-- An abstract initial additive character decoration with a polymorphic character
-- type.
class ContinuousDecoration s a => ContinuousCharacterDecoration s a | s -> a where


-- |
-- A decoration containing a character that has been scored using the algorithm for continuous characters.
class ( HasChildPrelimIntervals s ((Double, Double), (Double, Double))
      , HasIsLeaf s Bool
      , HasCharacterCost s Double
      , HasPreliminaryInterval s (Double, Double)
      ) => ContinuousAdditiveHybridDecoration s c | s -> c where


-- |
-- A decoration that can be constructed from a 'DiscreteCharacterDecoration' by
-- extending the decoration to contain the requisite fields for performing
-- the algorithm for continuous characters.
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
-- A 'Control.Lens.Type.Lens' for the 'childPrelimIntervals' field.
class HasChildPrelimIntervals s a | s -> a where

    childPrelimIntervals :: Lens' s a
    {-# MINIMAL childPrelimIntervals #-}


-- |
-- A 'Control.Lens.Type.Lens' for the 'preliminaryInterval' field.
class HasPreliminaryInterval s a | s -> a where

    preliminaryInterval :: Lens' s a
    {-# MINIMAL preliminaryInterval #-}
