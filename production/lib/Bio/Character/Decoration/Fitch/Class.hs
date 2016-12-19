-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Character.Decoration.Fitch.Class
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts, FunctionalDependencies, MultiParamTypeClasses #-}

module Bio.Character.Decoration.Fitch.Class where


import Bio.Character.Decoration.Discrete
import Bio.Character.Encodable
--import Bio.Metadata.Discrete
import Control.Lens
--import Data.Word


-- |
-- An optimization class for Fitch (non-additive) characters.
class DiscreteCharacterDecoration s a => FitchCharacterDecoration s a | s -> a where


-- |
-- A decoration containing a character that has been scored using Fitch's algorithm.
class ( DiscreteCharacterDecoration s c
      , HasChildMedians s (StaticCharacter, StaticCharacter)
      , HasIsLeaf s Bool
      , HasMinCost s Word
      , HasPreliminaryMedian s (StaticCharacter)
      ) => FitchDecoration s c | s -> c where


-- |
-- A decoration that can be constructed from a 'DiscreteCharacterDecoration' by
-- extending the decoration to contain the requisite fields for performing
-- Fitch's algorithm.
class ( FitchDecoration s c
      ) => DiscreteExtensionFitchDecoration s c | s -> c where

    extendDiscreteToFitch :: DiscreteCharacterDecoration x c
                          => x -> Word -> StaticCharacter -> StaticCharacter -> (StaticCharacter, StaticCharacter) -> Bool -> s

-- |
-- A 'Lens' for the 'fitchChildMedians' field.
class HasChildMedians s a | s -> a where

    childMedians :: Lens' s a
    {-# MINIMAL childMedians #-}


-- |
-- A 'Lens' for the 'fitchIsLeaf' field.
class HasIsLeaf s a | s -> a where

    isLeaf :: Lens' s a
    {-# MINIMAL isLeaf #-}


-- |
-- A 'Lens' for the 'fitchMinCost' field.
class HasMinCost s a | s -> a where

    minCost :: Lens' s a
    {-# MINIMAL minCost #-}


-- |
-- A 'Lens' for the 'fitchPreliminaryMedian' field.
class HasPreliminaryMedian s a | s -> a where

    preliminaryMedian :: Lens' s a
    {-# MINIMAL preliminaryMedian #-}

class HasFinalMedian s a | s -> a where

    finalMedian :: Lens' s a
    {-# MINIMAL finalMedian #-}