-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Character.Decoration.Fitch.Class
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

module Bio.Character.Decoration.Fitch.Class where


import Bio.Character.Decoration.Discrete
import Bio.Character.Decoration.Shared
--import Bio.Character.Encodable
--import Bio.Metadata.Discrete
import Control.Lens.Type                 (Lens')
--import Data.Word


-- |
-- An optimization class for Fitch (non-additive) characters.
class DiscreteCharacterDecoration s a => FitchCharacterDecoration s a | s -> a where


-- |
-- A decoration containing a character that has been scored using Fitch's algorithm.
class ( DiscreteCharacterDecoration s c
      , HasChildMedians s (c, c)
      , HasIsLeaf s Bool
      , HasCharacterCost s Word
      , HasPreliminaryMedian s c
      , HasFinalMedian s c

      ) => FitchDecoration s c | s -> c where


-- |
-- A decoration that can be constructed from a 'DiscreteCharacterDecoration' by
-- extending the decoration to contain the requisite fields for performing
-- Fitch's algorithm.
class ( FitchDecoration s c
      ) => DiscreteExtensionFitchDecoration s c | s -> c where

    extendDiscreteToFitch :: DiscreteCharacterDecoration x c
                          => x
                          -> Word
                          -> c
                          -> c
                          -> (c, c)
                          -> Bool
                          -> s


-- |
-- A 'Control.Lens.Type.Lens' for the 'childMedians' field.
class HasChildMedians s a | s -> a where

    {-# MINIMAL childMedians #-}
    childMedians :: Lens' s a


{-
-- |
-- A 'Control.Lens.Type.Lens' for the 'fitchMinCost' field.
class HasMinCost s a | s -> a where

    minCost :: Lens' s a
    {-# MINIMAL minCost #-}
-}


-- |
-- A 'Control.Lens.Type.Lens' for the 'preliminaryMedian' field.
class HasPreliminaryMedian s a | s -> a where

    {-# MINIMAL preliminaryMedian #-}
    preliminaryMedian :: Lens' s a


-- |
-- A 'Control.Lens.Type.Lens' for the 'finalMedian' field.
class HasFinalMedian s a | s -> a where

    {-# MINIMAL finalMedian #-}
    finalMedian :: Lens' s a
