-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Metadata.Discrete.Class
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses #-}

module Bio.Metadata.Discrete.Class where

import Control.Lens


-- |
-- A decoration of an initial encoding of a dynamic character which has the
-- appropriate 'Lens' & character class constraints.
class ( HasAlphabet s a
      , HasCharacterName s a
      , HasSymbolicTCMGenerator
      , HasTransitionCostMatrix s a
      , HasWeight s a
      ) => DiscreteCharacterMetadata s a | s -> a where

  
-- |
-- A 'Lens' for the 'alphabet' field
class HasAlphabet s a | s -> a where

    alphabet :: Lens' s a
    {-# MINIMAL alphabet #-}


-- |
-- A 'Lens' for the 'characterName' field
class HasCharacterName s a | s -> a where

    characterName :: Lens' s a
    {-# MINIMAL characterName #-}


-- |
-- A 'Lens' for the 'symbolicTCMGenerator' field
class HasSymbolicTCMGenerator s a | s -> a where

    symbolicTCMGenerator :: Lens' s a
    {-# MINIMAL symbolicTCMGenerator #-}


-- |
-- A 'Lens' for the 'transitionCostMatrix' field
class HasTransitionCostMatrix s a | s -> a where

    transitionCostMatrix :: Lens' s a
    {-# MINIMAL transitionCostMatrix #-}


-- |
-- A 'Lens' for the 'weight' field
class HasWeight s a | s -> a where

    weight :: Lens' s a
    {-# MINIMAL weight #-}


