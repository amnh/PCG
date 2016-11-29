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

{-# LANGUAGE FlexibleContexts, FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses #-}

module Bio.Metadata.Discrete.Class
  ( DiscreteCharacterMetadata()
  , GeneralCharacterMetadata()
  , HasCharacterAlphabet(..)
  , HasCharacterName(..)
  , HasCharacterSymbolTransitionCostMatrixGenerator(..)
  , HasCharacterTransitionCostMatrix(..)
  , HasCharacterWeight(..)
  ) where

import Bio.Character.Encodable
import Bio.Metadata.General 
import Control.Lens
import Data.Alphabet


-- |
-- A decoration of an initial encoding of a dynamic character which has the
-- appropriate 'Lens' & character class constraints.
class ( EncodableStreamElement c
      , GeneralCharacterMetadata s
      , HasCharacterAlphabet s (Alphabet String)
      , HasCharacterSymbolTransitionCostMatrixGenerator s (Int -> Int -> Int)
      , HasCharacterTransitionCostMatrix s (c -> c -> (c, Int))
      ) => DiscreteCharacterMetadata s c | s -> c where

  
-- |
-- A 'Lens' for the 'alphabet' field
class HasCharacterAlphabet s a | s -> a where

    characterAlphabet :: Lens' s a
    {-# MINIMAL characterAlphabet #-}


-- |
-- A 'Lens' for the 'symbolicTCMGenerator' field
class HasCharacterSymbolTransitionCostMatrixGenerator s a | s -> a where

    characterSymbolTransitionCostMatrixGenerator :: Lens' s a
    {-# MINIMAL characterSymbolTransitionCostMatrixGenerator #-}


-- |
-- A 'Lens' for the 'transitionCostMatrix' field
class HasCharacterTransitionCostMatrix s a | s -> a where

    characterTCM :: Lens' s a
    {-# MINIMAL characterTCM #-}


