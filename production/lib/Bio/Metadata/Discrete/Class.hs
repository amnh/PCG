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
  , HasCharacterAlphabet(..)
  , HasCharacterName(..)
  , HasCharacterWeight(..)
  ) where

import Bio.Character.Encodable
import Bio.Metadata.General 
import Control.Lens
import Data.Alphabet


-- |
-- A decoration of an initial encoding of a dynamic character which has the
-- appropriate 'Lens' & character class constraints.
class ( GeneralCharacterMetadata s
      , HasCharacterAlphabet     s (Alphabet String)
      ) => DiscreteCharacterMetadata s where

  
-- |
-- A 'Lens' for the 'alphabet' field
class HasCharacterAlphabet s a | s -> a where

    {-# MINIMAL characterAlphabet #-}
    characterAlphabet :: Lens' s a

