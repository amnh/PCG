-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Character.Decoration.Dynamic.Class
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts, FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses #-}

module Bio.Character.Decoration.Continuous.Class where


import Bio.Metadata.Continuous
import Control.Lens

-- |
-- A decoration of an initial encoding of a dynamic character which has the
-- appropriate 'Lens' & character class constraints.
class ( HasContinuousCharacter s c
      , ContinuousCharacter c
      , GeneralCharacterMetadata s
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
