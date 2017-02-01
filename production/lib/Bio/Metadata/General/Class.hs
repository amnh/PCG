------------------------------------------------------------------------------
-- |
-- Module      :  Bio.Metadata.General.Class
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts, FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses, TypeFamilies #-}

module Bio.Metadata.General.Class
  ( GeneralCharacterMetadata()
  , HasCharacterName(..)
  , HasCharacterWeight(..)
  ) where


import Bio.Metadata.CharacterName
import Control.Lens


-- |
-- A constraint for type containing metadata.
class ( HasCharacterName   s CharacterName
      , HasCharacterWeight s Double
      ) => GeneralCharacterMetadata s where

  
-- |
-- A 'Lens' for the 'characterName' field
class HasCharacterName s a | s -> a where

    {-# MINIMAL characterName #-}
    characterName :: Lens' s a


-- |
-- A 'Lens' for the 'characterWeight' field
class HasCharacterWeight s a | s -> a where

    {-# MINIMAL characterWeight #-}
    characterWeight :: Lens' s a

