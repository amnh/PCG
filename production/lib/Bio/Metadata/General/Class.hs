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


class ( HasCharacterName   s CharacterName
      , HasCharacterWeight s Double
      ) => GeneralCharacterMetadata s where

  
-- |
-- A 'Lens' for the 'characterName' field
class HasCharacterName s a | s -> a where

    characterName :: Lens' s a
    {-# MINIMAL characterName #-}


-- |
-- A 'Lens' for the 'characterWeight' field
class HasCharacterWeight s a | s -> a where

    characterWeight :: Lens' s a
    {-# MINIMAL characterWeight #-}

