------------------------------------------------------------------------------
-- |
-- Module      :  Bio.Metadata.General.Class
-- Copyright   :  (c) 2015-2021 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}

module Bio.Metadata.General.Class
  ( HasCharacterName(..)
  , HasCharacterWeight(..)
  ) where


import Control.Lens.Type (Lens')


-- |
-- A 'Control.Lens.Type.Lens' for the 'characterName' field.
class HasCharacterName s a | s -> a where

    {-# MINIMAL characterName #-}
    characterName :: Lens' s a


-- |
-- A 'Control.Lens.Type.Lens' for the 'characterWeight' field.
class HasCharacterWeight s a | s -> a where

    {-# MINIMAL characterWeight #-}
    characterWeight :: Lens' s a

