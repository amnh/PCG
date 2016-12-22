-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Character.Decoration.Shared
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts, FunctionalDependencies, MultiParamTypeClasses #-}

module Bio.Character.Decoration.Shared where


import Control.Lens


-- |
-- A 'Lens' for the 'fitchIsLeaf' field.
class HasIsLeaf s a | s -> a where

    isLeaf :: Lens' s a
    {-# MINIMAL isLeaf #-}

