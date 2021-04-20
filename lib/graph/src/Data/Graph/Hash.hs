------------------------------------------------------------------------------
-- |
-- Module      :  Data.Graph.Hash
-- Copyright   :  (c) 2015-2021 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Data.Graph.Hash where

import Control.Lens.Type


-- |
-- A 'Lens' for values which have a "hasValue."
class HasHashValue s a | s -> a where

    _hashValue :: Lens' s a



