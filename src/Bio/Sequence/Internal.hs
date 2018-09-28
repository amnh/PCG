-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Sequence.Internal
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Data structures and instances shared between different sequence types.
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Bio.Sequence.Internal
  ( HasBlocks(..)
  ) where

import Control.Lens


-- |
-- A 'Lens' for 'blockSequence'.
class HasBlocks s t a b | s -> a, t -> b where

    blockSequence :: Lens s t a b
