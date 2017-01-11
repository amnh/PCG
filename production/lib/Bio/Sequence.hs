-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Sequence
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Data structures and instances for coded characters
-- Coded characters are dynamic characters recoded as 
--
-----------------------------------------------------------------------------

-- TODO: Remove all commented-out code.

-- TODO: are all of these necessary?

module Bio.Sequence
  ( CharacterSequence()
  , toBlocks
  , fromBlocks
  , hexmap
  , hexTranspose
  , hexZipWith  
  ) where

import Bio.Sequence.Internal
