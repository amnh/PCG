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
  , CharacterBlock(..)
  , PartialCharacterBlock()
  , continuousSingleton
  , discreteSingleton
  , dynamicSingleton 
  , finalizeCharacterBlock
  , toMissingCharacters
  , toBlocks
  , fromBlocks
  , hexmap
  , hexTranspose
  , hexZipWith  
  ) where

import Bio.Sequence.Internal
import Bio.Sequence.Block hiding (hexmap, hexTranspose, hexZipWith)
import Bio.Sequence.Block.Builder
