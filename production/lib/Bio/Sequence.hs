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

module Bio.Sequence
  ( CharacterSequence()
  , CharacterBlock(..)
  , HasBlockCost
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
  , sequenceCost
  , sequenceRootCost
  , blockCost
  , staticCost
  ) where

import Bio.Sequence.Internal
import Bio.Sequence.Block hiding (hexmap, hexTranspose, hexZipWith)
import Bio.Sequence.Block.Builder
