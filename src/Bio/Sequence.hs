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
  , CharacterBlock()
  , HasBlockCost
  , HasRootCost
  , PartialCharacterBlock()
  -- * CharacterBlock construction
  , continuousSingleton
  , discreteSingleton
  , dynamicSingleton
  , finalizeCharacterBlock
  , fromBlocks
  , fromBlockVector
  -- * CharacterBlock deconstruction
  , toBlocks
  , toBlockVector
  -- * Block extraction
  , continuousCharacterBins
  , nonAdditiveCharacterBins
  , additiveCharacterBins
  , metricCharacterBins
  , nonMetricCharacterBins
  , dynamicCharacters
  , setDynamicCharacters
  -- * CharacterBlock transformations
  , toMissingCharacters
  , hexmap
  , hexTranspose
  , hexZipWith
  , hexZipWithMeta
  -- * Cost quantification
  , sequenceCost
  , sequenceRootCost
  , blockCost
  , staticCost
  ) where

import           Bio.Sequence.Block           hiding (hexTranspose, hexZipWith,
                                               hexZipWithMeta, hexmap)
import           Bio.Sequence.Block.Builder
import           Bio.Sequence.Block.Character (additiveCharacterBins,
                                               continuousCharacterBins,
                                               dynamicCharacters,
                                               finalizeCharacterBlock,
                                               metricCharacterBins,
                                               nonAdditiveCharacterBins,
                                               nonMetricCharacterBins,
                                               setDynamicCharacters)
import           Bio.Sequence.Character

