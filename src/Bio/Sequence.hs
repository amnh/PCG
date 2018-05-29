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
  -- * Cost quantification
  , sequenceCost
  , sequenceRootCost
  , blockCost
  , staticCost
  ) where

import Bio.Sequence.Character
import Bio.Sequence.Block hiding (hexmap, hexTranspose, hexZipWith)
import Bio.Sequence.Block.Builder
import Bio.Sequence.Block.Character ( finalizeCharacterBlock
                                    , continuousCharacterBins
                                    , nonAdditiveCharacterBins
                                    , additiveCharacterBins
                                    , metricCharacterBins
                                    , nonMetricCharacterBins
                                    , dynamicCharacters
                                    , setDynamicCharacters
                                    )

