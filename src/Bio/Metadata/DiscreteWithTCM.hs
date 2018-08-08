-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Metadata.DiscreteWithTCM
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Bio.Metadata.DiscreteWithTCM
    ( DiscreteCharacterMetadata(..)
    , DiscreteWithTcmCharacterMetadata()
    , DiscreteWithTCMCharacterMetadataDec()
    , GeneralCharacterMetadata(..)
    , HasCharacterAlphabet(..)
    , HasCharacterName(..)
    , HasCharacterWeight(..)
    , HasSymbolChangeMatrix(..)
    , HasTransitionCostMatrix(..)
    , HasSparseTransitionCostMatrix(..)
    , discreteMetadataFromTCM
    , discreteMetadataWithTCM
    ) where

import           Bio.Metadata.DiscreteWithTCM.Class
import           Bio.Metadata.DiscreteWithTCM.Internal
