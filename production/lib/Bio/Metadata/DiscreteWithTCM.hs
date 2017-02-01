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
    ( DiscreteWithTcmCharacterMetadata()
    , DiscreteWithTCMCharacterMetadataDec()
    , HasCharacterAlphabet(..)
    , HasCharacterName(..)
    , HasCharacterWeight(..)
    , HasSymbolChangeMatrix(..)
    , HasTransitionCostMatrix(..)
    , discreteMetadataWithTCM
    ) where 

import Bio.Metadata.DiscreteWithTCM.Class
import Bio.Metadata.DiscreteWithTCM.Internal
