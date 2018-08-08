-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Metadata.Discrete
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Bio.Metadata.Discrete
  ( DiscreteCharacterMetadataDec()
  , DiscreteCharacterMetadata(..)
  , GeneralCharacterMetadata(..)
  , HasCharacterAlphabet(..)
  , HasCharacterName(..)
  , HasCharacterWeight(..)
  , discreteMetadata
  ) where


import           Bio.Metadata.Discrete.Class
import           Bio.Metadata.Discrete.Internal
