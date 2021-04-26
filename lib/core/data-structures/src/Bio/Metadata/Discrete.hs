-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Metadata.Discrete
-- Copyright   :  (c) 2015-2021 Ward Wheeler
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
  , HasTcmSourceFile(..)
  , discreteMetadata
  ) where


import Bio.Metadata.Discrete.Class
import Bio.Metadata.Discrete.Internal
