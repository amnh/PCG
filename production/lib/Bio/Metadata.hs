-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Metadata
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Wrapper for all the metadata types
--
-----------------------------------------------------------------------------

module Bio.Metadata
  ( CharacterMetadata(..)
  , CharDataType(..)
  , CostStructure(..)
  , CostMatrix
  , Metadata(..)
  , ParsedMetadata(..)
  , prependName
  , toCostFunction
  , updateAligned
  , updateAlphabet
  , updateTcm
  ) where

import Bio.Metadata.Class
import Bio.Metadata.Internal
import Bio.Metadata.Parsed
