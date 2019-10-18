-----------------------------------------------------------------------------
-- |
-- Module      :  File.Format.Nexus
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- The Nexus file format was developed by an committee consisting of members
-- who had no concept of language design. Consequently the Nexus file format
-- is needlessly complex.
--
-- This parser works barely on a subset of the Nexus specification's ambiguously
-- defined features.
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module File.Format.Nexus
  ( AlphabetSymbol
  , AmbiguityGroup
  , Character
  , CharacterMetadata(..)
  , CharDataType(..)
  , Nexus(..)
  , Sequence
  , Sequences
  , TaxonIdentifier
  , TaxonSequenceMap
  , nexusStreamParser
  ) where


import Data.CaseInsensitive       (FoldCase)
import File.Format.Nexus.Data
import File.Format.Nexus.Parser
import File.Format.Nexus.Validate
import Text.Megaparsec


-- |
-- Parses the entirety of a stream consisting of a single Nexus file resulting a 'Nexus'.
nexusStreamParser :: (FoldCase (Tokens s), MonadFail m, MonadParsec e s m, Token s ~ Char {- , Show s -}) => m Nexus
nexusStreamParser = validateNexusParseResult =<< parseNexus <* eof
