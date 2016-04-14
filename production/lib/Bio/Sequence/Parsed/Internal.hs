-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Sequence.Parsed
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Module holding the data type for a parsed sequence
--
-----------------------------------------------------------------------------

module Bio.Sequence.Parsed.Internal where

import Data.Vector   (Vector)
import Data.Map      (Map)

-- TODO do ambiguity group types: more aliasing
-- TODO Add a definition for ParsedSeq for single characters
-- TODO change to ParsedChar

-- | A (nonempty) collection of possible character values. Singleton lists
--   represent a unambiguous character value.
type AmbiguityGroup = [String]

-- | An ordered sequence of ambiguitiy groups. This represents a dynamic
--   homology character.
type ParsedSeq = Vector AmbiguityGroup

-- TODO change to ParsedCharacters
-- | Represents a charcter sequence containing possibly missing character data.
type ParsedSequences = Vector (Maybe ParsedSeq)

-- TODO change to TaxaCharacters???
-- TODO add a TaxonIdentifier or TerminalName as type string - lots of aliasing
-- | A mapping from taxon identifiers to thier corresponding sequences.
type TreeSeqs = Map String ParsedSequences

-- TODO think about this type: change to a vector (or maybe list) of strings
-- Should definitely be a Vector, so length is readily available.
-- | An ordered list of possible character values.
type Alphabet = [String]
