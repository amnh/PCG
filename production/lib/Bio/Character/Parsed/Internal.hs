-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Character.Parsed.Internal
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Module holding the data type for a parsed character, which is the type
-- that comes from the parsers, and is then coverted into our various internal
-- character types
--
-----------------------------------------------------------------------------

module Bio.Character.Parsed.Internal where

import Data.Vector   (Vector)
import Data.Map      (Map)

-- TODO: do ambiguity group types: more aliasing

-- TODO: make AmbiguityGroup a nonempty list
-- | A (nonempty) collection of possible character values. Singleton lists
--   represent a unambiguous character value.
type AmbiguityGroup = [String]

-- | An ordered dynamic character of ambiguity groups. This represents a dynamic
--   homology character when it comes from the parser (so is not yet encoded
---  or packed, if those are options.)
type ParsedChar = Vector AmbiguityGroup

-- TODO: Remove Maybe?
-- | Represents a character sequence containing possibly missing character data.
type ParsedChars = Vector (Maybe ParsedChar)

-- TODO: add a TaxonIdentifier or TerminalName as type string - lots of aliasing
-- | A mapping from taxon identifiers to thier corresponding sequences.
type TreeChars = Map String ParsedChars

-- | An ordered list of possible character values.
type Alphabet = Vector String
