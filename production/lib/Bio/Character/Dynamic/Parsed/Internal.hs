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
-- Module holding the data type for a parsed character
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
--   homology character.
type ParsedDynChar = Vector AmbiguityGroup

-- TODO: Remove Maybe?
-- | Represents a charcter sequence containing possibly missing character data.
type ParsedDynChars = Vector (Maybe ParsedDynChar)

-- TODO: add a TaxonIdentifier or TerminalName as type string - lots of aliasing
-- | A mapping from taxon identifiers to thier corresponding sequences.
type TreeChars = Map String ParsedDynChars

-- | An ordered list of possible character values.
type Alphabet = Vector String
