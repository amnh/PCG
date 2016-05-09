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

import Data.Vector   (Vector, fromList, toList)
import Data.Map      (Map)
import Data.Maybe
import Test.Tasty.QuickCheck

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

-- | Higher level arbitrary helper
parsedCharsGivenAlph :: [Alphabet] -> Gen ParsedDynChars
parsedCharsGivenAlph inAlphs = fromList <$> sequence (map parsedMaybe inAlphs)

-- | Generates a maybe character
parsedMaybe :: Alphabet -> Gen (Maybe ParsedDynChar)
parsedMaybe inAlph = do
    c <- arbParsedGivenAlph inAlph
    elements [Just c, Nothing]

-- | Define an arbitrary helper function to create a parsed sequence over an Alphabet
arbParsedGivenAlph :: Alphabet -> Gen ParsedDynChar
arbParsedGivenAlph inAlph = fromList <$> listOf (sublistOf (toList inAlph))