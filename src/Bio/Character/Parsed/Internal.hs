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

{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE StrictData           #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Bio.Character.Parsed.Internal where

import Data.Alphabet
import Data.List.NonEmpty (NonEmpty)
import Data.Map           (Map)
import Data.Vector        (Vector)


-- |
-- A mapping from taxon identifiers to their corresponding sequences.
type TaxonCharacters = Map Identifier ParsedChars


-- |
-- Represents a character sequence containing possibly-missing character data.
type ParsedChars = Vector ParsedCharacter

-- |
-- The string value that uniquely identifies a taxon.
type Identifier = String


-- |
-- A generalized character type extracted from a parser.
-- A character can be real-valued, discrete and singular,
-- or discrete with variable length.
data ParsedCharacter
   = ParsedContinuousCharacter (Maybe Double)
   | ParsedDiscreteCharacter   (Maybe (AmbiguityGroup String))
   | ParsedDynamicCharacter    (Maybe (NonEmpty (AmbiguityGroup String)))
   deriving (Eq, Show)
