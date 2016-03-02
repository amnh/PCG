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
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Bio.Sequence.Parsed (ParsedSeq, TreeSeqs, Alphabet, ParsedSequences) where

import Data.Vector (Vector, fromList)
import Data.Map.Lazy (Map)
import Test.Tasty.QuickCheck

-- TODO do ambiguity group types: more aliasing
-- TODO Add a definition for ParsedSeq for single characters
-- TODO change to ParsedChar

type AmbiguityGroup = [String]

type ParsedSeq = Vector AmbiguityGroup
-- TODO change to ParsedCharacters
type ParsedSequences = Vector (Maybe ParsedSeq)
-- TODO change to TaxaCharacters???
-- TODO add a TaxonIdentifier or TerminalName as type string - lots of aliasing
type TreeSeqs = Map String ParsedSequences
-- TODO think about this type: change to a vector (or maybe list) of strings
type Alphabet = [String]

--instance Arbitrary ParsedSeq where
--    arbitrary = fromList <$> listOf (listOf (arbitrary :: Gen String))
