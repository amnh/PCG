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

import Prelude hiding ((++))
import Data.Monoid
import Data.Vector (Vector, fromList, cons, (++))
import Data.Map.Lazy (Map, intersectionWith, difference, unions, (\\), empty)
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

instance Monoid TreeSeqs where
    mempty = empty
    mappend lhs rhs = 
        --let
        --    sameSets = intersectionWith (++) seqSet1 seqSet2
        --    differenceLeft = fmap (`cons` Nothing) (difference seqSet1 seqSet2)
        --    differenceRight = fmap ((++) (pure Nothing)) (difference seqSet2 seqSet1)
        --in unions [sameSets, differenceLeft, differenceRight]
        let
            leftSide = intersectionWith (++) (lhs \\ rhs) rhs
            rightSide = intersectionWith (++) (rhs \\ lhs) lhs
            middle = intersectionWith (++) lhs rhs
        in unions [leftSide, middle, rightSide]
