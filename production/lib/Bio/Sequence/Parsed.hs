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

type ParsedSeq = Vector [String]

type ParsedSequences = Vector (Maybe ParsedSeq)

type TreeSeqs = Map String ParsedSequences

type Alphabet = Vector [String]

--instance Arbitrary ParsedSeq where
--    arbitrary = fromList <$> listOf (listOf (arbitrary :: Gen String))
