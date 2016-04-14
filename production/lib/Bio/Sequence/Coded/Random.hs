-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Sequences.Coded.Random
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Data structures and instances for random coded sequences
--
-----------------------------------------------------------------------------
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Bio.Sequence.Coded.Random where

import Bio.Sequence.Coded.Internal
import Bio.Sequence.Parsed

import Test.Tasty.QuickCheck

type FromParsed = (EncodedSeq, Alphabet)

instance Arbitrary FromParsed where
    arbitrary = do
        let charGen = resize 3 (arbitrary :: Gen String)
        alph <- vectorOf 5 charGen
        return undefined