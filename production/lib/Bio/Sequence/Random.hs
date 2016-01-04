-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Sequence.Random
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Module making sequence types instances of Arbitrary for testing
--
-----------------------------------------------------------------------------

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Bio.Sequence.Random where

import Bio.Sequence.Coded

import Data.Bits
import Data.Vector
import Data.BitVector

import Test.Tasty.QuickCheck

instance Arbitrary b => Arbitrary (EncodedSeq b) where
        arbitrary = do
        there <- arbitrary :: Gen Bool
        if there then Just <$> fromList <$> listOf arbitrary
            else return Nothing

instance Arbitrary b => Arbitrary (EncodedSequences b) where
    arbitrary = fromList <$> listOf arbitrary

instance Arbitrary BitVector where
    arbitrary = fromBits <$> listOf (arbitrary :: Gen Bool)