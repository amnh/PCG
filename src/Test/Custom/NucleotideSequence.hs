-----------------------------------------------------------------------------
-- |
-- Module      :  Test.Custom.NucleotideSequence
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Arbitrary instance for dynamic characters.
--
-- Allows for base ambiguities and gaps. The sequence will be non-empty.
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Test.Custom.NucleotideSequence
  ( NucleotideBase(..)
  , NucleotideSequence(..)
  ) where

import           Bio.Character
import           Bio.Character.Encodable.Dynamic
import           Data.Alphabet
import           Data.Alphabet.IUPAC
import           Data.Bimap                      (elems)
import           Data.Foldable
import           Data.List
import qualified Data.List.NonEmpty              as NE
import           Test.QuickCheck                 hiding (generate)
import           Test.SmallCheck.Series


-- |
-- Represents an arbitrary, non-empty sequence of nucleotide bases that may be
-- ambiguous and/or include gaps.
newtype NucleotideSequence = NS DynamicCharacter


instance Show NucleotideSequence where

    show (NS x) = showStream alphabet x


instance Arbitrary NucleotideSequence where

    arbitrary = NS . encodeStream alphabet . NE.fromList <$> streamGen
      where
        streamGen  = listOf1 elementGen
        elementGen = elements $ elems iupacToDna


-- |
-- Represents an arbitrary, non-empty ambiguity group which may include gaps.
newtype NucleotideBase = NB DynamicCharacterElement


instance Show NucleotideBase where

    show (NB x) = showStreamElement alphabet x


instance Arbitrary NucleotideBase where

    arbitrary = NB . encodeElement alphabet <$> elementGen
      where
        elementGen = elements $ elems iupacToDna


instance Monad m => Serial m NucleotideBase where

    series = generate $ const (NB . encodeElement alphabet <$> validSpace)
      where
        validSpace = fmap NE.fromList $ [] `delete` powerSet (toList alphabet)
        powerSet :: [a] -> [[a]]
        powerSet []     = [[]]
        powerSet (x:xs) = [x:ps | ps <- powerSet xs] <> powerSet xs


alphabet :: Alphabet String
alphabet = fromSymbols ["A","C","G","T"]
