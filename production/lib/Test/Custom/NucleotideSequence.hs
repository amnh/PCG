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

module Test.Custom.NucleotideSequence
  ( NucleotideSequence(..)
  ) where

import           Bio.Character
import           Data.Alphabet.IUPAC
import           Data.Bimap               (elems)
import qualified Data.List.NonEmpty as NE
import           Test.QuickCheck


-- |
-- Represents an arbitrary, non-empty sequence of nucleotide bases that may be
-- ambiguous and/or include gaps.
newtype NucleotideSequence = NS DynamicChar


instance Show NucleotideSequence where

    show (NS x) = showStream alphabet x


instance Arbitrary NucleotideSequence where

    arbitrary = NS . encodeStream alphabet . NE.fromList <$> streamGen
      where
        streamGen  = listOf1 elementGen
        elementGen = elements $ elems iupacToDna


alphabet :: Alphabet String
alphabet = fromSymbols ["A","C","G","T"] 
