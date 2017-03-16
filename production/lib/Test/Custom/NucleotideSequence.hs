module Test.Custom.NucleotideSequence
  ( NucleotideSequence(..)
  ) where

import           Bio.Character
import           Data.Alphabet.IUPAC
import           Data.Bimap               (elems)
import qualified Data.List.NonEmpty as NE
import           Test.QuickCheck

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
