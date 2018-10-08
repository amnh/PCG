-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Alphabet.Special
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Data.Alphabet.Special
  ( -- * Special Alphabet constructions
    aminoAcidAlphabet
  , dnaAlphabet
  , rnaAlphabet
  , discreteAlphabet
    -- * Special Alphabet Querries
  , isAlphabetAminoAcid
  , isAlphabetDna
  , isAlphabetRna
  , isAlphabetDiscrete
  ) where

import           Data.Alphabet.Internal
import           Data.Alphabet.IUPAC
import           Data.Bimap             (Bimap)
import qualified Data.Bimap             as BM
import           Data.Char              (isUpper)
import           Data.Foldable
import qualified Data.List.NonEmpty     as NE
import qualified Data.Set               as Set
import           Data.String


-- |
-- Alphabet of amino acids.
aminoAcidAlphabet :: (IsString s, Ord s) => Alphabet s
aminoAcidAlphabet = fromBimap iupacToAminoAcid


-- |
-- Alphabet of DNA bases.
dnaAlphabet :: (IsString s, Ord s) => Alphabet s
dnaAlphabet = fromBimap iupacToDna


-- |
-- Alphabet of RNA bases.
rnaAlphabet :: (IsString s, Ord s) => Alphabet s
rnaAlphabet = fromBimap iupacToRna

-- |
-- Alphabet of "discrete" values.
--
-- The discrete alphabet includes the following 63 values:
-- > ['0'..'9'] <> ['A'..'Z'] <> ['a'..'z'] <> "-"
discreteAlphabet :: (IsString s, Ord s) => Alphabet s
discreteAlphabet = fromSymbols $ fromString . pure <$> mconcat [['0'..'9'], ['A'..'Z'], ['a'..'z'], "-"]


-- |
-- /O(n)/
--
-- Determines if the supplied alphabet represents amino acid symbols.
--
-- Useful for determining if an 'AmbiguityGroup' should be rendered as an IUPAC
-- code.
isAlphabetAminoAcid :: (IsString s, Ord s) => Alphabet s -> Bool
isAlphabetAminoAcid = isAlphabetSubsetOf aminoAcidAlphabet


-- |
-- /O(n)/
--
-- Determines if the supplied alphabet represents DNA symbols.
--
-- Useful for determining if an 'AmbiguityGroup' should be rendered as an IUPAC
-- code.
isAlphabetDna :: (IsString s, Ord s) => Alphabet s -> Bool
isAlphabetDna = isAlphabetSubsetOf dnaAlphabet


-- |
-- /O(n)/
--
-- Determines if the supplied alphabet represents DNA symbols.
--
-- Useful for determining if an 'AmbiguityGroup' should be rendered as an IUPAC
-- code.
isAlphabetRna :: (IsString s, Ord s) => Alphabet s -> Bool
isAlphabetRna = isAlphabetSubsetOf rnaAlphabet


-- |
-- /O(n)/
--
-- Determines if the supplied alphabet represents DNA symbols.
--
-- Useful for determining if an 'AmbiguityGroup' should be rendered as an IUPAC
-- code.
isAlphabetDiscrete :: (IsString s, Ord s) => Alphabet s -> Bool
isAlphabetDiscrete = isAlphabetSubsetOf discreteAlphabet


isAlphabetSubsetOf :: Ord s => Alphabet s -> Alphabet s -> Bool
isAlphabetSubsetOf specialAlpahbet queryAlphabet = querySet `Set.isSubsetOf` specialSet
  where
    querySet   = Set.fromList $ toList queryAlphabet
    specialSet = Set.fromList $ toList specialAlpahbet


fromBimap :: (IsString s, Ord s) => Bimap (AmbiguityGroup String) a -> Alphabet s
fromBimap = fromSymbols . fmap fromString . filter isUpperCaseStr . fmap NE.head . BM.keys
  where
    isUpperCaseStr (x:_) = isUpper x
    isUpperCaseStr    _  = False


