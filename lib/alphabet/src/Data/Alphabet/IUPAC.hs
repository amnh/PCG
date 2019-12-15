-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Alphabet.IUPAC
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Data.Alphabet.IUPAC
  ( iupacToAminoAcid
  , iupacToDna
  , iupacToRna
  ) where


import           Control.Arrow          ((***))
import           Data.Alphabet.Internal (AmbiguityGroup)
import           Data.Bimap             (Bimap)
import qualified Data.Bimap             as BM
import qualified Data.List.NonEmpty     as NE
import           Data.String


-- |
-- Substitutions for converting to an Amino Acid sequence based on IUPAC codes.
iupacToAminoAcid :: (IsString s, Ord s) => Bimap (AmbiguityGroup s) (AmbiguityGroup s)
iupacToAminoAcid = toBimap
    [ ('A', "A")
    , ('B', "DN")
    , ('C', "C")
    , ('D', "D")
    , ('E', "E")
    , ('F', "F")
    , ('G', "G")
    , ('H', "H")
    , ('I', "I")
    , ('K', "K")
    , ('L', "L")
    , ('M', "M")
    , ('N', "N")
    , ('P', "P")
    , ('Q', "Q")
    , ('R', "R")
    , ('S', "S")
    , ('T', "T")
    , ('V', "V")
    , ('W', "W")
    , ('X', "ACDEFGHIKLMNPQRSTVWY")
    , ('Y', "Y")
    , ('Z', "EQ")
    , ('-', "-")
    , ('?', "ACDEFGHIKLMNPQRSTVWY-")
    ]


-- |
-- Substitutions for converting to a DNA sequence based on IUPAC codes.
iupacToDna :: (IsString s, Ord s) => Bimap (AmbiguityGroup s) (AmbiguityGroup s)
iupacToDna = toBimap
    [ ('A', "A")
    , ('C', "C")
    , ('G', "G")
    , ('T', "T")
    , ('R', "AG")
    , ('Y', "CT")
    , ('S', "CG")
    , ('W', "AT")
    , ('K', "GT")
    , ('M', "AC")
    , ('B', "CGT")
    , ('D', "AGT")
    , ('H', "ACT")
    , ('V', "ACG")
    , ('N', "ACGT")
    , ('-', "-")
    , ('?', "ACGT-")

    , ('a', "A-")
    , ('c', "C-")
    , ('g', "G-")
    , ('t', "T-")
    , ('r', "AG-")
    , ('y', "CT-")
    , ('s', "CG-")
    , ('w', "AT-")
    , ('k', "GT-")
    , ('m', "AC-")
    , ('b', "CGT-")
    , ('d', "AGT-")
    , ('h', "ACT-")
    , ('v', "ACG-")
    ]


-- |
-- Substitutions for converting to a RNA sequence based on IUPAC codes.
iupacToRna :: (IsString s, Ord s) => Bimap (AmbiguityGroup s) (AmbiguityGroup s)
iupacToRna = BM.mapMonotonic setUpdate $ BM.mapMonotonicR setUpdate iupacToDna
  where
    setUpdate = fmap f
      where
        f x
          | x == fromString "T" = fromString "U"
          | otherwise =  x


toBimap :: (IsString s, Ord s) => [(Char, String)] ->  Bimap (AmbiguityGroup s) (AmbiguityGroup s)
toBimap = BM.fromList . fmap transform
  where
    transform = pure . fromString . pure *** fmap (fromString . pure) . NE.fromList
