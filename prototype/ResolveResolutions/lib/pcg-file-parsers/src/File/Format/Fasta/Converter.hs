-----------------------------------------------------------------------------
-- |
-- Module      :  File.Format.Fasta.Converter
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Functions for interpreting and converting parsed abiguous FASTA sequences.
--
----------------------------------------------------------------------------- 

{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module File.Format.Fasta.Converter where

import           Data.List                         (intercalate,partition)
import           Data.List.NonEmpty                (NonEmpty) 
import qualified Data.List.NonEmpty         as NE 
import           Data.Map                   hiding (filter,foldr,partition,null)
import qualified Data.Map                   as M   (fromList)
import qualified Data.Vector                as V   (fromList)
import           File.Format.Fasta.Internal
import           File.Format.Fasta.Parser
import           Text.Megaparsec                   (MonadParsec)
import           Text.Megaparsec.Custom            (fails)


-- |
-- Different forms a 'FastaSequence' can be interpreted as.
data FastaSequenceType = DNA | RNA | AminoAcid deriving (Bounded,Eq,Enum,Read,Show)


-- |
-- Define and convert a 'FastaParseResult' to the expected sequence type 
fastaStreamConverter :: MonadParsec e s m => FastaSequenceType -> FastaParseResult -> m TaxonSequenceMap
fastaStreamConverter seqType = fmap (colate seqType) . validateStreamConversion seqType 


-- |
-- Validates that the stream contains a 'FastaParseResult' of the given 'FastaSequenceType'.
validateStreamConversion :: MonadParsec e s m => FastaSequenceType -> FastaParseResult -> m FastaParseResult
validateStreamConversion seqType xs =
  case partition hasErrors result of
    ([] , _) -> pure xs
    (err, _) -> fails $ errorMessage <$> err
  where
    result = containsIncorrectChars <$> xs  
    hasErrors = not . null . snd
    containsIncorrectChars (FastaSequence name seq') = (name, f seq')
    f = g seqType
    g AminoAcid = filter (not . (`elem` iupacAminoAcidChars ))
    g DNA       = filter (not . (`elem` iupacNucleotideChars))
    g RNA       = filter (not . (`elem` iupacRNAChars       ))
    errorMessage (name,badChars) = concat
     [ "In the sequence for taxon: '"
     , name
     , "' the following invalid characters were found: "
     , intercalate ", " $ (\c -> '\'':c:"'") <$> badChars
     ]


-- |
-- Interprets and converts an entire 'FastaParseResult according to the given 'FatsaSequenceType' .
colate :: FastaSequenceType -> FastaParseResult -> TaxonSequenceMap
colate seqType = foldr f empty
  where
    f (FastaSequence name seq') = insert name (seqCharMapping seqType seq')


-- |
-- Interprets and converts an ambiguous sequence according to the given 'FatsaSequenceType'
-- from the ambiguous form to a 'CharacterSequence' based on IUPAC codes.
seqCharMapping :: FastaSequenceType -> String -> CharacterSequence 
seqCharMapping seqType = V.fromList . fmap (f seqType)
  where 
    f AminoAcid = pure . pure
    f DNA       = (!) iupacNucleotideSubstitutions
    f RNA       = (!) iupacRNASubstitutions 


-- |
-- Substitutions for converting to a DNA sequence based on IUPAC codes.
iupacNucleotideSubstitutions :: Map Char (NonEmpty String)
iupacNucleotideSubstitutions = 
    (fmap pure . NE.fromList) <$> M.fromList 
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
    , ('.', "-")
    , ('?', "?")
    , ('#', "#")
    ]


-- |
-- Substitutions for converting to an RNA sequence based on IUPAC codes.
iupacRNASubstitutions :: Map Char (NonEmpty String)
iupacRNASubstitutions = insert 'U' (pure "U") . delete 'T' $ f <$> iupacNucleotideSubstitutions
  where
    f :: NonEmpty String -> NonEmpty String
    f = NE.fromList . foldr g []
    g "T" xs = "U":xs
    g   x xs =   x:xs
