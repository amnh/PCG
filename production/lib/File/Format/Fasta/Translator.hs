{-# LANGUAGE FlexibleContexts #-}

module File.Format.Fasta.Translator where

import           Data.Map                   hiding (foldr,fromList,partition, (!))
import           Data.Key                          ((!), lookup)
import qualified Data.Map                   as M   (fromList)
import qualified Data.Vector                as V   (fromList)
import           File.Format.Fasta.Internal
import           File.Format.Fasta.Parser
import           Text.Parsec
--import           Text.Parsec.Custom                (fails)

data FastaSequenceType = DNA | RNA | AminoAcid deriving (Bounded,Eq,Enum,Read,Show)

colate :: FastaSequenceType -> FastaParseResult -> TaxonSequenceMap
colate seqType = foldr f empty
  where
    f (FastaSequence name seq') = insert name (seqCharMapping seqType seq')

fastaStreamTranslator :: FastaSequenceType -> FastaParseResult -> ParsecT s u m TaxonSequenceMap
fastaStreamTranslator seqType = fmap (colate seqType) . validateInterpretedStream seqType 

validateInterpretedStream :: FastaSequenceType -> FastaParseResult -> ParsecT s u m FastaParseResult
validateInterpretedStream = undefined

seqCharMapping :: FastaSequenceType -> String -> CharacterSequence 
seqCharMapping seqType = V.fromList . fmap (f seqType)
  where 
    expandOrId x m = fromMaybe x $ x `lookup` m
    f AminoAcid = pure . pure
    f DNA       = expandOrId iupacNucleotideSubstitutions
    f RNA       = expandOrId iupacRNASubstitutions 

iupacNucleotideSubstitutions :: Map Char [String]
iupacNucleotideSubstitutions = 
  fmap pure <$> M.fromList 
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

iupacRNASubstitutions :: Map Char [String]
iupacRNASubstitutions = insert 'U' ["U"] . delete 'T' $ f <$> iupacNucleotideSubstitutions
  where
    f :: [String] -> [String]
    f = foldr g []
    g "T" xs = "U":xs
    g   x xs =   x:xs
