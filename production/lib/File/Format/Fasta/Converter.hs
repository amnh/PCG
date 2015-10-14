{-# LANGUAGE FlexibleContexts #-}

module File.Format.Fasta.Converter where

import           Data.List                         (intercalate,partition)
import           Data.Map                   hiding (filter,foldr,fromList,partition,null)
import qualified Data.Map                   as M   (fromList)
import qualified Data.Vector                as V   (fromList)
import           File.Format.Fasta.Internal
import           File.Format.Fasta.Parser
import           Text.Parsec
import           Text.Parsec.Custom                (fails)

data FastaSequenceType = DNA | RNA | AminoAcid deriving (Bounded,Eq,Enum,Read,Show)

colate :: FastaSequenceType -> FastaParseResult -> TaxonSequenceMap
colate seqType = foldr f empty
  where
    f (FastaSequence name seq') = insert name (seqCharMapping seqType seq')

fastaStreamConverter :: FastaSequenceType -> FastaParseResult -> ParsecT s u m TaxonSequenceMap
fastaStreamConverter seqType = fmap (colate seqType) . validateStreamConversion seqType 

validateStreamConversion :: FastaSequenceType -> FastaParseResult -> ParsecT s u m FastaParseResult
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

seqCharMapping :: FastaSequenceType -> String -> CharacterSequence 
seqCharMapping seqType = V.fromList . fmap (f seqType)
  where 
    f AminoAcid = pure . pure
    f DNA       = (!) iupacNucleotideSubstitutions
    f RNA       = (!) iupacRNASubstitutions 

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
  ]

iupacRNASubstitutions :: Map Char [String]
iupacRNASubstitutions = insert 'U' ["U"] . delete 'T' $ f <$> iupacNucleotideSubstitutions
  where
    f :: [String] -> [String]
    f = foldr g []
    g "T" xs = "U":xs
    g   x xs =   x:xs
