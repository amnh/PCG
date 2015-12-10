{-# LANGUAGE FlexibleContexts #-}

module File.Format.Fasta.Parser where

import Control.Arrow              ((&&&))
import Control.Monad              ((<=<))
import Data.Char                  (isLower,isUpper,toLower,toUpper)
import Data.List                  (nub,partition)
import Data.List.Utility
import Data.Maybe                 (fromJust)
import File.Format.Fasta.Internal
import Text.Megaparsec
import Text.Megaparsec.Custom
import Text.Megaparsec.Prim       (MonadParsec)

-- | Pairing of taxa with an unconverted sequence
data FastaSequence
   = FastaSequence
   { taxonName     :: Identifier
   , taxonSequence :: String
   } deriving (Eq,Show)

-- | Unconverted result of a fasta parse
type FastaParseResult = [FastaSequence]

-- | Consumes a stream of 'Char's and parses the stream into a 'FastaParseResult' that
-- has been validated for information consistency
fastaStreamParser :: MonadParsec s m Char => m FastaParseResult
fastaStreamParser = validate =<< seqTranslation <$> (some fastaTaxonSequenceDefinition <* eof)

-- | Parses a single FASTA defined taxon sequence from a Char stream
fastaTaxonSequenceDefinition :: MonadParsec s m Char => m FastaSequence
fastaTaxonSequenceDefinition = do
    name <- fastaTaxonName
    seq' <- try fastaSequence <?> ("Unable to read character sequence for taxon: '" ++ name ++ "'")
    _    <- space
    pure $ FastaSequence name seq'

-- | Consumes a line from the Char stream and parses a FASTA identifier
fastaTaxonName :: MonadParsec s m Char => m String
fastaTaxonName = identifierLine

-- | Consumes one or more lines from the Char stream to produce a list of Chars
--   constrained to a valid Char alphabet representing possible character states
fastaSequence :: MonadParsec s m Char => m String
fastaSequence = symbolSequence $ oneOf alphabet

-- | Takes a symbol combinator and constructs a combinator which matches
--   many of the symbols seperated by spaces and newlines and the enitire
--   sequence ends in a new line
symbolSequence :: MonadParsec s m Char => m a -> m [a]
symbolSequence sym = space *> fullSequence
  where
    fullSequence = concat <$> some (inlineSpace *> sequenceLine)
    sequenceLine = (sym <* inlineSpace) `manyTill` eol

-- | Various input alphabets
alphabet, otherValidChars, iupacAminoAcidChars, iupacNucleotideChars, iupacRNAChars :: String
alphabet             = unionAll [iupacAminoAcidChars, iupacNucleotideChars, iupacRNAChars]
otherValidChars      = ".-?#"
iupacAminoAcidChars  = caseInsensitiveOptions $ "ACDEFGHIKLMNPQRSTVWY" ++ otherValidChars
iupacNucleotideChars = caseInsensitiveOptions $ "ACGTRYSWKMBDHVN"      ++ otherValidChars
iupacRNAChars        = f <$> iupacNucleotideChars
  where
    f x 
      | x == 'T'  = 'U'
      | x == 't'  = 'u'
      | otherwise = x

-- | Naively takes the union of many lists to a single list
unionAll :: Eq a => [[a]] -> [a]
unionAll = nub . concat

-- | Adds the lowercase and uppercase Chars to string when only the upper or 
--   lower is present in the String
caseInsensitiveOptions :: String -> String
caseInsensitiveOptions = nub . foldr f []
  where
    f x xs
      | isLower x = x : toUpper x : xs
      | isUpper x = toLower x : x : xs
      | otherwise = x : xs

-- | Converts all Chars in the sequence to uppercase
--   This makes all subsequent processing easier 
seqTranslation :: [FastaSequence] -> [FastaSequence]
seqTranslation = foldr f [] 
  where
    f (FastaSequence name seq') a = FastaSequence name (toUpper <$> seq') : a

-- | Ensures that the parsed result has consistent data
validate :: MonadParsec s m Char => FastaParseResult -> m FastaParseResult
validate = validateSequenceConsistency <=< validateIdentifierConsistency


-- | Ensures that there are no duplicate identifiers in the stream
validateIdentifierConsistency :: MonadParsec s m Char => FastaParseResult -> m FastaParseResult
validateIdentifierConsistency xs =
  case dupes of
    [] -> pure xs
    _  -> fails errors
  where
    dupes = duplicates $ taxonName <$> xs
    errors         = errorMessage <$> dupes
    errorMessage x = "Multiple taxon labels found identified by: '"++x++"'" 

-- | Ensures that the charcters are all from a consistent alphabet
validateSequenceConsistency :: MonadParsec s m Char => FastaParseResult -> m FastaParseResult
validateSequenceConsistency = validateConsistentPartition <=< validateConsistentAlphabet

validateConsistentAlphabet :: MonadParsec s m Char => FastaParseResult -> m FastaParseResult
validateConsistentAlphabet xs =
  case partition snd results of
    (_,[]) -> pure xs
    (_,ys) -> fails $ errorMessage <$> ys
  where
    results            = validation <$> xs
    validation         = taxonName &&& consistentAlphabet . taxonSequence
    consistentAlphabet  seq' = all (`elem` iupacAminoAcidChars ) seq'
                            || all (`elem` iupacNucleotideChars) seq'
                            || all (`elem` iupacRNAChars       ) seq'
    errorMessage (n,_) = concat 
                       [ "Error in sequence for taxon name: '"
                       ,  n
                       , "' the sequence data includes characters from multiple data formats. "
                       , "Check this taxon's sequence to ensure that it contains characted codes "
                       , "from only one data format."
                       ]

validateConsistentPartition :: MonadParsec s m Char => FastaParseResult -> m FastaParseResult
validateConsistentPartition xs
  |  null xs
  || null errors  = pure xs
  |  otherwise    = fails errors
  where
    expectedPartitions     = fromJust . mostCommon $ fst <$> withPartitionCount
    partitionCount         = length . filter (=='#') . taxonSequence
    withPartitionCount     = (partitionCount &&& id) <$> xs
    inconsistentPartitions = filter ((/= expectedPartitions) . fst) withPartitionCount
    errors                 = errorMessage <$> inconsistentPartitions
    errorMessage (actualPartitions, taxa) = concat
      [ "Error in sequence for taxon name: '"
      ,  taxonName taxa
      , "' the sequence includes "
      , show actualPartitions
      , " partition characters ('#'). "
      , "Expecting "
      , show expectedPartitions
      , " partition characters in the sequence."
      ]
