{-# LANGUAGE FlexibleContexts #-}

module File.Format.Fasta.Parser where

import           Control.Arrow                     ((&&&))
import           Control.Monad                     ((<=<))
import           Data.Char                         (isLower,isUpper,toLower,toUpper)
import           Data.List                         (nub,partition,sortBy)
import           Data.List.Utility
import           Data.Map                          (empty,insertWith,toList)
import           Data.Ord                          (comparing) 
import           File.Format.Fasta.Internal
import           Text.Parsec
import           Text.Parsec.Custom

data FastaSequence
   = FastaSequence
   { taxonName     :: Identifier
   , taxonSequence :: String
   } deriving (Eq,Show)
type FastaParseResult = [FastaSequence]

-- | Consumes a Char stream and parses the stream into a FastaParseResult that
--   has been validated for information consistency
fastaStreamParser :: Stream s m Char => ParsecT s u m FastaParseResult
fastaStreamParser = validate =<< seqTranslation <$> (many1 fastaTaxonSequenceDefinition <* eof)

-- | Parses a single FASTA defined taxon sequence from a Char stream
fastaTaxonSequenceDefinition :: Stream s m Char => ParsecT s u m FastaSequence
fastaTaxonSequenceDefinition = do
    name <- fastaTaxonName
    seq' <- try fastaSequence <?> ("Unable to read character sequence for taxon: '" ++ name ++ "'")
    _    <- spaces
    pure $ FastaSequence name seq'

-- | Consumes a line from the Char stream and parses a FASTA identifier
fastaTaxonName :: Stream s m Char => ParsecT s u m String
fastaTaxonName = identifierLine

-- | Consumes one or more lines from the Char stream to produce a list of Chars
--   constrained to a valid Char alphabet representing possible character states
fastaSequence :: Stream s m Char => ParsecT s u m String
fastaSequence = symbolSequence $ oneOf alphabet

-- | Takes a symbol combinator and constructs a combinator which matches
--   many of the symbols seperated by spaces and newlines and the enitire
--   sequence ends in a new line
symbolSequence :: Stream s m Char => ParsecT s u m a -> ParsecT s u m [a]
symbolSequence sym = spaces *> fullSequence
  where
    fullSequence = concat <$> many1 (inlineSpaces *> sequenceLine)
    sequenceLine = (sym <* inlineSpaces) `manyTill` eol

-- | Various input alphabets
alphabet, otherValidChars, iupacAminoAcidChars, iupacNucleotideChars, iupacRNAChars :: String
alphabet             = unionAll [iupacAminoAcidChars, iupacNucleotideChars, iupacRNAChars]
otherValidChars      = ".-?#"
iupacAminoAcidChars  = caseInsensitiveOptions $ "ACDEFGHIKLMNPQRSTVWY" ++ otherValidChars
iupacNucleotideChars = caseInsensitiveOptions $ "ACGTRYSWKMBDHVN"      ++ otherValidChars
iupacRNAChars        = f <$> iupacNucleotideChars
  where
    f x = if x == 'T'
          then 'U'
          else if x == 't'
          then 'u'
          else x

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
validate :: Stream s m Char => FastaParseResult ->  ParsecT s u m FastaParseResult
validate = validateSequenceConsistency <=< validateIdentifierConsistency


-- | Ensures that there are no duplicate identifiers in the stream
validateIdentifierConsistency :: Stream s m Char => FastaParseResult -> ParsecT s u m FastaParseResult
validateIdentifierConsistency xs = do
    case dupes of
      [] -> pure xs
      _  -> fails errorMessages
  where
    dupes = duplicates $ taxonName <$> xs
    errorMessages  = errorMessage <$> dupes
    errorMessage x = "Multiple taxon labels found identified by: '"++x++"'" 

-- | Ensures that the charcters are all from a consistent alphabet
validateSequenceConsistency :: Stream s m Char => FastaParseResult -> ParsecT s u m FastaParseResult
validateSequenceConsistency = validateConsistentPartition <=< validateConsistentAlphabet

validateConsistentAlphabet :: Stream s m Char => FastaParseResult -> ParsecT s u m FastaParseResult
validateConsistentAlphabet xs = do
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

validateConsistentPartition :: Stream s m Char => FastaParseResult -> ParsecT s u m FastaParseResult
validateConsistentPartition xs = do
    case consistentPartition xs of
      [] -> pure xs
      ys -> fails $ errorMessage <$> ys
  where
    consistentPartition = interpretOccuranceMap . collateOccuranceMap . buildOccuranceMap
    buildOccuranceMap = foldr f empty
      where
        f e = insertWith (++) (partitionChars e) [taxonName e]
        partitionChars = length . filter (=='#') . taxonSequence
    collateOccuranceMap = sortBy comparator . toList
      where
        comparator x y = descending $ comparing (length . snd) x y
        descending LT  = GT
        descending GT  = LT
        descending x   = x
    interpretOccuranceMap []         = []
    interpretOccuranceMap [_]        = []
    interpretOccuranceMap ((n,_):ys) = concat $ errorTokens <$> ys
      where
        errorTokens (m,zs) = (\z -> (n,m,z)) <$> zs
    errorMessage (expected,actual,name) = concat
      [ "Error in sequence for taxon name: '"
      ,  name
      , "' the sequence includes "
      , show actual
      , " partition characters ('#'). "
      , "Expecting "
      , show expected
      , " partition characters in the sequence."
      ]

