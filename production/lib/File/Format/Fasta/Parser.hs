{-# LANGUAGE FlexibleContexts #-}

module File.Format.Fasta.Parser 
  ( FastaData(..) 
  , parseFastaStream
  ) where

import Data.Maybe (catMaybes)
import File.Format.Fasta.Internal
import Safe       (headMay)
import Text.Parsec

data FastaData 
   = FastaData
   { taxonName   :: String
   , nucleotides :: String
   } deriving (Show)

parseFastaStream :: Stream s m Char => s -> m (Either ParseError [FastaData])
parseFastaStream = runParserT (many1 fastaTaxonSequenceDefinition <* eof) () "A list of taxon names and corresponding character sequences"

fastaTaxonSequenceDefinition :: Stream s m Char => ParsecT s u m FastaData
fastaTaxonSequenceDefinition = do
    name          <- fastaTaxonName
    nukesSequence <- try fastaNucleotides <?> ("Unable to read character sequence for taxon: '" ++ name ++ "'")
    _             <- spaces
    pure $ FastaData name nukesSequence

fastaTaxonName :: Stream s m Char => ParsecT s u m String
fastaTaxonName = fastaLabelDefinition

fastaNucleotides :: Stream s m Char => ParsecT s u m String
fastaNucleotides = catMaybes . (headMay <$>) <$> fastaSequenceDefinition validNucleotideSymbols
  where
    validNucleotideSymbols = foldr1 (<|>) $ string . pure <$> "GATC-"
