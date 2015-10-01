{-# LANGUAGE FlexibleContexts #-}

module File.Format.Fastc.Parser
  ( FastcData(..) 
  , parseFastcStream
  ) where

import Data.Char  (isSpace)
import Data.Maybe (catMaybes)
import File.Format.Fasta.Internal
import Safe       (headMay)
import Text.Parsec
import Text.Parsec.Custom

data FastcData 
   = FastcData
   { fastcLabel   ::  String
   , fastcSymbols :: [String]
   } deriving (Show)

parseFastcStream :: Stream s m Char => s -> m (Either ParseError [FastcData])
parseFastcStream = runParserT (many1 fastaTaxonSequenceDefinition <* eof) () "A list of taxon names and corresponding character sequences"

fastaTaxonSequenceDefinition :: Stream s m Char => ParsecT s u m FastcData
fastaTaxonSequenceDefinition = do
    name    <- fastaLabelDefinition
    symbols <- try symbolSequence <?> ("Unable to read symbol sequence for label: '" ++ name ++ "'")
    _       <- spaces
    pure $ FastcData name symbols

symbolSequence :: Stream s m Char => ParsecT s u m [String]
symbolSequence = fastaSequenceDefinition validSymbols
  where
    validSymbols = validStart <:> many (satisfy (not .isSpace))
    validStart   = satisfy $ \x -> x /= '>' && (not . isSpace) x
