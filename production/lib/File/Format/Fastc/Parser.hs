{-# LANGUAGE FlexibleContexts #-}

module File.Format.Fastc.Parser
  ( FastcData(..) 
  , parseFastcStream
  ) where

import Data.Char  (isSpace)
import File.Format.Fasta.Internal
import Text.Parsec
import Text.Parsec.Custom
--import Text.Megaparsec

data FastcData 
   = FastcData
   { fastcLabel   ::  String
   , fastcSymbols :: [String]
   } deriving (Show)

parseFastcStream :: Stream s m Char => ParsecT s u m [FastcData]
parseFastcStream = many1 fastaTaxonSequenceDefinition <* eof

fastaTaxonSequenceDefinition :: Stream s m Char => ParsecT s u m FastcData
fastaTaxonSequenceDefinition = do
    name <- identifierLine
    seq' <- try symbols <?> ("Unable to read symbol sequence for label: '" ++ name ++ "'")
    _    <- spaces
    pure $ FastcData name seq'

symbols :: Stream s m Char => ParsecT s u m [String]
symbols = symbolSequence validSymbols
  where
    validSymbols = validStart <:> many (satisfy (not .isSpace))
    validStart   = satisfy $ \x -> x /= '>' && (not . isSpace) x
