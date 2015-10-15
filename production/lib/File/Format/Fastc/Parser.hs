{-# LANGUAGE FlexibleContexts #-}

module File.Format.Fastc.Parser 
  ( CharacterSequence
  , FastcParseResult
  , FastcSequence(..)
  , Identifier
  , fastcStreamParser
  , fastcSymbolSequence
  , fastcTaxonSequenceDefinition
  ) where

import Data.Char                  (isSpace)
import Data.Vector                (fromList)
import File.Format.Fasta.Internal
import Text.Parsec
import Text.Parsec.Custom

type FastcParseResult = [FastcSequence]
data FastcSequence
   = FastcSequence
   { fastcLabel   :: Identifier
   , fastcSymbols :: CharacterSequence
   } deriving (Eq,Show)

fastcStreamParser :: Stream s m Char => ParsecT s u m FastcParseResult
fastcStreamParser = many1 fastcTaxonSequenceDefinition <* eof

fastcTaxonSequenceDefinition :: Stream s m Char => ParsecT s u m FastcSequence
fastcTaxonSequenceDefinition = do
    name <- identifierLine
    seq' <- try fastcSymbolSequence <?> ("Unable to read symbol sequence for label: '" ++ name ++ "'")
    _    <- spaces
    pure $ FastcSequence name seq'

fastcSymbolSequence :: Stream s m Char =>  ParsecT s u m CharacterSequence
fastcSymbolSequence = fromList <$> (spaces *> fullSequence)
  where
    fullSequence = concat <$> many1 (inlineSpaces *> sequenceLine)
    sequenceLine = (symbolGroup <* inlineSpaces) `manyTill` eol

symbolGroup :: Stream s m Char => ParsecT s u m [String]
symbolGroup = ambiguityGroup
          <|> (pure <$> validSymbol)

ambiguityGroup :: Stream s m Char => ParsecT s u m [String]
ambiguityGroup = validSymbol `sepBy1` (char '|' <* inlineSpaces)

validSymbol :: Stream s m Char => ParsecT s u m String
validSymbol = (validStartChar <:> many validBodyChar) <* inlineSpaces
  where
    validStartChar = satisfy $ \x -> x /= '>' -- need to be able to match new taxa lines
                                  && x /= '|' -- need to be able to start an ambiguity list 
                                  && (not . isSpace) x
    validBodyChar  = satisfy $ \x -> x /= '|' -- need to be able to end an ambiguity sequence
                                  && (not . isSpace) x
