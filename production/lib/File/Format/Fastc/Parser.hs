{-# LANGUAGE FlexibleContexts #-}

module File.Format.Fastc.Parser
  ( FastcData(..) 
  , parseFastcStream
  ) where

import Data.Char                  (isSpace)
import Data.Vector                (fromList)
import File.Format.Fasta.Internal
import Text.Parsec
import Text.Parsec.Custom
--import Text.Megaparsec

data FastcData 
   = FastcData
   { fastcLabel   :: Identifier
   , fastcSymbols :: CharacterSequence
   } deriving (Show)

parseFastcStream :: Stream s m Char => ParsecT s u m [FastcData]
parseFastcStream = many1 fastcTaxonSequenceDefinition <* eof

fastcTaxonSequenceDefinition :: Stream s m Char => ParsecT s u m FastcData
fastcTaxonSequenceDefinition = do
    name <- identifierLine
    seq' <- try symbolSequence <?> ("Unable to read symbol sequence for label: '" ++ name ++ "'")
    _    <- spaces
    pure $ FastcData name seq'

symbolSequence :: Stream s m Char =>  ParsecT s u m CharacterSequence
symbolSequence = fromList <$> (spaces *> fullSequence)
  where
    fullSequence = concat <$> many1 (inlineSpaces *> sequenceLine)
    sequenceLine = (symbolGroup <* inlineSpaces) `manyTill` eol

symbolGroup :: Stream s m Char => ParsecT s u m [String]
symbolGroup = (pure <$> validSymbol)
          <|> ambiguityGroup

ambiguityGroup :: Stream s m Char => ParsecT s u m [String]
ambiguityGroup = do 
    _ <- char '['
    x <- many1 validSymbol
    _ <- char ']'
    pure x

validSymbol :: Stream s m Char => ParsecT s u m String
validSymbol = validStart <:> many (satisfy (not .isSpace)) <* inlineSpaces
  where
    validStart = satisfy $ \x -> x /= '>' && (not . isSpace) x
