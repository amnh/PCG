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

import           Data.Char                 (isSpace)
import           Data.List.NonEmpty hiding (fromList)
import qualified Data.List.NonEmpty as NE  (fromList)
import qualified Data.Vector        as V   (fromList)
import           File.Format.Fasta.Internal
import           Text.Megaparsec
import           Text.Megaparsec.Custom
import           Text.Megaparsec.Prim      (MonadParsec)

-- | Unconverted result of a fastc parse
type FastcParseResult = NonEmpty FastcSequence

-- | Pairing of taxa label with an unconverted sequence
data FastcSequence
   = FastcSequence
   { fastcLabel   :: Identifier
   , fastcSymbols :: CharacterSequence
   } deriving (Eq,Show)

-- | Consumes a stream of 'Char's and parses the stream into a 'FastcParseResult'
fastcStreamParser :: MonadParsec s m Char => m FastcParseResult
fastcStreamParser = NE.fromList <$> some fastcTaxonSequenceDefinition <* eof

fastcTaxonSequenceDefinition :: MonadParsec s m Char => m FastcSequence
fastcTaxonSequenceDefinition = do
    name <- identifierLine
    seq' <- try fastcSymbolSequence <?> ("Unable to read symbol sequence for label: '" ++ name ++ "'")
    _    <- space
    pure $ FastcSequence name seq'

fastcSymbolSequence :: MonadParsec s m Char => m CharacterSequence
fastcSymbolSequence = V.fromList <$> (space *> fullSequence)
  where
    fullSequence = concat <$> some (inlineSpace *> sequenceLine)
    sequenceLine = (symbolGroup <* inlineSpace) `manyTill` endOfLine

symbolGroup :: MonadParsec s m Char => m [String]
symbolGroup = ambiguityGroup
          <|> (pure <$> validSymbol)

ambiguityGroup :: MonadParsec s m Char => m [String]
ambiguityGroup = validSymbol `sepBy1` (char '|' <* inlineSpace)

validSymbol :: MonadParsec s m Char => m String
validSymbol = (validStartChar <:> many validBodyChar) <* inlineSpace
  where
    validStartChar = satisfy $ \x -> x /= '>' -- need to be able to match new taxa lines
                                  && x /= '|' -- need to be able to start an ambiguity list 
                                  && (not . isSpace) x
    validBodyChar  = satisfy $ \x -> x /= '|' -- need to be able to end an ambiguity sequence
                                  && (not . isSpace) x
