-----------------------------------------------------------------------------
-- |
-- Module      :  File.Format.Fastc.Parser
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Utility functions used for parsing both FASTA & FASTC file formats.
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module File.Format.Fastc.Parser 
  ( CharacterSequence
  , FastcParseResult
  , FastcSequence(..)
  , Identifier
  , Symbol
  , fastcStreamParser
  , fastcSymbolSequence
  , fastcTaxonSequenceDefinition
  ) where


import           Data.Char                 (isSpace)
import           Data.List.NonEmpty        (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Vector        as V
import           File.Format.Fasta.Internal
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Custom


-- |
-- Unconverted result of a fastc parse
type FastcParseResult = NonEmpty FastcSequence


-- |
-- Pairing of taxa label with an unconverted sequence
data FastcSequence
   = FastcSequence
   { fastcLabel   :: Identifier
   , fastcSymbols :: CharacterSequence
   } deriving (Eq,Show)


-- |
-- Consumes a stream of 'Char's and parses the stream into a 'FastcParseResult'
fastcStreamParser :: (MonadParsec e s m, Token s ~ Char) => m FastcParseResult
fastcStreamParser = NE.fromList <$> some fastcTaxonSequenceDefinition <* eof


-- |
-- Parses a FASTC 'Identifier' and the associated sequence, discarding any
-- comments
fastcTaxonSequenceDefinition :: (MonadParsec e s m, Token s ~ Char) => m FastcSequence
fastcTaxonSequenceDefinition = do
    name <- identifierLine
    seq' <- try fastcSymbolSequence <?> ("Unable to read symbol sequence for label: '" ++ name ++ "'")
    _    <- space
    pure $ FastcSequence name seq'


-- |
-- Parses a sequence of 'Symbol's represneted by a 'CharacterSequence'.
-- Symbols can be multi-character and are assumed to be seperated by whitespace.
fastcSymbolSequence :: (MonadParsec e s m, Token s ~ Char) => m CharacterSequence
fastcSymbolSequence = V.fromList <$> (space *> fullSequence)
  where
    fullSequence = concat <$> some (inlineSpace *> sequenceLine)
    sequenceLine = (symbolGroup <* inlineSpace) `manyTill` endOfLine


-- |
-- Parses either an ambiguity group of 'Symbol's or a single, unambiguous
-- 'Symbol'.
symbolGroup :: (MonadParsec e s m, Token s ~ Char) => m (NonEmpty String)
symbolGroup = ambiguityGroup <|> (pure <$> validSymbol)


-- |
-- Parses an ambiguity group of symbols. Ambiguity groups are delimited by the
-- '\'|\'' character.
ambiguityGroup :: (MonadParsec e s m, Token s ~ Char) => m (NonEmpty String)
ambiguityGroup = NE.fromList <$> (validSymbol `sepBy1` (char '|' <* inlineSpace))


-- |
-- Parses a 'Symbol' token ending with whitespace and excluding the forbidden
-- characters: '[\'>\',\'|\']'.
validSymbol :: (MonadParsec e s m, Token s ~ Char) => m String
validSymbol = (validStartChar <:> many validBodyChar) <* inlineSpace
  where
    validStartChar = satisfy $ \x -> x /= '>' -- need to be able to match new taxa lines
                                  && x /= '|' -- need to be able to start an ambiguity list 
                                  && (not . isSpace) x
    validBodyChar  = satisfy $ \x -> x /= '|' -- need to be able to end an ambiguity sequence
                                  && (not . isSpace) x
