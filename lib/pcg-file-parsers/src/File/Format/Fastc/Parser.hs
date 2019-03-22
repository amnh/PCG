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

{-# LANGUAGE ApplicativeDo    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

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


import           Data.Char                  (isSpace)
import           Data.List.NonEmpty         (NonEmpty)
import qualified Data.List.NonEmpty         as NE
import           Data.Maybe
import qualified Data.Vector                as V
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
    seq' <- try fastcSymbolSequence <?> ("Unable to read symbol sequence for label: '" <> name <> "'")
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
-- Parses an ambiguity group of symbols. Ambiguity groups are enclosed by square
-- brackets and delimited by whitespace.
ambiguityGroup :: (MonadParsec e s m, Token s ~ Char) => m (NonEmpty String)
ambiguityGroup = start *> group <* close
  where
    start = char '[' <* inlineSpace
    close = char ']' <* inlineSpace
    group = NE.fromList <$> (validSymbol `sepBy1` inlineSpace)


-- |
-- Parses a 'Symbol' token ending with whitespace and excluding the forbidden
-- characters: '[\'>\',\'[\',\']\']'.
validSymbol :: (MonadParsec e s m, Token s ~ Char) => m String
validSymbol = do
    syn <- syntenyDefinition <* notFollowedBy space1
    res <- validSymbolChars  <* inlineSpace
    pure $ handleSynteny syn res
  where
    syntenyDefinition = optional (char '~') <?> "synteny specification prefix: '~'"

    handleSynteny x
      | isJust x  = reverse
      | otherwise = id

    validSymbolChars = some validSymbolChar

    -- Techniclly we could relax the grammar in the following ways:
    --   * To only reject '>' and '[' only as the first character of a symbol
    --   * To only reject ']' as the last charcter of a symbol
    --   * Always reject ';'
    --
    -- This would assume that once a symbol has been started with a printable
    -- characater other than '>' or '[', that either of these characters can
    -- occur within the symbol, since we would know from context that niether
    -- would be the start of an ambiguity group or taxon identifier because a
    -- space character would need to be encountered first.
    --
    -- Additionally this would assume that if ']' was encountered and followed
    -- by another valid symbol character, then the ']' must be part of the
    -- symbol and not be the end of an ambiguity group.
    --
    -- However, this dramatically complicates the BNF grammar definition.
    -- Consequently, I have opted to use the simpler definition.
    validSymbolChar = satisfy ( \x -> x /= '>' -- need to be able to match new taxa lines
                                   && x /= ';' -- need to be able to start comments
                                   && x /= '[' -- need to be able to start an ambiguity list
                                   && x /= ']' -- need to be able to end   an ambiguity list
                                   && (not . isSpace) x
                              ) <?> "printable character that is not '>', ';', '[', or ']'"
