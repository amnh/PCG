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

{-# LANGUAGE ApplicativeDo       #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

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


import           Control.DeepSeq                    (NFData, force)
import           Control.Monad.Combinators.NonEmpty
import           Data.Char                          (isSpace)
import           Data.List.NonEmpty                 (NonEmpty (..))
import           Data.Maybe
import           Data.Proxy
import           Data.Semigroup.Foldable
import           Data.String
import           Data.Text.Short                    (ShortText, toString)
import qualified Data.Text.Short                    as T
import           Data.Vector.NonEmpty               (Vector)
import qualified Data.Vector.NonEmpty               as V
import           File.Format.Fasta.Internal
import           GHC.Generics                       (Generic)
import           Text.Megaparsec                    hiding (sepBy1, some)
import           Text.Megaparsec.Char
import           Text.Megaparsec.Custom


-- |
-- Unconverted result of a fastc parse
type FastcParseResult = NonEmpty FastcSequence


-- |
-- Pairing of taxa label with an unconverted sequence
data FastcSequence
   = FastcSequence
   { fastcLabel   :: {-# UNPACK #-} !Identifier
   , fastcSymbols :: {-# UNPACK #-} !CharacterSequence
   } deriving (Eq, Generic, NFData, Show)


-- |
-- Consumes a stream of 'Char's and parses the stream into a 'FastcParseResult'
{-# INLINEABLE fastcStreamParser #-}
fastcStreamParser :: (MonadParsec e s m, Token s ~ Char) => m FastcParseResult
fastcStreamParser = some fastcTaxonSequenceDefinition <* eof


-- |
-- Parses a FASTC 'Identifier' and the associated sequence, discarding any
-- comments
{-# INLINEABLE fastcTaxonSequenceDefinition #-}
fastcTaxonSequenceDefinition :: (MonadParsec e s m, Token s ~ Char) => m FastcSequence
fastcTaxonSequenceDefinition = do
    name <- identifierLine
    seq' <- try fastcSymbolSequence >>= toSequence name
    _    <- space
    pure $ FastcSequence name seq'
  where
    toSequence name    []  = fail $ "Empty character sequence for " <> toString name
    toSequence    _ (x:xs) = pure . V.fromNonEmpty $ x:|xs


-- |
-- Parses a sequence of 'Symbol's represneted by a 'CharacterSequence'.
-- Symbols can be multi-character and are assumed to be seperated by whitespace.
{-# INLINEABLE fastcSymbolSequence #-}
fastcSymbolSequence :: (MonadParsec e s m, Token s ~ Char) => m [Vector ShortText]
fastcSymbolSequence = space *> fullSequence
  where
    fullSequence = fold1 <$> some (inlineSpace *> sequenceLine)
    sequenceLine = (symbolGroup <* inlineSpace) `manyTill` endOfLine


-- |
-- Parses either an ambiguity group of 'Symbol's or a single, unambiguous
-- 'Symbol'.
{-# INLINE symbolGroup #-}
symbolGroup :: (MonadParsec e s m, Token s ~ Char) => m (Vector ShortText)
symbolGroup = ambiguityGroup <|> (pure <$> validSymbol)


-- |
-- Parses an ambiguity group of symbols. Ambiguity groups are enclosed by square
-- brackets and delimited by whitespace.
{-# INLINE ambiguityGroup #-}
ambiguityGroup :: (MonadParsec e s m, Token s ~ Char) => m (Vector ShortText)
ambiguityGroup = start *> group <* close
  where
    start = char '[' <* inlineSpace
    close = char ']' <* inlineSpace
    group = force . V.fromNonEmpty <$> (validSymbol `sepBy1` inlineSpace)


-- |
-- Parses a 'Symbol' token ending with whitespace and excluding the forbidden
-- characters: '[\'>\',\'[\',\']\']'.
{-# INLINE validSymbol #-}
validSymbol :: forall e s m. (MonadParsec e s m, Token s ~ Char) => m ShortText
validSymbol = do
    syn <- syntenyDefinition <* notFollowedBy space1
    res <- validSymbolChars  <* inlineSpace
    pure . force $ handleSynteny syn res
  where
    syntenyDefinition = optional (char '~') <?> "synteny specification prefix: '~'"

    handleSynteny x
      | isJust x  = T.reverse
      | otherwise = id

    validSymbolChars = fromString . chunkToTokens (Proxy :: Proxy s) <$> symbolStr
      where
        symbolStr = takeWhile1P Nothing validSymbolChar <?>
            "printable characters that are not '>', ';', '[', or ']'"

    -- Technically we could relax the grammar in the following ways:
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
    validSymbolChar x = x /= '>' -- need to be able to match new taxa lines
                     && x /= ';' -- need to be able to start comments
                     && x /= '[' -- need to be able to start an ambiguity list
                     && x /= ']' -- need to be able to end   an ambiguity list
                     && (not . isSpace) x

