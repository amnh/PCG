-----------------------------------------------------------------------------
-- |
-- Module      :  File.Format.Fasta.Internal
-- Copyright   :  (c) 2015-2021 Ward Wheeler
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
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module File.Format.Fasta.Internal where

import           Data.Char              (isSpace)
import           Data.Map               (Map)
import           Data.Proxy
import           Data.String
import qualified Data.Text              as T
import qualified Data.Text.Lazy         as LT
import           Data.Text.Short        (ShortText)
import           Data.Vector.NonEmpty   (Vector)
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Custom


-- |
-- Naive representation of a collection taxa sequences
type TaxonSequenceMap  = Map Identifier CharacterSequence


-- |
-- Unique identifier for a taxa
type Identifier        = ShortText


-- |
-- Component of a phylogenetic character
type Symbol            = ShortText


-- |
-- Indexed sequences of 'Symbol's with possible ambiguity at an index
type CharacterSequence = Vector (Vector Symbol)


-- |
-- Parses a line containing the sequence identifier along with an
-- optional conmment which is discarded.
{-# INLINE identifierLine #-}
{-# SPECIALISE identifierLine :: Parsec Void  T.Text Identifier #-}
{-# SPECIALISE identifierLine :: Parsec Void LT.Text Identifier #-}
{-# SPECIALISE identifierLine :: Parsec Void  String Identifier #-}
identifierLine :: (MonadParsec e s m, Token s ~ Char) => m Identifier
identifierLine = do
    _ <- char '>'
    _ <- hspace
    x <- identifier
    _ <- hspace
    _ <- optional (try commentBody <?> commentMessage x)
    _ <- endOfLine <?> lineEndMessage x
    pure $ fromString x
  where
    commentMessage x = "Invalid comment for following label: '" <> x <> "'"
    lineEndMessage x = "There is no end-of-line after label: '" <> x <> "'"


-- |
-- 'Identifier' of a sequence
{-# INLINE identifier #-}
{-# SPECIALISE identifier :: Parsec Void  T.Text String #-}
{-# SPECIALISE identifier :: Parsec Void LT.Text String #-}
{-# SPECIALISE identifier :: Parsec Void  String String #-}
identifier :: (MonadParsec e s m, Token s ~ Char) => m String
identifier = some $ satisfy validIdentifierChar


-- |
-- Defines if a 'Char' is valid to be contained within a sequence 'Identifier'
{-# INLINE validIdentifierChar #-}
validIdentifierChar :: Char -> Bool
validIdentifierChar c = (not . isSpace) c && c /= '$' && c /= ';'


-- |
-- Defines the comment format which can be expected after an identifier
{-# INLINE commentBody #-}
{-# SPECIALISE commentBody :: Parsec Void  T.Text String #-}
{-# SPECIALISE commentBody :: Parsec Void LT.Text String #-}
{-# SPECIALISE commentBody :: Parsec Void  String String #-}
commentBody :: forall e s m. (MonadParsec e s m, Token s ~ Char) => m String
commentBody  = do
    _  <- hspace
    _  <- optional $ char '$' *> hspace
    commentLine
  where
    -- |
    -- Defines the line of a comment
    commentLine =
        let result = takeWhileP (Just "Taxon comment content") $ \x -> x /= '\n' && x /= '\r'
        in  unwords . words . chunkToTokens (Proxy :: Proxy s) <$> result
