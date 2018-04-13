-----------------------------------------------------------------------------
-- |
-- Module      :  File.Format.Fasta.Internal
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

module File.Format.Fasta.Internal where

import Data.Char              (isSpace)
import Data.List.NonEmpty
import Data.Map               (Map)
import Data.Vector            (Vector)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Custom


-- |
-- Naive representation of a collection taxa sequences
type TaxonSequenceMap  = Map Identifier CharacterSequence


-- |
-- Unique identifier for a taxa 
type Identifier        = String


-- |
-- Component of a phylogenetic character
type Symbol            = String


-- |
-- Indexed sequences of 'Symbol's with possible abiguity at an index
type CharacterSequence = Vector (NonEmpty Symbol)


-- |
-- Parses a line containing the sequence identifier along with an
-- optional conmment which is discarded.
identifierLine :: (MonadParsec e s m, Token s ~ Char) => m Identifier
identifierLine = do
    _ <- char '>'
    _ <- inlineSpace
    x <- identifier 
    _ <- inlineSpace
    _ <- optional (try commentBody <?> commentMessage x)
    _ <- endOfLine <?> lineEndMessage x
    pure x
  where
    commentMessage x = "Invalid comment for following label: '" ++ x ++ "'"
    lineEndMessage x = "There is no end-of-line after label: '" ++ x ++ "'"


-- |
-- 'Identifier' of a sequence
identifier :: (MonadParsec e s m, Token s ~ Char) => m Identifier
identifier = some $ satisfy validIdentifierChar


-- |
-- Defines if a 'Char' is valid to be contained within a sequence 'Identifier'
validIdentifierChar :: Char -> Bool
validIdentifierChar c = (not . isSpace) c && c /= '$'


-- |
-- Defines the comment format which can be expected after an identifier
commentBody :: (MonadParsec e s m, Token s ~ Char) => m String
commentBody  = do
    _       <- inlineSpace
    _       <- optional $ char '$'
    _       <- inlineSpace
    content <- many (commentWord <* inlineSpace)
    pure $ unwords content


-- | Defines the words of a commenty
commentWord :: (MonadParsec e s m, Token s ~ Char) => m String
commentWord  = some (satisfy (not . isSpace)) <?> "Non-space characters"
