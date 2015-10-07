{-# LANGUAGE FlexibleContexts #-}

module File.Format.Fasta.Internal where

import Data.Char           (isSpace)
import Data.Map            (Map)
import Data.Vector         (Vector)
import Text.Parsec
import Text.Parsec.Custom

type TaxonSequenceMap  = Map Identifier CharacterSequence
type Identifier        = String
type Symbol            = String
type CharacterSequence = Vector [Symbol]

identifierLine :: Stream s m Char => ParsecT s u m Identifier
identifierLine = do
    _ <- char '>'
    _ <- inlineSpaces
    x <- identifier 
    _ <- inlineSpaces
    _ <- optional (try commentBody <?> commentMessage x)
    _ <- eol <?> lineEndMessage x
    pure x
  where
    commentMessage x = "Invalid comment for following label: '" ++ x ++ "'"
    lineEndMessage x = "There is no end-of-line after label: '" ++ x ++ "'"

identifier :: Stream s m Char => ParsecT s u m Identifier
identifier = many1 $ satisfy validIdentifierChar

validIdentifierChar :: Char -> Bool
validIdentifierChar c = (not . isSpace) c && c /= '$'

commentBody :: Stream s m Char => ParsecT s u m String
commentBody  = do
    _       <- inlineSpaces
    _       <- optional $ char '$'
    _       <- inlineSpaces
    content <- many1 (commentWord <* inlineSpaces)
    pure $ unwords content

commentWord :: Stream s m Char => ParsecT s u m String
commentWord  = many1 (satisfy (not . isSpace)) <?> "Non-space characters"

symbolSequence :: Stream s m Char => ParsecT s u m a -> ParsecT s u m [a]
symbolSequence sym = spaces *> fullSequence
  where
    fullSequence = concat <$> many1 (inlineSpaces *> sequenceLine)
    sequenceLine = (sym <* inlineSpaces) `manyTill` eol
