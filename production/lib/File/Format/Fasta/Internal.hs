{-# LANGUAGE FlexibleContexts #-}

module File.Format.Fasta.Internal where

import Data.Char              (isSpace)
import Data.Map               (Map)
import Data.Vector            (Vector)
import Text.Megaparsec
import Text.Megaparsec.Custom
import Text.Megaparsec.Prim   (MonadParsec)

type TaxonSequenceMap  = Map Identifier CharacterSequence
type Identifier        = String
type Symbol            = String
type CharacterSequence = Vector [Symbol]

identifierLine :: MonadParsec s m Char => m Identifier
identifierLine = do
    _ <- char '>'
    _ <- inlineSpaces
    x <- identifier 
    _ <- inlineSpaces
    _ <- optional (try commentBody <?> commentMessage x)
    _ <- endOfLine <?> lineEndMessage x
    pure x
  where
    commentMessage x = "Invalid comment for following label: '" ++ x ++ "'"
    lineEndMessage x = "There is no end-of-line after label: '" ++ x ++ "'"

identifier :: MonadParsec s m Char => m Identifier
identifier = some $ satisfy validIdentifierChar

validIdentifierChar :: Char -> Bool
validIdentifierChar c = (not . isSpace) c && c /= '$'

commentBody :: MonadParsec s m Char => m String
commentBody  = do
    _       <- inlineSpaces
    _       <- optional $ char '$'
    _       <- inlineSpaces
    content <- many (commentWord <* inlineSpaces)
    pure $ unwords content

commentWord :: MonadParsec s m Char => m String
commentWord  = some (satisfy (not . isSpace)) <?> "Non-space characters"
