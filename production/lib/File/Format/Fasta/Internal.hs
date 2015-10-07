{-# LANGUAGE FlexibleContexts #-}

module File.Format.Fasta.Internal where

import Data.Char           (isSpace)
import Text.Parsec
import Text.Parsec.Custom

fastaLabelLine :: Stream s m Char => ParsecT s u m String
fastaLabelLine = do
    _     <- char '>'
    _     <- inlineSpaces
    name  <- fastaLabel 
    _     <- inlineSpaces
    _     <- optional (try commentBody <?> commentMessage name)
    _     <- eol <?> lineEndMessage name
    pure name
  where
    commentMessage x = "Invalid comment for following label: '" ++ x ++ "'"
    lineEndMessage x = "There is no end-of-line after label: '" ++ x ++ "'"

fastaLabel :: Stream s m Char => ParsecT s u m String
fastaLabel = many1 $ satisfy validLabelChar

validLabelChar :: Char -> Bool
validLabelChar c = (not . isSpace) c && c /= '$'

commentBody :: Stream s m Char => ParsecT s u m String
commentBody  = do
    _       <- inlineSpaces
    _       <- optional $ char '$'
    _       <- inlineSpaces
    content <- many1 (commentWord <* inlineSpaces)
    pure $ unwords content

commentWord :: Stream s m Char => ParsecT s u m String
commentWord  = many1 (satisfy (not . isSpace)) <?> "Non-space characters"

fastaSequenceDefinition :: Stream s m Char => ParsecT s u m a -> ParsecT s u m [a]
fastaSequenceDefinition c = spaces *> fullSequence
  where
    fullSequence = concat <$> many1 (inlineSpaces *> sequenceLine)
    sequenceLine = (c <* inlineSpaces) `manyTill` eol
