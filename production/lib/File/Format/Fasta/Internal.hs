{-# LANGUAGE FlexibleContexts #-}

module File.Format.Fasta.Internal where

import Data.Char           (isSpace)
import Text.Parsec
import Text.Parsec.Custom

fastaLabelDefinition :: Stream s m Char => ParsecT s u m String
fastaLabelDefinition = do
    _     <- char '>'
    _     <- inlineSpaces
    name  <- fastaLabel 
    start <- optionMaybe commentStart
    _     <- case start of
               Just _  -> pure () <* (try commentBody <?> commentMessage name)
               Nothing -> pure ()
    _     <- try eol <?> lineEndMessage name
    pure name
  where
    fastaLabel          = many1 $ satisfy (not . isSpace)
    commentStart        = (char ' ' <* inlineSpaces) <|> (inlineSpaces *> char '$' <* inlineSpaces)
    commentBody         = unwords <$> many (commentWord <* inlineSpaces)
    commentWord         = many1 (satisfy (not . isSpace)) <?> "Non-space characters"
    commentMessage name = "Invalid comment for following label: '" ++ name ++ "'"
    lineEndMessage name = "There is no end-of-line after label: '" ++ name ++ "'"

fastaSequenceDefinition :: Stream s m Char => ParsecT s u m a -> ParsecT s u m [a]
fastaSequenceDefinition c = spaces *> fullSequence
  where
    fullSequence = concat <$> many1 sequenceLine 
    sequenceLine = (c <* inlineSpaces) `manyTill` eol