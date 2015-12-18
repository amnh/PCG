{-# LANGUAGE FlexibleContexts #-}
module File.Format.TNT.Parser where

import Data.Char              (isSpace)
import Text.Megaparsec
import Text.Megaparsec.Custom
import Text.Megaparsec.Lexer  (integer)
import Text.Megaparsec.Prim   (MonadParsec)

type TaxonInfo     = (TaxonName, TaxonSequence) 
type TaxonName     = String
type TaxonSequence = String

xreadBlock :: MonadParsec s m Char => m [TaxonInfo]
xreadBlock = do
    _        <- symbol $ string' "xread"
    _        <- optional $ try $ symbol $ comment (string "'") (string "'")
    numOne   <- symbol $ integer
    numTwo   <- symbol $ integer
    taxaSeqs <- symbol $ some taxonSequence
    _        <- symbol $ char ';'
    pure taxaSeqs

taxonSequence :: MonadParsec s m Char => m TaxonInfo
taxonSequence = do
    name <- symbol $ taxonName
    seq' <- taxonSeq
    pure (name, seq')
  where
    taxonName     = some validNameChar
    taxonSeq      = validSeqChar `someTill` terminal
    terminal      = whitespaceInline *> endOfLine
    validNameChar = satisfy (\x -> (not . isSpace) x && x /= ';')
    validSeqChar  = oneOf $ ['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z'] ++ "-?"

symbol :: MonadParsec s m Char => m a -> m a
symbol c = c <* whitespace

whitespace :: MonadParsec s m Char => m ()
whitespace = space

whitespaceInline :: MonadParsec s m Char => m ()
whitespaceInline =  inlineSpace
