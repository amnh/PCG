{-# LANGUAGE FlexibleContexts #-}
module File.Format.TNT.Parser where

{-- TODO:
  - Robust tests
  - Good documentation
  -}

import Data.Bifunctor         (second)
import Data.Char              (isSpace)
import Data.Maybe             (catMaybes)
import Text.Megaparsec
import Text.Megaparsec.Custom
import Text.Megaparsec.Lexer  (integer)
import Text.Megaparsec.Prim   (MonadParsec)

type TaxonInfo     = (TaxonName, TaxonSequence) 
type TaxonName     = String
type TaxonSequence = String

xreadCommand :: MonadParsec s m Char => m [TaxonInfo]
xreadCommand = xreadValidation =<< xreadDefinition
  where
    xreadDefinition :: MonadParsec s m Char => m (Int, Int, [TaxonInfo])
    xreadDefinition = do
      _        <- symbol $ string' "xread"
      _        <- optional $ try $ symbol $ comment (string "'") (string "'")
      numTaxa  <- fromEnum <$> symbol integer
      numChars <- fromEnum <$> symbol integer
      taxaSeqs <- deinterleaveTaxa =<< symbol (some taxonSequence)
      _        <- symbol $ char ';'
      pure (numTaxa, numChars, taxaSeqs)

    deinterleaveTaxa :: MonadParsec s m Char => [TaxonInfo] -> m [TaxonInfo]
    deinterleaveTaxa = undefined

    xreadValidation :: MonadParsec s m Char => (Int, Int, [TaxonInfo]) -> m [TaxonInfo]
    xreadValidation (taxaCount, charCount, taxaSeqs)
      | null errors = pure taxaSeqs
      | otherwise   = fails errors
      where
        errors = catMaybes $ [taxaCountError, charCountError]
        taxaCountError = let taxaLength = length taxaSeqs
                         in if taxaCount == taxaLength
                            then Nothing
                            else Just $ concat
                              [ "The number of taxa specified ("
                              , show taxaCount
                              , ") does not match the number of taxa found ("
                              , show $ length taxaSeqs
                              , ")"
                              ]
        charCountError = case filter ((/= charCount) . snd) . fmap (second length) $ taxaSeqs of
                           [] -> Nothing
                           xs -> Just $ concat
                              [ "The number of characters specified ("
                              , show charCount
                              , ") does not match the number of chararacters found for the following taxa: "
                              , show $ fst <$> xs
                              ]                            

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
