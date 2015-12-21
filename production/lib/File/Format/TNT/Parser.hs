{-# LANGUAGE FlexibleContexts #-}
module File.Format.TNT.Parser where

{-- TODO:
  - Robust tests
  - Good documentation
  -}

import           Data.Bifunctor         (second)
import           Data.Char              (isSpace)
import           Data.DList             (DList,append)
import qualified Data.DList as DL       (toList,fromList)
import           Data.Map.Strict        (Map,insertWith)
import qualified Data.Map.Strict as M   (toList)
import           Data.Maybe             (catMaybes)
import           Text.Megaparsec
import           Text.Megaparsec.Custom
import           Text.Megaparsec.Lexer  (integer,number)
import           Text.Megaparsec.Prim   (MonadParsec)

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
      taxaSeqs <- deinterleaveTaxa <$> symbol (some taxonSequence)
      _        <- symbol $ char ';'
      pure (numTaxa, numChars, taxaSeqs)

{-
    xreadPreamble :: MonadParsec s m Char => m (Either Integer Double, Either Integer Double)
    xreadPreamble = do
      _        <- symbol $ string' "xread"
      _        <- optional $ try $ symbol $ comment (string "'") (string "'")
      numTaxa  <- symbol $ signed number
      numChars <- symbol $ signed number
      pure (numTaxa,numChars)

    xreadPositiveInts :: MonadParsec s m Char => (Either Integer Double, Either Integer Double) -> m (Int, Int)
    xreadPositiveInts (x,y) = do
      case (x,y) of
        (Left taxaCount, Left charCount) = pure $ (fromEnum taxaCount, fromEnum charCount)
        (Right _       , Left _        ) =
        (

      where
        validateNumber :: Either Integer Double -> m Int
        
        isInt x = x == fromInteger (round x)
-}        
    deinterleaveTaxa :: [TaxonInfo] -> [TaxonInfo]
    deinterleaveTaxa = M.toList . fmap DL.toList . foldr f mempty 
      where
        f :: TaxonInfo -> Map TaxonName (DList Char) -> Map TaxonName (DList Char)
        f (taxonName, taxonSequence) = insertWith append taxonName (DL.fromList taxonSequence)

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

xreadPreamble :: MonadParsec s m Char => m (Int, Int)
xreadPreamble = xreadHeader *> ((,) <$> xreadTaxaCount <*> xreadCharCount)

xreadHeader :: MonadParsec s m Char => m ()
xreadHeader =  symbol (string' "xread")
            *> optional (try $ symbol $ comment (string "'") (string "'"))
            *> pure ()


xreadTaxaCount :: MonadParsec s m Char => m Int
xreadTaxaCount = symbol $ flexiblePositiveInt "taxa count" 

xreadCharCount :: MonadParsec s m Char => m Int
xreadCharCount = symbol $ flexiblePositiveInt "char count" 
                  
flexiblePositiveInt :: MonadParsec s m Char => String -> m Int
flexiblePositiveInt = undefined

taxonSequence :: MonadParsec s m Char => m TaxonInfo
taxonSequence = (,) <$> (symbol taxonName) <*> taxonSeq
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
