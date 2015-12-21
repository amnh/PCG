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
import           Data.List              (intersperse)
import           Data.Map.Strict        (Map,insertWith)
import qualified Data.Map.Strict as M   (toList)
import           Data.Maybe             (catMaybes)
import           Text.Megaparsec
import           Text.Megaparsec.Custom
import           Text.Megaparsec.Lexer  (integer,number,signed)
import           Text.Megaparsec.Prim   (MonadParsec)

type TaxonInfo     = (TaxonName, TaxonSequence) 
type TaxonName     = String
type TaxonSequence = String

xreadCommand :: MonadParsec s m Char => m [TaxonInfo]
xreadCommand = xreadValidation =<< xreadDefinition
  where
    xreadDefinition :: MonadParsec s m Char => m (Int, Int, [TaxonInfo])
    xreadDefinition = uncurry (,,) <$> xreadPreamble <*> xreadSequences <* symbol (char ';')

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
                              , ") does not match the number of chararacters found for the following taxa:\n"
                              , unlines $ prettyPrint <$> xs
                              ]                            
        prettyPrint (name, count) = concat ["\t",show name," found (",show count,") characters"]
                          
xreadPreamble :: MonadParsec s m Char => m (Int, Int)
xreadPreamble = xreadHeader *> ((,) <$> xreadTaxaCount <*> xreadCharCount)

xreadHeader :: MonadParsec s m Char => m ()
xreadHeader =  symbol (string' "xread")
            *> optional (try simpleComment)
            *> pure ()
  where
    simpleComment = delimiter *> anythingTill delimiter <* symbol delimiter
      where
        delimiter = char '\''

xreadTaxaCount :: MonadParsec s m Char => m Int
xreadTaxaCount = symbol $ flexiblePositiveInt "taxa count" 

xreadCharCount :: MonadParsec s m Char => m Int
xreadCharCount = symbol $ flexiblePositiveInt "char count" 
                  
xreadSequences :: MonadParsec s m Char => m [TaxonInfo]
xreadSequences = deinterleaveTaxa <$> symbol (some taxonSequence)
  where
    deinterleaveTaxa :: [TaxonInfo] -> [TaxonInfo]
    deinterleaveTaxa = M.toList . fmap DL.toList . foldr f mempty 
      where
        f :: TaxonInfo -> Map TaxonName (DList Char) -> Map TaxonName (DList Char)
        f (taxonName, taxonSequence) = insertWith append taxonName (DL.fromList taxonSequence)

taxonSequence :: MonadParsec s m Char => m TaxonInfo
taxonSequence = (,) <$> (symbol taxonName) <*> taxonSeq
  where
    taxonName     = some validNameChar
    taxonSeq      = validSeqChar `someTill` terminal
    terminal      = whitespaceInline *> endOfLine
    validNameChar = satisfy (\x -> (not . isSpace) x && x /= ';')
    validSeqChar  = oneOf $ ['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z'] ++ "-?"

flexiblePositiveInt :: MonadParsec s m Char => String -> m Int
flexiblePositiveInt labeling = either coerceIntegral coerceFloating
                             =<< signed whitespace number <?> ("positive integer for " ++ labeling)
  where
    coerceIntegral :: MonadParsec s m Char => Integer -> m Int
    coerceIntegral x
      | x <= 0      = fail $ concat ["The ",labeling," value (",show x,") is not a positive number"]
      | otherwise   = pure $ fromEnum x
    coerceFloating :: MonadParsec s m Char => Double -> m Int
    coerceFloating x
      | null errors = pure $ fromEnum rounded
      | otherwise   = fails errors
      where
        errors      = catMaybes $ [posError,intError]
        posError    = if x <= 0  then Nothing else Just $ concat ["The ",labeling," value (",show x,") is not a positive integer"]
        intError    = if isInt x then Nothing else Just $ concat ["The ",labeling," value (",show x,") is a real value, not an integral value"]
        isInt n     = n == fromInteger rounded
        rounded     = round x

symbol :: MonadParsec s m Char => m a -> m a
symbol c = c <* whitespace

whitespace :: MonadParsec s m Char => m ()
whitespace = space

whitespaceInline :: MonadParsec s m Char => m ()
whitespaceInline =  inlineSpace
