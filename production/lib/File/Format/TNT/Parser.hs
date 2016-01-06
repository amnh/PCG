{-# LANGUAGE FlexibleContexts #-}
module File.Format.TNT.Parser where

{-- TODO:
  - Robust tests
  - Good documentation
  - Deinterleave function with DList construction
  -}

import           Data.Bifunctor           (second)
import           Data.Char                (isSpace)
import           Data.DList               (DList,append)
import qualified Data.DList         as DL (toList,fromList)
import           Data.List                (intersperse)
import           Data.List.NonEmpty       (NonEmpty)
import qualified Data.List.NonEmpty as NE (filter,fromList,length)
import           Data.Map.Strict          (Map,insertWith)
import qualified Data.Map.Strict    as M  (toList)
import           Data.Maybe               (catMaybes)
import           Text.Megaparsec
import           Text.Megaparsec.Custom
import           Text.Megaparsec.Lexer    (integer,number,signed)
import           Text.Megaparsec.Prim     (MonadParsec)

-- | The sequence information for a taxon within the TNT file's XREAD command.
-- Contains the 'TaxonName' and the naive 'TaxonSequence' 
type TaxonInfo     = (TaxonName, TaxonSequence)

-- | The name of a taxon in a TNT file's XREAD command.
type TaxonName     = String

-- | The naive sequence of a taxon in a TNT files' XREAD command.
type TaxonSequence = String

-- | Parses an XREAD command. Correctly validates for taxa count
-- and character sequence length. Produces one or more taxa sequences.
xreadCommand :: MonadParsec s m Char => m (NonEmpty TaxonInfo)
xreadCommand = xreadValidation =<< xreadDefinition
  where
    xreadDefinition :: MonadParsec s m Char => m (Int, Int, NonEmpty TaxonInfo)
    xreadDefinition = uncurry (,,) <$> xreadPreamble <*> xreadSequences <* symbol (char ';')

    xreadValidation :: MonadParsec s m Char => (Int, Int, NonEmpty TaxonInfo) -> m (NonEmpty TaxonInfo)
    xreadValidation (charCount, taxaCount, taxaSeqs)
      | null errors = pure taxaSeqs
      | otherwise   = fails errors
      where
        errors = catMaybes $ [taxaCountError, charCountError]
        taxaCountError = let taxaLength = NE.length taxaSeqs
                         in if taxaCount == taxaLength
                            then Nothing
                            else Just $ concat
                              [ "The number of taxa specified ("
                              , show taxaCount
                              , ") does not match the number of taxa found ("
                              , show $ length taxaSeqs
                              , ")"
                              ]
        charCountError = case NE.filter ((/= charCount) . snd) . fmap (second length) $ taxaSeqs of
                           [] -> Nothing
                           xs -> Just $ concat
                              [ "The number of characters specified ("
                              , show charCount
                              , ") does not match the number of chararacters found for the following taxa:\n"
                              , unlines $ prettyPrint <$> xs
                              ]                            
        prettyPrint (name, count) = concat ["\t",show name," found (",show count,") characters"]

-- | Consumes everything in the XREAD command prior to the taxa sequences.
-- Produces the expected taxa count and the length of the character sequences.
xreadPreamble :: MonadParsec s m Char => m (Int, Int)
xreadPreamble = xreadHeader *> ((,) <$> xreadCharCount <*> xreadTaxaCount)

-- | The superflous information of an XREAD command.
-- Consumes the XREAD string identifier and zero or more comments
-- preceeding the taxa count and character cound parameters
xreadHeader :: MonadParsec s m Char => m ()
xreadHeader =  symbol (string' "xread")
            *> many simpleComment
            *> pure ()
  where
    simpleComment = delimiter *> anythingTill delimiter <* symbol delimiter
      where
        delimiter = char '\''

-- | The number of taxa present in the XREAD command.
-- __Naturally__ this number must be a positive integer.
xreadTaxaCount :: MonadParsec s m Char => m Int
xreadTaxaCount = symbol $ flexiblePositiveInt "taxa count" 

-- | The number of characters in a taxon sequence for this XREAD command.
-- __Naturally__ this number must be a positive integer.
xreadCharCount :: MonadParsec s m Char => m Int
xreadCharCount = symbol $ flexiblePositiveInt "char count"

-- | Reads one or more taxon sequences.
-- Performs deinterleaving of identically named taxon sequences. 
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> parse xreadSequences "" "taxonOne GATACA\ntaxonTwo GGAATT"
-- Right [ ("taxonOne", "GATACA")
--       , ("taxonTwo", "GGAATT")
--       ]
--
-- Interleaved usage:
--
-- >>> parse xreadSequences "" "taxonOne GATACA\ntaxonTwo GGAATT\ntaxonOne ACATAG\ntaxonTwo CCGATC\n"
-- Right [ ("taxonOne", "GATACAACATAG")
--       , ("taxonTwo", "GGAATTCCGATC")
--       ]
xreadSequences :: MonadParsec s m Char => m (NonEmpty TaxonInfo)
xreadSequences = NE.fromList . deinterleaveTaxa <$> symbol (taxonSequence `sepEndBy1` terminal)
  where
    terminal         = whitespaceInline *> endOfLine <* whitespace
    deinterleaveTaxa :: [TaxonInfo] -> [TaxonInfo]
    deinterleaveTaxa = M.toList . fmap DL.toList . foldr f mempty
      where
        f :: TaxonInfo -> Map TaxonName (DList Char) -> Map TaxonName (DList Char)
        f (taxonName, taxonSequence) = insertWith append taxonName (DL.fromList taxonSequence)

-- | Parses an PROCEDURE command that consisits of exacty
-- one of the following:
--
--  * Close file directive
--
--  * Fasta file to read-in
--
--  * Command file to be interpreted
procCommand :: MonadParsec s m Char => m ()
procCommand =  procHeader *> procBody
  where
    procBody = (try procFastaFile   *> pure ())
           <|> (try procCommandFile *> pure ())
           <|>      procCloseFile

-- | Consumes the superflous heading for a PROCEDURE command.
procHeader :: MonadParsec s m Char => m ()
procHeader = string' "proc" *> optional (string' "edure") *> pure ()

-- | A directive to interpret a file. We throw this info away later.
-- Interpreting a file kinda sucks, this ins't Lisp.
procCommandFile :: MonadParsec s m Char => m FilePath
procCommandFile = anythingTill (whitespace *> char ';') <* trim (char ';')

-- | A directive to read in a FASTA file as aligned, non-addative data.
-- Not sure if we should ignore this or acturally process additional files.
procFastaFile :: MonadParsec s m Char => m FilePath
procFastaFile = symbol (char '&') *> procCommandFile

-- | A close file directive. Closes all open files. Found at the end of all
-- properly formated TNT input files.
procCloseFile :: MonadParsec s m Char => m ()
procCloseFile = symbol (char '/') *> symbol (char ';') *> pure ()
    
-- | Parses a taxon name and sequence of characters for a given character.
-- Character values can be one of 64 states ordered @[0..9,A..Z,a..z]@ and also the Chars @\'-\'@ & @\'?\'@.
-- Taxon name cannot contain spaces or the @\';\'@ character.
taxonSequence :: MonadParsec s m Char => m TaxonInfo
taxonSequence = (,) <$> (symbol taxonName) <*> taxonSeq
  where
    taxonName     = some validNameChar
    taxonSeq      = some validSeqChar
    validNameChar = satisfy (\x -> (not . isSpace) x && x /= ';') -- <* whitespaceInline
    validSeqChar  = oneOf $ ['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z'] ++ "-?"

-- | Parses an positive integer from a variety of representations.
-- Parses both signed integral values and signed floating values
-- if the value is positive and an integer.
--
-- @flexiblePositiveInt labeling@ uses the @labeling@ parameter to
-- improve ParseError generation.
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> parse (flexiblePositiveInt "errorCount") "" "1\n"
-- Right 1
--
-- >>> parse (flexiblePositiveInt "errorCount") "" "0\n"
-- Left 1:2:
-- expecting rest of number
-- The errorCount value (0) is not a positive number
--
-- >>> parse (flexiblePositiveInt "errorCount") "" "42.0\n"
-- Right 42
--
-- >>> parse (flexiblePositiveInt "errorCount") "" "0.1337\n"
-- Left 1:7:
-- expecting 'E', 'e', or rest of number
-- The errorCount value (0.1337) is a real value, not an integral value
-- The errorCount value (0.1337) is not a positive integer
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
        posError    = if x >= 1  then Nothing else Just $ concat ["The ",labeling," value (",show x,") is not a positive integer"]
        intError    = if isInt x then Nothing else Just $ concat ["The ",labeling," value (",show x,") is a real value, not an integral value"]
        isInt n     = n == fromInteger rounded
        rounded     = round x

-- | Consumes trailing whitespace after the parameter combinator.
symbol :: MonadParsec s m Char => m a -> m a
symbol c = c <* whitespace

-- | Consumes trailing whitespace after the parameter combinator.
trim :: MonadParsec s m Char => m a -> m a
trim c = whitespace *> c <* whitespace

-- | Consumes zero or more whitespace characters __including__ line breaks.
whitespace :: MonadParsec s m Char => m ()
whitespace = space

-- | Consumes zero or more whitespace characters that are not line breaks.
whitespaceInline :: MonadParsec s m Char => m ()
whitespaceInline =  inlineSpace
