{-# LANGUAGE FlexibleContexts #-}
module File.Format.TNT.Command.XRead where

{-- TODO:
  - Robust tests
  - Good documentation
  - Deinterleave function with DList construction
  -}

import           Data.Bifunctor           (second)
import           Data.Char                (isSpace)
import           Data.DList               (DList,append)
import qualified Data.DList         as DL (toList,fromList)
import           Data.IntSet              (IntSet, singleton)
import qualified Data.IntSet        as IS (fromList)
import           Data.List                (intersperse)
import           Data.List.NonEmpty       (NonEmpty)
import qualified Data.List.NonEmpty as NE (filter,fromList,length)
import           Data.Map.Strict          (Map,insertWith)
import qualified Data.Map.Strict    as M  (toList)
import           Data.Maybe               (catMaybes)
import           File.Format.TNT.Internal
import           Text.Megaparsec
import           Text.Megaparsec.Custom
import           Text.Megaparsec.Lexer    (integer,number,signed)
import           Text.Megaparsec.Prim     (MonadParsec)

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
        errors = catMaybes [taxaCountError, charCountError]
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
        f :: TaxonInfo -> Map TaxonName (DList String) -> Map TaxonName (DList String)
        f (taxonName, taxonSequence) = insertWith append taxonName (DL.fromList taxonSequence)

-- | Parses a taxon name and sequence of characters for a given character.
-- Character values can be one of 64 states ordered @[0..9,A..Z,a..z]@ and also the Chars @\'-\'@ & @\'?\'@.
-- Taxon name cannot contain spaces or the @\';\'@ character.
taxonSequence :: MonadParsec s m Char => m TaxonInfo
taxonSequence = (,) <$> symbol taxonName <*> taxonSeq
  where
    taxonName     = some  validNameChar
    taxonSeq      = some (seqChar <|> ambiguity)
    seqChar       = pure <$> validSeqChar <* whitespaceInline
    ambiguity     = char '[' *> some (validSeqChar <* whitespaceInline) <* char ']' <* whitespaceInline
    validNameChar = satisfy (\x -> (not . isSpace) x && x /= ';')
    validSeqChar  = oneOf $ ['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z'] ++ "-?"
    
