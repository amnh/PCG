-----------------------------------------------------------------------------
-- |
-- Module      :  File.Format.Fasta.Converter
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Functions for interpreting and converting parsed abiguous FASTA sequences.
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module File.Format.Fasta.Converter
  ( FastaSequenceType(..)
  , fastaStreamConverter
  ) where

import           Data.Alphabet.IUPAC
import qualified Data.Bimap                 as BM
import           Data.List                  (intercalate, partition)
import           Data.List.NonEmpty         (NonEmpty)
import           Data.Map                   hiding (filter, foldr, null, partition)
import qualified Data.Vector                as V (fromList)
import           File.Format.Fasta.Internal
import           File.Format.Fasta.Parser
import           Text.Megaparsec            (MonadParsec)
import           Text.Megaparsec.Custom     (fails)


-- |
-- Different forms a 'FastaSequence' can be interpreted as.
data  FastaSequenceType
    = DNA
    | RNA
    | AminoAcid
    deriving (Bounded, Eq, Enum, Read, Show)


-- |
-- Define and convert a 'FastaParseResult' to the expected sequence type
fastaStreamConverter :: MonadParsec e s m => FastaSequenceType -> FastaParseResult -> m TaxonSequenceMap
fastaStreamConverter seqType =
    fmap (colate seqType) . validateStreamConversion seqType . processedChars seqType


-- |
-- Since Bimap's are bijective, and there are some elements of the range that are
-- not surjectively mapped to from the domain
-- (ie, more than one element on the left side goes to an element on the right side),
-- this means that we must preprocess these element so that there is a bijective mapping.
processedChars :: FastaSequenceType -> FastaParseResult -> FastaParseResult
processedChars seqType = fmap processElement
  where
    processElement :: FastaSequence -> FastaSequence
    processElement (FastaSequence name chars) = FastaSequence name $ replaceSymbol chars

    replaceSymbol :: String -> String
    replaceSymbol =
        case seqType of
          AminoAcid -> replace 'U' 'C' . replace '.' '-'
          DNA       -> replace 'n' '?' . replace '.' '-'
          RNA       -> replace 'n' '?' . replace '.' '-'

    replace :: (Functor f, Eq b) => b -> b -> f b -> f b
    replace a b = fmap $ \x -> if a == x then b else x


-- |
-- Validates that the stream contains a 'FastaParseResult' of the given 'FastaSequenceType'.
validateStreamConversion :: MonadParsec e s m => FastaSequenceType -> FastaParseResult -> m FastaParseResult
validateStreamConversion seqType xs =
  case partition hasErrors result of
    ([] , _) -> pure xs
    (err, _) -> fails $ errorMessage <$> err
  where
    result = containsIncorrectChars <$> xs
    hasErrors = not . null . snd
    containsIncorrectChars (FastaSequence name chars) = (name, f chars)

    f = filter ((`notElem` s) . pure . pure)
      where
        s  = keysSet $ BM.toMap bm
        bm = case seqType of
               AminoAcid -> iupacToAminoAcid
               DNA       -> iupacToDna
               RNA       -> iupacToRna

    errorMessage (name, badChars) = concat
        [ "In the sequence for taxon: '"
        , name
        , "' the following invalid characters were found: "
        , intercalate ", " $ enquote <$> badChars
        ]

    enquote c = '\'' : c : "'"


-- |
-- Interprets and converts an entire 'FastaParseResult according to the given 'FatsaSequenceType'.
colate :: FastaSequenceType -> FastaParseResult -> TaxonSequenceMap
colate seqType = foldr f empty
  where
    f (FastaSequence name seq') = insert name (seqCharMapping seqType seq')


-- |
-- Interprets and converts an ambiguous sequence according to the given 'FatsaSequenceType'
-- from the ambiguous form to a 'CharacterSequence' based on IUPAC codes.
seqCharMapping :: FastaSequenceType -> String -> CharacterSequence
seqCharMapping seqType = V.fromList . fmap (f seqType . pure . pure)
  where
    f :: FastaSequenceType -> NonEmpty String -> NonEmpty String
    f t = let bm = case t of
                     AminoAcid -> iupacToAminoAcid
                     DNA       -> iupacToDna
                     RNA       -> iupacToRna
          in (bm BM.!)