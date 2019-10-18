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

{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE UnboxedSums        #-}

module File.Format.Fasta.Converter
  ( FastaSequenceType(..)
  , fastaStreamConverter
  ) where

import           Control.DeepSeq
import           Data.Alphabet.IUPAC
import qualified Data.Bimap                 as BM
import           Data.Data
import           Data.List                  (intercalate)
import           Data.List.NonEmpty         (NonEmpty (..))
import           Data.Map                   hiding (filter, foldr, null, partition, (!))
import           Data.String
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as LT
import           Data.Text.Short            (ShortText, toString)
import qualified Data.Vector.NonEmpty       as VNE
import           Data.Vector.Unboxed        (Vector, (!))
import qualified Data.Vector.Unboxed        as V (filter, length, map, null, toList)
import           Data.Void
import           File.Format.Fasta.Internal
import           File.Format.Fasta.Parser
import           GHC.Generics
import           Text.Megaparsec            (MonadParsec, Parsec)
import           Text.Megaparsec.Custom     (fails)


-- |
-- Different forms a 'FastaSequence' can be interpreted as.
data  FastaSequenceType
    = DNA
    | RNA
    | AminoAcid
    deriving stock    (Data, Bounded, Eq, Enum, Generic, Read, Show, Typeable)
    deriving anyclass (NFData)


-- |
-- Define and convert a 'FastaParseResult' to the expected sequence type
{-# INLINEABLE fastaStreamConverter #-}
{-# SPECIALISE fastaStreamConverter :: FastaSequenceType -> FastaParseResult -> Parsec Void  T.Text TaxonSequenceMap #-}
{-# SPECIALISE fastaStreamConverter :: FastaSequenceType -> FastaParseResult -> Parsec Void LT.Text TaxonSequenceMap #-}
{-# SPECIALISE fastaStreamConverter :: FastaSequenceType -> FastaParseResult -> Parsec Void  String TaxonSequenceMap #-}
fastaStreamConverter :: MonadParsec e s m => FastaSequenceType -> FastaParseResult -> m TaxonSequenceMap
fastaStreamConverter seqType =
    fmap (colate seqType) . validateStreamConversion seqType . processedChars seqType


-- |
-- Since Bimap's are bijective, and there are some elements of the range that are
-- not surjectively mapped to from the domain
-- (ie, more than one element on the left side goes to an element on the right side),
-- this means that we must preprocess these element so that there is a bijective mapping.
{-# INLINE processedChars #-}
processedChars :: FastaSequenceType -> FastaParseResult -> FastaParseResult
processedChars seqType = fmap processElement
  where
    processElement :: FastaSequence -> FastaSequence
    processElement (FastaSequence name chars) = FastaSequence name $ replaceSymbol chars

    replaceSymbol :: Vector Char -> Vector Char
    replaceSymbol =
        case seqType of
          AminoAcid -> replace 'U' 'C' . replace '.' '-'
          DNA       -> replace 'n' '?' . replace '.' '-'
          RNA       -> replace 'n' '?' . replace '.' '-'

    replace :: Char -> Char -> Vector Char -> Vector Char
    replace a b = V.map $ \x -> if a == x then b else x


-- |
-- Validates that the stream contains a 'FastaParseResult' of the given 'FastaSequenceType'.
{-# INLINE validateStreamConversion #-}
{-# SPECIALISE validateStreamConversion :: FastaSequenceType -> FastaParseResult -> Parsec Void  T.Text FastaParseResult #-}
{-# SPECIALISE validateStreamConversion :: FastaSequenceType -> FastaParseResult -> Parsec Void LT.Text FastaParseResult #-}
{-# SPECIALISE validateStreamConversion :: FastaSequenceType -> FastaParseResult -> Parsec Void  String FastaParseResult #-}
validateStreamConversion :: MonadParsec e s m => FastaSequenceType -> FastaParseResult -> m FastaParseResult
validateStreamConversion seqType xs =
  case filter hasErrors result of
    []  -> pure xs
    err -> fails $ errorMessage <$> err
  where
    result = containsIncorrectChars <$> xs
    hasErrors = not . V.null . snd
    containsIncorrectChars (FastaSequence name chars) = (name, f chars)

    f = V.filter ((`notElem` s) . pure . pure)
      where
        s  = keysSet $ BM.toMap bm
        bm = case seqType of
               AminoAcid -> iupacToAminoAcid
               DNA       -> iupacToDna
               RNA       -> iupacToRna

    errorMessage (name, badChars) = concat
        [ "In the sequence for taxon: '"
        , toString name
        , "' the following invalid characters were found: "
        , intercalate ", " $ enquote <$> V.toList badChars
        ]

    enquote c = '\'' : c : "'"


-- |
-- Interprets and converts an entire 'FastaParseResult according to the given 'FastaSequenceType'.
{-# INLINE colate #-}
colate :: FastaSequenceType -> FastaParseResult -> TaxonSequenceMap
colate seqType = foldr f empty
  where
    f (FastaSequence name seq') = insert name (seqCharMapping seqType seq')


-- |
-- Interprets and converts an ambiguous sequence according to the given 'FastaSequenceType'
-- from the ambiguous form to a 'CharacterSequence' based on IUPAC codes.
{-# INLINE seqCharMapping #-}
seqCharMapping :: FastaSequenceType -> Vector Char -> CharacterSequence
seqCharMapping seqType v = transformVector
  where
    transformVector = VNE.unfoldr g 0

    n = V.length v

    g :: Int -> (VNE.Vector ShortText, Maybe Int)
    g !i = let e = force . f seqType $ v ! i
           in  (e, if i >= n then Nothing else Just $ i + 1)

    f :: FastaSequenceType -> Char -> VNE.Vector ShortText
    f t c =
        let ne = [c]:|[]
            bm = case t of
                   AminoAcid -> iupacToAminoAcid
                   DNA       -> iupacToDna
                   RNA       -> iupacToRna
          in force . VNE.fromNonEmpty . fmap fromString $ bm BM.! ne
