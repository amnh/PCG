-----------------------------------------------------------------------------
-- |
-- Module      :  File.Format.Fasta.Parser
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Functions for parsing FASTA files.
--
-----------------------------------------------------------------------------

{-# LANGUAGE ApplicativeDo       #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module File.Format.Fasta.Parser
  ( FastaParseResult
  , FastaSequence(..)
  , fastaStreamParser
  , fastaTaxonSequenceDefinition
  , fastaSequence
  ) where

import           Control.Arrow              ((&&&))
import           Control.DeepSeq            (NFData)
import           Control.Monad              ((<=<))
import           Data.Alphabet.IUPAC
import           Data.Bimap                 (Bimap, toMap)
import           Data.Char                  (isLower, isUpper, toLower, toUpper)
import           Data.Data
import           Data.Foldable
import           Data.Functor
import           Data.List                  (partition)
import           Data.List.NonEmpty         (NonEmpty)
import qualified Data.List.NonEmpty         as NE
import           Data.List.Utility
import           Data.Map                   (keysSet)
import           Data.Maybe                 (fromJust)
import           Data.Set                   (Set, mapMonotonic)
import qualified Data.Set                   as S
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as LT
import           Data.Text.Short            (toString)
import           Data.Vector.Unboxed        (Unbox, Vector)
import qualified Data.Vector.Unboxed        as V
import           Data.Void
import           File.Format.Fasta.Internal
import           GHC.Generics               (Generic)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Custom


-- |
-- Pairing of taxa with an unconverted sequence
data  FastaSequence
    = FastaSequence
    { taxonName     :: {-# UNPACK #-} !Identifier
    , taxonSequence :: {-# UNPACK #-} !(Vector Char)
    }
    deriving stock    (Data, Eq, Generic, Show, Typeable)
    deriving anyclass (NFData)


-- |
-- Unconverted result of a fasta parse
type FastaParseResult = [FastaSequence]


-- |
-- Consumes a stream of 'Char's and parses the stream into a 'FastaParseResult'
-- that has been validated for information consistency
{-# INLINEABLE fastaStreamParser #-}
{-# SPECIALISE fastaStreamParser :: Parsec Void  T.Text FastaParseResult #-}
{-# SPECIALISE fastaStreamParser :: Parsec Void LT.Text FastaParseResult #-}
{-# SPECIALISE fastaStreamParser :: Parsec Void  String FastaParseResult #-}
fastaStreamParser :: (MonadParsec e s m, Monoid (Tokens s), Token s ~ Char) => m FastaParseResult
fastaStreamParser = validate =<< {- seqTranslation <$> -} (some fastaTaxonSequenceDefinition <* eof)


-- |
-- Parses a single FASTA defined taxon sequence from a Char stream
{-# INLINEABLE fastaTaxonSequenceDefinition #-}
{-# SPECIALISE fastaTaxonSequenceDefinition :: Parsec Void  T.Text FastaSequence #-}
{-# SPECIALISE fastaTaxonSequenceDefinition :: Parsec Void LT.Text FastaSequence #-}
{-# SPECIALISE fastaTaxonSequenceDefinition :: Parsec Void  String FastaSequence #-}
fastaTaxonSequenceDefinition :: (MonadParsec e s m, Monoid (Tokens s), Token s ~ Char) => m FastaSequence
fastaTaxonSequenceDefinition = do
    name <- fastaTaxonName
    seq' <- try fastaSequence
    _    <- space
    pure $ FastaSequence name seq'


-- |
-- Consumes a line from the Char stream and parses a FASTA identifier
{-# INLINE fastaTaxonName #-}
{-# SPECIALISE fastaTaxonName :: Parsec Void  T.Text Identifier #-}
{-# SPECIALISE fastaTaxonName :: Parsec Void LT.Text Identifier #-}
{-# SPECIALISE fastaTaxonName :: Parsec Void  String Identifier #-}
fastaTaxonName :: (MonadParsec e s m, Token s ~ Char) => m Identifier
fastaTaxonName = identifierLine


-- |
-- Consumes one or more lines from the Char stream to produce a list of Chars
-- constrained to a valid Char alphabet representing possible character states
{-# INLINEABLE fastaSequence #-}
{-# SPECIALISE fastaSequence :: Parsec Void  T.Text (Vector Char) #-}
{-# SPECIALISE fastaSequence :: Parsec Void LT.Text (Vector Char) #-}
{-# SPECIALISE fastaSequence :: Parsec Void  String (Vector Char) #-}
fastaSequence :: forall e s m . (MonadParsec e s m, Monoid (Tokens s), Token s ~ Char) => m (Vector Char)
fastaSequence = space *> fullSequence
  where
    fullSequence = buildVector . fold <$> some taxonContentLine

    -- A line in the "taxon contents" can start with zero or more "inline whitespace" characters.
    -- After all leading whitespace has been consumed on the line, what remains must be either:
    --
    --   * A newline, signifying the end of the line
    --
    --   * One or more sequence data symbools, possibly seperated by spaces,
    --       followed by a newline or the end of the file.
    taxonContentLine = inlinedSpace *> (sequenceLine <|> (endOfLine $> mempty))

    -- Defines the contents of a taxon line which contains sequence data
    sequenceLine = fold <$> ((seqChunk <* inlinedSpace) `someTill` flexEOL)
      where
        seqChunk = someOfThese alphabet

    -- Matches on the end of line or the end of the stream.
    flexEOL      = void (try endOfLine) <|> lookAhead eof

    buildVector  :: Tokens s -> Vector Char
    buildVector  = V.fromList . chunkToTokens (Proxy :: Proxy s)


-- |
-- Extract the keys from a 'Bimap'.
{-# INLINE extractFromBimap #-}
extractFromBimap :: Bimap (NonEmpty String) a -> Set Char
extractFromBimap = mapMonotonic (head . NE.head) . keysSet . toMap


alphabet, otherValidChars, iupacAminoAcidChars, iupacNucleotideChars, iupacRNAChars :: Set Char
alphabet             = fold [iupacAminoAcidChars, iupacNucleotideChars, iupacRNAChars]
otherValidChars      = S.fromList ".-?#"
iupacAminoAcidChars  = otherValidChars <> caseInsensitiveOptions (extractFromBimap iupacToAminoAcid)
iupacNucleotideChars = otherValidChars <> caseInsensitiveOptions (extractFromBimap iupacToDna)
iupacRNAChars        = otherValidChars <> caseInsensitiveOptions (extractFromBimap iupacToRna)


-- |
-- Adds the lowercase and uppercase Chars to string when only the upper or
-- lower is present in the String
{-# INLINE caseInsensitiveOptions #-}
caseInsensitiveOptions :: Set Char -> Set Char
caseInsensitiveOptions = foldMap f
  where
    f x | isLower x = S.singleton x <> S.singleton (toUpper x)
        | isUpper x = S.singleton x <> S.singleton (toLower x)
        | otherwise = S.singleton x


{-
-- |
-- Converts all Chars in the sequence to uppercase
-- This makes all subsequent processing easier
seqTranslation :: [FastaSequence] -> [FastaSequence]
seqTranslation = foldr f []
  where
    f (FastaSequence name seq') a = FastaSequence name (toUpper <$> seq') : a
-}


-- |
-- Ensures that the parsed result has consistent data
{-# INLINE validate #-}
{-# SPECIALISE validate :: FastaParseResult -> Parsec Void  T.Text FastaParseResult #-}
{-# SPECIALISE validate :: FastaParseResult -> Parsec Void LT.Text FastaParseResult #-}
{-# SPECIALISE validate :: FastaParseResult -> Parsec Void  String FastaParseResult #-}
validate :: MonadParsec e s m => FastaParseResult -> m FastaParseResult
validate = validateSequenceConsistency <=< validateIdentifierConsistency


-- |
-- Ensures that there are no duplicate identifiers in the stream
{-# INLINE validateIdentifierConsistency #-}
{-# SPECIALISE validateIdentifierConsistency :: FastaParseResult -> Parsec Void  T.Text FastaParseResult #-}
{-# SPECIALISE validateIdentifierConsistency :: FastaParseResult -> Parsec Void LT.Text FastaParseResult #-}
{-# SPECIALISE validateIdentifierConsistency :: FastaParseResult -> Parsec Void  String FastaParseResult #-}
validateIdentifierConsistency :: MonadParsec e s m => FastaParseResult -> m FastaParseResult
validateIdentifierConsistency xs =
  case dupes of
    [] -> pure xs
    _  -> fails errors
  where
    dupes = duplicates $ taxonName <$> xs
    errors         = errorMessage <$> dupes
    errorMessage x = fold [ "Multiple taxon labels found identified by: '", toString x, "'"]


-- |
-- Ensures that the charcters are all from a consistent alphabet
{-# INLINE validateSequenceConsistency #-}
{-# SPECIALISE validateSequenceConsistency :: FastaParseResult -> Parsec Void  T.Text FastaParseResult #-}
{-# SPECIALISE validateSequenceConsistency :: FastaParseResult -> Parsec Void LT.Text FastaParseResult #-}
{-# SPECIALISE validateSequenceConsistency :: FastaParseResult -> Parsec Void  String FastaParseResult #-}
validateSequenceConsistency :: (MonadParsec e s m {- , Token s ~ Char -}) => FastaParseResult -> m FastaParseResult
validateSequenceConsistency = validateConsistentPartition <=< validateConsistentAlphabet


-- |
-- Validates that all elements of all sequences are consistent with each other
-- sequence. Sequences of differing types cannot be mixed.
{-# INLINE validateConsistentAlphabet #-}
{-# SPECIALISE validateConsistentAlphabet :: FastaParseResult -> Parsec Void  T.Text FastaParseResult #-}
{-# SPECIALISE validateConsistentAlphabet :: FastaParseResult -> Parsec Void LT.Text FastaParseResult #-}
{-# SPECIALISE validateConsistentAlphabet :: FastaParseResult -> Parsec Void  String FastaParseResult #-}
validateConsistentAlphabet :: (MonadParsec e s m {- , Token s ~ Char -}) => FastaParseResult -> m FastaParseResult
validateConsistentAlphabet xs =
  case partition snd results of
    (_,[]) -> pure xs
    (_,ys) -> fails $ errorMessage <$> ys
  where
    results            = validation <$> xs
    validation         = taxonName &&& consistentAlphabet . taxonSequence

    consistentAlphabet seq' = V.all (`elem` iupacAminoAcidChars ) seq'
                           || V.all (`elem` iupacNucleotideChars) seq'
                           || V.all (`elem` iupacRNAChars       ) seq'

    errorMessage (n,_) = concat
        [ "Error in sequence for taxon name: '"
        , toString n
        , "' the sequence data includes characters from multiple data formats. "
        , "Check this taxon's sequence to ensure that it contains characted codes "
        , "from only one data format."
        ]


-- |
-- Validates that sequences partitioned with the '\'#\'' character are all of
-- the same length.
{-# INLINE validateConsistentPartition #-}
{-# SPECIALISE validateConsistentPartition :: FastaParseResult -> Parsec Void  T.Text FastaParseResult #-}
{-# SPECIALISE validateConsistentPartition :: FastaParseResult -> Parsec Void LT.Text FastaParseResult #-}
{-# SPECIALISE validateConsistentPartition :: FastaParseResult -> Parsec Void  String FastaParseResult #-}
validateConsistentPartition :: (MonadParsec e s m {- , Token s ~ Char -}) => FastaParseResult -> m FastaParseResult
validateConsistentPartition xs
  |  null xs
  || null errors  = pure xs
  |  otherwise    = fails errors
  where
    countOccurances :: (Eq a, Unbox a) => a -> Vector a -> Word
    countOccurances v = V.foldl' (\ !a e -> if e == v then a+1 else a) 0

    expectedPartitions     = fromJust . mostCommon $ fst <$> withPartitionCount
    partitionCount         = countOccurances '#' . taxonSequence
    withPartitionCount     = (partitionCount &&& id) <$> xs
    inconsistentPartitions = filter ((/= expectedPartitions) . fst) withPartitionCount
    errors                 = errorMessage <$> inconsistentPartitions

    errorMessage (actualPartitions, taxa) = fold
        [ "Error in sequence for taxon name: '"
        , toString $ taxonName taxa
        , "' the sequence includes "
        , show actualPartitions
        , " partition characters ('#'). "
        , "Expecting "
        , show expectedPartitions
        , " partition characters in the sequence."
        ]
