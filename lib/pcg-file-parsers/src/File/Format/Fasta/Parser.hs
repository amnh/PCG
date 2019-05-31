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

{-# LANGUAGE BangPatterns        #-}
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
import           Control.DeepSeq
import           Control.Monad              ((<=<))
import           Data.Alphabet.IUPAC
import           Data.Bimap                 (Bimap, toMap)
import           Data.Char                  (isLower, isUpper, toLower, toUpper)
import           Data.Foldable
import           Data.Functor
import           Data.List                  (partition)
import           Data.List.NonEmpty         (NonEmpty)
import qualified Data.List.NonEmpty         as NE
import           Data.List.Utility
import           Data.Map                   (keysSet)
import           Data.Maybe                 (fromJust)
import           Data.Proxy
import           Data.Set                   (Set, mapMonotonic)
import qualified Data.Set                   as S
import           Data.Text.Short            (toString)
import           Data.Vector.Unboxed        (Unbox, Vector, (!))
import qualified Data.Vector.Unboxed        as V
import           File.Format.Fasta.Internal
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Custom
import           VectorBuilder.Builder      (Builder)
import qualified VectorBuilder.Builder      as V
import qualified VectorBuilder.Vector       as V

-- |
-- Pairing of taxa with an unconverted sequence
data FastaSequence
   = FastaSequence
   { taxonName     :: {-# UNPACk #-} !Identifier
   , taxonSequence :: {-# UNPACK #-} !(Vector Char)
   } deriving (Eq,Show)


-- |
-- Unconverted result of a fasta parse
type FastaParseResult = [FastaSequence]


-- |
-- Consumes a stream of 'Char's and parses the stream into a 'FastaParseResult'
-- that has been validated for information consistency
fastaStreamParser :: (MonadParsec e s m, Monoid (Tokens s), Token s ~ Char) => m FastaParseResult
fastaStreamParser = validate =<< {- seqTranslation <$> -} (some fastaTaxonSequenceDefinition <* eof)


-- |
-- Parses a single FASTA defined taxon sequence from a Char stream
fastaTaxonSequenceDefinition :: (MonadParsec e s m, Monoid (Tokens s), Token s ~ Char) => m FastaSequence
fastaTaxonSequenceDefinition = do
    name <- fastaTaxonName
    seq' <- try fastaSequence
    _    <- space
    pure $ FastaSequence name seq'


-- |
-- Consumes a line from the Char stream and parses a FASTA identifier
fastaTaxonName :: (MonadParsec e s m, Token s ~ Char) => m Identifier
fastaTaxonName = identifierLine


-- |
-- Consumes one or more lines from the Char stream to produce a list of Chars
-- constrained to a valid Char alphabet representing possible character states
fastaSequence :: forall e s m . (MonadParsec e s m, Monoid (Tokens s), Token s ~ Char) => m (Vector Char)
fastaSequence = space *> fullSequence
  where
    fullSequence = buildVector . mconcat <$> some sequenceLine
    sequenceLine = mconcat <$> ((lineChunk <* inlineSpace) `someTill` flexEOL)
    lineChunk    = takeWhile1P Nothing withinAlphabet

    -- Matches on the end of line or the end of the stream.
    flexEOL = void (try eol) <|> lookAhead eof

    buildVector  :: Tokens s -> Vector Char
    buildVector  = V.fromList . chunkToTokens (Proxy :: Proxy s)


{-# INLINE withinAlphabet #-}
withinAlphabet :: Char -> Bool
withinAlphabet =
    let !v = V.fromList $ toList alphabet
    in  withinVec v


{-# INLINE withinVec #-}
withinVec :: Vector Char -> Char -> Bool
withinVec v e = go 0 (V.length v - 1)
  where
    -- Perform a binary search on the unboxed vector
    -- to determine if a character is valid.
    --
    -- Equally fast, and uses less memory than a Set.
    {-# INLINE go #-}
    go !lo !hi
      | lo > hi   = False
      | otherwise = let !md = (hi + lo) `div` 2
                        !z  = v ! md
                    in  case z `compare` e of
                          EQ -> True
                          LT -> go    (md + 1) hi
                          GT -> go lo (md - 1)


-- |
-- Extract the keys from a 'Bimap'.
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
validate :: MonadParsec e s m => FastaParseResult -> m FastaParseResult
validate = validateSequenceConsistency <=< validateIdentifierConsistency


-- |
-- Ensures that there are no duplicate identifiers in the stream
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
validateSequenceConsistency :: (MonadParsec e s m {- , Token s ~ Char -}) => FastaParseResult -> m FastaParseResult
validateSequenceConsistency = validateConsistentPartition <=< validateConsistentAlphabet


-- |
-- Validates that all elements of all sequences are consistent with each other
-- sequence. Sequences of differing types cannot be mixed.
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
