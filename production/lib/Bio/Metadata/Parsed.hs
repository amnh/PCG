-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Metadata.Parsed
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Typeclass for metadata extracted from parsed results
--
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}

module Bio.Metadata.Parsed where

import           Bio.Metadata.Internal
import           Bio.Character.Dynamic.Coded
import           Bio.Character.Parsed
import           Bio.PhyloGraph.Solution
import           Data.Alphabet
import           Data.Char
import           Data.Foldable
import           Data.List              hiding (insert)
import           Data.Monoid
import           Data.Set                      (Set, insert)
import           Data.Vector                   (fromList, Vector)
import qualified Data.Vector            as V
import           File.Format.Fasta             (FastaParseResult,TaxonSequenceMap)
import           File.Format.Fastc
import           File.Format.Newick
import           File.Format.Nexus      hiding (CharacterMetadata, DNA, RNA, Nucleotide, TaxonSequenceMap)
import qualified File.Format.Nexus.Data as Nex
import qualified File.Format.TNT        as TNT
import qualified File.Format.TransitionCostMatrix as F
import           File.Format.VertexEdgeRoot

-- | Represents a parser result type which can have a character metadata
--   structure extracted from it.
class ParsedMetadata a where
    unifyMetadata :: a -> Vector StandardMetadata

-- | (✔)
instance ParsedMetadata FastaParseResult where
    unifyMetadata = makeEncodeInfo . unifyCharacters

-- | (✔)
instance ParsedMetadata TaxonSequenceMap where
    unifyMetadata = makeEncodeInfo . unifyCharacters

-- | (✔)
instance ParsedMetadata FastcParseResult where
    unifyMetadata = makeEncodeInfo . unifyCharacters

-- | (✔)
instance ParsedMetadata NewickForest where
    unifyMetadata _ = mempty

-- | (✔)
instance ParsedMetadata TNT.TntResult where
    unifyMetadata (Left _) = mempty
    unifyMetadata (Right withSeq) = fromList $ zipWith f (toList $ TNT.charMetaData withSeq) (snd . head . toList $ TNT.sequences withSeq)
        where
           f :: EncodableDynamicCharacter s => TNT.CharacterMetaData -> TNT.TntCharacter -> CharacterMetadata s
           f inMeta inChar =  let defaultMeta = makeOneInfo . Alphabet $ tntAlphabet inChar
                    in  defaultMeta { name       = TNT.characterName   inMeta
                                    , stateNames = TNT.characterStates inMeta
                                    , costs      = maybe (costs defaultMeta) TCM (TNT.costTCM inMeta)
                                    }
           tntAlphabet TNT.Continuous {} = mempty
           tntAlphabet TNT.Discrete   {} = disAlph -- TODO: get subset of maximum alphabet by doing a columwise set collection
           tntAlphabet TNT.Dna        {} = dnaAlph
           tntAlphabet TNT.Protein    {} = aaAlph

-- | (✔)
instance ParsedMetadata F.TCM where
    unifyMetadata (F.TCM alph mat) =
        let defaultMeta = makeOneInfo . Alphabet . fromList $ toList alph
        in  pure (defaultMeta {costs = TCM mat})

-- | (✔)
instance ParsedMetadata VertexEdgeRoot where
    unifyMetadata _ = mempty

-- | (✔)
instance ParsedMetadata Nexus where
    unifyMetadata (Nexus (_, metas)) = V.map convertNexusMeta metas
        where
            convertNexusMeta inMeta =
                let defaultMeta = makeOneInfo . Alphabet . fromList $ Nex.alphabet inMeta
                in  defaultMeta { name      = Nex.name inMeta
                                , isIgnored = Nex.ignored inMeta
                                , costs     = maybe (costs defaultMeta) (TCM . F.transitionCosts) (Nex.costM inMeta)
                                }

disAlph, dnaAlph, rnaAlph, aaAlph :: Vector String
-- | The acceptable DNA character values (with IUPAC codes).
dnaAlph = fromList $ pure <$> addOtherCases "AGCTRMWSKTVDHBNX?-"
-- | The acceptable RNA character values (with IUPAC codes).
rnaAlph = fromList $ pure <$> addOtherCases "AGCURMWSKTVDHBNX?-"
-- | The acceptable amino acid/protein character values (with IUPAC codes).
aaAlph  = fromList $ pure <$> addOtherCases "ABCDEFGHIKLMNPQRSTVWXYZ-"
-- | The acceptable discrete character values.
disAlph = fromList $ pure <$> (['0'..'9'] <> ['A'..'Z'] <> ['a'..'z'] <> "-" <> "?")

-- | Adds case insensitive values to a 'String'.
addOtherCases :: String -> String
addOtherCases [] = []
addOtherCases (x:xs)
  | isLower x && toUpper x `notElem` xs = toUpper x : x : addOtherCases xs
  | isUpper x && toLower x `notElem` xs = x : toLower x : addOtherCases xs
  | otherwise = x : addOtherCases xs

-- | Make a single info given an alphabet
makeOneInfo :: EncodableDynamicCharacter s => Alphabet String -> CharacterMetadata s
makeOneInfo alph = CharMeta DirectOptimization alph mempty False False 1 mempty (constructDynamic [], constructDynamic []) 1 (GeneralCost 1 1)

-- | Functionality to make char info from tree seqs
makeEncodeInfo :: EncodableDynamicCharacter s => TreeChars -> Vector (CharacterMetadata s)
makeEncodeInfo seqs = V.map makeOneInfo alphabets
    where alphabets = developAlphabets seqs

-- | Internal function(s) to create alphabets
-- First is the new version. Following is the old version, which looks like it tosses the accumulator every once in a while.
-- Notes on data types follow
-- TreeChars :: Map String Maybe Vector [String]
-- bases are ambiguous, possibly multi-Char containers, hence [String]
-- characters are ordered groups of bases, hence Vector [String]
-- characters may be missing, hence Maybe Vector [String]
-- each taxon may have a sequence (multiple characters), hence Vector Maybe Vector [String]
-- sequences are values mapped to using taxon names as keys, hence Map String Vector Maybe Vector [String]
--developAlphabets :: TreeChars -> Vector Alphabet

--old version
developAlphabets :: TreeChars -> Vector (Alphabet String)
developAlphabets inTaxSeqMap = (setGapChar . Alphabet . V.fromList . sort . toList) <$> foldr (V.zipWith getNodeAlphAt) partialAlphabets inTaxSeqMap
    where
        seqLength        = length . head $ toList inTaxSeqMap
        partialAlphabets = V.replicate seqLength mempty

        getNodeAlphAt :: Maybe ParsedChar -> Set String -> Set String
        getNodeAlphAt inCharMay partialAlphabet =
          case inCharMay of
            Nothing     -> partialAlphabet
            Just inChar -> foldr (flip $ foldr insert  -- this is set insertion
                                 ) partialAlphabet inChar


-- | Ensure that the gap char is present and correctly positioned in an alphabet
setGapChar :: Alphabet String -> Alphabet String
setGapChar (Alphabet inAlph) = Alphabet $ V.filter (/= "-") inAlph <> pure "-"
