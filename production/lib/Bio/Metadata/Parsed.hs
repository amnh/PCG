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

import           Bio.Character.Parsed
import           Data.Alphabet
import           Data.Bifunctor                          (second)
import           Data.Char
import           Data.Foldable
import           Data.List                               (transpose)
import           Data.Monoid
import           Data.TCM                                (TCM)
import qualified Data.TCM                         as TCM
import           Data.Vector                             (Vector)
import qualified Data.Vector                      as V
import           File.Format.Fasta                       (FastaParseResult,TaxonSequenceMap)
import           File.Format.Fastc
import           File.Format.Newick
import           File.Format.Nexus                hiding (CharacterMetadata, DNA, RNA, Nucleotide, TaxonSequenceMap)
import qualified File.Format.Nexus.Data           as Nex
import qualified File.Format.TNT                  as TNT
import qualified File.Format.TransitionCostMatrix as F
import           File.Format.VertexEdgeRoot

import Debug.Trace
  

-- | An intermediate composite type for parse result coercion.
data ParsedCharacterMetadata
   = ParsedCharacterMetadata
   { alphabet      :: Alphabet String
   , characterName :: String
   , weight        :: Double
   , parsedTCM     :: Maybe TCM
   , isDynamic     :: Bool
   , isIgnored     :: Bool
   } deriving (Show)


-- | Represents a parser result type which can have a character metadata
--   structure extracted from it.
class ParsedMetadata a where
    unifyMetadata :: a -> Vector ParsedCharacterMetadata


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
    unifyMetadata (Right withSeq) | trace (show . snd . head . toList $ TNT.sequences withSeq) False = undefined
    unifyMetadata (Left        _) = mempty
    unifyMetadata (Right withSeq) = V.fromList $ zipWith f parsedMetadatas parsedCharacters
      where
        parsedMetadatas  = toList $ TNT.charMetaData withSeq
        parsedCharacters = snd . head . toList $ TNT.sequences withSeq

        f :: TNT.CharacterMetaData -> TNT.TntCharacter -> ParsedCharacterMetadata
        f inMeta inChar =
            ParsedCharacterMetadata
            { alphabet      = characterAlphabet
            , characterName = TNT.characterName         inMeta
            , weight        = fromRational rationalWeight * suppliedWeight
            , parsedTCM     = unfactoredTcmMay
            , isDynamic     = False
            , isIgnored     = not $ TNT.active          inMeta
            }
          where

            -- |
            -- When constructing the Alphabet for a given character, we need to
            -- take into account several things.
            --
            -- * We must consider the well typed nature of the character. If
            --   the character type is Continuous, no alphabet exists. We
            --   supply 'undefined' in place of the alphabet as a hack to make
            --   later processing easier. This is inherently unsafe, but with
            --   proper character type checking later, we will never attempt
            --   to reference the undefined value.
            --
            -- * If the charcter type is Discrete (not DNA or Amino Acid), then
            --   we must check for supplied state names
            characterAlphabet =
                case inChar of
                  TNT.Continuous {} -> undefined -- I'm sure this will never blow up in our face /s
                  TNT.Dna        {} -> fromSymbols dnaAlph
                  TNT.Protein    {} -> fromSymbols aaAlph
                  TNT.Discrete   {} ->
                      let stateNameValues = TNT.characterStates inMeta
                      in
                          if   null stateNameValues
                          then fromSymbols disAlph
                          else fromSymbolsWithStateNames $ zip (toList disAlph) (toList stateNameValues)
                          
            (rationalWeight, unfactoredTcmMay) = maybe (1, Nothing) (second Just)
                                               $ TCM.fromList . toList <$> TNT.costTCM inMeta

            suppliedWeight = fromIntegral $ TNT.weight inMeta


-- | (✔)
instance ParsedMetadata F.TCM where
    unifyMetadata (F.TCM alph mat) =
        pure ParsedCharacterMetadata
        { alphabet      = fromSymbols alph
        , characterName = ""
        , weight        = fromRational rationalWeight
        , parsedTCM     = Just unfactoredTCM 
        , isDynamic     = False
        , isIgnored     = False -- Maybe this should be True?
        }
      where
        (rationalWeight, unfactoredTCM) = TCM.fromList $ toList mat


-- | (✔)
instance ParsedMetadata VertexEdgeRoot where
    unifyMetadata _ = mempty


-- | (✔)
instance ParsedMetadata Nexus where
    unifyMetadata (Nexus (_, metas) _) | trace (show $ (show . fromSymbols . Nex.alphabet) <$> metas) False = undefined
    unifyMetadata (Nexus (_, metas) _) = convertNexusMeta <$> metas
      where
        convertNexusMeta inMeta =
            ParsedCharacterMetadata
            { alphabet      = {- (\x -> trace (show x) x) . -} fromSymbols $ Nex.alphabet inMeta
            , characterName = Nex.name inMeta
            , weight        = fromRational rationalWeight * suppliedWeight
            , parsedTCM     = unfactoredTcmMay
            , isDynamic     = not $ Nex.isAligned inMeta 
            , isIgnored     = Nex.ignored inMeta
            }
          where
            suppliedWeight = fromIntegral $ Nex.weight inMeta
            (rationalWeight, unfactoredTcmMay) = maybe (1, Nothing) (second Just)
                                               $ TCM.fromList . toList . F.transitionCosts <$> Nex.costM inMeta


disAlph, dnaAlph, rnaAlph, aaAlph :: Vector String
-- | The acceptable DNA character values (with IUPAC codes).
dnaAlph = V.fromList $ pure <$> addOtherCases "AGCTRMWSKTVDHBNX?-"
-- | The acceptable RNA character values (with IUPAC codes).
rnaAlph = V.fromList $ pure <$> addOtherCases "AGCURMWSKTVDHBNX?-"
-- | The acceptable amino acid/protein character values (with IUPAC codes).
aaAlph  = V.fromList $ pure <$> addOtherCases "ABCDEFGHIKLMNPQRSTVWXYZ-"
-- | The acceptable discrete character values.
disAlph = V.fromList $ pure <$> (['0'..'9'] <> ['A'..'Z'] <> ['a'..'z'] <> "-" <> "?")


-- | Adds case insensitive values to a 'String'.
addOtherCases :: String -> String
addOtherCases [] = []
addOtherCases (x:xs)
  | isLower x && toUpper x `notElem` xs = toUpper x : x : addOtherCases xs
  | isUpper x && toLower x `notElem` xs = x : toLower x : addOtherCases xs
  | otherwise = x : addOtherCases xs


-- | Functionality to make char info from tree seqs
makeEncodeInfo :: TreeChars -> Vector ParsedCharacterMetadata
makeEncodeInfo = fmap makeOneInfo . developAlphabets


-- | Make a single info given an alphabet without state names
makeOneInfo :: Alphabet String -> ParsedCharacterMetadata
makeOneInfo alph =
    ParsedCharacterMetadata
    { alphabet      = alph
    , characterName = ""
    , weight        = 1
    , parsedTCM     = Nothing 
    , isDynamic     = True
    , isIgnored     = False
    }


-- | Internal function(s) to create alphabets
-- First is the new version. Following is the old version, which looks like it tosses the accumulator every once in a while.
-- Notes on data types follow
-- TreeChars :: Map String Maybe Vector [String]
-- bases are ambiguous, possibly multi-Char containers, hence [String]
-- characters are ordered groups of bases, hence Vector [String]
-- characters may be missing, hence Maybe Vector [String]
-- each taxon may have a sequence (multiple characters), hence Vector Maybe Vector [String]
-- sequences are values mapped to using taxon names as keys, hence Map String Vector Maybe Vector [String]
developAlphabets :: TreeChars -> Vector (Alphabet String)
developAlphabets = V.fromList . fmap (fromSymbols . foldMap (foldMap (foldMap toList))) . transpose . fmap toList . toList 
