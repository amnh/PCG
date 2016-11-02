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

import           Bio.Character.Dynamic
import           Bio.Character.Parsed
--import           Bio.Metadata.Internal
--import           Bio.PhyloGraph.Solution
import           Data.Alphabet
import           Data.Bifunctor                (second)
import           Data.Char
import           Data.Foldable
import           Data.List                     (transpose)
--import           Data.Matrix.NotStupid  hiding (toList,fromList)
import           Data.Maybe                    (fromMaybe)
import           Data.Monoid
import           Data.Set                      (Set, insert)
import qualified Data.Set               as Set
import           Data.TCM                      (TCM)
import qualified Data.TCM               as TCM
import           Data.Vector                   (Vector)
import qualified Data.Vector            as V
import           File.Format.Fasta             (FastaParseResult,TaxonSequenceMap)
import           File.Format.Fastc
import           File.Format.Newick
import           File.Format.Nexus      hiding (CharacterMetadata, DNA, RNA, Nucleotide, TaxonSequenceMap)
import qualified File.Format.Nexus.Data as Nex
import qualified File.Format.TNT        as TNT
import qualified File.Format.TransitionCostMatrix as F
import           File.Format.VertexEdgeRoot

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
    unifyMetadata       (Left        _) = mempty
    unifyMetadata input@(Right withSeq) = V.fromList $ zipWith f parsedMetadatas parsedCharacters
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
          -- TODO: fix
          {-
               let defaultMeta = makeOneInfo . fromSymbols $ tntAlphabet inChar
               in  defaultMeta { name       = TNT.characterName   inMeta
                               , stateNames = TNT.characterStates inMeta
                               , costs      = maybe (costs defaultMeta) TCM (TNT.costTCM inMeta)
                               }
          -}

--        alphabetSets = fmap g . transpose . fmap toList . toList $ unifyCharacters input
--          where
--            g = foldMap (foldMap (foldMap (Set.fromList . toList)))
                   
--        tntAlphabet TNT.Continuous {} = undefined -- I'm sure this will never blow up /s
--        tntAlphabet TNT.Discrete   {} = disAlph   -- TODO: get subset of maximum alphabet by doing a columwise set collection
--        tntAlphabet TNT.Dna        {} = dnaAlph
--        tntAlphabet TNT.Protein    {} = aaAlph


-- | (✔)
instance ParsedMetadata F.TCM where
    unifyMetadata (F.TCM alph mat) = undefined -- TODO: fix
{-
        let defaultMeta = makeOneInfo . fromSymbols $ toList alph
        in  pure (defaultMeta {costs = TCM mat})
-}


-- | (✔)
instance ParsedMetadata VertexEdgeRoot where
    unifyMetadata _ = mempty


-- | (✔)
instance ParsedMetadata Nexus where
    unifyMetadata (Nexus (_, metas)) = V.map convertNexusMeta metas
        where
            convertNexusMeta inMeta = undefined -- TODO: fix
            {-
                let defaultMeta = makeOneInfo . fromSymbols $ Nex.alphabet inMeta
                in  defaultMeta { name      = Nex.name inMeta
                                , isIgnored = Nex.ignored inMeta
                                , costs     = maybe (costs defaultMeta) (TCM . F.transitionCosts) (Nex.costM inMeta)
                                }
-}

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


-- | Functionality to make char info from tree seqs
makeEncodeInfo :: TreeChars -> Vector ParsedCharacterMetadata
makeEncodeInfo seqs = V.map makeOneInfo alphabets
  where
    alphabets = developAlphabets seqs


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
developAlphabets inTaxSeqMap = fromSymbols <$> foldr (V.zipWith getNodeAlphAt) partialAlphabets inTaxSeqMap
    where
        seqLength        = length . head $ toList inTaxSeqMap
        partialAlphabets = V.replicate seqLength mempty

        getNodeAlphAt :: Maybe ParsedChar -> Set String -> Set String
        getNodeAlphAt inCharMay partialAlphabet =
          case inCharMay of
            Nothing     -> partialAlphabet
            Just inChar -> foldr (flip $ foldr insert  -- this is set insertion
                                 ) partialAlphabet inChar
