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

module Bio.Metadata.Parsed
  ( ParsedCharacterMetadata(..)
  , ParsedMetadata(..)
  ) where

import           Bio.Character.Parsed
import           Data.Alphabet
import           Data.Char
import           Data.Foldable
import           Data.Key
import           Data.List                               (transpose)
import           Data.List.NonEmpty                      (NonEmpty)
import           Data.Monoid
import           Data.TCM                                (TCM, TCMDiagnosis(..), TCMStructure(..), diagnoseTcm, )
import qualified Data.TCM                         as TCM
import           Data.Vector                             (Vector)
import qualified Data.Vector                      as V
import           Data.Vector.Instances                   ()
import           File.Format.Dot
import           File.Format.Fasta                       (FastaParseResult,TaxonSequenceMap)
import           File.Format.Fastc
import           File.Format.Newick
import           File.Format.Nexus                hiding (CharacterMetadata(..), DNA, RNA, Nucleotide, TaxonSequenceMap)
import qualified File.Format.Nexus                as Nex
import qualified File.Format.TNT                  as TNT
import qualified File.Format.TransitionCostMatrix as F
import           File.Format.VertexEdgeRoot
import           Prelude                          hiding (zip, zipWith)


-- |
-- An intermediate composite type for parse result coercion.
data ParsedCharacterMetadata
   = ParsedCharacterMetadata
   { alphabet      :: Alphabet String
   , characterName :: String
   , weight        :: Double
   , parsedTCM     :: Maybe (TCM, TCMStructure)
   , isDynamic     :: Bool
   , isIgnored     :: Bool
   } deriving (Show)


-- |
-- Represents a parser result type which can have a character metadata
-- structure extracted from it.
class ParsedMetadata a where

    unifyMetadata :: a -> Vector ParsedCharacterMetadata


-- | (✔)
instance ParsedMetadata (DotGraph n) where

    unifyMetadata _ = mempty


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
instance ParsedMetadata (NonEmpty NewickForest) where

    unifyMetadata _ = mempty


-- | (✔)
instance ParsedMetadata TNT.TntResult where

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
            (rationalWeight, characterAlphabet, unfactoredTcmMay) = chooseAppropriateMatrixAndAlphabet

            suppliedWeight = fromIntegral $ TNT.weight inMeta
          
            chooseAppropriateMatrixAndAlphabet
              | TNT.sankoff  inMeta =
                case TCM.fromList . toList <$> TNT.costTCM inMeta of
                  Nothing       -> (1, fullAlphabet, Nothing)
                  Just (v, tcm) ->
                    let truncatedSymbols = V.take (TCM.size tcm - 1) initialSymbolSet
                    in  (v, toAlphabet truncatedSymbols, Just (tcm, NonSymmetric)) -- TODO: Maybe we can do the diagnosis here
                  
              | TNT.additive inMeta = (1, fullAlphabet, Just (TCM.generate matrixDimension genAdditive,    Additive))
              | otherwise           = (1, fullAlphabet, Just (TCM.generate matrixDimension genFitch   , NonAdditive))
              where
                matrixDimension   = length initialSymbolSet
                fullAlphabet      = toAlphabet initialSymbolSet

                toAlphabet xs
                  | null stateNameValues = fromSymbols xs
                  | otherwise            = fromSymbolsWithStateNames $ zip xs stateNameValues
                  where
                    stateNameValues = TNT.characterStates inMeta

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
                --
                -- * If the charcter has a Sankoff TCM defined, we truncate the
                -- character alphabet
                initialSymbolSet =
                    case inChar of
                      TNT.Continuous {} -> undefined -- I'm sure this will never blow up in our face /s
                      TNT.Dna        {} -> dnaAlph
                      TNT.Protein    {} -> aaAlph
                      TNT.Discrete   {} -> disAlph
{-                        
                          let stateNameValues = TNT.characterStates inMeta
                          in
                              if   null stateNameValues
                              then fromSymbols disAlph
                              else fromSymbolsWithStateNames $ zip (toList disAlph) (toList stateNameValues)
-}
                          

-- | (✔)
instance ParsedMetadata F.TCM where
    unifyMetadata (F.TCM alph mat) =
        pure ParsedCharacterMetadata
        { alphabet      = fromSymbols alph
        , characterName = ""
        , weight        = fromRational rationalWeight * fromIntegral coefficient
        , parsedTCM     = Just (resultTCM, structure) 
        , isDynamic     = False
        , isIgnored     = False -- Maybe this should be True?
        }
      where
        (coefficient, resultTCM, structure) = (,,) <$> factoredWeight <*> factoredTcm <*> tcmStructure $ diagnoseTcm unfactoredTCM
        (rationalWeight, unfactoredTCM)     = TCM.fromList $ toList mat


-- | (✔)
instance ParsedMetadata VertexEdgeRoot where
    unifyMetadata _ = mempty


-- | (✔)
instance ParsedMetadata Nexus where

    unifyMetadata input @(Nexus (_, metas) _) = V.zipWith convertNexusMeta alphabetVector metas
      where
        alphabetVector = developAlphabets $ unifyCharacters input
        convertNexusMeta developedAlphabet inMeta =
            ParsedCharacterMetadata
            { alphabet      = developedAlphabet
            , characterName = Nex.name inMeta
            , weight        = fromRational chosenWeight * suppliedWeight
            , parsedTCM     = chosenTCM
            , isDynamic     = not $ Nex.isAligned inMeta 
            , isIgnored     = Nex.ignored inMeta
            }
          where
            suppliedWeight = fromIntegral $ Nex.weight inMeta
            (chosenWeight, chosenTCM) =
              if Nex.additive inMeta
              then (1, Just (TCM.generate (length $ Nex.alphabet inMeta) genAdditive, Additive))
              else
                case Nex.costM inMeta of
                  Nothing   -> (1, Nothing)
                  Just vals ->
                    let (rationalWeight, unfactoredTcm)     = TCM.fromList . toList $ F.transitionCosts vals
                        (coefficient, resultTCM, structure) = (,,) <$> factoredWeight <*> factoredTcm <*> tcmStructure $ diagnoseTcm unfactoredTcm
                    in  (fromIntegral coefficient * rationalWeight, Just (resultTCM, structure))


-- |
-- Adds case insensitive values to a 'String'.
addOtherCases :: String -> String
addOtherCases [] = []
addOtherCases (x:xs)
  | isLower x && toUpper x `notElem` xs = toUpper x : x : addOtherCases xs
  | isUpper x && toLower x `notElem` xs = x : toLower x : addOtherCases xs
  | otherwise = x : addOtherCases xs


-- |
-- Internal function to create alphabets
-- First is the new version. Following is the old version, which looks like it tosses the accumulator every once in a while.
-- Notes on data types follow
-- TreeChars :: Map String Maybe Vector [String]
-- bases are ambiguous, possibly multi-Char containers, hence [String]
-- characters are ordered groups of bases, hence Vector [String]
-- characters may be missing, hence Maybe Vector [String]
-- each taxon may have a sequence (multiple characters), hence Vector Maybe Vector [String]
-- sequences are values mapped to using taxon names as keys, hence Map String Vector Maybe Vector [String]
developAlphabets :: TaxonCharacters -> Vector (Alphabet String)
developAlphabets = V.fromList . fmap (fromSymbols . foldMap f) . transpose . fmap toList . toList
  where
    f (ParsedContinuousCharacter     _) = mempty
    f (ParsedDiscreteCharacter  static) = foldMap toList static
    f (ParsedDynamicCharacter  dynamic) = foldMap (foldMap toList) dynamic


-- Alphabet values.
-- TODO: Probably move to Data.Alphabet.Default at some point

-- |
-- The acceptable amino acid/protein character values (with IUPAC codes).
aaAlph :: Vector String
aaAlph  = V.fromList $ pure <$> addOtherCases "ABCDEFGHIKLMNPQRSTVWXYZ-"

-- |
-- The acceptable discrete character values.
disAlph :: Vector String
disAlph = V.fromList $ pure <$> (['0'..'9'] <> ['A'..'Z'] <> ['a'..'z'] <> "-" <> "?")


-- |
-- The acceptable DNA character values (with IUPAC codes).
dnaAlph :: Vector String
dnaAlph = V.fromList $ pure <$> addOtherCases "AGCTRMWSKTVDHBNX?-"


{-
-- |
-- The acceptable RNA character values (with IUPAC codes).
-- rnaAlph :: Vector String
-- rnaAlph = V.fromList $ pure <$> addOtherCases "AGCURMWSKTVDHBNX?-"
-}


-- |
-- Functionality to make char info from tree seqs
makeEncodeInfo :: TaxonCharacters -> Vector ParsedCharacterMetadata
makeEncodeInfo = fmap makeOneInfo . developAlphabets


-- |
-- Make a single info given an alphabet without state names
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


-- |
-- Generate the norm metric function.
genAdditive :: (Int, Int) -> Int
genAdditive (i,j) = max i j - min i j


-- |
-- Generate the discrete metric function.
genFitch :: (Int, Int) -> Int
genFitch    (i,j) = if i == j then 0 else 1

