-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Normalization.Metadata.Class
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

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Data.Normalization.Metadata.Class
  ( NormalizedMetadata(..)
  , HasNormalizedMetadata(..)
  ) where

import           Control.Applicative
import           Data.Alphabet
import           Data.Foldable
import           Data.Key
import           Data.List.NonEmpty                   (NonEmpty (..))
import           Data.Matrix.NotStupid                (Matrix, nrows)
import           Data.Monoid
import           Data.Normalization.Character
import           Data.Normalization.Metadata.Internal
import           Data.String                          (IsString (fromString))
import           Data.TCM                             (TCMStructure (..))
import qualified Data.TCM                             as TCM
import qualified Data.Vector                          as V
import           Data.Vector.Instances                ()
import           Data.Vector.NonEmpty                 (Vector)
import qualified Data.Vector.NonEmpty                 as VNE
import           File.Format.Dot
import           File.Format.Fasta                    (FastaParseResult, TaxonSequenceMap)
import           File.Format.Fastc
import           File.Format.Newick
import           File.Format.Nexus                    hiding (CharacterMetadata (..), DNA, Nucleotide, RNA,
                                                       TaxonSequenceMap)
import qualified File.Format.Nexus                    as Nex
import qualified File.Format.TNT                      as TNT
import qualified File.Format.TransitionCostMatrix     as F
import           File.Format.VertexEdgeRoot
import           Prelude                              hiding (zip, zipWith)


-- |
-- Represents a parser result type which can have a character metadata
-- structure extracted from it.
class HasNormalizedMetadata a where

    getNormalizedMetadata :: a -> Maybe (Vector NormalizedMetadata)


instance HasNormalizedMetadata (DotGraph n) where

    getNormalizedMetadata = const mempty


instance HasNormalizedMetadata FastaParseResult where

    getNormalizedMetadata = makeEncodeInfo . getNormalizedCharacters


instance HasNormalizedMetadata TaxonSequenceMap where

    getNormalizedMetadata = makeEncodeInfo . getNormalizedCharacters


instance HasNormalizedMetadata FastcParseResult where

    getNormalizedMetadata = makeEncodeInfo . getNormalizedCharacters


instance HasNormalizedMetadata (NonEmpty NewickForest) where

    getNormalizedMetadata = const mempty


instance HasNormalizedMetadata TNT.TntResult where

    getNormalizedMetadata (Left        _) = mempty
    getNormalizedMetadata (Right withSeq) = VNE.fromNonEmpty <$> liftA2 (zipWith f) parsedMetadatas parsedCharacters
      where
        parsedMetadatas  =
          case toList $ TNT.charMetaData withSeq of
            []   -> Nothing
            x:xs -> Just $ x:|xs

        parsedCharacters =
          case toList $ TNT.sequences withSeq of
            []  -> Nothing
            x:_ -> case toList $ snd x of
                     []   -> Nothing
                     y:ys -> Just $ y:|ys

        f :: TNT.CharacterMetadata -> TNT.TntCharacter -> NormalizedMetadata
        f inMeta inChar =
            NormalizedMetadata
            { alphabet      = fmap fromString characterAlphabet
            , characterName = fromString . TNT.characterName $ inMeta
            , weight        = coefficientWeight * suppliedWeight
            , parsedTCM     = factoredTcmMay
            , isDynamic     = False
            , isIgnored     = not $ TNT.active  inMeta
            }
          where
            (coefficientWeight, characterAlphabet, factoredTcmMay) = chooseAppropriateMatrixAndAlphabet

            suppliedWeight = fromIntegral $ TNT.weight inMeta

            chooseAppropriateMatrixAndAlphabet
              | TNT.sankoff  inMeta =
                case TNT.costTCM inMeta of
                  Nothing       ->
                    let stateNameValues = TNT.characterStates inMeta
                        (alphabet', _, _, _)
                          | null stateNameValues = getMetadataFromInputSymbolsAndTCM initialSymbolSet (undefined :: Matrix Word)
                          | otherwise            = getMetadataFromInputSymbolsStatesAndTCM (zip initialSymbolSet stateNameValues) (undefined :: Matrix Word)
                    in  (1, alphabet', Nothing)
                    
                  Just mat ->
                    let truncatedSymbols = V.take (nrows mat - 1) initialSymbolSet
                        stateNameValues  = TNT.characterStates inMeta
                        (alphabet', coefficient, resultTCM, structure)
                          | null stateNameValues = getMetadataFromInputSymbolsAndTCM truncatedSymbols mat
                          | otherwise            = getMetadataFromInputSymbolsStatesAndTCM (zip truncatedSymbols stateNameValues) mat
                    in  ( coefficient
                        , alphabet'
                        , Just (resultTCM, structure)
                        )

              | TNT.additive inMeta =
                    let stateNameValues = TNT.characterStates inMeta
                        (alphabet', _, _, _)
                          | null stateNameValues = getMetadataFromInputSymbolsAndTCM initialSymbolSet (undefined :: Matrix Word)
                          | otherwise            = getMetadataFromInputSymbolsStatesAndTCM (zip initialSymbolSet stateNameValues) (undefined :: Matrix Word)
                    in  (1, alphabet', Just (TCM.generate matrixDimension genAdditive, Additive))
              | otherwise           =
                    let stateNameValues = TNT.characterStates inMeta
                        (alphabet', _, _, _)
                          | null stateNameValues = getMetadataFromInputSymbolsAndTCM initialSymbolSet (undefined :: Matrix Word)
                          | otherwise            = getMetadataFromInputSymbolsStatesAndTCM (zip initialSymbolSet stateNameValues) (undefined :: Matrix Word)
                    in  (1, alphabet', Just (TCM.generate matrixDimension genFitch, NonAdditive))
              where
                matrixDimension   = length initialSymbolSet

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
                --   we must check for supplied state names.
                --
                -- * If the charcter has a Sankoff TCM defined, we truncate the
                --   character alphabet.
                initialSymbolSet =
                    case inChar of
                      TNT.Continuous {} -> undefined -- I'm sure this will never blow up in our face /s
                      TNT.Dna        {} -> V.fromListN <$> length <*> alphabetSymbols $       dnaAlphabet
                      TNT.Protein    {} -> V.fromListN <$> length <*> alphabetSymbols $ aminoAcidAlphabet
                      TNT.Discrete   {} -> V.fromListN <$> length <*> alphabetSymbols $  discreteAlphabet


instance HasNormalizedMetadata F.TCM where

    getNormalizedMetadata (F.TCM alph mat) =
        Just $ pure NormalizedMetadata
        { alphabet      = alphabet'
        , characterName = ""
        , weight        = coefficient
        , parsedTCM     = Just (resultTCM, structure)
        , isDynamic     = False
        , isIgnored     = False -- Maybe this should be True?
        }
      where
        (alphabet', coefficient, resultTCM, structure) = getMetadataFromInputSymbolsAndTCM alph mat


instance HasNormalizedMetadata VertexEdgeRoot where

    getNormalizedMetadata = const mempty


instance HasNormalizedMetadata Nexus where

    getNormalizedMetadata input@(Nexus (_, metas) _) =
        zipWith convertNexusMeta alphabetVector <$> VNE.fromVector metas
      where
        alphabetVector = developAlphabets $ getNormalizedCharacters input
        convertNexusMeta developedAlphabet inMeta =
            NormalizedMetadata
            { alphabet      = finalAlphabet
            , characterName = fromString . Nex.name $ inMeta
            , weight        = chosenWeight * suppliedWeight
            , parsedTCM     = chosenTCM
            , isDynamic     = not $ Nex.isAligned inMeta
            , isIgnored     = Nex.ignored inMeta
            }
          where
            suppliedWeight = fromIntegral $ Nex.weight inMeta
            
            (finalAlphabet, chosenWeight, chosenTCM)
              | Nex.additive inMeta =
                let (alphabet', _, _, _) = getMetadataFromInputSymbolsAndTCM developedAlphabet (undefined :: Matrix Word)
                in  (alphabet', 1, Just (TCM.generate (length $ Nex.alphabet inMeta) genAdditive, Additive))
              | otherwise =
                case F.transitionCosts <$> Nex.costM inMeta of
                  Nothing   ->
                    let (alphabet', _, _, _) = getMetadataFromInputSymbolsAndTCM developedAlphabet (undefined :: Matrix Word)
                    in  (alphabet', 1, Nothing)
                  Just vals ->
                    let (alphabet', coefficient, tcm, structure) = getMetadataFromInputSymbolsAndTCM developedAlphabet vals
                    in  (alphabet', coefficient, Just (tcm, structure))


{-
getMetadataFromInputSymbolsAndTCM :: (IsString a, FoldableWithKey t, Ord a, Real b) => t a -> Matrix b -> (Alphabet a, Double, TCM.TCM, TCMStructure)
getMetadataFromInputSymbolsAndTCM symbols mat = (alphabet', fromRational rationalWeight * fromIntegral coefficient, resultTCM, structure)
  where
    (alphabet', permMat) = fromSymbolsWithTCM symbols mat
    (rationalWeight, unfactoredTCM) = TCM.fromList $ toList permMat
    (coefficient, resultTCM, structure) =
        let r@(c,_,t) = (,,) <$> factoredWeight <*> factoredTcm <*> tcmStructure $ diagnoseTcm unfactoredTCM
        in  if t /= Additive
            then r
            else (c, snd . TCM.fromList $ toList permMat, t)


getMetadataFromInputSymbolsStatesAndTCM :: (IsString a, FoldableWithKey t, Ord a, Real b) => t (a, a) -> Matrix b -> (Alphabet a, Double, TCM.TCM, TCMStructure)
getMetadataFromInputSymbolsStatesAndTCM symbols mat = (alphabet', fromRational rationalWeight * fromIntegral coefficient, resultTCM, structure)
  where
    (alphabet', permMat) = fromSymbolsWithStateNamesAndTCM symbols mat
    (rationalWeight, unfactoredTCM) = TCM.fromList $ toList permMat
    (coefficient, resultTCM, structure) =
        let r@(c,_,t) = (,,) <$> factoredWeight <*> factoredTcm <*> tcmStructure $ diagnoseTcm unfactoredTCM
        in  if t /= Additive
            then r
            else (c, snd . TCM.fromList $ toList permMat, t)
-}
