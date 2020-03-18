-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Normalization.Metadata.Internal
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

{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Data.Normalization.Metadata.Internal
  ( NormalizedMetadata(..)
  , developAlphabets
  , makeEncodeInfo
  , getMetadataFromInputSymbolsAndTCM
  , getMetadataFromInputSymbolsStatesAndTCM
  , genAdditive
  , genFitch
  ) where

import           Control.DeepSeq
import           Data.Alphabet
import           Data.Data
import           Data.Foldable
import           Data.Key
import           Data.List.Utility            (transpose)
import           Data.Matrix.NotStupid        (Matrix)
import           Data.Normalization.Character
import           Data.Semigroup.Foldable      (Foldable1 (..))
import           Data.String                  (IsString)
import           Data.TCM                     (TCM, TCMDiagnosis (..), TCMStructure (..), diagnoseTcm)
import qualified Data.TCM                     as TCM
import           Data.Text.Short              (ShortText)
import           Data.Vector.NonEmpty         (Vector)
import qualified Data.Vector.NonEmpty         as VNE
import           GHC.Generics


-- |
-- An intermediate composite type for parse result coercion.
data  NormalizedMetadata
    = NormalizedMetadata
    { alphabet      :: Alphabet ShortText
    , characterName :: ShortText
    , weight        :: Double
    , parsedTCM     :: Maybe (TCM, TCMStructure)
    , isDynamic     :: Bool
    , isIgnored     :: Bool
    }
    deriving stock    (Data, Eq, Generic, Show, Typeable)
    deriving anyclass (NFData)


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
developAlphabets :: NormalizedCharacters -> Vector (Alphabet ShortText)
developAlphabets = VNE.fromNonEmpty . fmap (fromSymbols . foldMap f)
                 . transpose . fmap toNonEmpty . toList
  where
    f  NormalizedContinuousCharacter {}       = mempty
    f (NormalizedDiscreteCharacter   static ) = foldMap toList static
    f (NormalizedDynamicCharacter    dynamic) = foldMap (foldMap toList) dynamic


-- |
-- Functionality to make char info from tree seqs
makeEncodeInfo :: NormalizedCharacters -> Maybe (Vector NormalizedMetadata)
makeEncodeInfo = Just . fmap makeOneInfo . developAlphabets


-- |
-- Make a single info given an alphabet without state names
makeOneInfo :: Alphabet ShortText -> NormalizedMetadata
makeOneInfo alph =
    NormalizedMetadata
    { alphabet      = sortedAlphabet
    , characterName = ""
    , weight        = 1
    , parsedTCM     = Nothing
    , isDynamic     = True
    , isIgnored     = False
    }
  where
    (sortedAlphabet, _, _, _) = getMetadataFromInputSymbolsAndTCM alph (undefined :: Matrix Word)


getMetadataFromInputSymbolsAndTCM :: (IsString a, FoldableWithKey t, Ord a, Real b) => t a -> Matrix b -> (Alphabet a, Double, TCM, TCMStructure)
getMetadataFromInputSymbolsAndTCM symbols mat = (alphabet', fromRational rationalWeight * fromIntegral coefficient, resultTCM, structure)
  where
    (alphabet', permMat) = fromSymbolsWithTCM symbols mat
    (rationalWeight, unfactoredTCM) = TCM.fromList $ toList permMat
    (coefficient, resultTCM, structure) =
        let r@(c,_,t) = (,,) <$> factoredWeight <*> factoredTcm <*> tcmStructure $ diagnoseTcm unfactoredTCM
        in  if t /= Additive
            then r
            else (c, snd . TCM.fromList $ toList permMat, t)


getMetadataFromInputSymbolsStatesAndTCM :: (IsString a, FoldableWithKey t, Ord a, Real b) => t (a, a) -> Matrix b -> (Alphabet a, Double, TCM, TCMStructure)
getMetadataFromInputSymbolsStatesAndTCM symbols mat = (alphabet', fromRational rationalWeight * fromIntegral coefficient, resultTCM, structure)
  where
    (alphabet', permMat) = fromSymbolsWithStateNamesAndTCM symbols mat
    (rationalWeight, unfactoredTCM) = TCM.fromList $ toList permMat
    (coefficient, resultTCM, structure) =
        let r@(c,_,t) = (,,) <$> factoredWeight <*> factoredTcm <*> tcmStructure $ diagnoseTcm unfactoredTCM
        in  if t /= Additive
            then r
            else (c, snd . TCM.fromList $ toList permMat, t)


-- |
-- Generate the norm metric function.
genAdditive :: (Int, Int) -> Int
genAdditive (i,j) = max i j - min i j


-- |
-- Generate the discrete metric function.
genFitch :: (Int, Int) -> Int
genFitch    (i,j) = if i == j then 0 else 1
