-----------------------------------------------------------------------------
-- |
-- Module      :  Test.Custom.NucleotideSequence
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Arbitrary instance for dynamic characters.
--
-- Allows for base ambiguities and gaps. The sequence will be non-empty.
--
-----------------------------------------------------------------------------

{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Test.Custom.NucleotideSequence
  ( NucleotideBase(..)
  , NucleotideSequence(..)
  ) where

import           Bio.Character.Encodable
import           Bio.Character.Encodable.Dynamic
import           Bio.Character.Encodable.Dynamic.Element
import           Data.Alphabet
import           Data.Alphabet.IUPAC
import           Data.Bits
import           Data.BitMatrix
import qualified Data.Bimap                            as B                                            
import           Data.Foldable
import           Data.Key
import           Data.List              (delete)
import           Data.List.NonEmpty     (NonEmpty(..))
import qualified Data.List.NonEmpty     as NE
import           Data.Map               (Map)
import           Data.MonoTraversable
import           Data.String            (fromString)
import           Prelude                hiding (lookup)
import           Test.QuickCheck        (Arbitrary(..), Gen, suchThat, vectorOf)
import           Test.SmallCheck.Series hiding (NonEmpty)

import Debug.Trace


-- |
-- Represents an arbitrary, non-empty ambiguity group which may include gaps.
newtype NucleotideBase = NB DynamicCharacterElement


-- |
-- Represents an arbitrary, non-empty sequence of nucleotide bases that may be
-- ambiguous and/or include gaps.
newtype NucleotideSequence = NS DynamicCharacter


instance Arbitrary NucleotideSequence where

    arbitrary = fmap NS . arbitraryDynamicCharacterOfWidth . toEnum $ length alphabet


instance Show NucleotideSequence where

    show (NS x) = showDNA x
      where
        showDNA = foldMap renderBase . otoList


instance Show NucleotideBase where

    show (NB x) = renderBase x


instance Monad m => Serial m NucleotideBase where

    series = generate $ const (NB <$> validSpace)
      where
        validSpace   = fold
                     [ pure . gapElement . symbolCount $ head validMedians
                     , deleteElement <$> validMedians
                     , insertElement <$> validMedians
                     ,  alignElement <$> validMedians <*> validMedians
                     ]
        validMedians = fmap (encodeElement alphabet . NE.fromList) $ [] `delete` powerSet (toList alphabet)
        powerSet :: [a] -> [[a]]
        powerSet []     = [[]]
        powerSet (x:xs) = [x:ps | ps <- powerSet xs] <> powerSet xs


alphabet :: Alphabet String
alphabet = fromSymbols ["A","C","G","T"]


renderBase :: DynamicCharacterElement -> String
renderBase x =
    let dnaIUPAC     = convertBimap iupacToDna
        convertBimap :: B.Bimap (NonEmpty String) (NonEmpty String) -> Map (NonEmpty String) Char
        convertBimap = fmap (head . NE.head) . B.toMapR . B.mapR (fmap fromString)
        encoding     = case getContext x of
                         Gapping   -> bit . fromEnum $ symbolCount x - 1
                         Deletion  -> let v = getRight x in v .|. getGapElement v
                         Insertion -> let v = getLeft  x in v .|. getGapElement v
                         Alignment -> let l = getLeft  x
                                          r = getRight x
                                          m | l == r = l
                                            | popCount (l .&. r) > 0 = l .&. r
                                            | otherwise = l .|. r
                                      in  m
    in case decodeElement alphabet encoding `lookup` dnaIUPAC of
         Just v  -> [v]
         Nothing -> error $ unlines
                      [ "Could not find key for:"
                      , show encoding
                      , show $ decodeElement alphabet encoding
                      ]
