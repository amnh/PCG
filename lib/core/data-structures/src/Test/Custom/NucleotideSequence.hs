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
  , NucleotideBasePair(..)
  , NucleotideSequence(..)
  ) where

import           Bio.Character.Encodable
import           Bio.Character.Encodable.Dynamic
import           Bio.Character.Encodable.Dynamic.Element
import           Control.Arrow                           ((&&&), (***))
import           Data.Alphabet
import           Data.Alphabet.IUPAC
import qualified Data.Bimap                              as B
import           Data.Bits
import           Data.Foldable
import           Data.Key
import           Data.List                               (delete)
import           Data.List.NonEmpty                      (NonEmpty(..))
import qualified Data.List.NonEmpty                      as NE
import           Data.Map                                (Map)
import           Data.MetricRepresentation
import           Data.MonoTraversable
import           Data.String                             (fromString)
import           Prelude                                 hiding (lookup)
import           Test.QuickCheck                         (Arbitrary(..))
import           Test.SmallCheck.Series                  hiding (NonEmpty)


-- |
-- Represents an arbitrary, non-empty ambiguity group which may include gaps.
newtype NucleotideBase = NB DynamicCharacterElement
    deriving newtype (Eq, Ord)


newtype NucleotideBasePair = NBP (NucleotideBase, NucleotideBase)
    deriving newtype (Eq, Ord)

-- |
-- Represents an arbitrary, non-empty sequence of nucleotide bases that may be
-- ambiguous and/or include gaps.
newtype NucleotideSequence = NS DynamicCharacter


instance Arbitrary NucleotideSequence where

    arbitrary = fmap NS . arbitraryDynamicCharacterOfWidth . toEnum $ length alphabet


instance Show NucleotideSequence where

    show (NS x) = fold ["(",shownDNA,",",shownContext,")"]
      where
        (shownDNA, shownContext) = (fold *** fold) . unzip
                                 $ (grabMedian &&& id) . renderBase <$> otoList x
        grabMedian = pure . head . tail


instance Show NucleotideBase where

    show (NB x) = renderBase x


instance Show NucleotideBasePair where

    show (NBP (NB x, NB y)) = fold ["(",renderBase x,",", renderBase y, ")"]


instance Monad m => Serial m NucleotideBase where

    series = generate $ const (NB <$> validNucleotideElements)


instance Monad m => Serial m NucleotideBasePair where

    series = generate $ const validPairs
      where
        validPairs = [ NBP (NB x, NB y)
                     | x <- validNucleotideElements
                     , y <- validNucleotideElements
                     , x <= y
                     ]


validNucleotideElements :: [DynamicCharacterElement]
validNucleotideElements = fold
   [ pure . gapElement . symbolCount $ head validMedians
   , buildElem deleteElement <$> validMedians
   , buildElem insertElement <$> validMedians
   , [ unionElem alignElement x y | x <- validMedians, y <- validMedians, x <= y ]
   ]
  where
    gap = getGapElement $ head validMedians

    med x y = fst (discreteMetricPairwiseLogic x y :: (AmbiguityGroup, Word))

    validMedians = fmap (encodeElement alphabet . NE.fromList) $
                     [] `delete` powerSet (toList alphabet)

    powerSet :: [a] -> [[a]]
    powerSet []     = [[]]
    powerSet (x:xs) = [x:ps | ps <- powerSet xs] <> powerSet xs

    buildElem
      :: (AmbiguityGroup -> AmbiguityGroup -> DynamicCharacterElement)
      -> AmbiguityGroup
      -> DynamicCharacterElement
    buildElem f x = f (med gap x) x

    unionElem
      :: (AmbiguityGroup -> AmbiguityGroup -> AmbiguityGroup -> DynamicCharacterElement)
      -> AmbiguityGroup
      -> AmbiguityGroup
      -> DynamicCharacterElement
    unionElem f x y = f (med x y) x y


alphabet :: Alphabet String
alphabet = fromSymbols ["A","C","G","T"]


renderBase :: DynamicCharacterElement -> String
renderBase x =
    let dnaIUPAC     = convertBimap iupacToDna
        convertBimap :: B.Bimap (NonEmpty String) (NonEmpty String) -> Map (NonEmpty String) Char
        convertBimap = fmap (head . NE.head) . B.toMapR . B.mapR (fmap fromString)
        decodeBase v = decodeElement alphabet v `lookup` dnaIUPAC
        errorMsg   v = error $ unlines [ "Could not find key for:", show v, show $ decodeElement alphabet v ]
        (pref, median, lVal, rVal) =
          case getContext x of
            Gapping   -> let g = bit . fromEnum $ symbolCount x - 1 in ('G', g, g, g)
            Deletion  -> let v = getRight x in ('D', getMedian x, getGapElement v, v)
            Insertion -> let v = getLeft  x in ('I', getMedian x, v, getGapElement v)
            Alignment -> let l = getLeft  x
                             r = getRight x
                             m = getMedian x
                         in  ('A', m, l, r)
    in  case decodeBase median of
          Nothing ->  errorMsg median
          Just a  ->
            case decodeBase lVal of
              Nothing ->  errorMsg lVal
              Just b  ->
                case decodeBase rVal of
                  Nothing ->  errorMsg rVal
                  Just c  -> [pref,a,b,c]
