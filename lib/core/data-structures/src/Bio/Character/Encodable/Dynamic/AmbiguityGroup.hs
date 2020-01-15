-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Character.Encodable.Dynamic.Internal
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Data structures and instances for coded characters
-- Coded characters are dynamic characters recoded as
--
-----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE UnboxedSums                #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Bio.Character.Encodable.Dynamic.AmbiguityGroup
  ( AmbiguityGroup(..)
  ) where

import           Bio.Character.Encodable.Internal
import           Bio.Character.Encodable.Stream
import           Bio.Character.Exportable
import           Control.DeepSeq
import           Control.Lens
import           Data.Bits
import           Data.BitVector.LittleEndian
import           Data.Foldable
import           Data.Key
import           Data.List.NonEmpty                    (NonEmpty (..))
import           Data.Maybe                            (fromJust)
import           Data.MonoTraversable
import           Data.Range
import           GHC.Generics
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary.Instances   ()
import           TextShow                              (TextShow (showb), toString)


-- |
-- Represents an ambiguity group of symbols, one atomic component of a DynamicCharacter.
--
-- *NOTE:* That the dynamic character element will require a number of bits equal
-- to the size of the alphabet to represent the power-set of possible symbols.
newtype AmbiguityGroup
      = AG { packAmbiguityGroup :: BitVector }
      deriving stock   (Generic)
      deriving newtype (Bits, Eq, FiniteBits, MonoFoldable, MonoFunctor, NFData, Ord)


type instance Element AmbiguityGroup = Bool


type instance Bound AmbiguityGroup = Word


instance Arbitrary AmbiguityGroup where

    arbitrary = do
        alphabetLen <- arbitrary `suchThat` (\x -> 2 <= x && x <= 62) :: Gen Int
        AG . fromNumber (toEnum alphabetLen) <$> (choose (1, 2 ^ alphabetLen - 1) :: Gen Integer)


instance CoArbitrary AmbiguityGroup


instance EncodedAmbiguityGroupContainer AmbiguityGroup where

    {-# INLINE symbolCount #-}
    symbolCount = dimension . packAmbiguityGroup


instance EncodedGapElementContainer AmbiguityGroup where

    {-# INLINE getGapElement #-}
    getGapElement = bit . fromEnum . pred . dimension . packAmbiguityGroup


instance EncodableStreamElement AmbiguityGroup where

    decodeElement alphabet character@(AG bv)
      | aLen /= cLen = error errMsg
      | otherwise    =
        case foldMapWithKey f alphabet of
          []   -> error "Attempting to decode an empty dynamic character element."
          x:xs -> x:|xs
      where
        f i symbol
          | character `testBit` i = [symbol]
          | otherwise             = []
        aLen   = toEnum $ length alphabet
        cLen   = symbolCount character
        errMsg = fold
          [ "The alpabet size ("
          , show $ aLen
          , ") does not match the character size ("
          , show $ cLen
          , "), "
          , show bv
          ]

    -- Use foldl here to do an implicit reversal of the alphabet!
    -- The head element of the list is the most significant bit when calling fromBits.
    -- We need the first element of the alphabet to correspond to the least significant bit.
    -- Hence foldl, don't try foldMap or toList & fmap without careful thought.
    encodeElement alphabet ambiguity = AG . fromBits $ (`elem` ambiguity) <$> toList alphabet


instance Enum AmbiguityGroup where

    fromEnum = toUnsignedNumber . unwrap

    toEnum i = AG $ fromNumber dim i
      where
        dim = toEnum $ finiteBitSize i - countLeadingZeros i


instance ExportableBuffer AmbiguityGroup where

    toExportableBuffer e@(AG bv) = ExportableCharacterBuffer 1 widthValue $ bitVectorToBufferChunks 1 widthValue bv
      where
        widthValue = symbolCount e

    fromExportableBuffer ecs = AG newBitVec
      where
        newBitVec = bufferChunksToBitVector 1 elemWidth $ exportedBufferChunks ecs
        elemWidth = ecs ^. exportedElementWidth


instance MonoTraversable AmbiguityGroup where

    {-# INLINE otraverse #-}
    otraverse f = fmap (AG . fromBits) . traverse f . otoList

    {-# INLINE omapM #-}
    omapM = otraverse


instance Ranged AmbiguityGroup where

    toRange dce = fromTupleWithPrecision (firstSetBit, lastSetBit) totalBits
      where
        firstSetBit = toEnum $ countLeadingZeros dce
        lastSetBit  = toEnum . max 0 $ totalBits - countTrailingZeros dce - 1
        totalBits   = finiteBitSize dce

    fromRange x
      | ub == lb  = toAG $ 2 ^ ub
      | otherwise = allBitsUpperBound `xor` allBitsLowerBound
      where
        toAG = AG . fromNumber dim
        dim  = toEnum . fromJust $ precision x
        ub   = upperBound x
        lb   = lowerBound x
        allBitsUpperBound = toAG $ (2 :: Integer) ^ upperBound x - 1
        allBitsLowerBound = toAG $ (2 :: Integer) ^ lowerBound x - 1

    zeroRange sc = fromTupleWithPrecision (0,0) $ finiteBitSize sc


instance Show AmbiguityGroup where

    show = toString . showb


instance TextShow AmbiguityGroup where

    showb (AG bv) = ofoldMap g bv
      where
        g b | b         = "1"
            | otherwise = "0"


{-# INLINE unwrap #-}
unwrap :: AmbiguityGroup -> BitVector
unwrap (AG x) = x
