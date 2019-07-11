-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Character.Encodable.Static.Internal
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

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Bio.Character.Encodable.Static.Internal
  ( StaticCharacter(SC)
  , StaticCharacterBlock()
  ) where

import           Bio.Character.Encodable.Internal
import           Bio.Character.Encodable.Static.Class
import           Bio.Character.Encodable.Stream
import           Bio.Character.Exportable
import           Control.DeepSeq
import           Control.Lens
import           Data.Alphabet
import           Data.Alphabet.IUPAC
import qualified Data.Bimap                           as B
import           Data.BitMatrix
import           Data.Bits
import           Data.BitVector.LittleEndian
import           Data.Foldable
import           Data.Key
import qualified Data.List.NonEmpty                   as NE
import           Data.Maybe
import           Data.Monoid                          ()
import           Data.MonoTraversable
import           Data.Range
import           Data.String                          (fromString)
import           GHC.Generics
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary.Instances  ()
import           Text.XML
import           TextShow                             (TextShow)


-- |
-- Represents an encoded static character. Supports binary and numeric operations.
newtype StaticCharacter
      = SC BitVector
      deriving stock   (Generic)
      deriving newtype (Arbitrary, Bits, Eq, MonoFunctor, MonoFoldable, Ord, Show, TextShow)


-- |
-- Represents an encoded stream of static characters, consisting of one or more
-- static characters. The static character stream relies on the encoding of the
-- individual static characters to define the encoding of the entire static
-- character stream.
newtype StaticCharacterBlock
      = SCB BitMatrix
      deriving stock (Eq, Generic, Show)


type instance Bound StaticCharacter = Word


type instance Element StaticCharacter = Bool


type instance Element StaticCharacterBlock = StaticCharacter


instance Arbitrary StaticCharacterBlock where

    arbitrary = do
        alphabetLen  <- arbitrary `suchThat` (\x -> 0 < x && x <= 62) :: Gen Int
        characterLen <- arbitrary `suchThat` (> 0) :: Gen Int
        let randVal  =  choose (1, 2 ^ alphabetLen - 1) :: Gen Integer
        bitRows      <- vectorOf characterLen randVal
        pure . SCB . fromRows $ fromNumber (toEnum alphabetLen) <$> bitRows


instance CoArbitrary StaticCharacter


instance EncodableStaticCharacter StaticCharacter where

    {-# INLINE emptyStatic #-}
    emptyStatic (SC x) = SC $ fromNumber (dimension x) (0 :: Integer)


instance EncodableStaticCharacterStream StaticCharacterBlock where

    constructStaticStream = SCB . fromRows . fmap unwrap . toList


instance EncodableStream StaticCharacterBlock where

    decodeStream alphabet char
      | isAlphabetDna alphabet  = (dnaIUPAC B.!) <$> rawResult
      | isAlphabetRna alphabet  = (rnaIUPAC B.!) <$> rawResult
      | otherwise               = rawResult
      where
        rawResult    = NE.fromList . ofoldMap (pure . decodeElement alphabet) . otoList $ char
        dnaIUPAC     = convertBimap iupacToDna
        rnaIUPAC     = convertBimap iupacToRna
        convertBimap = B.mapR (fmap fromString) . B.map (fmap fromString)

    encodeStream alphabet = SCB . fromRows . fmap (unwrap . encodeElement alphabet) . toList

    lookupStream (SCB bm) i
      | 0 <= i = let j = toEnum i
                 in  if j < numRows bm
                     then Just . SC $ bm `row` j
                     else Nothing
      | otherwise = Nothing

    {-# INLINE gapOfStream #-}
    gapOfStream = bit . fromEnum . pred . symbolCount


instance EncodableStreamElement StaticCharacter where

    decodeElement alphabet character = NE.fromList $ foldMapWithKey f alphabet
      where
        f i symbol
          | character `testBit` i = [symbol]
          | otherwise             = []

    -- Use foldl here to do an implicit reversal of the alphabet!
    -- The head element of the list is the most significant bit when calling fromBits.
    -- We need the first element of the alphabet to correspond to the least significant bit.
    -- Hence foldl, don't try foldMap or toList & fmap without careful thought.
    encodeElement alphabet ambiguity = SC encoding
      where
        encoding
          | containsMissing ambiguity = fromBits $ replicate (length alphabet) True
          | otherwise                 = fromBits $ (`elem` ambiguity) <$> toList alphabet
          where
            containsMissing = elem (fromString "?")


instance EncodedAmbiguityGroupContainer StaticCharacter where

    {-# INLINE symbolCount #-}
    symbolCount = dimension . unwrap


instance EncodedAmbiguityGroupContainer StaticCharacterBlock where

    {-# INLINE symbolCount #-}
    symbolCount = numCols . unstream


instance Enum StaticCharacter where

    fromEnum = toUnsignedNumber . unwrap

    toEnum i = SC $ fromNumber dim i
      where
        dim = toEnum $ finiteBitSize i - countLeadingZeros i


instance Exportable StaticCharacter where

    toExportableBuffer e@(SC bv) = ExportableCharacterSequence 1 widthValue $ bitVectorToBufferChunks 1 widthValue bv
      where
        widthValue = symbolCount e

    fromExportableBuffer ecs = SC newBitVec
      where
        newBitVec = bufferChunksToBitVector 1 elemWidth $ exportedBufferChunks ecs
        elemWidth = ecs ^. exportedElementWidth

    toExportableElements e@(SC bv)
      | bitsInElement > bitsInLocalWord = Nothing
      | otherwise                       = Just $ ExportableCharacterElements 1 bitsInElement [toUnsignedNumber bv]
      where
        bitsInElement   = symbolCount e

    fromExportableElements = SC . exportableCharacterElementsHeadToBitVector


instance Exportable StaticCharacterBlock where

    toExportableBuffer (SCB bm) = ExportableCharacterSequence x y . bitVectorToBufferChunks x y $ expandRows bm
      where
        x = numRows bm
        y = numCols bm

    fromExportableBuffer = error "When did we start using static character block?! Please implement Exportable.fromExportableBuffer"

    toExportableElements = encodableStreamToExportableCharacterElements

    fromExportableElements = SCB . exportableCharacterElementsToBitMatrix


instance FiniteBits StaticCharacter where

    {-# INLINE finiteBitSize #-}
    finiteBitSize = finiteBitSize . unwrap

    {-# INLINE countLeadingZeros #-}
    countLeadingZeros  = countLeadingZeros . unwrap

    {-# INLINE countTrailingZeros #-}
    countTrailingZeros = countTrailingZeros  . unwrap


instance MonoFoldable StaticCharacterBlock where

    {-# INLINE ofoldMap #-}
    ofoldMap f = ofoldMap (f . SC) . unstream

    {-# INLINE ofoldr #-}
    ofoldr f e = ofoldr (f . SC) e . unstream

    {-# INLINE ofoldl' #-}
    ofoldl' f e = ofoldl' (\acc x -> f acc (SC x)) e . unstream

    {-# INLINE ofoldr1Ex #-}
    ofoldr1Ex f = SC . ofoldr1Ex (\x y -> unwrap $ f (SC x) (SC y)) . unstream

    {-# INLINE ofoldl1Ex' #-}
    ofoldl1Ex' f = SC . ofoldl1Ex' (\x y -> unwrap $ f (SC x) (SC y)) . unstream

    {-# INLINE onull #-}
    onull = const False

    {-# INLINE olength #-}
    olength = fromEnum . numRows . unstream


instance MonoFunctor StaticCharacterBlock where

    omap f (SCB c)  = SCB $ omap (unwrap . f . SC) c


instance MonoTraversable StaticCharacter where

    {-# INLINE otraverse #-}
    otraverse f = fmap (SC . fromBits) . traverse f . otoList

    {-# INLINE omapM #-}
    omapM = otraverse


instance NFData StaticCharacter


instance NFData StaticCharacterBlock


instance PossiblyMissingCharacter StaticCharacter where

    {-# INLINE toMissing  #-}
    toMissing c = complement $ c `xor` c

    {-# INLINE isMissing  #-}
    isMissing c = c == toMissing c


instance Ranged StaticCharacter where

    toRange sc = fromTupleWithPrecision (firstSetBit, lastSetBit) totalBits
        where
            firstSetBit = toEnum $ countLeadingZeros sc
            lastSetBit  = toEnum . max 0 $ totalBits - countTrailingZeros sc - 1
            totalBits   = finiteBitSize sc

    fromRange x
      | ub == lb  = toSC $ 2 ^ ub
      | otherwise = allBitsUpperBound `xor` allBitsLowerBound
      where
        toSC = SC . fromNumber dim
        dim  = toEnum . fromJust $ precision x
        ub   = upperBound x
        lb   = lowerBound x
        allBitsUpperBound = toSC $ (2 :: Integer) ^ upperBound x - 1
        allBitsLowerBound = toSC $ (2 :: Integer) ^ lowerBound x - 1
{-
      where
            allBitsUpperBound = SC . fromNumber (toEnum boundaryBit) $ (2 ^ upperBound x - 1 :: Integer)
            allBitsLowerBound = SC . fromNumber (toEnum boundaryBit) $ (2 ^ lowerBound x - 1 :: Integer)
            zeroVector  = (zeroBits `setBit` boundaryBit) `clearBit` boundaryBit
            boundaryBit = fromJust (precision x) - 1
-}

    zeroRange sc = fromTupleWithPrecision (0,0) $ finiteBitSize sc


instance Semigroup StaticCharacterBlock where

    (SCB lhs) <> (SCB rhs)
      | m == n    = SCB . factorRows m $ expandRows lhs `mappend` expandRows rhs
      | otherwise = error $ unwords ["Attempt to concatentate two StaticCharacterBlock of differing stateCounts:", show m, show n]
      where
        m = numCols lhs
        n = numCols rhs


instance ToXML StaticCharacter where

    toXML (SC bv) = xmlElement "Static_character" attributes contents
        where
            attributes = []
            contents   = [intRep, bitRep]
            intRep     = Left ("Integer_representation", show (toUnsignedNumber bv :: Integer))
            bitRep     = Left ("Bit_representation"    , (\x -> if x then '1' else '0') <$> toBits bv)


{-# INLINE unstream #-}
unstream :: StaticCharacterBlock -> BitMatrix
unstream (SCB x) = x


{-# INLINE unwrap #-}
unwrap :: StaticCharacter -> BitVector
unwrap (SC x) = x
