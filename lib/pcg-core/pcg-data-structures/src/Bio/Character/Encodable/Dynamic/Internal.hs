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

module Bio.Character.Encodable.Dynamic.Internal
  ( DynamicCharacter (DC, Missing)
  , DynamicCharacters
  , DynamicCharacterElement()
  ) where

import           Bio.Character.Encodable.Dynamic.Class
import           Bio.Character.Encodable.Internal
import           Bio.Character.Encodable.Stream
import           Bio.Character.Exportable
import           Control.DeepSeq
import           Control.Lens                          hiding (mapping)
import           Data.Alphabet
import           Data.Alphabet.IUPAC
import qualified Data.Bimap                            as B
import           Data.BitMatrix
import           Data.Bits
import           Data.BitVector.LittleEndian
import           Data.Foldable
import           Data.Hashable
import           Data.Key
import           Data.List.NonEmpty                    (NonEmpty (..))
import qualified Data.List.NonEmpty                    as NE
import           Data.List.Utility                     (invariantTransformation, occurances)
import           Data.Maybe                            (fromJust)
import           Data.MonoTraversable
import           Data.Range
import           Data.String                           (fromString)
import           Data.Vector                           (Vector)
import           GHC.Generics
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary.Instances   ()
import           Text.XML
import           TextShow                              (TextShow (showb))


-- |
-- Represents an encoded dynamic character, consisting of one or more static
-- characters. 'DynamicCharacter's treat entire static characters as the
-- character states of the dynamic character. The dynamic character relies on
-- the encoding of the individual static characters to define the encoding of
-- the entire dynamic character.
data  DynamicCharacter
    = Missing {-# UNPACK #-} !Word
    | DC      {-# UNPACK #-} !BitMatrix
    deriving (Eq, Generic, Ord, Show)


-- |
-- Represents a sinlge element of a dynamic character.
newtype DynamicCharacterElement
      = DCE BitVector
      deriving stock   (Generic)
      deriving newtype (Bits, Eq, FiniteBits, MonoFoldable, MonoFunctor, Ord, Show, TextShow)


type instance Bound DynamicCharacterElement = Word


type instance Element DynamicCharacter = DynamicCharacterElement


type instance Element DynamicCharacterElement = Bool

-- |
-- A sequence of many 'DynamicCharacter's. Probably should be asserted as non-empty.
type DynamicCharacters = Vector DynamicCharacter


-- |
-- Functionality to unencode many encoded sequences
-- decodeMany :: DynamicCharacters -> Alphabet -> ParsedChars
-- decodeMany seqs alph = fmap (Just . decodeOverAlphabet alph) seqs


-- We restrict the 'DynamicCharacter' values generated to be non-empty.
-- Most algorithms assume a nonempty dynamic character.
instance Arbitrary DynamicCharacter where

    arbitrary = do
        alphabetLen  <- arbitrary `suchThat` (\x -> 2 <= x && x <= 62) :: Gen Int
        characterLen <- arbitrary `suchThat` (> 0) :: Gen Int
        let randVal  =  choose (1, 2 ^ alphabetLen - 1) :: Gen Integer
        bitRows      <- vectorOf characterLen randVal
        pure . DC . fromRows $ fromNumber (toEnum alphabetLen) <$> bitRows


instance Arbitrary DynamicCharacterElement where

    arbitrary = do
        alphabetLen <- arbitrary `suchThat` (\x -> 2 <= x && x <= 62) :: Gen Int
        DCE . fromNumber (toEnum alphabetLen) <$> (choose (1, 2 ^ alphabetLen - 1) :: Gen Integer)


instance CoArbitrary DynamicCharacterElement


instance EncodedAmbiguityGroupContainer DynamicCharacter where

    {-# INLINE symbolCount #-}
    symbolCount (Missing n) = n
    symbolCount (DC c)      = numCols c


instance EncodedAmbiguityGroupContainer DynamicCharacterElement where

    {-# INLINE symbolCount  #-}
    symbolCount = dimension . unwrap


instance EncodableDynamicCharacter DynamicCharacter where

    constructDynamic = DC . fromRows . fmap unwrap . toList

    destructDynamic = NE.nonEmpty . otoList

    encodeDynamic alphabet = encodeStream alphabet . NE.fromList . fmap (NE.fromList . toList) . toList


instance EncodableStream DynamicCharacter where

    decodeStream alphabet char
      | isAlphabetDna alphabet  = (dnaIUPAC B.!) <$> rawResult
      | isAlphabetRna alphabet  = (rnaIUPAC B.!) <$> rawResult
      | otherwise               = rawResult
      where
        rawResult    = NE.fromList . ofoldMap (pure . decodeElement alphabet) . otoList $ char
        dnaIUPAC     = convertBimap iupacToDna
        rnaIUPAC     = convertBimap iupacToRna
        convertBimap = B.mapR (fmap fromString) . B.map (fmap fromString)

    encodeStream alphabet = DC . fromRows . fmap (unwrap . encodeElement alphabet) . toList

    lookupStream (DC bm) i
      | 0 <= i    = let !j = toEnum i
                    in  if j < numRows bm
                        then Just . DCE $ bm `row` j
                        else Nothing
      | otherwise = Nothing
    lookupStream _ _ = Nothing

    {-# INLINE gapOfStream #-}
    gapOfStream = bit . fromEnum . pred . symbolCount


instance EncodableStreamElement DynamicCharacterElement where

    decodeElement alphabet character =
        case foldMapWithKey f alphabet of
          []   -> error "Attempting to decode an empty dynamic character element."
          x:xs -> x:|xs
      where
        f i symbol
          | character `testBit` i = [symbol]
          | otherwise             = []

    -- Use foldl here to do an implicit reversal of the alphabet!
    -- The head element of the list is the most significant bit when calling fromBits.
    -- We need the first element of the alphabet to correspond to the least significant bit.
    -- Hence foldl, don't try foldMap or toList & fmap without careful thought.
    encodeElement alphabet ambiguity = DCE . fromBits $ (`elem` ambiguity) <$> toList alphabet


instance Enum DynamicCharacterElement where

    fromEnum = toUnsignedNumber . unwrap

    toEnum i = DCE $ fromNumber dim i
      where
        dim = toEnum $ finiteBitSize i - countLeadingZeros i


instance Exportable DynamicCharacter where

    toExportableBuffer Missing {} = error "Attempted to 'Export' a missing dynamic character to foreign functions."
    toExportableBuffer (DC bm) = ExportableCharacterSequence x y . bitVectorToBufferChunks x y $ expandRows bm
      where
        x = numRows bm
        y = numCols bm

    fromExportableBuffer ecs = DC $ factorRows elemWidth newBitVec
      where
        newBitVec = bufferChunksToBitVector elemCount elemWidth $ exportedBufferChunks ecs
        elemCount = ecs ^. exportedElementCount
        elemWidth = ecs ^. exportedElementWidth

    toExportableElements = encodableStreamToExportableCharacterElements

    fromExportableElements = DC . exportableCharacterElementsToBitMatrix


instance Exportable DynamicCharacterElement where

    toExportableBuffer e@(DCE bv) = ExportableCharacterSequence 1 widthValue $ bitVectorToBufferChunks 1 widthValue bv
      where
        widthValue = symbolCount e

    fromExportableBuffer ecs = DCE newBitVec
      where
        newBitVec = bufferChunksToBitVector 1 elemWidth $ exportedBufferChunks ecs
        elemWidth = ecs ^. exportedElementWidth

    toExportableElements e@(DCE bv)
      | bitsInElement > bitsInLocalWord = Nothing
      | otherwise                       = Just $ ExportableCharacterElements 1 bitsInElement [toUnsignedNumber bv]
      where
        bitsInElement   = symbolCount e

    fromExportableElements = DCE . exportableCharacterElementsHeadToBitVector


instance Hashable DynamicCharacter where

    hashWithSalt salt (Missing n) = salt `xor` fromEnum n
    hashWithSalt salt (DC bm) = salt `xor` fromEnum (numRows bm) `xor` hashWithSalt salt (toUnsignedNumber (expandRows bm) :: Integer)


instance MonoFoldable DynamicCharacter where

    {-# INLINE ofoldMap #-}
    ofoldMap _ Missing{} = mempty
    ofoldMap f (DC c)    = ofoldMap (f . DCE) c

    {-# INLINE ofoldr #-}
    ofoldr _ e Missing{} = e
    ofoldr f e (DC c)    = ofoldr (f . DCE) e c

    {-# INLINE ofoldl' #-}
    ofoldl' _ e Missing{} = e
    ofoldl' f e (DC c)    = ofoldl' (\acc x -> f acc (DCE x)) e c

    {-# INLINE ofoldr1Ex #-}
    ofoldr1Ex _ Missing{} = error "Trying to mono-morphically fold over an empty structure without supplying an inital accumulator!"
    ofoldr1Ex f (DC c)    = DCE . ofoldr1Ex (\x y -> unwrap $ f (DCE x) (DCE y)) $ c

    {-# INLINE ofoldl1Ex' #-}
    ofoldl1Ex' _ Missing{} = error "Trying to mono-morphically fold over an empty structure without supplying an inital accumulator!"
    ofoldl1Ex' f (DC c)    = DCE . ofoldl1Ex' (\x y -> unwrap $ f (DCE x) (DCE y)) $ c

    {-# INLINE onull #-}
    onull Missing{} = True
    onull _         = False

    {-# INLINE olength #-}
    olength Missing{} = 0
    olength (DC c)    = olength c

    {-# INLINE headEx #-}
    headEx dc =
      case dc of
        (DC c) | (not . onull) c -> DCE $ headEx c
        _                        -> error $ "call to DynamicCharacter.headEx with: " <> show dc

    {-# INLINE lastEx #-}
    lastEx dc =
      case dc of
        (DC c) | (not . onull) c -> DCE $ lastEx c
        _                        -> error $ "call to DynamicCharacter.lastEx with: " <> show dc


instance MonoFunctor DynamicCharacter where

    omap f bm =
        case f <$> otoList bm of
          []   -> bm
          dces -> case invariantTransformation finiteBitSize dces of
             Just i  -> DC . factorRows (toEnum i) $ foldMap unwrap dces
             Nothing -> error $ unlines
                 [ "The mapping function over the Dynamic Character did not return *all* all elements of equal length."
                 , show . occurances $ finiteBitSize <$> dces
                 , show $ finiteBitSize <$> dces
                 , unlines $ foldMap (\x -> if x then "1" else "0") . toBits . unwrap <$> dces
                 , show bm
                 ]


instance MonoTraversable DynamicCharacterElement where

    {-# INLINE otraverse #-}
    otraverse f = fmap (DCE . fromBits) . traverse f . otoList

    {-# INLINE omapM #-}
    omapM = otraverse


instance NFData DynamicCharacter

instance NFData DynamicCharacterElement


instance PossiblyMissingCharacter DynamicCharacter where

    {-# INLINE toMissing  #-}
    toMissing c = Missing $ symbolCount c

    {-# INLINE isMissing  #-}
    isMissing Missing{} = True
    isMissing _         = False


instance Ranged DynamicCharacterElement where

    toRange dce = fromTupleWithPrecision (firstSetBit, lastSetBit) totalBits
      where
        firstSetBit = toEnum $ countLeadingZeros dce
        lastSetBit  = toEnum . max 0 $ totalBits - countTrailingZeros dce - 1
        totalBits   = finiteBitSize dce

    fromRange x
      | ub == lb  = toDCE $ 2 ^ ub
      | otherwise = allBitsUpperBound `xor` allBitsLowerBound
      where
        toDCE = DCE . fromNumber dim
        dim   = toEnum . fromJust $ precision x
        ub    = upperBound x
        lb    = lowerBound x
        allBitsUpperBound = toDCE $ (2 :: Integer) ^ upperBound x - 1
        allBitsLowerBound = toDCE $ (2 :: Integer) ^ lowerBound x - 1

    zeroRange sc = fromTupleWithPrecision (0,0) $ finiteBitSize sc


instance TextShow DynamicCharacter where

    showb (Missing w)  = "Missing " <> showb w
    showb (DC      bm) = "DC "      <> showb bm


instance ToXML DynamicCharacter where

    toXML dynamicChar = xmlElement "Dynamic_character" attributes contents
        where
            attributes            = []
            contents              = Left . contentTuple <$> otoList dynamicChar -- toXML on all dynamic character elements
            contentTuple (DCE bv) = ("Character_states", (\x -> if x then '1' else '0') <$> toBits bv) -- the value of this character

{- Don't think I need this, since it's taken care of in ToXML DynamicCharacter
instance ToXML DynamicCharacterElement where

    toXML (DCE bv) = xmlElement "Dynamic_character_element" attributes content
        where
            attributes = []
            content    = [("Character_states", (\x -> if x then '1' else '0') <$> toBits bv)] -- the value of this character
-}

{-
{-# INLINE unstream #-}
unstream :: DynamicCharacter -> BitMatrix
unstream (DC x) = x
-}

{-# INLINE unwrap #-}
unwrap :: DynamicCharacterElement -> BitVector
unwrap (DCE x) = x
