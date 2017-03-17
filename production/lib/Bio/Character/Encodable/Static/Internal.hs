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

-- TODO: Remove all commented-out code.

-- TODO: are all of these necessary?
{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies #-}
-- TODO: fix and remove this ghc option (is it needed for Arbitrary?):
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Bio.Character.Encodable.Static.Internal
  ( StaticCharacter()
  , StaticCharacterBlock()
  ) where

import           Bio.Character.Encodable.Internal
import           Bio.Character.Encodable.Static.Class
import           Bio.Character.Encodable.Stream
import           Bio.Character.Exportable.Class
import           Control.Arrow                       ((***))
import           Data.Alphabet
import           Data.Bits
import           Data.BitMatrix
import           Data.BitMatrix.Internal             (BitMatrix(..))
import           Data.BitVector               hiding (foldr, join, not, replicate)
import           Data.BitVector.Instances            ()
import           Data.Char                           (toLower)
import           Data.Foldable
import           Data.Key
import qualified Data.List.NonEmpty           as NE
import qualified Data.Map                     as M
import           Data.Monoid                  hiding ((<>))
import           Data.MonoTraversable
import           Data.Range
import           Data.Semigroup
import           Data.String                         (fromString)
import           Data.Tuple                          (swap)
import           Prelude                      hiding (lookup)
import           Test.QuickCheck.Arbitrary.Instances ()
import           Test.Tasty.QuickCheck        hiding ((.&.))

--import Debug.Trace

-- TODO: Change DynamicChar/Sequences to DynamicCharacters
        -- Make a missing a null vector
        -- Think about a nonempty type class or a refinement type for this

-- |
-- Represents an encoded static character. Supports binary and numeric operations.
newtype StaticCharacter
      = SC BitVector
      deriving (Bits, Eq, Enum, Integral, Num, Ord, Real, Show)


-- |
-- Represents an encoded stream of static characters, consisting of one or more
-- static characters. The static character stream relies on the encoding of the
-- individual static characters to defined the encoding of the entire static
-- character stream.
newtype StaticCharacterBlock
      = SCB BitMatrix
      deriving (Eq, Show)


type instance Element StaticCharacterBlock = StaticCharacter


instance EncodedAmbiguityGroupContainer StaticCharacter where

    {-# INLINE symbolCount #-}
    symbolCount = width . unwrap


instance FiniteBits StaticCharacter where

    {-# INLINE finiteBitSize #-}
    finiteBitSize = symbolCount

    -- Default implementation gets these backwards for no apparent reason.

    {-# INLINE countLeadingZeros #-}
    countLeadingZeros  = countLeadingZeros . unwrap

    {-# INLINE countTrailingZeros #-}
    countTrailingZeros = countTrailingZeros  . unwrap


instance PossiblyMissingCharacter StaticCharacter where

    {-# INLINE toMissing  #-}
    toMissing c = complement $ c `xor` c

    {-# INLINE isMissing  #-}
    isMissing c = c == toMissing c


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
          | otherwise                 = fromBits $ foldl' (\xs x -> (x `elem` ambiguity) : xs) [] alphabet
          where
            containsMissing = elem (fromString "?")


instance EncodableStaticCharacter StaticCharacter where

    {-# INLINE emptyStatic #-}
    emptyStatic (SC x) = SC $ bitVec (width x) (0 :: Integer)


instance MonoFunctor StaticCharacterBlock where

    {-# INLINE omap #-}
    omap f = SCB . omap (unwrap . f . SC) . unstream


instance Semigroup StaticCharacterBlock where

    (SCB lhs) <> (SCB rhs)
      | m == n    = SCB . expandVector m $ collapseRows lhs `mappend` collapseRows rhs
      | otherwise = error $ unwords ["Attempt to concatentate two StaticCharacterBlock of differing stateCounts:", show m, show n]
      where
        m = numCols lhs
        n = numCols rhs


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
    olength = numRows . unstream


-- | Monomorphic containers that can be traversed from left to right.
instance MonoTraversable StaticCharacterBlock where

    {-# INLINE otraverse #-}
    otraverse f = fmap SCB . otraverse (fmap unwrap . f . SC) . unstream

    {-# INLINE omapM #-}
    omapM = otraverse


instance EncodedAmbiguityGroupContainer StaticCharacterBlock where

    {-# INLINE symbolCount #-}
    symbolCount   = numCols . unstream


instance EncodableStream StaticCharacterBlock where

    decodeStream alphabet char
      | alphabet /= dnaAlphabet = rawResult
      | otherwise               = (dnaIUPAC !) <$> rawResult
      where
        rawResult   = NE.fromList . ofoldMap (pure . decodeElement alphabet) . otoList $ char
        dnaAlphabet = fromSymbols $ fromString <$> ["A","C","G","T"]
--        dnaIUPAC :: (IsString a, Ord a) => Map [a] [a]
        dnaIUPAC    = M.fromList . fmap (swap . (NE.fromList . pure . fromChar *** NE.fromList . fmap fromChar)) $ mapping
          where
            fromChar = fromString . pure
            mapping  = gapMap <> noGapMap <> [('-', "-")]
            gapMap   = (toLower *** (<> "-")) <$> noGapMap
            noGapMap =
              [ ('A', "A"   )
              , ('C', "C"   )
              , ('G', "G"   )
              , ('T', "T"   )
              , ('M', "AC"  )
              , ('R', "AG"  )
              , ('W', "AT"  )
              , ('S', "CG"  )
              , ('Y', "CT"  )
              , ('K', "GT"  )
              , ('V', "ACG" )
              , ('H', "ACT" )
              , ('D', "AGT" )
              , ('B', "CGT" )
              , ('N', "ACGT")
              ]

    encodeStream alphabet = SCB . fromRows . fmap (unwrap . encodeElement alphabet) . toList

    lookupStream (SCB bm) i
      | 0 <= i && i < numRows bm = Just . SC $ bm `row` i
      | otherwise                = Nothing

    {-# INLINE gapOfStream #-}
    gapOfStream = bit . pred . symbolCount


instance EncodableStaticCharacterStream StaticCharacterBlock where

    constructStaticStream = SCB . fromRows . fmap unwrap . toList


instance Arbitrary StaticCharacterBlock where
    arbitrary = do
        alphabetLen  <- arbitrary `suchThat` (\x -> 0 < x && x <= 62) :: Gen Int
        characterLen <- arbitrary `suchThat` (> 0) :: Gen Int
        let randVal  =  choose (1, 2 ^ alphabetLen - 1) :: Gen Integer
        bitRows      <- vectorOf characterLen randVal
        pure . SCB . fromRows $ bitVec alphabetLen <$> bitRows


instance Exportable StaticCharacterBlock where

    toExportableBuffer (SCB bm@(BitMatrix _ bv)) = ExportableCharacterSequence x y $ bitVectorToBufferChunks x y bv
      where
        x = numRows bm
        y = numCols bm

    fromExportableBuffer = undefined

    toExportableElements = encodableStreamToExportableCharacterElements

    fromExportableElements = SCB . exportableCharacterElementsToBitMatrix


type instance Bound StaticCharacter = Word


instance Ranged StaticCharacter where

    toRange :: a -> Range (Bound a)
    toRange sc = Range (countLeadingZeros sc) lastSetBit
        where
            lastSetBit = finiteBitSize sc - countTrailingZeros sc - 1

    fromRange :: Range (Bound a) -> a
    fromRange (Range lhs rhs) value = zeroVector .|. (allBitsUpperBound `xor` allBitsLowerBound)
        where
            allBitsUpperBound = 2 ^ rhs - 1
            allBitsLowerBound = 2 ^ lhs - 1
            zeroVector  = (zeroBits `setBit` boundaryBit) `clearBit` boundaryBit
            boundaryBit = symbolCount value - 1


{-# INLINE unstream #-}
unstream :: StaticCharacterBlock -> BitMatrix
unstream (SCB x) = x


{-# INLINE unwrap #-}
unwrap :: StaticCharacter -> BitVector
unwrap (SC x) = x
