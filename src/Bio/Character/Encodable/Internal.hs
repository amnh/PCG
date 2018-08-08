-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Character.Encodable.Internal
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Bio.Character.Encodable.Internal
  ( EncodedAmbiguityGroupContainer(..)
  , PossiblyMissingCharacter(..)
  , bitVectorToBufferChunks
  , bufferChunksToBitVector
  , exportableCharacterElementsToBitMatrix
  , exportableCharacterElementsHeadToBitVector
  ) where

import           Bio.Character.Exportable
import           Control.Lens
import           Data.Bifunctor              (bimap)
import           Data.BitMatrix              (BitMatrix, fromRows)
import           Data.Bits
import           Data.BitVector.LittleEndian
import           Data.Foldable
import           Foreign.C.Types


-- |
-- Represents a type which stores one or more abiguity groups from an alphabet.
-- Allows /O(1)/ derivation of the number of possibly-present symbols in the
-- ambiguity group.
class EncodedAmbiguityGroupContainer w where

    symbolCount :: w -> Word


-- |
-- Represents a type which may be a missing character.
-- Allows for /O(1)/ testing and construction of missing characters.
class PossiblyMissingCharacter c where

    toMissing :: c -> c

    isMissing :: c -> Bool


-- | (✔)
instance PossiblyMissingCharacter c => PossiblyMissingCharacter (Maybe c) where

    toMissing = fmap toMissing

    isMissing = maybe False isMissing


-- |
-- A local compile-time constant defining the width of the 'CULong' type. Useful
-- for ensuring a safe exporting to C or C++ code over the FFI interface.
longWidth :: Word
longWidth = toEnum $ finiteBitSize (minBound :: CULong)


-- |
-- Converts a 'BitVector' to a collection of 'CULong' values which represent the
-- packed memory layout of the 'BitVector'.
--
-- The inverse of 'bufferChunksToBitVector'.
--
bitVectorToBufferChunks :: Word -> Word -> BitVector -> [CULong]
bitVectorToBufferChunks elemWidth elemCount bv = fmap toUnsignedNumber $ ((`subRange` bv) <$> slices) <> tailWord
  where
    totalBits = elemWidth * elemCount
    (fullWords, remainingBits) = totalBits `divMod` longWidth
    slices   = take (fromEnum fullWords) $ iterate ((longWidth +) `bimap` (longWidth +)) ((0, longWidth - 1) :: (Word, Word))
    tailWord = if   remainingBits == 0
               then []
               else [ (totalBits - remainingBits, totalBits - 1) `subRange` bv ]


-- |
-- Converts a collection of 'CULong' values to a 'BitVector'.
--
-- The inverse of 'bitVectorToBufferChunks'.
--
bufferChunksToBitVector :: Foldable t => Word -> Word -> t CULong -> BitVector
bufferChunksToBitVector elemWidth elemCount chunks = fromNumber totalBits . fst $ foldl' f initialAccumulator chunks
  where
    initialAccumulator :: (Integer, Int)
    initialAccumulator = (0,0)

    totalBits = elemWidth * elemCount

    longWidth' = fromEnum longWidth

    f (summation, shiftDistance) e = (summation + addend, shiftDistance + longWidth')
      where
        addend = fromIntegral e `shift` shiftDistance


-- Use 'Data.BitMatrix.fromRows', which corrects the Semigroup operator for
-- 'BitVector's to behave correctly.
-- |
-- Converts an exportable character context to a 'BitMatrix'.
exportableCharacterElementsToBitMatrix :: ExportableCharacterElements -> BitMatrix
exportableCharacterElementsToBitMatrix ece = fromRows $ fromNumber elementWidth <$> integralValues
  where
    elementWidth   = ece ^. exportedElementWidth
    integralValues = exportedCharacterElements ece


-- |
-- Converts an exportable character context to a 'BitVector'.
exportableCharacterElementsHeadToBitVector :: ExportableCharacterElements -> BitVector
exportableCharacterElementsHeadToBitVector ece = fromNumber elementWidth $ head integralValues
  where
    elementWidth   = ece ^. exportedElementWidth
    integralValues = exportedCharacterElements ece
