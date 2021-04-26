-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Character.Encodable.Internal
-- Copyright   :  (c) 2015-2021 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Bio.Character.Encodable.Internal
  ( EncodedAmbiguityGroupContainer(..)
  , EncodedGapElementContainer(..)
  , PossiblyMissingCharacter(..)
  , bitVectorToBufferChunks
  , bufferChunksToBitVector
  ) where

import Control.Lens
import Data.BitVector.LittleEndian
import Data.Bits
import Data.Foldable
import Foreign.C.Types


-- |
-- Represents a type which stores one or more ambiguity groups from an alphabet.
-- Allows /O(1)/ derivation of the number of possibly-present symbols in the
-- ambiguity group.
class EncodedAmbiguityGroupContainer b where

    symbolCount :: b -> Word


-- |
-- Represents a type which can store a "gap" element.
-- Allows /O(1)/ derivation of the "gap" element.
class EncodedGapElementContainer b where

    getGapElement :: b -> b


-- |
-- Represents a type which may be a missing character.
-- Allows for /O(1)/ testing and construction of missing characters.
class PossiblyMissingCharacter c where

    toMissing :: c -> c

    isMissing :: c -> Bool


instance PossiblyMissingCharacter c => PossiblyMissingCharacter (Maybe c) where

    toMissing = fmap toMissing

    isMissing = any isMissing


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
    tailWord = [ (totalBits - remainingBits, totalBits - 1) `subRange` bv
               | remainingBits /= 0
               ]


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


{-
-- |
-- Converts an exportable character context to a 'BitVector'.
exportableCharacterElementsHeadToBitVector :: ExportableCharacterElements -> BitVector
exportableCharacterElementsHeadToBitVector ece = fromNumber elementWidth $ head integralValues
  where
    elementWidth   = ece ^. exportedElementWidth
    integralValues = exportedCharacterElements ece
-}
