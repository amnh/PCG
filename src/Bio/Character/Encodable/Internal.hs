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

module Bio.Character.Encodable.Internal where

import Bio.Character.Exportable
import Control.Lens
import Data.Bifunctor          (bimap)
import Data.BitMatrix.Internal (BitMatrix, fromRows)
import Data.Bits
import Data.BitVector
import Data.Foldable
import Data.Semigroup
import Foreign.C.Types


-- |
-- Represents a type which stores one or more abiuguity groups from an alphabet.
-- Allows /O(1)/ derivation of the number of possiblly prsent symbols in the
-- ambiguity group.
class EncodedAmbiguityGroupContainer w where

    symbolCount :: w -> Int


-- |
-- Represents a type which may be a missing character.
-- Allows for /O(1)/ testing and construction of missing character.
class PossiblyMissingCharacter c where

    toMissing :: c -> c

    isMissing :: c -> Bool


-- | (✔) 
instance PossiblyMissingCharacter c => PossiblyMissingCharacter (Maybe c) where

    toMissing = fmap toMissing

    isMissing = maybe False isMissing


-- |
-- A local compile time constant defining the width of the 'CULong' type. Useful
-- for ensuring a safe exporting to C or C++ code over the FFI interface.
longWidth :: Int
longWidth = finiteBitSize (minBound :: CULong)


-- |
-- Converts a 'BitVector' to a collection of 'CULong' values which represent the
-- packed memory layout of the 'BitVector'.
--
-- The inverse of 'bufferChunksToBitVector'.
--
bitVectorToBufferChunks :: Int -> Int -> BitVector -> [CULong]
bitVectorToBufferChunks elemWidth elemCount bv = fmap fromIntegral $ ((bv @@) <$> slices) <> tailWord
  where
    totalBits = elemWidth * elemCount
    (fullWords, remainingBits) = totalBits `divMod` longWidth
    slices   = take fullWords $ iterate ((longWidth +) `bimap` (longWidth +)) ((longWidth - 1, 0) :: (Int,Int))
    tailWord = if   remainingBits == 0
               then []
               else [ bv @@ (totalBits - 1, totalBits - remainingBits) ]


-- |
-- Converts a collection of 'CULong' values to a 'BitVector'.
--
-- The inverse of 'bitVectorToBufferChunks'.
--
bufferChunksToBitVector :: Foldable t => Int -> Int -> t CULong -> BitVector
bufferChunksToBitVector elemWidth elemCount chunks = bitVec totalBits . fst $ foldl' f initialAccumulator chunks
  where
    initialAccumulator :: (Integer, Int)
    initialAccumulator = (0,0)

    totalBits = elemWidth * elemCount

    f (summation, shiftDistance) e = (summation + addend, shiftDistance + longWidth)
      where
        addend = fromIntegral e `shift` shiftDistance
    

-- Use 'Data.BitMatrix.fromRows' which corrects the Semigroup operator for
-- 'BitVector's to behave correctly.
-- |
-- Converts a exportable character context to a 'BitMatrix'.
exportableCharacterElementsToBitMatrix :: ExportableCharacterElements -> BitMatrix
exportableCharacterElementsToBitMatrix ece = fromRows $ bitVec elementWidth <$> integralValues
  where
    elementWidth   = ece ^. exportedElementWidth
    integralValues = exportedCharacterElements ece


-- |
-- Converts a exportable character context to a 'BitVector'.
exportableCharacterElementsHeadToBitVector :: ExportableCharacterElements -> BitVector
exportableCharacterElementsHeadToBitVector ece = bitVec elementWidth $ head integralValues
  where
    elementWidth   = ece ^. exportedElementWidth
    integralValues = exportedCharacterElements ece
