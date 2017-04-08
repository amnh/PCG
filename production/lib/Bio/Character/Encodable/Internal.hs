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
import Data.BitMatrix.Internal (BitMatrix(..))
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


bitVectorToBufferChunks :: Int -> Int -> BitVector -> [CULong]
bitVectorToBufferChunks elemWidth elemCount bv = fmap fromIntegral $ ((bv @@) <$> slices) <> tailWord
  where
    totalBits = elemWidth * elemCount
    (fullWords, remainingBits) = totalBits `divMod` 64
    slices   = take fullWords $ iterate ((64 +) `bimap` (64 +)) ((63, 0) :: (Int,Int))
    tailWord = if   remainingBits == 0
               then []
               else [ bv @@ (totalBits - 1, totalBits - remainingBits) ]


bufferChunksToBitVector :: Int -> Int -> [CULong] -> BitVector
bufferChunksToBitVector elemWidth elemCount chunks = bitVec totalBits . fst $ foldl' f initialAccumulator chunks
  where
    initialAccumulator :: (Integer, Int)
    initialAccumulator = (0,0)

    totalBits = elemWidth * elemCount

    f (summation, shiftDistance) e = (summation + addend, shiftDistance + 64)
      where
        addend = fromIntegral e `shift` shiftDistance
    

exportableCharacterElementsToBitMatrix :: ExportableCharacterElements -> BitMatrix
exportableCharacterElementsToBitMatrix ece = BitMatrix elementWidth $ foldMap (bitVec elementWidth) integralValues
  where
    elementWidth   = ece ^. exportedElementWidth
    integralValues = exportedCharacterElements ece


exportableCharacterElementsHeadToBitVector :: ExportableCharacterElements -> BitVector
exportableCharacterElementsHeadToBitVector ece = bitVec elementWidth $ head integralValues
  where
    elementWidth   = ece ^. exportedElementWidth
    integralValues = exportedCharacterElements ece
