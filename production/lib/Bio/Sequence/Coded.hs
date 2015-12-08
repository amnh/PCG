

module Bio.Sequence.Coded where

import Prelude hiding (map, length, zipWith, null, foldr, head)
import Data.Vector (map, length, zipWith, empty, null, foldr, Vector, head, (!))
import Control.Monad (join)
import Data.Functor ((<$>))
import Data.Bits
import Data.Maybe

type EncodedSeq b = Maybe (Vector (EncodedChar b))
type EncodedChar b = Vector b

instance Bits b => Bits (Vector b) where
    (.&.) bit1 bit2     
        | (length bit1) /= (length bit2) = error "Attempt to take and of bits of different length"
        | otherwise = zipWith (.&.) bit1 bit2
    (.|.) bit1 bit2 
        | (length bit1) /= (length bit2) = error "Attempt to take or of bits of different length"
        | otherwise = zipWith (.|.) bit1 bit2
    xor bit1 bit2 = (.&.) ((.|.) bit1 bit2) (complement ((.&.) bit1 bit2))
    complement bit = map complement bit
    shift bit s = map (\b -> shift b s) bit
    rotate bit r = map (\b -> rotate b r) bit
    setBit _ _= empty -- these methods are not meaningful so they just wipe it
    bit _ = empty
    bitSize bit 
        | null bit = 0
        | otherwise = (length bit) * (bitSize $ head bit)
    bitSizeMaybe bit 
        | null bit = Just 0
        | otherwise = ((length bit) *) <$> (bitSizeMaybe $ head bit)
    isSigned bit 
        | null bit = True
        | otherwise = isSigned $ head bit
    popCount bit = foldr (\b acc -> acc + popCount b) 0 bit
    testBit bit index
        | null bit = False
        | otherwise = 
            let
                myBit = div index (bitSize $ head bit)
                myPos = rem index (bitSize $ head bit)
            in testBit (bit ! myBit) myPos

instance Bits b => Bits (Maybe b) where
    (.&.) bit1 bit2
        | isNothing bit1 || isNothing bit2 = Nothing
        | otherwise = Just $ (fromJust bit1) .&. (fromJust bit2)
    (.|.) bit1 bit2 
        | isNothing bit1 || isNothing bit2 = Nothing
        | otherwise = Just $ (fromJust bit1) .|. (fromJust bit2)
    xor bit1 bit2 = (.&.) ((.|.) bit1 bit2) (complement ((.&.) bit1 bit2))
    complement bit
        | isNothing bit = Nothing
        | otherwise = Just $ complement $ fromJust bit
    shift bit s
        | isNothing bit = Nothing
        | otherwise = Just $ shift (fromJust bit) s
    rotate bit r
        | isNothing bit = Nothing
        | otherwise = Just $ rotate (fromJust bit) r
    setBit bit s
        | isNothing bit = Nothing
        | otherwise = Just $ setBit (fromJust bit) s
    bit i = Just (bit i)
    bitSize bit
        | isNothing bit = 0
        | otherwise = bitSize $ fromJust bit
    bitSizeMaybe bit
        | isNothing bit =  Nothing
        | otherwise = bitSizeMaybe $ fromJust bit
    isSigned bit 
        | isNothing bit = False
        | otherwise = isSigned $ fromJust bit
    popCount bit
        | isNothing bit = 0
        | otherwise = popCount $ fromJust bit
    testBit bit index
        | isNothing bit = False
        | otherwise = testBit (fromJust bit) index
