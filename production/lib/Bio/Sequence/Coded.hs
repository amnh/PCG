{-# OPTIONS_GHC -fno-warn-orphans #-}

module Bio.Sequence.Coded where

import Prelude hiding (map, length, zipWith, null, foldr, head)
import Data.Vector (map, length, zipWith, empty, null, foldr, Vector, head, (!), singleton)
import Data.Bits
import Data.Maybe
import Bio.Sequence.Coded.Class
import Control.Monad (join)
import Data.Monoid

-- | An encoded sequence is stored as a Maybe of an encoded sequence character
type EncodedSeq b = Maybe (EncodedChar b)
type EncodedChar b = Vector b

instance Bits b => CodedSequence (EncodedSeq b) where
    numChars s = case s of 
        Nothing -> 0
        Just vec -> length vec
    gapChar = Just $ singleton $ singleton $ bit 1
    grabChar s pos = (flip $ (!) s) <$> s

instance Monoid EncodedSeq where
    mempty = Nothing
    mappend s1 s2 = liftM2 mappend s1 s2

-- | To make this work, EncodedSeq is also an instance of bits because a Vector of bits is and a Maybe bits is
instance Bits b => Bits (Vector b) where
    (.&.) bit1 bit2     
        | (length bit1) /= (length bit2) = error "Attempt to take and of bits of different length"
        | otherwise = zipWith (.&.) bit1 bit2
    (.|.) bit1 bit2 
        | (length bit1) /= (length bit2) = error "Attempt to take or of bits of different length"
        | otherwise = zipWith (.|.) bit1 bit2
    xor bit1 bit2 = (.&.) ((.|.) bit1 bit2) (complement ((.&.) bit1 bit2))
    complement x = map complement x
    shift  bits s = map (\b -> shift  b s) bits
    rotate bits r = map (\b -> rotate b r) bits
    setBit _ _= empty -- these methods are not meaningful so they just wipe it
    bit _ = empty
    bitSize bits 
        | null bits = 0
        | otherwise = (length bits) * (maybe 0 id . bitSizeMaybe $ head bits)
    bitSizeMaybe bits 
        | null bits = Just 0
        | otherwise = ((length bits) *) <$> (bitSizeMaybe $ head bits)
    isSigned bits 
        | null bits = True
        | otherwise = isSigned $ head bits
    popCount bits = foldr (\b acc -> acc + popCount b) 0 bits
    testBit bits index
        | null bits = False
        | otherwise =
          case bitSizeMaybe $ head bits of
            Nothing      -> False
            Just numBits -> let myBit = div index numBits
                                myPos = rem index numBits
                            in testBit (bits ! myBit) myPos

instance Bits b => Bits (Maybe b) where
    (.&.) bit1 bit2
        | isNothing bit1 || isNothing bit2 = Nothing
        | otherwise = Just $ (fromJust bit1) .&. (fromJust bit2)
    (.|.) bit1 bit2 
        | isNothing bit1 || isNothing bit2 = Nothing
        | otherwise = Just $ (fromJust bit1) .|. (fromJust bit2)
    xor bit1 bit2 = (.&.) ((.|.) bit1 bit2) (complement ((.&.) bit1 bit2))
    complement bits
        | isNothing bits = Nothing
        | otherwise = Just $ complement $ fromJust bits
    shift bits s
        | isNothing bits = Nothing
        | otherwise = Just $ shift (fromJust bits) s
    rotate bits r
        | isNothing bits = Nothing
        | otherwise = Just $ rotate (fromJust bits) r
    setBit bits s
        | isNothing bits = Nothing
        | otherwise = Just $ setBit (fromJust bits) s
    bit i = Just (bit i)
    bitSize bits
        | isNothing bits = 0
        | otherwise = maybe 0 id . bitSizeMaybe $ fromJust bits
    bitSizeMaybe bits
        | isNothing bits =  Nothing
        | otherwise = bitSizeMaybe $ fromJust bits
    isSigned bits
        | isNothing bits = False
        | otherwise = isSigned $ fromJust bits
    popCount bits
        | isNothing bits = 0
        | otherwise = popCount $ fromJust bits
    testBit bits index
        | isNothing bits = False
        | otherwise = testBit (fromJust bits) index
