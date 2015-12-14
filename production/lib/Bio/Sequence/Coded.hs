{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Bio.Sequence.Coded (CodedSequence(..), EncodedSeq (..), EncodedSequences(..)) where

import Prelude hiding (map, length, zipWith, null, foldr, head)
import Control.Applicative  (liftA2, liftA)
import Data.Vector    (map, length, zipWith, empty, null, foldr, Vector, head, (!), singleton, (!?))
import Data.Bits
import Data.Maybe
import Bio.Sequence.Coded.Class
import Control.Monad (join, liftM2)
import Data.Monoid

-- | An encoded sequence is stored as a Maybe of an encoded sequence character
type EncodedSequences b = Vector (EncodedSeq b)
type EncodedSeq b = Maybe (EncodedChar b)
type EncodedChar b = Vector b

instance Bits b => CodedSequence (EncodedSeq b) b where
    numChars s = case s of 
        Nothing -> 0
        Just vec -> length vec
    gapChar = Just $ singleton $ bit 1
    grabSubChar s pos = liftA ((flip (!)) pos) s
    emptySeq = Nothing
    isEmpty = isNothing

-- | To make this work, EncodedSeq is also an instance of bits because a Vector of bits is and a Maybe bits is
instance Bits b => Bits (Vector b) where
    (.&.) bit1 bit2     
        | length bit1 /= length bit2 = error "Attempt to take and of bits of different length"
        | otherwise = zipWith (.&.) bit1 bit2
    (.|.) bit1 bit2 
        | length bit1 /= length bit2 = error "Attempt to take or of bits of different length"
        | otherwise = zipWith (.|.) bit1 bit2
    xor bit1 bit2 = (.&.) ((.|.) bit1 bit2) (complement ((.&.) bit1 bit2))
    complement = map complement
    shift  bits s = (`shift`  s) <$> bits
    rotate bits r = (`rotate` r) <$> bits
    setBit _ _= empty -- these methods are not meaningful so they just wipe it
    bit _ = empty
    bitSize bits 
        | null bits = 0
        | otherwise = length bits * (fromMaybe 0 . bitSizeMaybe $ head bits)
    bitSizeMaybe bits 
        | null bits = Just 0
        | otherwise = (length bits *) <$> bitSizeMaybe (head bits)
    isSigned bits 
        | null bits = True
        | otherwise = isSigned $ head bits
    popCount = foldr (\b acc -> acc + popCount b) 0
    testBit bits index
        | null bits = False
        | otherwise =
          case bitSizeMaybe $ head bits of
            Nothing      -> False
            Just numBits -> let myBit = div index numBits
                                myPos = rem index numBits
                            in testBit (bits ! myBit) myPos

instance Bits b => Bits (Maybe b) where
    (.&.)           = liftA2 (.&.)
    (.|.)           = liftA2 (.|.)
    xor             = liftA2 xor
    complement      = fmap complement
    shift  bits s   = fmap (`shift`  s) bits
    rotate bits r   = fmap (`rotate` r) bits
    setBit bits s   = fmap (`setBit` s) bits
    bit             = Just . bit
    bitSize         = fromMaybe 0 . (bitSizeMaybe =<<)
    bitSizeMaybe    = (bitSizeMaybe =<<)
    isSigned        = maybe False isSigned
    popCount        = maybe 0 popCount
    testBit  bits i = maybe False (`testBit` i) bits
