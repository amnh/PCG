-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Megaparsec.Custom
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Data structures and instances for coded sequences
--
-----------------------------------------------------------------------------


{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Bio.Sequence.Coded (CodedSequence(..), EncodedSeq, EncodedSequences, CodedChar(..)) where

import Prelude hiding (map, length, zipWith, null, head)
import Data.List (elemIndex)
import Control.Applicative  (liftA2, liftA)
import Data.Vector    (map, length, zipWith, empty, null, Vector, head, (!), singleton)
import qualified Data.Vector as V (filter)
import Data.Bits
import Data.Maybe
import Bio.Sequence.Coded.Class
import Bio.Sequence.Character.Coded

-- | EncodedSequences is short for a vector of EncodedSeq
type EncodedSequences b = Vector (EncodedSeq b)

-- | An EncodedSeq (encoded sequence) is a maybe vector of characters
type EncodedSeq b = Maybe (Vector b)

-- | Make a coded sequence and coded character instances of bits
instance Bits b => CodedSequence (EncodedSeq b) b where
    numChars s = case s of 
        Nothing -> 0
        Just vec -> length vec
    charToSeq = Just . singleton
    grabSubChar s pos = liftA (! pos) s
    emptySeq = Nothing
    isEmpty = isNothing
    filterSeq s condition = liftA (V.filter condition) s
    encode strSeq = 
        let 
            alphabet = foldr (\ambig acc -> filter (not . (flip elem) acc) ambig ++ acc) [] strSeq
            coded = map (\ambig -> foldr (\c acc -> setElemAt c acc alphabet) zeroBits ambig) strSeq
            final = if length coded == 0 then Nothing else Just coded
        in final

    encodeOverAlphabet strSeq alphabet = 
        let
            coded = map (\ambig -> foldr (\c acc -> setElemAt c acc alphabet) zeroBits ambig) strSeq
            final = if length coded == 0 then Nothing else Just coded
        in final

setElemAt :: (Bits b) => String -> b -> [String] -> b
setElemAt char orig alphabet
    | char == "-" = setBit orig 0
    | otherwise = case elemIndex char alphabet of
                        Nothing -> orig
                        Just pos -> setBit orig (pos + 1)     

instance Bits b => CodedChar b where
    gapChar = setBit zeroBits 0

-- | To make this work, the underlying types are also an instance of bits because a Vector of bits is and a Maybe bits is
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
