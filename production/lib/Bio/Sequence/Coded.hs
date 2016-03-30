-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Sequences.Coded
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

module Bio.Sequence.Coded (CodedSequence(..), EncodedSeq, EncodedSequences, CodedChar(..), encodeAll, unencodeMany) where

import           Prelude        hiding (and, head, or)
import           Bio.Sequence.Coded.Class
import           Bio.Sequence.Character.Coded
import           Bio.Sequence.Packed.Class
import           Bio.Sequence.Parsed
import           Control.Applicative   (liftA2)
import           Control.Monad
import           Data.Bits
import           Data.BitVector hiding (foldl, foldr, join, not)
import           Data.List             (elemIndex)
import           Data.Maybe
import           Data.Vector           (ifoldl, fromList, foldr, Vector, (!))
-- import qualified Data.Vector as V      (filter)

-- import GHC.Stack
-- import Data.Foldable
-- import Debug.Trace

-- TODO: Change EncodedSeq/Sequences to EncodedCharacters
        -- Make a missing a null vector
        -- Think about a nonempty type class or a refinement type for this

-- | EncodedSequences is short for a vector of EncodedSeq
-- TODO: change name to make clear the difference between a CodedSequence and an EncodedSequence
type EncodedSequences = Vector EncodedSeq

-- | An EncodedSeq (encoded sequence) is a maybe vector of characters
type EncodedSeq = Maybe BitVector

-- | Make EncodedSeq and instance of CodedSequence
-- TODO: Change so alphabet length doesn't need to be computed?
instance CodedSequence EncodedSeq where
    charToSeq x = Just x
    decode = undefined
    emptySeq = Nothing -- TODO: Should this be Just $ bitVec 1 0
    -- This works over minimal alphabet
    encode inSeq = encodeOverAlphabet inSeq alphabet 
        where
            alphabet = Data.Vector.foldr (\ambig acc -> filter (not . flip elem acc) ambig ++ acc) [] inSeq

    encodeOverAlphabet inSeq alphabet 
        | null inSeq = Nothing
        | otherwise  = Just $ Data.Vector.foldr (\x acc -> (createSingletonChar alphabet x) # acc ) zeroBits inSeq 
            
    filterSeq s condition = undefined -- liftA (V.filter condition) s
    grabSubChar s pos = undefined -- liftA (@. pos) s
    isEmpty seqs = case seqs of
        Nothing -> True
        Just x  -> x /= zeroBits
    numChars s = case s of 
        Nothing  -> 0
        Just vec -> width vec

-- | Sequence to map encoding over ParsedSequences
encodeAll :: ParsedSequences -> EncodedSequences
encodeAll = fmap (\s -> join $ encode <$> s)

-- TODO: Does this belong in this module?
instance PackedSequence EncodedSeq where
    packOverAlphabet = undefined

-- | Simple functionality to set a single element in a BitVector
-- That element is a singleton character, but may be ambiguous
-- For quickness, hardcoded as BV, so I could use fromBits.
-- If we generalize later, I'll have to update this.
createSingletonChar :: Alphabet -> AmbiguityGroup -> BitVector 
createSingletonChar alphabet inChar = bitRepresentation
    where 
        bits = map (flip elem inChar) alphabet
        bitRepresentation = fromBits bits

{-
-- | Simple functionality to set a single element in a bitvector
-- That element is a singleton character, but may be ambiguous
setElem :: Bits b => Alphabet -> b -> Int -> AmbiguityGroup -> b 
setElem alphabet curBit charNum ambChar = foldl g curBit ambChar
    where g curSeq char = case elemIndex char alphabet of
                       Nothing -> curSeq
                       Just idx -> setBit curSeq (idx + (charNum * alphLen))
-}

-- TODO: make sure this works under current, BV scheme
setElemAt :: (Bits b) => String -> b -> [String] -> b
setElemAt char orig alphabet
    | char == "-" = setBit orig 0
    | otherwise = case elemIndex char alphabet of
                        Nothing -> orig
                        Just pos -> setBit orig (pos + 1)    

-- | Added functionality for unencoded
unencodeOverAlphabet :: EncodedSeq -> Alphabet -> ParsedSeq 
unencodeOverAlphabet encoded alphabet = case encoded of 
    Nothing        -> mempty
    (Just allBits) -> decodedSeq
        where 
            alphLen = length alphabet
            decodeOneChar inBits = Prelude.foldr (\(charValExists, char) acc -> if charValExists 
                                                                               then char : acc 
                                                                               else acc
                                                ) [] (zip inBits alphabet)
            decodedSeq = fromList $ Prelude.foldr (\x acc -> (decodeOneChar (toBits x)) : acc) [] (group alphLen allBits)

-- | Functionality to unencode many
unencodeMany :: EncodedSequences -> Alphabet -> ParsedSequences
unencodeMany seqs alph = fmap (Just . flip unencodeOverAlphabet alph) seqs

instance Bits b => CodedChar b where
    gapChar = setBit zeroBits 0


instance Bits EncodedSeq where
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
