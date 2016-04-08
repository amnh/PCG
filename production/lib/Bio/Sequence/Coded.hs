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
-- TODO: Explain what the heck a coded sequence is, and what it's used for.
--
-----------------------------------------------------------------------------

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
-- TODO: fix and remove this ghc option:
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Bio.Sequence.Coded (CodedSequence(..), EncodedSeq, EncodedSequences, CodedChar(..), encodeAll, decodeMany) where

import           Prelude        hiding (and, head, or)
import           Bio.Sequence.Coded.Class
import           Bio.Sequence.Character.Coded
import           Bio.Sequence.Packed.Class
import           Bio.Sequence.Parsed
import           Control.Applicative   (liftA2)
import           Control.Monad
import           Data.Bits
import           Data.BitVector hiding (foldr, foldl, join, not)
import           Data.Foldable
import           Data.Maybe
import           Data.Monoid           ((<>))
import           Data.Vector           (Vector, fromList, singleton)
-- import qualified Data.Vector as V      (filter)

-- import GHC.Stack
-- import Data.Foldable
-- import Debug.Trace

-- TODO: Change EncodedSeq/Sequences to EncodedCharacters
        -- Make a missing a null vector
        -- Think about a nonempty type class or a refinement type for this

-- | EncodedSequences is short for a vector of EncodedSeq
type EncodedSequences = Vector EncodedSeq

-- | An EncodedSeq (encoded sequence) is a maybe vector of characters
-- TODO: Can we get rid of this Maybe, and just set to [0]0, instead?
-- TODO: change name to make clear the difference between a CodedSequence and an EncodedSeq
type EncodedSeq = BitVector

-- | Make EncodedSeq an instance of CodedSequence
instance CodedSequence EncodedSeq where
    decodeOverAlphabet encoded alphabet 
        | length alphabet == 0 = mempty
        | width  encoded == 0  = mempty
        | otherwise            = decodedSeq
            where 
                alphLen    = length alphabet
                decodedSeq = foldr (\theseBits acc -> (decodeOneChar theseBits alphabet) <> acc) mempty (group alphLen encoded)
    decodeOneChar inSeq alphabet = singleton $ foldr (\(charValExists, char) acc -> if charValExists 
                                                                        then char : acc 
                                                                        else acc
                                         ) [] (zip (toBits inSeq) alphabet)
    emptySeq = bitVec 0 0 -- TODO: Should this be bitVec alphLen 0?
    -- This works over minimal alphabet
    encodeOverAlphabet inSeq alphabet 
        | null inSeq = bitVec 0 0
        | otherwise  = foldr (\x acc -> (encodeOneChar alphabet x) <> acc ) (bitVec 0 0) inSeq 
    encodeOneChar alphabet inChar = bitRepresentation
        where 
        -- For each (yeah, foreach!) letter in (ordered) alphabet, decide whether it's present in the ambiguity group.
        -- Collect into [Bool].
            bits = map (flip elem inChar) alphabet
            bitRepresentation = fromBits bits
    filterGaps inSeq gap alphabet 
        | width gap == 0 = inSeq
        | otherwise      = if width inSeq == 0
                           then inSeq
                           else foldr (f gap) (bitVec 0 0) $ group alphLen inSeq
            where 
                alphLen = length alphabet
                f gapVal x acc = if   x ==. gapVal
                                 then x <> acc
                                 else acc

    grabSubChar inSeq pos alphLen = extract left right <$> inSeq
        where
            left = ((pos + 1) * alphLen) - 1
            right = pos * alphLen
    isEmpty seqs 
        | width seqs == 0 = True
        | otherwise       = seqs == zeroBits
    numChars inSeq alphLen 
        | width inSeq == 0 = 0
        | otherwise        = width inSeq `div` alphLen
    -- TODO: For next two fns, make instance of functor and (foldable?), traversable, instead. Make point-free?
    mapChars f alphLen inSeq = fmap f $ group alphLen inSeq
    foldrChars f acc alphLen inSeq = foldr f acc $ group alphLen inSeq

{-
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
    testBit bits i  = maybe False (`testBit` i) bits
-}

instance PackedSequence EncodedSeq where
    packOverAlphabet = undefined

instance CodedChar BitVector where
    gapChar alphLen = setBit (bitVec alphLen 0) 0

{-
-- | Get parsed sequenceS, return encoded sequenceS.
-- Recall that each is Vector of Maybes, to this type is actually
-- Vector Maybe Vector [String] -> Vector Maybe BV.
-- (I only wish I were kidding.)
encodeAll :: ParsedSequences -> EncodedSequences
encodeAll = fmap (\s -> join $ encode <$> s)
-}


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
-- Actually, it's unused. Do we even need it?
{-
setElemAt :: (Bits b) => String -> b -> [String] -> b
setElemAt char orig alphabet
    | char == "-" = setBit orig 0
    | otherwise = case elemIndex char alphabet of
                        Nothing -> orig
                        Just pos -> setBit orig (pos + 1)    
-}


-- | Functionality to unencode many encoded sequences
decodeMany :: EncodedSequences -> Alphabet -> ParsedSequences
decodeMany seqs alph = fmap (Just . flip decodeOverAlphabet alph) seqs