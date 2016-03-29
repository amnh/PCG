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

import           Prelude        hiding (and, head, map, or, zipWith)
import           Bio.Sequence.Coded.Class
import           Bio.Sequence.Character.Coded
import           Bio.Sequence.Packed.Class
import           Bio.Sequence.Parsed
import           Control.Applicative   (liftA2, liftA)
import           Control.Monad
import           Data.Bits
import           Data.BitVector hiding (foldr, join, not)
import           Data.List             (elemIndex)
import           Data.Maybe
import           Data.Vector           (map, zipWith, empty, Vector, head, (!), singleton)
import qualified Data.Vector as V      (filter)

import GHC.Stack
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

-- | Make a coded sequence and coded character instances of bits
instance CodedSequence EncodedSeq b where
    charToSeq = Just . bitVec 1 0
    emptySeq = nil
    -- This works over minimal alphabet
    encode strSeq = final
        where 
            alphabet = foldr (\ambig acc -> filter (not . flip elem acc) ambig ++ acc) [] strSeq
            coded = map (foldr (\c acc -> setElemAt c acc alphabet) zeroBits) strSeq
            final = if null coded 
                        then Nothing 
                        else Just coded

    encodeOverAlphabet strSeq inAlphabet = final
        where
            alphabet = inAlphabet
            coded = map (foldr (\c acc -> setElemAt c acc alphabet) zeroBits) strSeq
            final = if null coded then Nothing else Just coded
    filterSeq s condition = liftA (V.filter condition) s
    grabSubChar s pos = liftA (@. pos) s
    isEmpty = isNothing
    numChars s = case s of 
        Nothing -> 0
        Just vec -> width vec
    
    
    

----  = 
--    let
--        alphWidth = length alphabet
--        bitWidth = bitSizeMaybe gapChar
--        coded = map (\ambig -> foldr (\c acc -> setElemAt c acc alphabet) zeroBits ambig) strSeq
--        regroup = case bitWidth of
--                    Nothing -> ifoldr (\i val acc -> shift val (i * alphWidth) + acc) zeroBits coded
--                    Just width -> let grouped = foldr (\i acc -> (slice i alphWidth coded) ++ acc) mempty [alphWidth, 2 * alphWidth..width - alphWidth]
--                                  in P.map (\g -> ifoldr (\i val acc -> shift val (i * alphWidth) + acc)) grouped
--    in undefined

-- | Sequence to map encoding over ParsedSequences
encodeAll :: ParsedSequences -> EncodedSequences
encodeAll = fmap (\s -> join $ encode <$> s)

instance PackedSequence EncodedSeq b where
    packOverAlphabet = undefined

setElemAt :: (Bits b) => String -> b -> [String] -> b
setElemAt char orig alphabet
    | char == "-" = setBit orig 0
    | otherwise = case elemIndex char alphabet of
                        Nothing -> orig
                        Just pos -> setBit orig (pos + 1)    

-- | Added functionality for unencoded
unencodeOverAlphabet :: EncodedSeq -> Alphabet -> ParsedSeq 
unencodeOverAlphabet encoded alph = 
    let 
        alphLen = length alph
        oneBit inBit = foldr (\i acc -> if testBit inBit i then alph !! (i `mod` alphLen) : acc else acc) mempty [0..width inBit]
        allBits = fmap oneBit <$> encoded
    in fromMaybe mempty allBits

-- | Functionality to unencode many
unencodeMany :: EncodedSequences -> Alphabet -> ParsedSequences
unencodeMany seqs alph = fmap (Just . flip unencodeOverAlphabet alph) seqs

instance Bits b => CodedChar b where
    gapChar = setBit zeroBits 0


instance Bits EncodedSeq where
    (.&.)           = liftA2 and
    (.|.)           = liftA2 or
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
