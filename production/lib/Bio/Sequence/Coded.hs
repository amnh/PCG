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

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}
-- TODO: fix and remove this ghc option:
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Bio.Sequence.Coded
  ( CodedSequence(..)
  , EncodedSeq
  , EncodedSequences
  , CodedChar(..)
--  , encodeAll
  , decodeMany) where

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
import           Data.MonoTraversable
import           Data.Vector           (Vector, fromList, singleton)
-- import qualified Data.Vector as V      (filter)

-- import GHC.Stack
-- import Data.Foldable
-- import Debug.Trace

-- TODO: Change EncodedChar/Sequences to EncodedCharacters
        -- Make a missing a null vector
        -- Think about a nonempty type class or a refinement type for this

-- | EncodedChars is short for a vector of EncodedChar
type EncodedChars = Vector EncodedChar

-- | An EncodedChar (encoded sequence) is a maybe vector of characters
-- TODO: change name to make clear the difference between a CodedSequence and an EncodedChar
type EncodedChar = BitVector

--data EncodedSequenceOverAlphabet a = forall a. Bits a => BBV Int a

type instance Element DynamicCharacterBV = BitVector
data DynamicCharacterBV
   = DynamicBV Int BitVector
   deriving (Show)

unpackCharacters :: DynamicCharacterBV -> [Element DynamicCharacterBV]
unpackCharacters (DynamicBV n bv) = (bv @@) <$> [((c+1)*m-1,c*m) | c <- [0..n-1]]
  where
    m = width bv `div` n

instance MonoFunctor DynamicCharacterBV where
  omap f t@(DynamicBV n bv) = DynamicBV n . mconcat $ f <$> unpackCharacters t

instance MonoFoldable DynamicCharacterBV where
  -- | Map each element of a monomorphic container to a 'Monoid'
  -- and combine the results.
  ofoldMap f xs = ofoldr mempty (mappend . f)
  {-# INLINE ofoldMap #-}

  -- | Right-associative fold of a monomorphic container.
  ofoldr f e xs = foldr f e $ unpackCharacters xs
  {-# INLINE ofoldr #-}

  -- | Strict left-associative fold of a monomorphic container.
  ofoldl' f e xs = foldl' f e $ unpackCharacters xs
  {-# INLINE ofoldl' #-}

  -- | Right-associative fold of a monomorphic container with no base element.
  --
  -- Note: this is a partial function. On an empty 'MonoFoldable', it will
  -- throw an exception.
  --
  -- /See 'Data.MinLen.ofoldr1Ex' from "Data.MinLen" for a total version of this function./
  ofoldr1Ex f e xs = foldr1 f e $ unpackCharacters xs
  {-# INLINE ofoldr1Ex #-}

  -- | Strict left-associative fold of a monomorphic container with no base
  -- element.
  --
  -- Note: this is a partial function. On an empty 'MonoFoldable', it will
  -- throw an exception.
  --
  -- /See 'Data.MinLen.ofoldl1Ex'' from "Data.MinLen" for a total version of this function./
  ofoldl1Ex' f e xs = foldl1 f e $ unpackCharacters xs
  {-# INLINE ofoldl1Ex' #-}


  
{-
instance Foldable EncodedSequenceOverAlphabet where
    foldr f e (BBV n bv) = foldr f e $ g <$> [0 .. len-1]
      where
        len = bv `div` n
        g i = (clearBit (setBit zeroBits (n - 1)) (n - 1)) .|. (shiftR b right)
          where
            left  = ((i + 1) * n) - 1
            right = i * n
        g' i = (compliment (clearBit (setBit zeroBits (n - 1)) (n - 1))) .|. (shiftR b right)
-}

-- | Make EncodedChar an instance of CodedSequence
instance CodedChar EncodedChar where
    decodeOverAlphabet encoded alphabet 
        | length alphabet == 0 = mempty
        | width  encoded  == 0 = mempty
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
--    gapChar alphLen = setBit (bitVec alphLen 0) 0
    grabSubChar inSeq pos alphLen = extract left right inSeq
        where
            left = ((pos + 1) * alphLen) - 1
            right = pos * alphLen
    isEmpty seqs 
        | width seqs == 0 = True
        | otherwise       = seqs == zeroBits
    numChars inSeq alphLen 
        | width inSeq == 0 = 0
        | otherwise        = width inSeq `div` alphLen

{-
instance Bits EncodedChar where
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

instance PackedSequence EncodedChar where
    packOverAlphabet = undefined

{-
-- | Get parsed sequenceS, return encoded sequenceS.
-- Recall that each is Vector of Maybes, to this type is actually
-- Vector Maybe Vector [String] -> Vector Maybe BV.
-- (I only wish I were kidding.)
encodeAll :: ParsedSequences -> EncodedChars
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
decodeMany :: EncodedChars -> Alphabet -> ParsedSequences
decodeMany seqs alph = fmap (Just . flip decodeOverAlphabet alph) seqs
