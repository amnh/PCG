-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Sequences.Coded.Internal
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Data structures and instances for coded characters
-- TODO: Explain what the heck a coded character is, and what it's used for.
--
-----------------------------------------------------------------------------

-- TODO: Remove all commented-out code.

-- TODO: are all of these necessary?
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, TypeFamilies #-}
-- TODO: fix and remove this ghc option (is it needed for Arbitrary?):
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Bio.Sequence.Coded.Internal where

import           Prelude        hiding (and, head, or)
import           Bio.Sequence.Coded.Class
import           Bio.Sequence.Packed.Class
import           Bio.Sequence.Parsed
import           Data.Bits
import           Data.BitVector hiding (foldr, foldl, join, not)
import           Data.Foldable
import           Data.Function.Memoize
import           Data.Monoid           ((<>))
--import           Data.MonoTraversable
import           Data.Vector           (Vector, fromList, ifilter)
import           Test.Tasty.QuickCheck

-- TODO: Change DynamicChar/Sequences to DynamicCharacters
        -- Make a missing a null vector
        -- Think about a nonempty type class or a refinement type for this

-- | DynamicChars is short for a vector of DynamicChar
type DynamicChars = Vector DynamicChar

-- | An DynamicChar (encoded sequence) is a maybe vector of characters
-- TODO: change name to make clear the difference between a CodedSequence and an DynamicChar
data DynamicChar
   = DynamicChar
   { alphLen   :: Int
   , character :: BitVector
   , gap       :: BitVector
   } deriving (Eq, Show)

concatCharacter :: DynamicChar -> DynamicChar -> DynamicChar
concatCharacter (DynamicChar len bv1 g) (DynamicChar _ bv2 _) = DynamicChar len (bv1 <> bv2) g

--data EncodedSequenceOverAlphabet a = forall a. Bits a => BBV Int a

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

-- | Make DynamicChar an instance of EncodableDynamicCharacter
instance EncodableDynamicCharacter DynamicChar where
    decodeOverAlphabet alphabet (DynamicChar n inChar gc) -- n is alphabet length
        | length alphabet == 0 = mempty
        | width inChar    == 0 = mempty
        | length alphabet /= n = error "Alphabet lengths don't match in a CodedChar instance."
        | otherwise            = decodedSeq
            where
                decodedSeq = foldr (\theseBits acc -> (decodeOneChar alphabet (DynamicChar n theseBits gc)) <> acc) mempty (group n inChar)

    decodeOneChar alphabet (DynamicChar _ inChar _) = pure . toList $ ifilter (\i _ -> inChar `testBit` i) alphabet

    emptyChar = DynamicChar 0 zeroBitVec zeroBitVec -- TODO: Should this be bitVec alphLen 0?

    -- This works over minimal alphabet
    encodeOverAlphabet alphabet inChar = foldl' concatCharacter emptyChar $ encodeOneChar alphabet <$> inChar

    encodeOneChar alphabet inChar = DynamicChar alphabetLen bitRepresentation (bitVec alphabetLen (0 :: Integer) `setBit` (alphabetLen - 1))
        where
        -- For each (yeah, foreach!) letter in (ordered) alphabet, decide whether it's present in the ambiguity group.
        -- Collect into [Bool].
            alphabetLen = (length alphabet)
            bits = (`elem` inChar) <$> alphabet
            bitRepresentation = fromBits $ toList bits

    filterGaps (DynamicChar n inChar g) = DynamicChar n ((foldl' f zeroBitVec) $ group n inChar) g
        where
            f acc x = if   x ==. g
                      then acc <> x
                      else acc
    gapChar (DynamicChar n _ g) = DynamicChar n g g

    getAlphLen (DynamicChar n _ _) = n

    grabSubChar (DynamicChar n inChar g) pos = DynamicChar n (extract high low inChar) g
        where
            high = ((pos + 1) * n) - 1
            low  = pos * n

    isEmpty (DynamicChar _ inChar _) = width inChar == 0

    numChars (DynamicChar n inChar _)
        | n == 0    = 0
        | otherwise = width inChar `div` n

zeroBitVec :: BitVector
zeroBitVec = bitVec 0 (0 :: Integer)

{-
instance Bits DynamicChar where
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

instance Memoizable BitVector where
    memoize f char = memoize (f . bitVec w) (nat char)
                        where w = width char

instance PackedSequence DynamicChar where
    packOverAlphabet = undefined

{-
-- | Get parsed sequenceS, return encoded sequenceS.
-- Recall that each is Vector of Maybes, to this type is actually
-- Vector Maybe Vector [String] -> Vector Maybe BV.
-- (I only wish I were kidding.)
encodeAll :: ParsedSequences -> DynamicChars
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
decodeMany :: DynamicChars -> Alphabet -> ParsedSequences
decodeMany seqs alph = fmap (Just . decodeOverAlphabet alph) seqs

instance Arbitrary BitVector where
    arbitrary = fromBits <$> listOf (arbitrary :: Gen Bool)

instance Arbitrary b => Arbitrary (Vector b) where
    arbitrary = fromList <$> listOf arbitrary

instance Arbitrary DynamicChar where
    arbitrary = do 
        len    <- arbitrary :: Gen Int
        charCt <- arbitrary :: Gen Int
        charBv <- vector (charCt * len) :: Gen [Bool]
        let gapBv = setBit (bitVec len (0 :: Integer)) (len - 1)
        pure $ DynamicChar len (fromBits charBv) gapBv
