-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Character.Dynamic.Internal
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Data structures and instances for coded characters
-- Coded characters are dynamic characters recoded as 
--
-----------------------------------------------------------------------------

-- TODO: Remove all commented-out code.

-- TODO: are all of these necessary?
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies, TypeSynonymInstances, UndecidableInstances #-}
-- TODO: fix and remove this ghc option (is it needed for Arbitrary?):
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Bio.Character.Dynamic.Internal
  ( DynamicChar (DC)
  , DynamicChars
  ) where

import           Bio.Character.Dynamic.Class
import           Bio.Character.Stream
import           Bio.Character.Exportable.Class
import           Control.Arrow                       ((***))
import           Data.Alphabet
import           Data.Bifunctor                      (bimap)
import           Data.BitMatrix
import           Data.BitMatrix.Internal(BitMatrix(..))
import           Data.Char                           (toLower)
import           Data.Key
import           Data.Bits
import           Data.BitVector               hiding (foldr, join, not, replicate)
import           Data.Foldable
import qualified Data.List.NonEmpty           as NE
import qualified Data.Map                     as M
import           Data.Maybe                          (fromMaybe)
import           Data.Monoid
import           Data.MonoTraversable
import           Data.String                         (fromString)
import           Data.Tuple                          (swap)
import           Data.Vector                         (Vector)
import           Prelude                      hiding (lookup)
import           Test.Tasty.QuickCheck        hiding ((.&.))
import           Test.QuickCheck.Arbitrary.Instances ()

--import Debug.Trace

-- TODO: Change DynamicChar/Sequences to DynamicCharacters
        -- Make a missing a null vector
        -- Think about a nonempty type class or a refinement type for this

-- | Represents an encoded dynamic character, consisting of one or more static
--   characters. Dynamic characters treat entire static characters as the
--   character states of the dynamic character. The dynamic character relies on
--   the encoding of the individual static characters to defined the encoding of
--   the entire dynamic character.
newtype DynamicChar
      = DC BitMatrix
      deriving (Eq, Show)


type instance Element DynamicChar = BitVector


-- | A sequence of many dynamic characters. Probably should be asserted as non-empty.y
type DynamicChars = Vector DynamicChar


--instance NFData DynamicChar


instance MonoFunctor DynamicChar where
  omap f (DC bm) = DC $ omap f bm


instance MonoFoldable DynamicChar where
  -- | Map each element of a monomorphic container to a 'Monoid'
  -- and combine the results.
  {-# INLINE ofoldMap #-}
  ofoldMap f (DC bm) = ofoldMap f bm

  -- | Right-associative fold of a monomorphic container.
  {-# INLINE ofoldr #-}
  ofoldr f e (DC bm) = ofoldr f e bm

  -- | Strict left-associative fold of a monomorphic container.
  {-# INLINE ofoldl' #-}
  ofoldl' f e (DC bm) = ofoldl' f e bm

  -- | Right-associative fold of a monomorphic container with no base element.
  --
  -- Note: this is a partial function. On an empty 'MonoFoldable', it will
  -- throw an exception.
  --
  -- /See 'Data.MinLen.ofoldr1Ex' from "Data.MinLen" for a total version of this function./
  {-# INLINE ofoldr1Ex #-}
  ofoldr1Ex f (DC bm) = ofoldr1Ex f bm

  -- | Strict left-associative fold of a monomorphic container with no base
  -- element.
  --
  -- Note: this is a partial function. On an empty 'MonoFoldable', it will
  -- throw an exception.
  --
  -- /See 'Data.MinLen.ofoldl1Ex'' from "Data.MinLen" for a total version of this function./
  {-# INLINE ofoldl1Ex' #-}
  ofoldl1Ex' f (DC bm) = ofoldl1Ex' f bm

  {-# INLINE onull #-}
  onull (DC bm) = onull bm

  {-# INLINE olength #-}
  olength (DC bm) = numRows bm


-- | Monomorphic containers that can be traversed from left to right.
instance MonoTraversable DynamicChar where
    -- | Map each element of a monomorphic container to an action,
    -- evaluate these actions from left to right, and
    -- collect the results.
    {-# INLINE otraverse #-}
    otraverse f (DC bm) = DC <$> otraverse f bm

    -- | Map each element of a monomorphic container to a monadic action,
    -- evaluate these actions from left to right, and
    -- collect the results.
    {-# INLINE omapM #-}
    omapM = otraverse


instance EncodableStreamElement BitVector where

  decodeElement alphabet character = NE.fromList $ foldMapWithKey f alphabet
    where
      f i symbol
        | character `testBit` i = [symbol]
        | otherwise             = []

  -- Use foldl here to do an implicit reversal of the alphabet!
  -- The head element of the list is the most significant bit when calling fromBits.
  -- We need the first element of the alphabet to correspond to the least significant bit.
  -- Hence foldl, don't try foldMap or toList & fmap without careful thought.
  encodeElement alphabet ambiguity = fromBits $ foldl' (\xs x -> (x `elem` ambiguity) : xs) [] alphabet

  stateCount = width


instance EncodableStream DynamicChar where

  decodeStream alphabet char
    | alphabet /= dnaAlphabet = rawResult 
    | otherwise               = (dnaIUPAC !) <$> rawResult
    where
      rawResult   = NE.fromList . ofoldMap (pure . decodeElement alphabet) . otoList $ char
      dnaAlphabet = fromSymbols $ fromString <$> ["A","C","G","T"]
--      dnaIUPAC :: (IsString a, Ord a) => Map [a] [a]
      dnaIUPAC    = M.fromList . fmap (swap . (NE.fromList . pure . fromChar *** NE.fromList . fmap fromChar)) $ mapping
        where
          fromChar = fromString . pure
          mapping  = gapMap <> noGapMap <> [('-', "-")]
          gapMap   = (toLower *** (<> "-")) <$> noGapMap
          noGapMap =
            [ ('A', "A"   )
            , ('C', "C"   )
            , ('G', "G"   )
            , ('T', "T"   )
            , ('M', "AC"  ) 
            , ('R', "AG"  )
            , ('W', "AT"  )
            , ('S', "CG"  )
            , ('Y', "CT"  )
            , ('K', "GT"  )
            , ('V', "ACG" )
            , ('H', "ACT" )
            , ('D', "AGT" )
            , ('B', "CGT" )
            , ('N', "ACGT")
            ]

  encodeStream alphabet = DC . fromRows . fmap (encodeElement alphabet) . toList

  lookupStream (DC bm) i
    | 0 <= i && i < numRows bm = Just $ bm `row` i
    | otherwise                = Nothing


instance EncodableDynamicCharacter DynamicChar where

  constructDynamic = DC . fromRows . toList

  encodeDynamic alphabet = encodeStream alphabet . NE.fromList . fmap (NE.fromList . toList) . toList


-- TODO: Probably remove?
instance Bits DynamicChar where
    (.&.)        (DC lhs) (DC rhs)  = DC $ lhs  .&.  rhs
    (.|.)        (DC lhs) (DC rhs)  = DC $ lhs  .|.  rhs
    xor          (DC lhs) (DC rhs)  = DC $ lhs `xor` rhs
    complement   (DC b)             = DC $ complement b
    shift        (DC b)   n         = DC $ b `shift`  n
    rotate       (DC b)   n         = DC $ b `rotate` n
    setBit       (DC b)   i         = DC $ b `setBit` i
    testBit      (DC b)   i         = b `testBit` i
    bit i                           = DC $ fromRows [bit i]
    bitSize                         = fromMaybe 0 . bitSizeMaybe
    bitSizeMaybe (DC b)             = bitSizeMaybe b
    isSigned     (DC b)             = isSigned b
    popCount     (DC b)             = popCount b


{-
instance Memoizable DynamicChar where
    memoize f (DC bm) = memoize (f . DC) bm
-}


-- We restrict the DynamicChar values generated to be non-empty.
-- Most algorithms assume a nonempty dynamic character.
instance Arbitrary DynamicChar where
    arbitrary = do 
        symbolCount  <- arbitrary `suchThat` (\x -> 0 < x && x <= 62) :: Gen Int
        characterLen <- arbitrary `suchThat` (> 0) :: Gen Int
        let randVal  =  choose (1, 2 ^ symbolCount - 1) :: Gen Integer
        bitRows      <- vectorOf characterLen randVal
        pure . DC . fromRows $ bitVec symbolCount <$> bitRows


-- | Functionality to unencode many encoded sequences
-- decodeMany :: DynamicChars -> Alphabet -> ParsedChars
-- decodeMany seqs alph = fmap (Just . decodeOverAlphabet alph) seqs


instance Exportable DynamicChar where
    toExportable (DC bm@(BitMatrix _ bv)) =
        ExportableCharacterSequence
        { characterCount = x
        , characterWidth = y
        , bufferChunks   = fmap fromIntegral $ ((bv @@) <$> slices) <> tailWord
        }
      where
        x = numRows bm
        y = numCols bm
        totalBits = x * y
        (fullWords, remainingBits) = totalBits `divMod` 64
        slices   = take fullWords $ iterate ((64 +) `bimap` (64 +)) ((63, 0) :: (Int,Int))
        tailWord = if   remainingBits == 0
                   then []
                   else [ bv @@ (totalBits - 1, totalBits - remainingBits) ]
        
    fromExportable = undefined
