-----------------------------------------------------------------------------
-- |
-- Module      :  Data.BitVector.LittleEndian
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- A bit vector similar to 'Data.BitVector' from the @bv@ package, however the
-- Endianness reversed. This module defines /Little-endian/ pseudo 
-- size-polymorphic bit-vectors.
--
-- Little-endian bit vectors are isomorphic to a @[Bool]@ with the /least/
-- significant bit at the head of the list and the /most/ significant bit at the
-- tail of the list.
--
-- Consequently, the endian-ness of a bit vector affects the 'Bits', 'FiniteBits',
-- 'Semigroup', 'Monoid', 'MonoFoldable', and 'MonoTraversable' instances. 
--
-- If you want Bitvectors which are isomorphic to a @[Bool]@ with the /most/
-- significant bit at the head of the list and the /least/ significant bit at the
-- tail of the list, then you should use the @bv@ package and /not/ @bv-little@.
--
-- This module does /not/ define numeric instances for 'BitVector'. This is 
-- intentional! If you want to interact with a bit vector as an 'Integral' value,
-- convert the 'BitVector' using either 'toSignedNumber' or 'toUnsignedNumber'.
--
-----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, MagicHash, TypeFamilies #-}

module Data.BitVector.LittleEndian
  ( BitVector()
  -- * Construction from values
  , bitvector
  -- * Queries
  , dimension
  , subRange
  -- * Numeric conversion
  , toSignedNumber
  , toUnsignedNumber
  -- * Bit stream conversion
  , fromBits
  , toBits
  ) where


import Control.DeepSeq
import Data.Bits
import Data.Data
import Data.Foldable
import Data.List.NonEmpty (NonEmpty(..))
import Data.Monoid        ()
import Data.MonoTraversable
import Data.Ord
import Data.Primitive.ByteArray
import Data.Semigroup
--import Data.Semigroup.Foldable
import Data.Word
import GHC.Exts
import GHC.Generics
import GHC.Integer.GMP.Internals
import GHC.Integer.Logarithms
import Test.QuickCheck (Arbitrary(..), CoArbitrary(..), NonNegative(..), suchThat)


data  BitVector
    = BV
    { dim :: !Int     -- ^ The /dimension/ of a bit-vector.
    , nat :: !Integer -- ^ The value of a bit-vector, as a natural number.
    } deriving (Data, Generic, Typeable)


type instance Element BitVector = Bool


instance Arbitrary BitVector where

    arbitrary = do
        dimVal <- (getNonNegative <$> arbitrary)
        let upperBound = 2^dimVal
        intVal <- (getNonNegative <$> arbitrary) `suchThat` (< upperBound)
        pure $ BV dimVal intVal


instance Bits BitVector where

    {-# INLINE (.&.) #-}
    (BV w1 a) .&. (BV w2 b) = BV (max w1 w2) $ a .&. b
  
    {-# INLINE (.|.) #-}
    (BV w1 a) .|. (BV w2 b) = BV (max w1 w2) $ a .|. b
  
    {-# INLINE xor #-}
    (BV w1 a) `xor` (BV w2 b) = BV (max w1 w2) $ a `xor` b
  
    {-# INLINE complement #-}
    complement (BV w n) = BV w $ 2^w - 1 - n
  
    {-# INLINE zeroBits #-}
    zeroBits = BV 0 0

    {-# INLINE bit #-}
    bit i = BV (i+1) (2^i)

    {-# INLINE clearBit #-}
    clearBit bv@(BV w n) i
      | i < 0 || i >= w = bv
      | otherwise       = BV w $ n `clearBit` i
  
{-
    {-# INLINE setBit #-}
    setBit bv@(BV w n) i
      | i < 0 || i >= w = bv
      | otherwise       = BV w $ n `setBit` i
-}

    {-# INLINE testBit #-}
    testBit (BV w n) i = i >= 0 && i < w && n `testBit` i
  
    bitSize = undefined

    {-# INLINE bitSizeMaybe #-}
    bitSizeMaybe (BV w _) = Just w

    {-# INLINE isSigned #-}
    isSigned = const False
  
    {-# INLINE shiftL #-}
    shiftL (BV w n) k
      | k > w     = BV w 0
      | otherwise = BV w $ shiftL n k `mod` 2^w
  
    {-# INLINE shiftR #-}
    shiftR (BV w n) k
      | k > w     = BV w 0
      | otherwise = BV w $ shiftR n k

    {-# INLINE rotateL #-}
    rotateL bv       0 = bv
    rotateL (BV w n) k
      | k == w    = BV w n
      | k >  w    = rotateL (BV w n) (k `mod` w)
      | otherwise = BV w $ h + l
      where
        s = w - k
        l = n `shiftR` s
        h = (n `shiftL` k) `mod` 2^w

    {-# INLINE rotateR #-}
    rotateR bv       0 = bv
    rotateR (BV w n) k
      | k == w    = BV w n
      | k >  w    = rotateR (BV w n) (k `mod` w)
      | otherwise = BV w $ h + l
      where
        s = w - k
        l = n `shiftR` k
        h = (n `shiftL` s) `mod` 2^w
  
    {-# INLINE popCount #-}
    popCount = popCount . nat


instance CoArbitrary BitVector


instance Eq BitVector where

    {-# INLINE (==) #-}
    (==) (BV w1 m) (BV w2 n) = w1 == w2 && m == n


instance FiniteBits BitVector where

    {-# INLINE finiteBitSize #-}
    finiteBitSize = dim

    {-# INLINE countTrailingZeros #-}
    countTrailingZeros (BV w n) = max 0 $ w - lastSetBit - 1
      where
        lastSetBit = I# (integerLog2# n)

    {-# INLINE countLeadingZeros #-}
    countLeadingZeros (BV w      0) = w
    countLeadingZeros (BV w intVal) =
        case intVal of
          S#       v  -> countTrailingZeros $ iMask .|. (I# v)
          Jp# (BN# v) -> f $ ByteArray v
          Jn# (BN# v) -> f $ ByteArray v
      where
        iMask = (complement zeroBits) `xor` (2 ^ w - 1)

        f :: ByteArray -> Int
        f byteArr = g 0
          where
            (q, r) = w `quotRem` 64
            wMask  = (complement zeroBits) `xor` (2 ^ r - 1) :: Word64

            g :: Int -> Int
            g !i
              | i >= q = countTrailingZeros $ wMask .|. value
              | otherwise = 
                  case countTrailingZeros value of
                    64 -> 64 + g (i+1)
                    v  -> v
              where
                value :: Word64
                value = byteArr `indexByteArray` i


instance Monoid BitVector where

    {-# INLINE mappend #-}
    mappend = (<>)

    {-# INLINE mconcat #-}
    mconcat bs =
        case bs of
          []   -> mempty
          x:xs -> sconcat $ x:|xs

    {-# INLINE mempty #-}
    mempty = BV 0 0


instance MonoFoldable BitVector where

    {-# INLINE ofoldMap #-}
    ofoldMap f = mconcat . fmap f. toBits

    {-# INLINE ofoldr #-}
    ofoldr f e = foldr f e . toBits

    {-# INLINE ofoldl' #-}
    ofoldl' f e = foldl' f e . toBits

    {-# INLINE ofoldr1Ex #-}
    ofoldr1Ex f = foldr1 f . toBits

    {-# INLINE ofoldl1Ex' #-}
    ofoldl1Ex' f = foldl1 f . toBits

    {-# INLINE onull #-}
    onull   = (== 0) . dim

    {-# INLINE olength #-}
    olength = dim


instance MonoFunctor BitVector where

    omap f (BV w n) = BV w . go w $ n `xor` n
    -- NB: 'setBit' is a GMP function, faster than regular addition.
      where 
        go  0 !acc = acc 
        go !i !acc = go i' acc'
          where
            i' = i - 1
            acc'
              | f (testBit n i') = acc `setBit` i'
              | otherwise        = acc
            


instance MonoTraversable BitVector where

    {-# INLINE otraverse #-}
    otraverse f = fmap fromBits . traverse f . toBits

    {-# INLINE omapM #-}
    omapM = otraverse


instance NFData BitVector where

    -- Already a strict data type,
    -- always in normal form.
    {-# INLINE rnf #-}
    rnf = const ()


instance Ord BitVector where
  
    {-# INLINE compare #-}
    compare lhs rhs = 
        case comparing dim lhs rhs of
          EQ -> comparing nat lhs rhs
          v  -> v


instance Semigroup BitVector where

    {-# INLINE (<>) #-}
    (<>) (BV x m) (BV y n) = BV (x + y) $ (n `shiftL` x) + m

    {-# INLINABLE sconcat #-}
    sconcat xs = uncurry BV $ foldl' f (0,0) xs
      where
        f (bitCount, natVal) (BV w n) = (bitCount + w, natVal + (n `shiftL` bitCount))

    {-# INLINE stimes #-}
    stimes 0  _       = mempty
    stimes e (BV w n) = BV limit $ go (limit - w) n
      where
        limit = fromEnum e * w
        go  0 !acc = acc
        go !k !acc = go (k-w) $ (n `shiftL` k) + acc


instance Show BitVector where

    show (BV w n) = mconcat [ "[", show w, "]", show n ]


-- |
-- \( \mathcal{O} \left( 1 \right) \)
--
-- Create a bit-vector of non-negative dimension from an integral value.
-- The integral value will be interpreted as /little-endian/ so that the least
-- significant bit of the integral value will be the value of the 0th index of 
-- the resulting bit-vector, and the most significant bit of the inegral value
-- will be the largest index @i@ such that @bv `testBit` i@ is @True@.
--
-- Note that if the integral value exceeds the dimension, the set bits in the
-- integral value that exceed the provided bit-vector dimension will be ignored.
--
-- >>> bitvector 7 96
-- [7]96
{-# INLINE bitvector #-}
bitvector 
  :: Integral v 
  => Word  -- ^ Bit vector dimension
  -> v     -- ^ Bit vector integral value, /little-endian/
  -> BitVector
bitvector !dimValue !intValue = BV width $ mask .&. toInteger intValue
  where
    !width = fromEnum dimValue
    !mask  = 2 ^ dimValue - 1


-- |
-- \( \mathcal{O} \left( 1 \right) \)
--
-- Get the dimension of a 'BitVector'. Preferable over 'finiteBitSize' as it
-- returns a type which cannot represent a non-negative value.
--
-- >>> dimension [2]3
-- 2
--
-- >>> dimension [4]12
-- 4
{-# INLINE dimension #-}
dimension :: BitVector -> Word
dimension = toEnum . dim


-- |
-- \( \mathcal{O} \left( 1 \right) \)
--
-- Get the /inclusive/ range of bits in 'BitVector' as a new 'BitVector'.
{-# INLINE subRange #-}
subRange :: (Word, Word) -> BitVector -> BitVector
subRange (!lower, !upper) (BV _ n)
  | lower > upper = zeroBits
  | otherwise     = BV m $ (n `shiftR` i) `mod` 2^m
  where
    i = fromEnum lower
    m = fromEnum $ upper - lower + 1


-- |
-- \( \mathcal{O} \left( 1 \right) \)
--
-- 2's complement value of a bit-vector.
--
-- >>> toSignedNumber [4]0
-- 0
--
-- >>> toSignedNumber [4]3
-- 3
--
-- >>> toSignedNumber [4]7
-- 7
--
-- >>> toSignedNumber [4]8
-- -8
--
-- >>> toSignedNumber [4]12
-- -4
--
-- >>> toSignedNumber [4]15
-- -1
{-# INLINE toSignedNumber #-}
toSignedNumber :: Num a => BitVector -> a
toSignedNumber (BV w n) = fromInteger v
  where
    v | n `testBit` (w-1) = negate $ 2^w - n
      | otherwise         = n


-- | 
-- \( \mathcal{O} \left( 1 \right) \)
--
-- Unsigned value of a bit-vector.
--
-- >>> toSignedNumber [4]0
-- 0
--
-- >>> toSignedNumber [4]3
-- 3
--
-- >>> toSignedNumber [4]7
-- 7
--
-- >>> toSignedNumber [4]8
-- 8
--
-- >>> toSignedNumber [4]12
-- 12
--
-- >>> toSignedNumber [4]15
-- 15
{-# INLINE toUnsignedNumber #-}
toUnsignedNumber :: Num a => BitVector -> a
toUnsignedNumber = fromInteger . nat


-- | 
-- \( \mathcal{O} \left( n \right) \)
--
-- Create a bit-vector from a /little-endian/ list of bits.
--
-- The following will hold:
--
-- > length . takeWhile not === countLeadingZeros . fromBits
--
-- > length . takeWhile not . reverse === countTrailingZeros . fromBits
--
-- >>> fromBits [True, False, False]
-- [3]1
{-# INLINE fromBits #-}
fromBits :: Foldable f => f Bool -> BitVector
fromBits bs = BV n k
  -- NB: 'setBit' is a GMP function, faster than regular addition.
  where
    (!n, !k) = foldl' go (0, 0) bs 
    go (!i, !v) b
      | b         = (i+1, setBit v i)
      | otherwise = (i+1, v)


-- | 
-- \( \mathcal{O} \left( n \right) \)
--
-- Create a /little-endian/ list of bits from a bit-vector.
--
-- The following will hold:
--
-- > length . takeWhile not . toBits === countLeadingZeros
--
-- > length . takeWhile not . reverse . toBits === countTrailingZeros
--
-- >>> toBits [4]11
-- [True, True, False, True]
{-# INLINE toBits #-}
toBits :: BitVector -> [Bool]
toBits (BV w n) = testBit n <$> [ 0 .. w - 1 ]

