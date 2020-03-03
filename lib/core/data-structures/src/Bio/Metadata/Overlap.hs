-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Metadata.Dynamic.Internal
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MagicHash              #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StrictData             #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UnboxedTuples          #-}

module Bio.Metadata.Overlap
  ( overlap
  , overlap'
  , overlap2
  , overlap3
  ) where

import           Bio.Character.Encodable
import           Data.Bits
import           Data.List.NonEmpty           (NonEmpty (..))
import           Data.Semigroup
import           Data.Semigroup.Foldable


-- |
-- Takes one or more elements of 'FiniteBits' and a symbol change cost function
-- and returns a tuple of a new character, along with the cost of obtaining that
-- character. The return character may be (or is even likely to be) ambiguous.
-- Will attempt to intersect the two characters, but will union them if that is
-- not possible, based on the symbol change cost function.
--
-- To clarify, the return character is an intersection of all possible least-cost
-- combinations, so for instance, if @ char1 == A,T @ and @ char2 == G,C @, and
-- the two (non-overlapping) least cost pairs are A,C and T,G, then the return
-- value is A,C,G,T.
{-# INLINE overlap #-}
{-# SPECIALISE overlap :: FiniteBits e => (Word -> Word -> Word) -> NonEmpty e -> (e, Word) #-}
{-# SPECIALISE overlap :: (Word -> Word -> Word) -> NonEmpty DynamicCharacterElement -> (DynamicCharacterElement, Word) #-}
overlap
  :: ( FiniteBits e
     , Foldable1 f
     , Functor f
     )
  => (Word -> Word -> Word) -- ^ Symbol change matrix (SCM) to determin cost
  -> f e                    -- ^ List of elements for of which to find the k-median and cost
  -> (e, Word)              -- ^ K-median and cost
overlap sigma xs = go size maxBound zero
  where
    (size, zero) = let wlog = getFirst $ foldMap1 First xs
                   in  (finiteBitSize wlog, wlog `xor` wlog)

    go 0 theCost bits = (bits, theCost)
    go i oldCost bits =
        let i' = i - 1
            newCost = sum $ getDistance (toEnum i') <$> xs
            (minCost, bits') = case oldCost `compare` newCost of
                                 EQ -> (oldCost, bits `setBit` i')
                                 LT -> (oldCost, bits            )
                                 GT -> (newCost, zero `setBit` i')
        in go i' minCost bits'

    getDistance i b = go' size (maxBound :: Word)
      where
        go' :: Int -> Word -> Word
        go' 0 a = a
        go' j a =
          let j' = j - 1
              a' = if b `testBit` j' then min a $ sigma i (toEnum j') else a
          in  go' j' a'


overlap'
  :: ( Foldable1 f
     , Functor f
--     , NFData (f DynamicCharacterElement)
     )
  => (Word -> Word -> Word)
  -> f DynamicCharacterElement
  -> (DynamicCharacterElement, Word)
overlap' sigma xs = go size maxBound zero
  where
    (size, !zero) = let !wlog = getFirst $ foldMap1 First xs
                   in  (finiteBitSize wlog, wlog `xor` wlog)
    size'  = size - 1

  --  go :: Int -> Word -> e -> (e, Word)
    go 0 theCost bits = (bits, theCost)
    go i oldCost bits =
        let !newCost = sum $ getDistance (toEnum i) <$>  xs
            (minCost, bits') = case oldCost `compare` newCost of
                                 EQ -> (oldCost, bits `setBit` i)
                                 LT -> (oldCost, bits           )
                                 GT -> (newCost, zero `setBit` i)
        in go (i-1) minCost bits'

    getDistance i !b = go' 0 0 (maxBound :: Word)
      where
        go' :: Int -> Word -> Word -> Word
        go' j _ a | j == size' = a
        go' j o a =
          let
            o' = o + 1
            (a', j') =
                if b `testBit` j then (min a $ sigma i (toEnum j), j + 1)
                else
                  case selectDC b o' of
                    Just !ind ->  (min a $ sigma i ind, (fromEnum ind) + 1)
                    _      ->  (a, size')
                  in  go' j' o' a'


{-# INLINE overlap2 #-}
{-# SPECIALISE overlap2 :: (Word -> Word -> Word) -> DynamicCharacterElement -> DynamicCharacterElement -> (DynamicCharacterElement, Word) #-}
overlap2
  :: FiniteBits e -- EncodableStreamElement e {- , Show e -})
  => (Word -> Word -> Word)
  -> e
  -> e
  -> (e, Word)
overlap2 sigma char1 char2 = overlap sigma $ char1 :| [char2]


{-# INLINE overlap3 #-}
{-# SPECIALISE overlap3 :: (Word -> Word -> Word) -> DynamicCharacterElement -> DynamicCharacterElement -> DynamicCharacterElement -> (DynamicCharacterElement, Word) #-}
overlap3
  :: FiniteBits e -- EncodableStreamElement e {- , Show e -})
  => (Word -> Word -> Word)
  -> e
  -> e
  -> e
  -> (e, Word)
overlap3 sigma char1 char2 char3 = overlap sigma $ char1 :| [char2, char3]
