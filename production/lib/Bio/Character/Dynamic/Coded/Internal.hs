-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Character.Dynamic.Coded.Internal
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
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, TypeFamilies #-}
-- TODO: fix and remove this ghc option (is it needed for Arbitrary?):
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Bio.Character.Dynamic.Coded.Internal
  ( DynamicChar()
  , DynamicChars
  , arbitraryDynamicsGA
  ) where

import Bio.Character.Dynamic.Coded.Class
import Bio.Character.Parsed
import Data.Alphabet
import Data.BitMatrix
import Data.Key
import Data.Bits
import Data.BitVector               hiding (join, replicate)
import Data.Foldable
import Data.Function.Memoize
import Data.Maybe                          (fromJust, fromMaybe)
import Data.Monoid                         ((<>))
import Data.MonoTraversable
import Data.Vector                         (Vector, fromList)
import Test.Tasty.QuickCheck        hiding ((.&.))
import Test.QuickCheck.Arbitrary.Instances ()

-- TODO: Change DynamicChar/Sequences to DynamicCharacters
        -- Make a missing a null vector
        -- Think about a nonempty type class or a refinement type for this

newtype DynamicChar
      = DC BitMatrix
      deriving (Eq, Show)

type instance Element DynamicChar = BitVector

type DynamicChars = Vector DynamicChar

instance MonoFunctor DynamicChar where
  omap f (DC bm) = DC $ omap f bm

instance MonoFoldable DynamicChar where
  -- | Map each element of a monomorphic container to a 'Monoid'
  -- and combine the results.
  ofoldMap f (DC bm) = ofoldMap f bm
  {-# INLINE ofoldMap #-}

  -- | Right-associative fold of a monomorphic container.
  ofoldr f e (DC bm) = ofoldr f e bm
  {-# INLINE ofoldr #-}

  -- | Strict left-associative fold of a monomorphic container.
  ofoldl' f e (DC bm) = ofoldl' f e bm
  {-# INLINE ofoldl' #-}

  -- | Right-associative fold of a monomorphic container with no base element.
  --
  -- Note: this is a partial function. On an empty 'MonoFoldable', it will
  -- throw an exception.
  --
  -- /See 'Data.MinLen.ofoldr1Ex' from "Data.MinLen" for a total version of this function./
  ofoldr1Ex f (DC bm) = ofoldr1Ex f bm
  {-# INLINE ofoldr1Ex #-}

  -- | Strict left-associative fold of a monomorphic container with no base
  -- element.
  --
  -- Note: this is a partial function. On an empty 'MonoFoldable', it will
  -- throw an exception.
  --
  -- /See 'Data.MinLen.ofoldl1Ex'' from "Data.MinLen" for a total version of this function./
  ofoldl1Ex' f (DC bm) = ofoldl1Ex' f bm
  {-# INLINE ofoldl1Ex' #-}

-- | Monomorphic containers that can be traversed from left to right.
instance MonoTraversable DynamicChar where
    -- | Map each element of a monomorphic container to an action,
    -- evaluate these actions from left to right, and
    -- collect the results.
    otraverse f (DC bm) = DC <$> otraverse f bm
    {-# INLINE otraverse #-}

    -- | Map each element of a monomorphic container to a monadic action,
    -- evaluate these actions from left to right, and
    -- collect the results.
    omapM = otraverse
    {-# INLINE omapM #-}

instance EncodableStaticCharacter BitVector where

  decodeChar alphabet character = foldMapWithKey f alphabet
    where
      f i symbol
        | character `testBit` i = [symbol]
        | otherwise             = []
                                  
  encodeChar alphabet ambiguity = fromBits $ (`elem` ambiguity) <$> toList alphabet

instance EncodableDynamicCharacter DynamicChar where

  decodeDynamic alphabet (DC bm) = ofoldMap (pure . decodeChar alphabet) $ rows bm

  encodeDynamic alphabet = DC . fromRows . fmap (encodeChar alphabet) . toList

  indexChar i = fromJust . lookupChar i

  lookupChar (DC bm) i
    |  0 <= i
    && i <  numRows bm = Just $ bm `row` i
    | otherwise        = Nothing

  -- TODO: Think about the efficiency of this
  unsafeCons static (DC dynamic) = DC . fromRows $ [static] <> (rows dynamic)

  unsafeAppend (DC dynamic1) bv = DC . fromRows $ rows dynamic1 <> [bv]

instance OldEncodableDynamicCharacterToBeRemoved DynamicChar where
    
--    emptyChar          :: s
    emptyChar = DC $ bitMatrix 0 0 (const False)
    
--    filterGaps         :: s -> s
    filterGaps c@(DC bm) = DC . fromRows . filter (== gapBV) $ rows bm
      where
        gapBV = gapChar c
    
--    gapChar            :: s -> s
    gapChar (DC bm) = zeroBits `setBit` (numCols bm - 1)
    
--    getAlphLen         :: s -> Int
    getAlphLen (DC bm) = numCols bm

--   grabSubChar        :: s -> Int -> s
    grabSubChar char i = {-trace ("grabSubChar " ++ show char ++ " " ++ show i) $ -} char `indexChar` i
    
--    isEmpty            :: s -> Bool
    isEmpty (DC bm) = isZeroMatrix bm

--    numChars           :: s -> Int
    numChars (DC bm) = numRows bm

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


instance Memoizable DynamicChar where
    memoize f (DC bm) = memoize (f . DC) bm

instance Arbitrary DynamicChar where
    arbitrary = do 
      arbAlph <- arbitrary :: Gen (Alphabet String)
      arbitraryDynamicGivenAlph arbAlph

-- | Function to generate an arbitrary DynamicChar given an alphabet
arbitraryDynamicGivenAlph :: Alphabet String -> Gen DynamicChar
arbitraryDynamicGivenAlph inAlph = do
  arbParsed <- arbitrary :: Gen ParsedChar
  pure $ encodeDynamic inAlph arbParsed

-- | Generate many dynamic characters using the above
arbitraryDynamicsGA :: Alphabet String -> Gen DynamicChars
arbitraryDynamicsGA inAlph = fromList <$> listOf (arbitraryDynamicGivenAlph inAlph)
