{-# LANGUAGE TypeFamilies, TypeSynonymInstances #-}
module Bio.Sequence.Coded.Internal where

import Data.Bifunctor
import Data.Bits
import Data.BitVector hiding (foldr)
import Data.Foldable
import Data.Monoid
import Data.MonoTraversable

import           Bio.Sequence.Coded.Class
import           Data.Alphabet
import           Data.BitMatrix
import           Data.BitVector
import           Data.Key
import           Data.List            (intercalate,nub)
import           Data.Maybe           (fromJust)
import           Data.Monoid
import           Data.MonoTraversable
import           Data.Vector          (Vector, (!?))
import qualified Data.Vector     as V (fromList, imap)

newtype DynamicChar
      = DC BitMatrix

type instance Element DynamicChar = BitVector

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

instance StaticCoded BitVector where

  decodeChar alphabet character = foldMapWithKey f alphabet
    where
      f index symbol
        | character `testBit` index = [symbol]
        | otherwise                 = []
                                  
  encodeChar alphabet ambiguity = fromBits $ (`elem` ambiguity) <$> toList alphabet

instance DynamicCoded DynamicChar where

  decodeDynamic alphabet (DC bm) = ofoldMap (pure . decodeChar alphabet) $ rows bm

  encodeDynamic alphabet = DC . fromRows . fmap (encodeChar alphabet) . toList

  indexChar i = fromJust . lookupChar i

  lookupChar (DC bm) i
    | numRows bm <= i = Just $ bm `row` i
    | otherwise       = Nothing

instance EncodableDynamicChar DynamicChar where
      -- TODO: I switched the order of input args in decode fns and encodeOver...
    decodeOverAlphabet :: Alphabet -> s -> ParsedDynChar
    decodeOverAlphabet alphabet = decodeDynamic (constructAlphabet alphabet)

    decodeOneChar      :: Alphabet -> s -> ParsedDynChar
    decodeOneChar = decodeOverAlphabet

    encodeOverAlphabet :: Alphabet -> ParsedDynChar -> s
    encodeOverAlphabet alphabet = encodeDynamic (constructAlphabet alphabet)
    
    encodeOneChar      :: Alphabet -> AmbiguityGroup -> s
    encodeOneChar alphabet = encodeOverAlphabet alphabet . pure
    
    emptyChar          :: s
    emptyChar = DC $ bitMatrix 0 0 (const 0)
    
    filterGaps         :: s -> s
    filterGaps c@(DC bm) = DC . fromRows . filter (== gapBV) $ rows bm
      where
        gapBV = head . (\(DC bm) -> fromRows bm) gapChar c
    
    gapChar            :: s -> s
    gapChar (DC bm) = DC $ fromRows [zeroBits `setBit` (numCols bm - 1)] 
    
    getAlphLen         :: s -> Int
    getAlphLen (DC bm) = numCols bm

    grabSubChar        :: s -> Int -> s
    grabSubChar = indexChar
    
    isEmpty            :: s -> Bool
    isEmpty = (0 ==) . numChars

    numChars           :: s -> Int
    numChars (DC bm) = numRows bm
