-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Sequence.Coded.Class
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Class for needed operations of coded sequences and characters
-- 
--
-----------------------------------------------------------------------------


{-# LANGUAGE FlexibleContexts, FunctionalDependencies, MultiParamTypeClasses #-}

module Bio.Sequence.Coded.Class where

--import Bio.Sequence.Character.Coded
import Bio.Sequence.Parsed

import Data.Alphabet
import Data.BitVector
import Data.Maybe           (fromJust)
--import Data.Monoid
import Data.MonoTraversable

{- LAWS:
 - decodeChar alphabet . encodeChar alphabet . toList == id
 - encodeChar alphabet [alphabet !! i] == bit i
 - encodeChar alphabet alphabet == compliment zeroBits
 - decodeChar alphabet (encodeChar alphabet xs .|. encodeChar alphabet ys) == toList alphabet `Data.List.intersect` (toList xs `Data.List.union` toList ys)
 - decodeChar alphabet (encodeChar alphabet xs .&. encodeChar alphabet ys) == toList alphabet `Data.List.intersect` (toList xs `Data.List.intersect` toList ys)
 - finiteBitSize . encodeChar alphabet == const (length alphabet)
 -}
class Bits b => StaticCoded b where
--  gapChar    ::  Eq a              => Alphabet a -> b
  decodeChar ::  Eq a              => Alphabet' a -> b   -> [a]
  encodeChar :: (Eq a, Foldable t) => Alphabet' a -> t a -> b

{- LAWS:
 - decodeMany alphabet . encodeMany alphabet . fmap toList . toList = id
 - TODO: Add more laws here
 -}
class ( StaticCoded (Element s)
      , MonoTraversable s
      ) => DynamicCoded s where
  -- All default instances can be "overidden" for efficientcy.
  decodeDynamic ::  Eq a => Alphabet' a -> s -> [[a]]
  decodeDynamic alphabet = ofoldr (\e acc -> decodeChar alphabet e : acc) []

  encodeDynamic :: (Eq a, Foldable t, Foldable c) => Alphabet' a -> c (t a) -> s

  indexChar  :: s -> Int -> (Element s)
  indexChar i = fromJust . lookupChar i

  lookupChar :: s -> Int -> Maybe (Element s)
  lookupChar xs i = fst $ ofoldl' f (Nothing, 0) xs
    where
      f (Nothing, n) e = if n == i then (Just e, n) else (Nothing, n + 1)
      f acc          _ = acc

  unsafeAppend  :: s -> s -> s
  unsafeCons :: Element s -> s -> s

-- | A coded sequence allows grabbing of a character, filtering, and some standard types
class EncodableDynamicCharacter s where
    -- TODO: I switched the order of input args in decode fns and encodeOver...
  decodeOverAlphabet :: Alphabet -> s -> ParsedDynChar
  decodeOneChar      :: Alphabet -> s -> ParsedDynChar
  encodeOverAlphabet :: Alphabet -> ParsedDynChar -> s
  encodeOneChar      :: Alphabet -> AmbiguityGroup -> s
  emptyChar          :: s
  filterGaps         :: s -> s
  gapChar            :: s -> s
  getAlphLen         :: s -> Int
  grabSubChar        :: s -> Int -> s
  isEmpty            :: s -> Bool
  numChars           :: s -> Int
