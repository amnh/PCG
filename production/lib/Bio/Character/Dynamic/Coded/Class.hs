-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Character.Dynamic.Coded.Class
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

{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses #-}

module Bio.Character.Dynamic.Coded.Class where

import Bio.Character.Parsed

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
class Bits b => EncodableStaticCharacter b where
--  gapChar    ::  Eq a              => Alphabet a -> b
--  decodeChar' ::                       Alphabet -> b   -> [String]
--  encodeChar' :: (Eq a, Foldable t) => Alphabet -> t a -> b
  decodeChar  ::  Eq a              => Alphabet' a -> b   -> [a]
  encodeChar  :: (Eq a, Foldable t) => Alphabet' a -> t a -> b

{- LAWS:
 - decodeMany alphabet . encodeMany alphabet . fmap toList . toList = id
 - TODO: Add more laws here
 -}
class ( EncodableStaticCharacter (Element s)
      , MonoTraversable s
      , OldEncodableDynamicCharacterToBeRemoved s
      ) => EncodableDynamicCharacter s where
  -- All default instances can be "overidden" for efficientcy.
  decodeDynamic ::  Eq a => Alphabet' a -> s -> [[a]]
  decodeDynamic alphabet = ofoldr (\e acc -> decodeChar alphabet e : acc) []

  encodeDynamic :: (Eq a, Foldable t, Foldable c) => Alphabet' a -> c (t a) -> s

  indexChar  :: s -> Int -> Element s
  indexChar i = fromJust . lookupChar i

  lookupChar :: s -> Int -> Maybe (Element s)
  lookupChar xs i = fst $ ofoldl' f (Nothing, 0) xs
    where
      f (Nothing, n) e = if n == i then (Just e, n) else (Nothing, n + 1)
      f acc          _ = acc

  unsafeAppend  :: s -> BitVector -> s
  unsafeCons :: BitVector -> s -> s

-- | A coded sequence allows grabbing of a character, filtering, and some standard types
class OldEncodableDynamicCharacterToBeRemoved s where
    -- TODO: I switched the order of input args in decode fns and encodeOver...
  decodeOverAlphabet :: Alphabet -> s -> ParsedChar
  decodeOneChar      :: Alphabet -> s -> ParsedChar
  encodeOverAlphabet :: Alphabet -> ParsedChar -> s
  encodeOneChar      :: Alphabet -> AmbiguityGroup -> s
  emptyChar          :: s
  filterGaps         :: s -> s
  gapChar            :: s -> BitVector
  getAlphLen         :: s -> Int
  grabSubChar        :: s -> Int -> BitVector
  isEmpty            :: s -> Bool
  numChars           :: s -> Int
