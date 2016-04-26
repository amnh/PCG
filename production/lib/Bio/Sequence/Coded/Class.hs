-----------------------------------------------------------------------------
--
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

{- This is Sequence/Coded/Class module, which should be renamed. -}

{-# LANGUAGE FlexibleContexts, FunctionalDependencies, MultiParamTypeClasses #-}

module Bio.Sequence.Coded.Class where

--import Bio.Sequence.Character.Coded
import Bio.Sequence.Parsed

import Data.BitVector
import Data.Maybe           (fromJust)
import Data.Monoid          ((<>))
import Data.MonoTraversable
import Data.Vector          (Vector)

{- LAWS:
 - decodeChar alphabet . encodeChar alphabet . toList == id
 - encodeChar alphabet [alphabet !! i] == bit i
 - encodeChar alphabet alphabet == compliment zeroBits
 - decodeChar alphabet (encodeChar alphabet xs .|. encodeChar alphabet ys) == toList alphabet `Data.List.intersect` (toList xs `Data.List.union` toList ys)
 - decodeChar alphabet (encodeChar alphabet xs .&. encodeChar alphabet ys) == toList alphabet `Data.List.intersect` (toList xs `Data.List.intersect` toList ys)
 - finiteBitSize . encodeChar alphabet == const (length alphabet)
 -}

type Alphabet' a = Vector a

class Bits b => StaticCoded b where
--  gapChar    ::  Eq a              => Alphabet a -> b
  decodeChar ::  Eq a              => Alphabet' a -> b   -> [a]
  encodeChar :: (Eq a, Foldable t) => Alphabet' a -> t a -> b

{- LAWS:
 - decodeMany alphabet . encodeMany alphabet . fmap toList . toList = id
 - TODO: Add more laws here
 -}
class ( Bits s
      , StaticCoded (Element s)
      , Monoid s
      , MonoTraversable s
      ) => DynamicCoded s where
  -- All default instances can be "overidden" for efficientcy.
  decodeDynamic ::  Eq a => Alphabet' a -> s -> [[a]]
  decodeDynamic alphabet = ofoldr (\e acc -> decodeChar alphabet e : acc) []

--  encodeDynamic :: (Eq a, Foldable t, Foldable c) => Alphabet' a -> c (t a) -> s
--  encodeDynamic alphabet = ofoldl' (\acc e -> acc <> encodeChar alphabet e) mempty
--    where
--      f :: Foldable t => t a -> Element s
--      f acc e = acc <> encodeChar alphabet e

  indexChar  :: s -> Int -> (Element s)
  indexChar i = fromJust . lookupChar i

  lookupChar :: s -> Int -> Maybe (Element s)
  lookupChar xs i = fst $ ofoldl' f (Nothing, 0) xs
    where
      f (Nothing, n) e = if n == i then (Just e, n) else (Nothing, n + 1)
      f acc          _ = acc

-- OLD structure
-- | A coded sequence allows grabbing of a character, filtering, and some standard types
class Monoid s => CodedSequence s where
    decodeOverAlphabet   :: s -> Alphabet -> ParsedSeq
    decodeOneChar        :: s -> Alphabet -> ParsedSeq 
    -- TODO: This should be translated to:
    -- encode :: (Foldable f, Functor f, Foldable t, Foldable c, Ord a) => f (t a) -> c a -> s
    encodeOverAlphabet   :: ParsedSeq -> Alphabet -> s
    encodeOneChar        :: Alphabet -> AmbiguityGroup -> s
    emptySeq             :: s
    filterGaps           :: s -> s -> Alphabet -> s
--    gapChar              :: Int -> s
    grabSubChar          :: s -> Int -> Int -> s
    isEmpty              :: s -> Bool
    numChars             :: s -> Int -> Int

