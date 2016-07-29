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

import Data.Alphabet
import Data.BitVector
import Data.Maybe           (fromMaybe)
import Data.MonoTraversable
import Data.String          (IsString)

import GHC.Stack            (errorWithStackTrace)

{- | Represents a character of fixed width encoding one or more character states.
 -
 - Laws:
 -
 - @decodeChar alphabet . encodeChar alphabet . toList == id@
 -
 - @encodeChar alphabet [alphabet ! i] == bit i@
 -
 - @encodeChar alphabet alphabet == complement (bit (length alphabet - 1) `clearBit` (bit (length alphabet - 1))@
 -
 - @decodeChar alphabet (encodeChar alphabet xs .|. encodeChar alphabet ys) == toList alphabet `Data.List.intersect` (toList xs `Data.List.union` toList ys)@
 - 
 - @decodeChar alphabet (encodeChar alphabet xs .&. encodeChar alphabet ys) == toList alphabet `Data.List.intersect` (toList xs `Data.List.intersect` toList ys)@
 -
 -}
-- TODO: Add more laws here.
class (Bits b, Num b) => EncodableStaticCharacter b where
  decodeChar ::  Eq a              => Alphabet a -> b   -> [a]
  encodeChar :: (Eq a, Foldable t) => Alphabet a -> t a -> b
  stateCount ::  b -> Int
  getGapChar ::  b -> b
  getGapChar = bit . pred . stateCount

{- | Represents a character of variable length representing multiple encoded static characters.
 -
 - decodeMany alphabet . encodeMany alphabet == fmap toList . toList
 - 
 -}
-- TODO: Add more laws here
class ( EncodableStaticCharacter (Element s)
      , MonoTraversable s
      ) => EncodableDynamicCharacter s where
  -- All default instances can be "overidden" for efficientcy.
  constructDynamic :: Foldable t => t (Element s) -> s
  
  decodeDynamic :: (Ord a, IsString a) => Alphabet a -> s -> [[a]]
  decodeDynamic alphabet = ofoldr (\e acc -> decodeChar alphabet e : acc) []

  encodeDynamic :: (Ord a, Foldable t, Foldable c, IsString a) => Alphabet a -> c (t a) -> s

  indexChar  :: s -> Int -> Element s
  indexChar xs i = fromMaybe raiseError $ xs `lookupChar` i
    where
      raiseError = errorWithStackTrace
                 $ mconcat ["Index ", show i, " is out of range [0,", show $ olength xs - 1,"]."] 

  lookupChar :: s -> Int -> Maybe (Element s)
  lookupChar xs i = fst $ ofoldl' f (Nothing, 0) xs
    where
      f (Nothing, n) e = if n == i then (Just e, n) else (Nothing, n + 1)
      f acc          _ = acc

