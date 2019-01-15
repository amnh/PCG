-----------------------------------------------------------------------------
-- |
-- Module      :  ShortText.Custom
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Utility Functions for ShortText not provided by the library.
--
-----------------------------------------------------------------------------
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module ShortText.Custom where

import Data.Char       (intToDigit, isSpace)
import Data.Text.Short as TS
import Prelude         hiding (break, dropWhile, reverse, words)


-- |
-- Convert an 'Int' to 'ShortText'.
intToShortText :: Int -> ShortText
intToShortText n0 =
  if n0 < 0
    then singleton '-' <>  go (quotRem (abs n0) 10) mempty
    else go (quotRem n0 10) mempty
  where
    go :: (Int, Int) -> ShortText -> ShortText
    go (n, d) rest =
        if n == 0
          then  acc
          else  go (quotRem n 10) acc
      where
        char :: Char
        !char = intToDigit d

        acc :: ShortText
        acc = singleton char <> rest


-- |
-- Safely get the head and tail of a 'ShortText'
headAndTail :: ShortText -> (ShortText, ShortText)
headAndTail short = case uncons short of
  Just (c, rest) -> (singleton c, rest)
  Nothing        -> (mempty, mempty)


safeLast :: ShortText -> ShortText
safeLast short = case uncons short of
  Just (c, rest) -> if TS.null rest then TS.singleton c else safeLast rest
  Nothing        -> mempty


words :: ShortText -> [ShortText]
words s =  case dropWhile isSpace s of
                "" -> []
                s' -> w : words s''
                        where (w, s'') = break isSpace s'

strip :: ShortText -> ShortText
strip = lstrip . rstrip

-- | Same as 'strip', but applies only to the left side of the string.
lstrip :: ShortText -> ShortText
lstrip s = case uncons s of
  Just (c, rest) -> if c `elem` wschars
                    then lstrip rest
                    else s
  Nothing        -> mempty

-- | Same as 'strip', but applies only to the right side of the string.
rstrip :: ShortText -> ShortText
rstrip = TS.reverse . lstrip . TS.reverse
      -- this implementation is horrible

wschars :: String
wschars = [' ', '\t','\r', 'n']
