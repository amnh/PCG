-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Megaparsec.Custom
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Class for needed operations of coded sequences and characters
--
-----------------------------------------------------------------------------

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Bio.Sequence.Coded.Class where

-- | A coded sequence allows grabbing of a character, filtering, and some standard types
class Monoid s => CodedSequence s b | s -> b where
    numChars :: s -> Int
    emptySeq :: s
    isEmpty :: s -> Bool
    grabSubChar :: s -> Int -> Maybe b
    filterSeq :: s -> (b -> Bool) -> s
    charToSeq :: b -> s

-- | A coded character is a character of a sequence (allows for standard chars like gap)
class CodedChar b where
    gapChar :: b
