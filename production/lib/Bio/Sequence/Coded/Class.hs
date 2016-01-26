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
-- TODO: Remove concrete dependancy of conctrete types Vector and String
--
-----------------------------------------------------------------------------

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Bio.Sequence.Coded.Class where

import Bio.Sequence.Character.Coded
import Bio.Sequence.Parsed

-- | A coded sequence allows grabbing of a character, filtering, and some standard types
class (Monoid s, CodedChar b) => CodedSequence s b | s -> b where
    numChars :: s -> Int
    emptySeq :: s
    isEmpty :: s -> Bool
    grabSubChar :: s -> Int -> Maybe b
    filterSeq :: s -> (b -> Bool) -> s
    charToSeq :: b -> s
-- This should be translated to:
-- encode :: (Foldable f, Functor f, Foldable t, Ord a) => f (t a) -> s
    encode :: ParsedSeq -> s

-- This should be translated to:
-- encode :: (Foldable f, Functor f, Foldable t, Foldable c, Ord a) => f (t a) -> c a-> s
    encodeOverAlphabet :: ParsedSeq -> [String] -> s




