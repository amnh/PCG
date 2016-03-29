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
-- TODO: Remove concrete dependancy of conctrete types Vector and String
--
-----------------------------------------------------------------------------

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Bio.Sequence.Coded.Class where

--import Bio.Sequence.Character.Coded
import Bio.Sequence.Parsed

-- TODO: require instance of foldable to remove some of these

-- | A coded sequence allows grabbing of a character, filtering, and some standard types
class Monoid s => CodedSequence s b where
    charToSeq   :: b -> s  -- What is the purpose of this?
    numChars    :: s -> Int
    -- TODO: This should be translated to:
    -- encode :: (Foldable f, Functor f, Foldable t, Ord a) => f (t a) -> s
    encode      :: ParsedSeq -> s
    -- TODO: This should be translated to:
    -- encode :: (Foldable f, Functor f, Foldable t, Foldable c, Ord a) => f (t a) -> c a-> s
    encodeOverAlphabet :: ParsedSeq -> [String] -> s
    emptySeq    :: s
    filterSeq   :: s -> (b -> Bool) -> s
    grabSubChar :: s -> Int -> Maybe b
    isEmpty     :: s -> Bool