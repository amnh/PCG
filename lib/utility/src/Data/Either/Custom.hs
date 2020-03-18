-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Either.Custom
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Functions for collecting 'Left' values of 'Either' forming a 'Validation'
-- similar context.
--
-----------------------------------------------------------------------------

module Data.Either.Custom where


-- |
-- A short hand type used for converting between 'Either a a' and '(Tag, a)'.
--
-- See 'toTaggedRep' and 'fromTaggedRep'.
data Tag = L | R


-- |
-- Observes one half of the isomorphism between a + a and 2 * a
toTaggedRep :: Either a a -> (Tag, a)
toTaggedRep optA = case optA of
  Left  a1 -> (L, a1)
  Right a2 -> (R, a2)


-- |
-- Observes the other half of the isomorphism between 2 * a and a + a
fromTaggedRep :: Tag -> a -> Either a a
fromTaggedRep tag a = case tag of
  L -> Left  a
  R -> Right a
