-- |
-- Module      :  Control.Alternative.Permutation
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable  
--
-- This module is a generalization of the package @parsec-permutation@
-- authored by Samuel Hoffstaetter:
--
-- http://hackage.haskell.org/package/parsec-permutation
--
-- This module also takes inspiration from the algorithm is described
-- in: /Parsing Permutation Phrases/, by Arthur Baars, Andres Loh and
-- Doaitse Swierstra. Published as a functional pearl at the Haskell
-- Workshop 2001.
--
-- From these two works we derive a flexible and general method for
-- parsing permutations over an 'Applicative' strucuture. Quite useful
-- in conjunction with \"Free\" constructions of Applicatives, Monads,
-- etc.
--
-- Other permutation parsing libraries tend towards using special \"almost
-- applicative\" combinators for constuction which denies the library user
-- the ability to lift and unlift permutation parsing into any Applicative
-- computational context.

module Control.Alternative.Permutation
  ( Perm()
  , runPermParser
  , runPermParserWithSeperator
  , toPerm
  , toPermWithDefault
  ) where


import Control.Applicative (Alternative(..), optional)


-- |
-- An Applicative wrapper-type for constructing permutation parsers.
data Perm m a = P (Maybe a) (m (Perm m a))


instance Functor m => Functor (Perm m) where

    fmap f (P v p) = P (f <$> v) (fmap f <$> p)


instance Alternative m => Applicative (Perm m) where

    pure value = P (Just value) empty

    lhs@(P f v) <*> rhs@(P g w) = P (f <*> g) (lhsAlt <|> rhsAlt)
      where
        lhsAlt = (<*> rhs) <$> v
        rhsAlt = (lhs <*>) <$> w


-- |
-- \"Unlifts\" a permutation parser into a parser to be evaluated.
runPermParser :: (Alternative m, Monad m) => Perm m a -> m a
runPermParser (P value parser) = optional parser >>= f
   where
      f  Nothing = maybe empty pure value
      f (Just p) = runPermParser p


-- |
-- \"Unlifts\" a permutation parser into a parser to be evaluated.
runPermParserWithSeperator :: (Alternative m, Monad m) => m b -> Perm m a -> m a
runPermParserWithSeperator sep perm = run (pure ()) sep perm
   where
     run :: (Alternative m, Monad m) => m c -> m b -> Perm m a -> m a
     run headSep tailSep (P value parser) = optional headSep >>= g
       where
         f  Nothing = maybe empty pure value
         f (Just p) = run tailSep tailSep p
         g  Nothing = maybe empty pure value
         g (Just p) = optional parser >>= f



-- |
-- \"Lifts\" a parser to a permutation parser.
toPerm :: Alternative m => m a -> Perm m a 
toPerm p = P Nothing $ pure <$> p


-- |
-- \"Lifts\" a parser with a default value to a permutation parser.
--
-- If no permutation containg the supplied parser can be parsed from the input,
-- then the supplied defualt value is returned in lieu of a parse result.
toPermWithDefault :: Alternative m => a -> m a -> Perm m a 
toPermWithDefault v p = P (Just v) $ pure <$> p
