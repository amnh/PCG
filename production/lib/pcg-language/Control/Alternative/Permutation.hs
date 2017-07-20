-- |
-- Module      :  Text.Megaparsec.Perm2
-- Copyright   :  © 2015–2017 Megaparsec contributors
--                © 2007 Paolo Martini
--                © 1999–2001 Daan Leijen
-- License     :  FreeBSD
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module implements permutation parsers. The algorithm is described
-- in: /Parsing Permutation Phrases/, by Arthur Baars, Andres Loh and
-- Doaitse Swierstra. Published as a functional pearl at the Haskell
-- Workshop 2001.

{-# LANGUAGE CPP                       #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE ExistentialQuantification #-}

module Text.Megaparsec.Perm2
  ( Perm()
  , runPermParser
  , toPerm
  , toPermWithDefault
  ) where


import Control.Applicative (Alternative(..), optional)


-- |
-- \"Unlifts\" a permutation parser into a parser to be evaluated.
runPermParser :: (Alternative m, Monad m) => Perm m a -> m a
runPermParser (P value parser) = optional parser >>= f
   where
      f  Nothing = maybe empty pure value
      f (Just p) = runPermParser p


-- |
-- \"Lifts\" a parser to a permutation parser.
toPerm :: Alternative m => m a -> Perm m a 
toPerm p = P Nothing $ pure <$> p


toPermWithDefault :: Alternative m => a -> m a -> Perm m a 
toPermWithDefault v p = P (Just v) $ pure <$> p


data Perm m a = P (Maybe a) (m (Perm m a))


instance Functor m => Functor (Perm m) where

    fmap f (P v p) = P (f <$> v) (fmap f <$> p)


instance Alternative m => Applicative (Perm m) where

    pure value = P (Just value) empty

    lhs@(P f v) <*> rhs@(P g w) = P (f <*> g) (lhsAlt <|> rhsAlt)
      where
        lhsAlt = (<*> rhs) <$> v
        rhsAlt = (lhs <*>) <$> w
