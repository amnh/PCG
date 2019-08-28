-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Pair.Strict
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Functions for working with strict pair
--
-----------------------------------------------------------------------------

{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveFunctor          #-}


module Data.Pair.Strict
  ( Pair(..)
  , type (:!:)
  , pattern Pair
  , HasLeft(..)
  , HasRight(..)
  ) where

import Control.Lens
import Data.Bifunctor

data Pair a b = !a :!: !b
  deriving stock (Eq, Ord, Show, Read, Bounded)
  deriving stock Functor

type (:!:) = Pair

pattern Pair :: a -> b -> Pair a b
pattern Pair a b <- (a :!: b)
  where
    Pair a b = a :!: b
{-# COMPLETE Pair #-}


instance Bifunctor Pair where
  bimap f g (a :!: b) = f a :!: g b


class HasLeft s a | s -> a where
  _left :: Lens' s a

class HasRight s a | s -> a where
  _right :: Lens' s a


instance HasLeft (Pair a b) a where
  _left = lens (\(Pair a1 _) -> a1) (\(Pair _ a2) a1' -> Pair a1' a2)

instance HasRight (Pair a b) b where
  _right = lens (\(Pair _ a2) -> a2) (\(Pair a1 _) a2' -> Pair a1 a2')
