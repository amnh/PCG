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

{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DeriveAnyClass         #-}


module Data.Pair.Strict
  ( Pair(..)
  , type (:!:)
  , pattern Pair
  , HasLeft(..)
  , HasRight(..)
  , withinP
  , _both
  ) where

import Control.Lens hiding (both)
import Control.DeepSeq
import GHC.Generics

data Pair a b = !a :!: !b
  deriving stock    (Eq, Ord, Functor, Show, Read, Bounded, Generic)
  deriving anyclass (NFData)


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


both :: (a -> b) -> a :!: a -> b :!: b
both f = bimap f f


_both :: Lens' a b -> Lens' (a :!: a) (b :!: b)
_both l = lens (both (view l)) (\ (a1 :!: a2) (b1 :!: b2) ->
                                  set l b1 a1 :!: set l b2 a2)

withinP ::
  Lens' s (a :!: a) -> Lens' a b -> Lens' s (b :!: b)
withinP lsaa lab = lsaa . _both lab
