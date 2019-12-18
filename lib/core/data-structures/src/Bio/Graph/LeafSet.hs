------------------------------------------------------------------------------
-- |
-- Module      :  Bio.Graph.LeafSet
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Provides a class for HasLeafSet, as well as an instance of HasLeafSet
-- for NonEmptyList
--
-----------------------------------------------------------------------------

{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}


module Bio.Graph.LeafSet
  ( LeafSet(..)
  , HasLeafSet (..)
  , fromLeafSet
  ) where


import Control.Lens
import Data.Coerce
import Data.Vector
import Text.XML.Custom ()

-- |
-- Set of unique leaf labels.
newtype LeafSet n = LeafSet (Vector n)
    deriving stock (Foldable, Functor, Show)


-- |
-- A 'Lens' for 'LeafSet'
class HasLeafSet s a | s -> a where

    {-# MINIMAL leafSet #-}
    leafSet :: Getter s a


-- | (✔)
instance Eq n => Semigroup (LeafSet n) where

    (<>) (LeafSet lhs) (LeafSet rhs) = LeafSet $ (<>) lhs rhs


fromLeafSet :: forall n .  LeafSet n -> Vector n
fromLeafSet = coerce
