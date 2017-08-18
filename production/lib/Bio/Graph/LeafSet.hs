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

{-# LANGUAGE AllowAmbiguousTypes, DeriveFoldable, DeriveFunctor, FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses #-}


module Bio.Graph.LeafSet
  ( LeafSet(..)
  , HasLeafSet (..)
  ) where


import Control.Lens
--import Data.Foldable
-- import Data.List.NonEmpty
import Data.Semigroup
import Text.Newick.Class
import Text.XML.Custom


newtype LeafSet n = LeafSet [n] deriving (Foldable, Functor, Show)


-- |
-- A 'Lens' for 'LeafSet'
class HasLeafSet s a | s -> a where

    {-# MINIMAL leafSet #-}
    leafSet :: Lens' s a


{--
instance ToXML (LeafSet (Maybe String)) where

    toXML (LeafSet lst) = xmlElement "Leaf_set" attrs contents
        where
            attrs    = []
            contents = [Left ("Leaves", foldr leafStr "" lst)]

            leafStr input accum = case input of Just item -> item <> ", " <> accum
                                                Nothing   -> accum
--}
