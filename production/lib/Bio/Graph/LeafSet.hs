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

{-# LANGUAGE AllowAmbiguousTypes, DeriveFunctor, FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses, UndecidableInstances #-}


module Bio.Graph.LeafSet
  ( LeafSet(..)
  , HasLeafSet (..)
  ) where


import Control.Lens
import Data.Foldable
-- import Data.List.NonEmpty
import Data.Monoid
-- import Text.XML.Custom


{-# LANGUAGE UndecidableInstances #-}


newtype LeafSet n = LeafSet [n] deriving (Functor, Show)


-- |
-- A 'Lens' for 'LeafSet'
class HasLeafSet s a | s -> a where

    {-# MINIMAL leafSet #-}
    leafSet :: Lens' s a


-- instance ToXML (LeafSet (Maybe String)) where

--     toXML (LeafSet lst) = xmlElement "Leaf set" attrs contents
--         where
--             attrs    = []
--             contents = [Left ("Leaves", foldr leafStr "" lst)]

--             leafStr input accum = case input of Just item -> item <> ", " <> accum
--                                                 Nothing   -> accum


-- instance (Foldable f) => HasLeafSet (f a) (LeafSet a) where

--     leafSet lst = lens getter setter
--         where
--             result = head $ foldMap (^. leafSet) (toList lst)
--             getter e   =
--             setter e _ = id e