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

{-# LANGUAGE DeriveFunctor, FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses #-}


module Bio.Graph.LeafSet
  ( LeafSet(..)
  , HasLeafSet (..)
  ) where


import Control.Lens
import Text.XML.Custom


newtype LeafSet n = LeafSet [n] deriving (Functor, Show)


-- |
-- A 'Lens' for 'LeafSet'
class HasLeafSet s a | s -> a where

    {-# MINIMAL leafSet #-}
    leafSet :: Lens' s a


instance ToXML n => ToXML (LeafSet n) where

    toXML (LeafSet lst) = xmlElement "Leaf set" attrs contents
        where
            attrs    = []
            contents = (Right . toXML) <$> lst
