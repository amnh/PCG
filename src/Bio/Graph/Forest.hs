-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Graph.Forest
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- The Phylogentic Graph types.
--
--
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Bio.Graph.Forest
  ( PhylogeneticForest(..)
  ) where

import Bio.Graph.LeafSet
import Control.DeepSeq
import Control.Lens           hiding (Indexable)
import Data.Foldable
import Data.GraphViz.Printing
import Data.Key
import Data.List                     (intercalate)
import Data.List.NonEmpty            (NonEmpty(..))
import Data.Maybe
import Data.Semigroup
import Data.Semigroup.Foldable
-- import Data.Semigroup.Traversable
import Prelude                hiding (lookup, zip, zipWith)
import GHC.Generics
import Text.Newick.Class
import Text.XML.Custom
-- import Text.XML.Light.Types


-- |
-- A newtype wrapper for a 'NonEmpty' collection of forests.
newtype PhylogeneticForest a
      = PhylogeneticForest (NonEmpty a)
      deriving (Foldable, Foldable1, Functor, Generic, Semigroup, Traversable)


type instance Key PhylogeneticForest = Int


instance Adjustable PhylogeneticForest where

    {-# INLINE adjust  #-}
    adjust  f i = PhylogeneticForest . adjust  f i . unwrap

    {-# INLINE replace #-}
    replace i e = PhylogeneticForest . replace i e . unwrap


instance FoldableWithKey PhylogeneticForest where

    {-# INLINE toKeyedList #-}
    toKeyedList = toKeyedList . unwrap

    {-# INLINE foldMapWithKey #-}
    foldMapWithKey f   = foldMapWithKey f   . unwrap

    {-# INLINE foldrWithKey #-}
    foldrWithKey   f e = foldrWithKey   f e . unwrap

    {-# INLINE foldlWithKey #-}
    foldlWithKey   f e = foldlWithKey   f e . unwrap


instance FoldableWithKey1 PhylogeneticForest where

    {-# INLINE foldMapWithKey1 #-}
    foldMapWithKey1 f = foldMapWithKey1 f . unwrap


instance (HasLeafSet a b, Semigroup b) => HasLeafSet (PhylogeneticForest a) b where

    leafSet = lens getter undefined
      where
        getter = foldMap1 (^. leafSet) . unwrap


instance Indexable PhylogeneticForest where

    {-# INLINE index #-}
    index forest i = fromMaybe errorMessage $ i `lookup` unwrap forest
      where
        errorMessage =  error $ mconcat
            [ "Could not index PhylogeneticForest at location '"
            , show i
            , "' with a valid range of [0,"
            , show $ length forest
            , "]."
            ]


instance Keyed PhylogeneticForest where

    {-# INLINE mapWithKey #-}
    mapWithKey f = PhylogeneticForest . mapWithKey f . unwrap


instance Lookup PhylogeneticForest where

    {-# INLINE lookup #-}
    lookup i = lookup i . unwrap


instance NFData a => NFData (PhylogeneticForest a)


instance PrintDot a => PrintDot (PhylogeneticForest a) where

    unqtDot       = unqtListToDot . toList . unwrap

    toDot         = listToDot . toList . unwrap

    unqtListToDot = fmap mconcat . traverse unqtDot

    listToDot     = fmap mconcat . traverse toDot


instance ToNewick a => ToNewick (PhylogeneticForest a) where

    toNewick forest = intercalate "\n" (toList $ fmap toNewick (unwrap forest))


instance ToXML a => ToXML (PhylogeneticForest a) where

    toXML = collapseElemList "Forest" [] . unwrap


instance Traversable1 PhylogeneticForest where

    {-# INLINE traverse1 #-}
    traverse1 f = fmap PhylogeneticForest . traverse1 f . unwrap


instance TraversableWithKey PhylogeneticForest where

    {-# INLINE traverseWithKey #-}
    traverseWithKey  f = fmap PhylogeneticForest . traverseWithKey f . unwrap

    {-# INLINE mapWithKeyM #-}
    mapWithKeyM      f = fmap PhylogeneticForest . mapWithKeyM     f . unwrap


instance TraversableWithKey1 PhylogeneticForest where

    {-# INLINE traverseWithKey1 #-}
    traverseWithKey1 f = fmap PhylogeneticForest . traverseWithKey1 f . unwrap


instance Zip PhylogeneticForest where

    {-# INLINE zipWith #-}
    zipWith f lhs rhs = PhylogeneticForest $ zipWith f (unwrap lhs) (unwrap rhs)

    {-# INLINE zip #-}
    zip lhs rhs = PhylogeneticForest $ zip (unwrap lhs) (unwrap rhs)

    {-# INLINE zap #-}
    zap lhs rhs = PhylogeneticForest $ zap (unwrap lhs) (unwrap rhs)


instance ZipWithKey PhylogeneticForest where

    {-# INLINE zipWithKey #-}
    zipWithKey f lhs rhs = PhylogeneticForest $ zipWithKey f (unwrap lhs) (unwrap rhs)

    {-# INLINE zapWithKey #-}
    zapWithKey   lhs rhs = PhylogeneticForest $ zapWithKey (unwrap lhs) (unwrap rhs)


{-# INLINE unwrap #-}
unwrap :: PhylogeneticForest a -> NonEmpty a
unwrap (PhylogeneticForest x) = x
