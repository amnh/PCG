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

{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Bio.Graph.Forest
  ( PhylogeneticForest(..)
  , HasPhylogeneticComponents(..)
  ) where

import Bio.Graph.LeafSet
import Control.DeepSeq
import Control.Lens            as Lens hiding (Indexable)
import Data.Binary
import Data.Foldable
import Data.GraphViz.Printing
import Data.Key
import Data.List.NonEmpty      (NonEmpty(..))
import Data.Maybe
import Data.Semigroup
import Data.Semigroup.Foldable
import Data.Text               (intercalate)
import GHC.Generics
import Prelude                 hiding (lookup, zip, zipWith)
import Text.Newick.Class
import Text.XML.Custom


-- |
-- A newtype wrapper for a 'NonEmpty' collection of forests.
newtype PhylogeneticForest a
      = PhylogeneticForest {getPhylogeneticForest :: NonEmpty a}
      deriving anyclass (Binary)
      deriving stock    (Foldable, Generic, Traversable)
      deriving newtype  (Foldable1, Functor, Semigroup)


type instance Key PhylogeneticForest = Int


-- |
-- A 'Lens' for the 'phylogeneticComponents' field.
{-# SPECIALISE _phylogeneticComponents :: Lens (PhylogeneticForest a) (PhylogeneticForest a') (NonEmpty a) (NonEmpty a') #-}
class HasPhylogeneticComponents s t a b | s -> a, t -> b,  s b -> t, t a -> s where

    _phylogeneticComponents :: Lens s t a b


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

    leafSet = Lens.to getter
      where
        getter = foldMap1 (^. leafSet) . unwrap


instance HasPhylogeneticComponents  (PhylogeneticForest a) (PhylogeneticForest a') (NonEmpty a) (NonEmpty a') where

    {-# INLINE _phylogeneticComponents #-}
    _phylogeneticComponents = lens getPhylogeneticForest (\f cs -> f {getPhylogeneticForest = cs})


instance Indexable PhylogeneticForest where

    {-# INLINE index #-}
    index forest i = fromMaybe errorMessage $ i `lookup` unwrap forest
      where
        errorMessage =  error $ fold
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

    unqtListToDot = fmap fold . traverse unqtDot

    listToDot     = fmap fold . traverse toDot


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
