------------------------------------------------------------------------------
-- |
-- Module      :  Bio.Metadata.Sequence.Block.Internal
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Bio.Sequence.Block.Internal
  ( Block(..)
  , HasBlockMetadata(..)
  , HasContinuousBin(..)
  , HasNonAdditiveBin(..)
  , HasAdditiveBin(..)
  , HasMetricBin(..)
  , HasNonMetricBin(..)
  , HasDynamicBin(..)
  ) where


import Control.Lens
import Control.DeepSeq
import Data.Bifunctor
import Data.Foldable
import Data.Vector           (Vector)
import Data.Vector.Instances ()
import GHC.Generics
import Text.XML


-- |
-- Represents a block of data which are optimized atomically together across
-- networks.
--
-- Use '(<>)' to construct larger blocks.
data  Block m u v w x y z
    = Block
    { _blockMetadata  :: m
    , _continuousBin  :: Vector u
    , _nonAdditiveBin :: Vector v
    , _additiveBin    :: Vector w
    , _metricBin      :: Vector x
    , _nonMetricBin   :: Vector y
    , _dynamicBin     :: Vector z
    } deriving (Eq, Generic)


-- |
-- A 'Lens' for the 'blockMetadata' field
class HasBlockMetadata s a | s -> a where

    blockMetadata :: Lens' s a
    {-# MINIMAL blockMetadata #-}


-- |
-- A 'Lens' for the 'continuousBin' field
class HasContinuousBin s a | s -> a where

    continuousBin :: Lens' s a
    {-# MINIMAL continuousBin #-}


-- |
-- A 'Lens' for the 'nonAdditiveBin' field
class HasNonAdditiveBin s a | s -> a where

    nonAdditiveBin :: Lens' s a
    {-# MINIMAL nonAdditiveBin #-}


-- |
-- A 'Lens' for the 'additiveBin' field
class HasAdditiveBin s a | s -> a where

    additiveBin :: Lens' s a
    {-# MINIMAL additiveBin #-}


-- |
-- A 'Lens' for the 'metricBin' field
class HasMetricBin s a | s -> a where

    metricBin :: Lens' s a
    {-# MINIMAL metricBin #-}


-- |
-- A 'Lens' for the 'nonMetricBin' field
class HasNonMetricBin s a | s -> a where

    nonMetricBin :: Lens' s a
    {-# MINIMAL nonMetricBin #-}


-- |
-- A 'Lens' for the 'dynamicBin' field
class HasDynamicBin s a | s -> a where

    dynamicBin :: Lens' s a
    {-# MINIMAL dynamicBin #-}


instance HasBlockMetadata (Block m u v w x y z) m where

    {-# INLINE blockMetadata #-}
    blockMetadata = lens _blockMetadata $
                    \e x -> e { _blockMetadata = x }


instance HasContinuousBin (Block m u v w x y z) (Vector u) where

    {-# INLINE continuousBin #-}
    continuousBin = lens _continuousBin $
                    \e x -> e { _continuousBin = x }


instance HasNonAdditiveBin (Block m u v w x y z) (Vector v) where

    {-# INLINE nonAdditiveBin #-}
    nonAdditiveBin = lens _nonAdditiveBin $
                     \e x -> e { _nonAdditiveBin = x }


instance HasAdditiveBin (Block m u v w x y z) (Vector w) where

    {-# INLINE additiveBin #-}
    additiveBin = lens _additiveBin $
                  \e x -> e { _additiveBin = x }


instance HasMetricBin (Block m u v w x y z) (Vector x) where

    {-# INLINE metricBin #-}
    metricBin = lens _metricBin $
                \e x -> e { _metricBin = x }


instance HasNonMetricBin (Block m u v w x y z) (Vector y) where

    {-# INLINE nonMetricBin #-}
    nonMetricBin = lens _nonMetricBin $
                   \e x -> e { _nonMetricBin = x }


instance HasDynamicBin (Block m u v w x y z) (Vector z) where

    {-# INLINE  dynamicBin #-}
    dynamicBin = lens _dynamicBin $
                 \e x -> e { _dynamicBin = x }


instance Bifunctor (Block m u v w x) where

    bimap f g =
        Block
          <$> _blockMetadata
          <*> _continuousBin
          <*> _nonAdditiveBin
          <*> _additiveBin
          <*> _metricBin
          <*> fmap f . _nonMetricBin
          <*> fmap g . _dynamicBin

    first f =
        Block
          <$> _blockMetadata
          <*> _continuousBin
          <*> _nonAdditiveBin
          <*> _additiveBin
          <*> _metricBin
          <*> fmap f . _nonMetricBin
          <*> _dynamicBin

    second = fmap


instance Functor (Block m u v w x y) where

    fmap f =
        Block
          <$> _blockMetadata
          <*> _continuousBin
          <*> _nonAdditiveBin
          <*> _additiveBin
          <*> _metricBin
          <*> _nonMetricBin
          <*> fmap f . _dynamicBin

    (<$) v =
        Block
          <$> _blockMetadata
          <*> _continuousBin
          <*> _nonAdditiveBin
          <*> _additiveBin
          <*> _metricBin
          <*> _nonMetricBin
          <*> (v <$) . _dynamicBin


instance ( NFData m
         , NFData u
         , NFData v
         , NFData w
         , NFData x
         , NFData y
         , NFData z
         ) => NFData (Block m u v w x y z)


-- | (✔)
instance Semigroup (Block m u v w x y z) where

    lhs <> rhs =
        Block
          { _blockMetadata  = _blockMetadata rhs
          , _continuousBin  = _continuousBin  lhs <> _continuousBin  rhs
          , _nonAdditiveBin = _nonAdditiveBin lhs <> _nonAdditiveBin rhs
          , _additiveBin    = _additiveBin    lhs <> _additiveBin    rhs
          , _metricBin      = _metricBin      lhs <> _metricBin      rhs
          , _nonMetricBin   = _nonMetricBin   lhs <> _nonMetricBin   rhs
          , _dynamicBin     = _dynamicBin     lhs <> _dynamicBin     rhs
          }


-- | (✔)
instance ( Show u
         , Show v
         , Show w
         , Show x
         , Show y
         , Show z
         ) => Show (Block m u v w x y z) where

    show block = unlines
        [ "Non-additive s:"
        , niceRendering $ _nonAdditiveBin block
        , "Additive s:"
        , niceRendering $ _additiveBin block
        , "NonMetric s:"
        , niceRendering $ _nonMetricBin block
        , "Continuous s: "
        , niceRendering $ _continuousBin block
        , "Metric s:"
        , niceRendering $ _metricBin block
        , "Dynamic s:"
        , niceRendering $ _dynamicBin block
        ]
      where
        niceRendering :: (Foldable t, Show a) => t a -> String
        niceRendering = unlines . fmap (unlines . fmap ("  " <>) . lines . show) . toList


-- | (✔)
instance ( ToXML u -- This is NOT a redundant constraint.
         , ToXML v
         , ToXML w
         , ToXML y
         , ToXML z
         ) => ToXML (Block m u v w x y z) where

    toXML block = xmlElement "_block" attributes contents
        where
            attributes = []
            contents   = [ Right . collapseElemList "Non-additive_character_block" [] $ _nonAdditiveBin block
                         , Right . collapseElemList "Additive_character_block"     [] $ _additiveBin    block
                         , Right . collapseElemList "NonMetric_character_block"    [] $ _nonMetricBin   block
                         , Right . collapseElemList "Continuous_character_block"   [] $ _continuousBin  block
                         , Right . collapseElemList "Metric_character_block"       [] $ _nonMetricBin   block
                         , Right . collapseElemList "Dynamic_character_block"      [] $ _dynamicBin        block
                         ]
