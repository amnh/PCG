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

{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}

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


import Control.DeepSeq
import Control.Lens
import Data.Bifunctor
import Data.Foldable
import Data.Semigroup
import Data.Vector           (Vector, fromListN)
import Data.Vector.Instances ()
import GHC.Generics
import Text.XML
import qualified Data.Text as T (Text, lines, unlines)
import TextShow (TextShow (showb, showt), fromText)


-- |
-- Represents a block of data which are optimized atomically together across
-- networks.
--
-- Use '(<>)' to construct larger blocks.
data  Block u v w x y z
    = Block
    { _continuousBin  :: Vector u
    , _nonAdditiveBin :: Vector v
    , _additiveBin    :: Vector w
    , _metricBin      :: Vector x
    , _nonMetricBin   :: Vector y
    , _dynamicBin     :: Vector z
    } deriving (Eq, Generic, NFData)


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
class HasDynamicBin s t a b | s -> a, t -> b where

    dynamicBin :: Lens s t a b
    {-# MINIMAL dynamicBin #-}


instance HasContinuousBin (Block u v w x y z) (Vector u) where

    {-# INLINE continuousBin #-}
    continuousBin = lens _continuousBin $
                    \e x -> e { _continuousBin = x }


instance HasNonAdditiveBin (Block u v w x y z) (Vector v) where

    {-# INLINE nonAdditiveBin #-}
    nonAdditiveBin = lens _nonAdditiveBin $
                     \e x -> e { _nonAdditiveBin = x }


instance HasAdditiveBin (Block u v w x y z) (Vector w) where

    {-# INLINE additiveBin #-}
    additiveBin = lens _additiveBin $
                  \e x -> e { _additiveBin = x }


instance HasMetricBin (Block u v w x y z) (Vector x) where

    {-# INLINE metricBin #-}
    metricBin = lens _metricBin $
                \e x -> e { _metricBin = x }


instance HasNonMetricBin (Block u v w x y z) (Vector y) where

    {-# INLINE nonMetricBin #-}
    nonMetricBin = lens _nonMetricBin $
                   \e x -> e { _nonMetricBin = x }


instance HasDynamicBin (Block u v w x y z) (Block u v w x y z') (Vector z) (Vector z') where

    {-# INLINE  dynamicBin #-}
    dynamicBin = lens _dynamicBin $
                 \e x -> e { _dynamicBin = x }


instance Bifunctor (Block u v w x) where

    bimap f g =
        Block
          <$> _continuousBin
          <*> _nonAdditiveBin
          <*> _additiveBin
          <*> _metricBin
          <*> fmap f . _nonMetricBin
          <*> fmap g . _dynamicBin

    first f =
        Block
          <$> _continuousBin
          <*> _nonAdditiveBin
          <*> _additiveBin
          <*> _metricBin
          <*> fmap f . _nonMetricBin
          <*> _dynamicBin

    second = fmap


instance Functor (Block u v w x y) where

    fmap f =
        Block
          <$> _continuousBin
          <*> _nonAdditiveBin
          <*> _additiveBin
          <*> _metricBin
          <*> _nonMetricBin
          <*> fmap f . _dynamicBin

    (<$) v =
        Block
          <$> _continuousBin
          <*> _nonAdditiveBin
          <*> _additiveBin
          <*> _metricBin
          <*> _nonMetricBin
          <*> (v <$) . _dynamicBin


instance Semigroup (Block u v w x y z) where

    lhs <> rhs =
        Block
          { _continuousBin  = _continuousBin  lhs <> _continuousBin  rhs
          , _nonAdditiveBin = _nonAdditiveBin lhs <> _nonAdditiveBin rhs
          , _additiveBin    = _additiveBin    lhs <> _additiveBin    rhs
          , _metricBin      = _metricBin      lhs <> _metricBin      rhs
          , _nonMetricBin   = _nonMetricBin   lhs <> _nonMetricBin   rhs
          , _dynamicBin     = _dynamicBin     lhs <> _dynamicBin     rhs
          }

    sconcat =
        Block
          <$> sconcat . fmap  _continuousBin
          <*> sconcat . fmap _nonAdditiveBin
          <*> sconcat . fmap    _additiveBin
          <*> sconcat . fmap      _metricBin
          <*> sconcat . fmap   _nonMetricBin
          <*> sconcat . fmap     _dynamicBin

    stimes i _ | i < 1 = error $ mconcat
        [ "Call to Bio.Sequence.Block.stimes with non-positive value: "
        , show (fromIntegral i :: Integer)
        , " <= 0"
        ]
    stimes i v =
        let n = force $ fromIntegral i
            genVect x = fromListN (n * length x) . fold . replicate n $ toList x
        in  Block
              <$> genVect .  _continuousBin
              <*> genVect . _nonAdditiveBin
              <*> genVect .    _additiveBin
              <*> genVect .      _metricBin
              <*> genVect .   _nonMetricBin
              <*> genVect .     _dynamicBin
              $ v


instance ( Show u
         , Show v
         , Show w
         , Show x
         , Show y
         , Show z
         ) => Show (Block u v w x y z) where

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

instance ( TextShow u
         , TextShow v
         , TextShow w
         , TextShow x
         , TextShow y
         , TextShow z
         ) => TextShow (Block u v w x y z) where

    showb block = fromText . T.unlines $
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
        niceRendering :: (Foldable t, TextShow a) => t a -> T.Text
        niceRendering = T.unlines . fmap (T.unlines . fmap ("  " <>) . T.lines . showt) . toList


instance ( ToXML u -- This is NOT a redundant constraint.
         , ToXML v
         , ToXML w
         , ToXML y
         , ToXML z
         ) => ToXML (Block u v w x y z) where

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
