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

{-# LANGUAGE DeriveGeneric, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module Bio.Sequence.Block.Internal
  ( Block(..)
  ) where


import Control.DeepSeq
import Data.Bifunctor
import Data.Foldable
import Data.Semigroup
import Data.Vector            (Vector)
import Data.Vector.Instances  ()
import GHC.Generics
import Text.XML
-- import Text.XML.Light.Types


-- |
-- Represents a block of data which are optimized atomically together across
-- networks.
--
-- Use '(<>)' to construct larger blocks.
data  Block m u v w x y z
    = Block
    { blockMetadata   :: m
    , continuousBins  :: Vector u
    , nonAdditiveBins :: Vector v
    , additiveBins    :: Vector w
    , metricBins      :: Vector x
    , nonMetricBins   :: Vector y
    , dynamicBins     :: Vector z
    } deriving (Eq, Generic)


instance Bifunctor (Block m u v w x) where

    bimap f g =
        Block
          <$> blockMetadata
          <*> continuousBins
          <*> nonAdditiveBins
          <*> additiveBins
          <*> metricBins
          <*> fmap f . nonMetricBins
          <*> fmap g . dynamicBins

    first f =
        Block
          <$> blockMetadata
          <*> continuousBins
          <*> nonAdditiveBins
          <*> additiveBins
          <*> metricBins
          <*> fmap f . nonMetricBins
          <*> dynamicBins

    second = fmap


instance Functor (Block m u v w x y) where

    fmap f =
        Block
          <$> blockMetadata
          <*> continuousBins
          <*> nonAdditiveBins
          <*> additiveBins
          <*> metricBins
          <*> nonMetricBins
          <*> fmap f . dynamicBins

    (<$) v =
        Block
          <$> blockMetadata
          <*> continuousBins
          <*> nonAdditiveBins
          <*> additiveBins
          <*> metricBins
          <*> nonMetricBins
          <*> (v <$) . dynamicBins


instance (NFData m, NFData u, NFData v, NFData w, NFData x, NFData y, NFData z) => NFData (Block m u v w x y z)


-- | (✔)
instance Semigroup (Block m u v w x y z) where

    lhs <> rhs =
        Block
          { blockMetadata   = blockMetadata rhs
          , continuousBins  = continuousBins  lhs <> continuousBins  rhs
          , nonAdditiveBins = nonAdditiveBins lhs <> nonAdditiveBins rhs
          , additiveBins    = additiveBins    lhs <> additiveBins    rhs
          , metricBins      = metricBins      lhs <> metricBins      rhs
          , nonMetricBins   = nonMetricBins   lhs <> nonMetricBins   rhs
          , dynamicBins     = dynamicBins     lhs <> dynamicBins     rhs
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
        , niceRendering $ nonAdditiveBins block
        , "Additive s:"
        , niceRendering $ additiveBins block
        , "NonMetric s:"
        , niceRendering $ nonMetricBins block
        , "Continuous s: "
        , niceRendering $ continuousBins block
        , "Metric s:"
        , niceRendering $ metricBins block
        , "Dynamic s:"
        , niceRendering $ dynamicBins block
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
            contents   = [ Right . collapseElemList "Non-additive_character_block" [] $ nonAdditiveBins block
                         , Right . collapseElemList "Additive_character_block"     [] $ additiveBins    block
                         , Right . collapseElemList "NonMetric_character_block"    [] $ nonMetricBins   block
                         , Right . collapseElemList "Continuous_character_block"   [] $ continuousBins  block
                         , Right . collapseElemList "Metric_character_block"       [] $ nonMetricBins   block
                         , Right . collapseElemList "Dynamic_character_block"      [] $ dynamicBins        block
                         ]
