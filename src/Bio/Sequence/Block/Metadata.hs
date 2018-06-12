------------------------------------------------------------------------------
-- |
-- Module      :  Bio.Metadata.Sequence.Block.Metadata
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}

module Bio.Sequence.Block.Metadata
  ( MetadataBlock(..)
  , continuousToMetadataBlock
  , discreteToMetadataBlock
  , dynamicToMetadataBlock
  ) where

import Bio.Metadata.Continuous
import Bio.Metadata.Discrete
import Bio.Metadata.DiscreteWithTCM
import Bio.Metadata.Dynamic
import Bio.Sequence.Block.Internal
import Control.DeepSeq
import Control.Lens
import Data.TCM
import GHC.Generics


-- |
-- Represents a block of data which are optimized atomically together across
-- networks.
--
-- Use '(<>)' to construct larger blocks.
newtype MetadataBlock m e d = MB
    ( Block
         m
         ContinuousCharacterMetadataDec
         DiscreteCharacterMetadataDec
         DiscreteCharacterMetadataDec
        (DiscreteWithTCMCharacterMetadataDec e)
        (DiscreteWithTCMCharacterMetadataDec e)
        (DynamicCharacterMetadataDec d)
    )
    deriving (NFData, Generic, Semigroup)


continuousToMetadataBlock
  :: ContinuousCharacterMetadataDec
  -> MetadataBlock () e d
continuousToMetadataBlock v = MB $
    Block 
    { blockMetadata   = ()
    , continuousBins  = pure v 
    , nonAdditiveBins = mempty
    , additiveBins    = mempty
    , metricBins      = mempty
    , nonMetricBins   = mempty
    , dynamicBins     = mempty
    }


discreteToMetadataBlock
  :: TCMStructure
  -> DiscreteWithTCMCharacterMetadataDec e
  -> MetadataBlock () e d
discreteToMetadataBlock struct v =
    case struct of
      NonAdditive  -> nonAdditive
      Additive     -> additive
      Metric       -> metric
      UltraMetric  -> metric
      NonSymmetric -> nonMetric
      Symmetric    -> nonMetric
  where
    stipDec = discreteMetadata <$> (^. characterName) <*> (^. characterWeight) <*> (^. characterAlphabet)
    
    nonAdditive = MB $
        Block 
        { blockMetadata   = ()
        , continuousBins  = mempty
        , nonAdditiveBins = pure $ stipDec v
        , additiveBins    = mempty
        , metricBins      = mempty
        , nonMetricBins   = mempty
        , dynamicBins     = mempty
        }

    additive = MB $
        Block 
        { blockMetadata   = ()
        , continuousBins  = mempty
        , nonAdditiveBins = mempty
        , additiveBins    = pure $ stipDec v
        , metricBins      = mempty
        , nonMetricBins   = mempty
        , dynamicBins     = mempty
        }

    metric = MB $
        Block 
        { blockMetadata   = ()
        , continuousBins  = mempty
        , nonAdditiveBins = mempty
        , additiveBins    = mempty
        , metricBins      = pure v
        , nonMetricBins   = mempty
        , dynamicBins     = mempty
        }

    nonMetric = MB $
        Block 
        { blockMetadata   = ()
        , continuousBins  = mempty
        , nonAdditiveBins = mempty
        , additiveBins    = mempty
        , metricBins      = mempty
        , nonMetricBins   = pure v
        , dynamicBins     = mempty
        }


dynamicToMetadataBlock
  :: DynamicCharacterMetadataDec d
  -> MetadataBlock () e d
dynamicToMetadataBlock v = MB $
    Block 
    { blockMetadata   = ()
    , continuousBins  = mempty
    , nonAdditiveBins = mempty
    , additiveBins    = mempty
    , metricBins      = mempty
    , nonMetricBins   = mempty
    , dynamicBins     = pure v
    }
