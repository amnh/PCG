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
  , getBlockMetadata
  , getDynamicMetadata
  -- * Construction
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
import Data.Vector (Vector)
import GHC.Generics
import Text.XML
import Text.XML.Light.Types


-- |
-- Represents a block of data which are optimized atomically together across
-- networks.
--
-- Use '(<>)' to construct larger blocks.
newtype MetadataBlock e d m = MB
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


instance Functor (MetadataBlock e d) where

    fmap f (MB b) = MB $ b { blockMetadata = f $ blockMetadata b }
    
    (<$) v (MB b) = MB $ b { blockMetadata = v }


instance ToXML (MetadataBlock e d m) where

    toXML (MB block) = Element name attrs contents Nothing
      where
        name     = QName "Metadata_Block" Nothing Nothing
        attrs    = []
        contents = fmap Elem $
            [ -- toXML . blockMetadata
              collapseElemList "Continuous"  [] . continuousBins
            , collapseElemList "NonAdditive" [] . nonAdditiveBins
            , collapseElemList "Additive"    [] . additiveBins
            , collapseElemList "Metric"      [] . metricBins
            , collapseElemList "Non_Mertic"  [] . nonMetricBins
            , collapseElemList "Dynamic"     [] . dynamicBins
            ] <*> [block]


getBlockMetadata :: MetadataBlock e d m -> m
getBlockMetadata (MB x) = blockMetadata x


getDynamicMetadata :: MetadataBlock e d m -> Vector (DynamicCharacterMetadataDec d)
getDynamicMetadata (MB x) = dynamicBins x


continuousToMetadataBlock
  :: ContinuousCharacterMetadataDec
  -> MetadataBlock e d ()
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
  -> MetadataBlock e d ()
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
  -> MetadataBlock e d ()
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
