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

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Bio.Sequence.Block.Metadata
  ( MetadataBlock(..)
  , getBlockMetadata
  , getDynamicMetadata
  -- * Construction
  , continuousToMetadataBlock
  , discreteToMetadataBlock
  , dynamicToMetadataBlock
  , setAllFoci
  , setFoci
  ) where

import Bio.Character.Encodable
import Bio.Metadata.Continuous
import Bio.Metadata.Discrete
import Bio.Metadata.DiscreteWithTCM
import Bio.Metadata.Dynamic
import Bio.Sequence.Block.Internal
import Control.DeepSeq
import Control.Lens
import Data.Key
import qualified Data.MonoTraversable as MT
import Data.TCM
import Data.Vector                  (Vector)
import GHC.Generics
import Text.XML
import Text.XML.Light.Types
import Prelude hiding (zipWith)

-- |
-- Represents a block of data which are optimized atomically together across
-- networks.
--
-- Use '(<>)' to construct larger blocks.
newtype MetadataBlock m = MB
    ( Block
         m
         ContinuousCharacterMetadataDec
         DiscreteCharacterMetadataDec
         DiscreteCharacterMetadataDec
        (DiscreteWithTCMCharacterMetadataDec StaticCharacter)
        (DiscreteWithTCMCharacterMetadataDec StaticCharacter)
        (DynamicCharacterMetadataDec (MT.Element DynamicChar))
    )
    deriving (NFData, Generic, Semigroup)


instance Functor MetadataBlock where

    fmap f (MB b) = MB $ b { blockMetadata = f $ blockMetadata b }

    (<$) v (MB b) = MB $ b { blockMetadata = v }


instance ToXML (MetadataBlock m) where

    toXML (MB block) = Element name attrs contents Nothing
      where
        name     = QName "Metadata_Block" Nothing Nothing
        attrs    = []
        contents = fmap Elem $
            [ collapseElemList "Continuous"  [] . continuousBins
            , collapseElemList "NonAdditive" [] . nonAdditiveBins
            , collapseElemList "Additive"    [] . additiveBins
            , collapseElemList "Metric"      [] . metricBins
            , collapseElemList "Non_Mertic"  [] . nonMetricBins
            , collapseElemList "Dynamic"     [] . dynamicBins
            ] <*> [block]


setAllFoci :: TraversalFoci -> MetadataBlock m -> MetadataBlock m
setAllFoci foci (MB x) = MB $ x { dynamicBins = (traversalFoci ?~ foci) <$> dynamicBins x }


setFoci :: Vector TraversalFoci -> MetadataBlock m -> MetadataBlock m
setFoci fociVec (MB x) = MB $ x { dynamicBins = zipWith (\foci dec -> dec & traversalFoci ?~ foci) fociVec $ dynamicBins x }


getBlockMetadata :: MetadataBlock m -> m
getBlockMetadata (MB x) = blockMetadata x


getDynamicMetadata :: MetadataBlock m -> Vector (DynamicCharacterMetadataDec (MT.Element DynamicChar))
getDynamicMetadata (MB x) = dynamicBins x


continuousToMetadataBlock
  :: ContinuousCharacterMetadataDec
  -> MetadataBlock ()
continuousToMetadataBlock v = MB
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
  -> DiscreteWithTCMCharacterMetadataDec StaticCharacter
  -> MetadataBlock ()
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
    nonAdditive = MB
        Block
        { blockMetadata   = ()
        , continuousBins  = mempty
        , nonAdditiveBins = pure $ stipDec v
        , additiveBins    = mempty
        , metricBins      = mempty
        , nonMetricBins   = mempty
        , dynamicBins     = mempty
        }

    additive = MB
        Block
        { blockMetadata   = ()
        , continuousBins  = mempty
        , nonAdditiveBins = mempty
        , additiveBins    = pure $ stipDec v
        , metricBins      = mempty
        , nonMetricBins   = mempty
        , dynamicBins     = mempty
        }

    metric = MB
        Block
        { blockMetadata   = ()
        , continuousBins  = mempty
        , nonAdditiveBins = mempty
        , additiveBins    = mempty
        , metricBins      = pure v
        , nonMetricBins   = mempty
        , dynamicBins     = mempty
        }

    nonMetric = MB
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
  :: DynamicCharacterMetadataDec (MT.Element DynamicChar)
  -> MetadataBlock ()
dynamicToMetadataBlock v = MB
    Block
    { blockMetadata   = ()
    , continuousBins  = mempty
    , nonAdditiveBins = mempty
    , additiveBins    = mempty
    , metricBins      = mempty
    , nonMetricBins   = mempty
    , dynamicBins     = pure v
    }
