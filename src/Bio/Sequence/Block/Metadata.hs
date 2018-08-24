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
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Bio.Sequence.Block.Metadata
  ( MetadataBlock(MB)
  -- * Lenses
  , HasBlockMetadata(..)
  , HasContinuousBin(..)
  , HasNonAdditiveBin(..)
  , HasAdditiveBin(..)
  , HasMetricBin(..)
  , HasNonMetricBin(..)
  , HasDynamicBin(..)
  -- * Deprecated accessors
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
import Data.TCM
import Data.Vector                  (Vector)
import GHC.Generics
import Prelude                      hiding (zipWith)
import Text.XML
import Text.XML.Light.Types

-- |
-- Represents a block of data which are optimized atomically together across
-- networks.
--
-- Use '(<>)' to construct larger blocks.
newtype MetadataBlock m = MB
    { unwrap ::
      ( Block
         m
         ContinuousCharacterMetadataDec
         DiscreteCharacterMetadataDec
         DiscreteCharacterMetadataDec
        (DiscreteWithTCMCharacterMetadataDec StaticCharacter)
        (DiscreteWithTCMCharacterMetadataDec StaticCharacter)
        (DynamicCharacterMetadataDec DynamicCharacterElement)
      )
    }
    deriving (NFData, Generic, Semigroup)


instance Functor MetadataBlock where

    fmap f (MB b) = MB $ b { _blockMetadata = f $ _blockMetadata b }

    (<$) v (MB b) = MB $ b { _blockMetadata = v }


{--}
instance HasBlockMetadata (MetadataBlock m) m where

    {-# INLINE blockMetadata #-}
    blockMetadata = lens (_blockMetadata . unwrap)
                  $ \(MB b) x -> MB (b { _blockMetadata = x })


instance HasContinuousBin (MetadataBlock m) (Vector ContinuousCharacterMetadataDec) where

    {-# INLINE continuousBin #-}
    continuousBin = lens (_continuousBin . unwrap)
                  $ \(MB b) x -> MB (b { _continuousBin = x })


instance HasNonAdditiveBin (MetadataBlock m) (Vector DiscreteCharacterMetadataDec) where

    {-# INLINE nonAdditiveBin #-}
    nonAdditiveBin = lens (_nonAdditiveBin . unwrap)
                   $ \(MB b) x -> MB (b { _nonAdditiveBin = x })


instance HasAdditiveBin (MetadataBlock m) (Vector DiscreteCharacterMetadataDec) where

    {-# INLINE additiveBin #-}
    additiveBin = lens (_additiveBin . unwrap)
                $ \(MB b) x -> MB (b { _additiveBin = x })


instance HasMetricBin (MetadataBlock m) (Vector (DiscreteWithTCMCharacterMetadataDec StaticCharacter)) where

    {-# INLINE metricBin #-}
    metricBin = lens (_metricBin . unwrap)
              $ \(MB b) x -> MB (b { _metricBin = x })


instance HasNonMetricBin (MetadataBlock m) (Vector (DiscreteWithTCMCharacterMetadataDec StaticCharacter)) where

    {-# INLINE nonMetricBin #-}
    nonMetricBin = lens (_nonMetricBin . unwrap)
                 $ \(MB b) x -> MB (b { _nonMetricBin = x })


instance HasDynamicBin (MetadataBlock m) (Vector (DynamicCharacterMetadataDec DynamicCharacterElement)) where

    {-# INLINE  dynamicBin #-}
    dynamicBin = lens (_dynamicBin . unwrap)
               $ \(MB b) x -> MB (b { _dynamicBin = x })
{--}


instance ToXML (MetadataBlock m) where

    toXML (MB block) = Element name attrs contents Nothing
      where
        name     = QName "Metadata_Block" Nothing Nothing
        attrs    = []
        contents = fmap Elem $
            [ collapseElemList "Continuous"  [] . (^. continuousBin)
            , collapseElemList "NonAdditive" [] . (^. nonAdditiveBin)
            , collapseElemList "Additive"    [] . (^. additiveBin)
            , collapseElemList "Metric"      [] . (^. metricBin)
            , collapseElemList "Non_Mertic"  [] . (^. nonMetricBin)
            , collapseElemList "Dynamic"     [] . (^. dynamicBin)
            ] <*> [block]


setAllFoci :: TraversalFoci -> MetadataBlock m -> MetadataBlock m
setAllFoci foci (MB x) = MB $ x { _dynamicBin = (traversalFoci ?~ foci) <$> _dynamicBin x }


setFoci :: Vector TraversalFoci -> MetadataBlock m -> MetadataBlock m
setFoci fociVec (MB x) = MB $ x { _dynamicBin = zipWith (\foci dec -> dec & traversalFoci ?~ foci) fociVec $ _dynamicBin x }


getBlockMetadata :: MetadataBlock m -> m
getBlockMetadata (MB x) = x ^. blockMetadata

{-
getContinuousMetadata :: MetadataBlock m -> Vector ContinuousCharacterMetadataDec
getContinuousMetadata (MB x) = continuousBins x


getNonAdditiveMetadata :: MetadataBlock m -> Vector DiscreteCharacterMetadataDec
getNonAditivecMetadata (MB x) = nonAdditiveBins x


getAdditiveMetadata :: MetadataBlock m -> Vector DiscreteCharacterMetadataDec
getAdditiveMetadata (MB x) = additiveBins x


getMetricMetadata :: MetadataBlock m -> Vector DiscreteCharacterMetadataDec
getMetricMetadata (MB x) = metricBins x


getNonMetricMetadata :: MetadataBlock m -> Vector DiscreteCharacterMetadataDec
getNonMetricMetadata (MB x) = nonMetricBins x
-}

getDynamicMetadata :: MetadataBlock m -> Vector (DynamicCharacterMetadataDec DynamicCharacterElement)
getDynamicMetadata (MB x) = x ^. dynamicBin


continuousToMetadataBlock
  :: ContinuousCharacterMetadataDec
  -> MetadataBlock ()
continuousToMetadataBlock v = MB
    Block
    { _blockMetadata  = ()
    , _continuousBin  = pure v
    , _nonAdditiveBin = mempty
    , _additiveBin    = mempty
    , _metricBin      = mempty
    , _nonMetricBin   = mempty
    , _dynamicBin     = mempty
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
        { _blockMetadata  = ()
        , _continuousBin  = mempty
        , _nonAdditiveBin = pure $ stipDec v
        , _additiveBin    = mempty
        , _metricBin      = mempty
        , _nonMetricBin   = mempty
        , _dynamicBin     = mempty
        }

    additive = MB
        Block
        { _blockMetadata  = ()
        , _continuousBin  = mempty
        , _nonAdditiveBin = mempty
        , _additiveBin    = pure $ stipDec v
        , _metricBin      = mempty
        , _nonMetricBin   = mempty
        , _dynamicBin     = mempty
        }

    metric = MB
        Block
        { _blockMetadata  = ()
        , _continuousBin  = mempty
        , _nonAdditiveBin = mempty
        , _additiveBin    = mempty
        , _metricBin      = pure v
        , _nonMetricBin   = mempty
        , _dynamicBin     = mempty
        }

    nonMetric = MB
        Block
        { _blockMetadata  = ()
        , _continuousBin  = mempty
        , _nonAdditiveBin = mempty
        , _additiveBin    = mempty
        , _metricBin      = mempty
        , _nonMetricBin   = pure v
        , _dynamicBin     = mempty
        }


dynamicToMetadataBlock
  :: DynamicCharacterMetadataDec DynamicCharacterElement
  -> MetadataBlock ()
dynamicToMetadataBlock v = MB
    Block
    { _blockMetadata  = ()
    , _continuousBin  = mempty
    , _nonAdditiveBin = mempty
    , _additiveBin    = mempty
    , _metricBin      = mempty
    , _nonMetricBin   = mempty
    , _dynamicBin     = pure v
    }
