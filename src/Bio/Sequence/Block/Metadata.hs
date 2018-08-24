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

{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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
data MetadataBlock m = MB
    { _blockMetadata :: m
    , _blockDataSet  :: {-# UNPACK #-}
      !(Block
           ContinuousCharacterMetadataDec
           DiscreteCharacterMetadataDec
           DiscreteCharacterMetadataDec
          (DiscreteWithTCMCharacterMetadataDec StaticCharacter)
          (DiscreteWithTCMCharacterMetadataDec StaticCharacter)
          (DynamicCharacterMetadataDec DynamicCharacterElement)
      )
    }
    deriving (NFData, Generic)


instance Functor MetadataBlock where

    fmap f (MB m b) = MB (f m) b

    (<$) v (MB _ b) = MB    v  b



instance HasBlockMetadata (MetadataBlock m) m where

    {-# INLINE blockMetadata #-}
    blockMetadata = lens _blockMetadata
                  $ \e x -> e { _blockMetadata = x }


instance HasContinuousBin (MetadataBlock m) (Vector ContinuousCharacterMetadataDec) where

    {-# INLINE continuousBin #-}
    continuousBin = lens (_continuousBin . _blockDataSet)
                  $ \(MB m b) x -> MB m (b { _continuousBin = x })


instance HasNonAdditiveBin (MetadataBlock m) (Vector DiscreteCharacterMetadataDec) where

    {-# INLINE nonAdditiveBin #-}
    nonAdditiveBin = lens (_nonAdditiveBin . _blockDataSet)
                   $ \(MB m b) x -> MB m (b { _nonAdditiveBin = x })


instance HasAdditiveBin (MetadataBlock m) (Vector DiscreteCharacterMetadataDec) where

    {-# INLINE additiveBin #-}
    additiveBin = lens (_additiveBin . _blockDataSet)
                $ \(MB m b) x -> MB m (b { _additiveBin = x })


instance HasMetricBin (MetadataBlock m) (Vector (DiscreteWithTCMCharacterMetadataDec StaticCharacter)) where

    {-# INLINE metricBin #-}
    metricBin = lens (_metricBin . _blockDataSet)
              $ \(MB m b) x -> MB m (b { _metricBin = x })


instance HasNonMetricBin (MetadataBlock m) (Vector (DiscreteWithTCMCharacterMetadataDec StaticCharacter)) where

    {-# INLINE nonMetricBin #-}
    nonMetricBin = lens (_nonMetricBin . _blockDataSet)
                 $ \(MB m b) x -> MB m (b { _nonMetricBin = x })


instance HasDynamicBin (MetadataBlock m) (MetadataBlock m) (Vector (DynamicCharacterMetadataDec DynamicCharacterElement)) (Vector (DynamicCharacterMetadataDec DynamicCharacterElement)) where

    {-# INLINE  dynamicBin #-}
    dynamicBin = lens (_dynamicBin . _blockDataSet)
               $ \(MB m b) x -> MB m (b { _dynamicBin = x })


instance Semigroup (MetadataBlock m) where

    (MB _ b1) <> (MB m2 b2) = MB m2 $ b1 <> b2


instance ToXML (MetadataBlock m) where

    toXML (MB _ block) = Element name attrs contents Nothing
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
setAllFoci foci (MB m b) = MB m $ b { _dynamicBin = (traversalFoci ?~ foci) <$> _dynamicBin b }


setFoci :: Vector TraversalFoci -> MetadataBlock m -> MetadataBlock m
setFoci fociVec (MB m b) = MB m $ b { _dynamicBin = zipWith (\foci dec -> dec & traversalFoci ?~ foci) fociVec $ _dynamicBin b }


getBlockMetadata :: MetadataBlock m -> m
getBlockMetadata = _blockMetadata


getDynamicMetadata :: MetadataBlock m -> Vector (DynamicCharacterMetadataDec DynamicCharacterElement)
getDynamicMetadata (MB _ x) = x ^. dynamicBin


continuousToMetadataBlock
  :: ContinuousCharacterMetadataDec
  -> MetadataBlock ()
continuousToMetadataBlock v = MB ()
    Block
    { _continuousBin  = pure v
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
    nonAdditive = MB ()
        Block
        { _continuousBin  = mempty
        , _nonAdditiveBin = pure $ stipDec v
        , _additiveBin    = mempty
        , _metricBin      = mempty
        , _nonMetricBin   = mempty
        , _dynamicBin     = mempty
        }

    additive = MB ()
        Block
        { _continuousBin  = mempty
        , _nonAdditiveBin = mempty
        , _additiveBin    = pure $ stipDec v
        , _metricBin      = mempty
        , _nonMetricBin   = mempty
        , _dynamicBin     = mempty
        }

    metric = MB ()
        Block
        { _continuousBin  = mempty
        , _nonAdditiveBin = mempty
        , _additiveBin    = mempty
        , _metricBin      = pure v
        , _nonMetricBin   = mempty
        , _dynamicBin     = mempty
        }

    nonMetric = MB ()
        Block
        { _continuousBin  = mempty
        , _nonAdditiveBin = mempty
        , _additiveBin    = mempty
        , _metricBin      = mempty
        , _nonMetricBin   = pure v
        , _dynamicBin     = mempty
        }


dynamicToMetadataBlock
  :: DynamicCharacterMetadataDec DynamicCharacterElement
  -> MetadataBlock ()
dynamicToMetadataBlock v = MB ()
    Block
    { _continuousBin  = mempty
    , _nonAdditiveBin = mempty
    , _additiveBin    = mempty
    , _metricBin      = mempty
    , _nonMetricBin   = mempty
    , _dynamicBin     = pure v
    }
