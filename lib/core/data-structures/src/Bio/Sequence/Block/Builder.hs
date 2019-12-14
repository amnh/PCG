------------------------------------------------------------------------------
-- |
-- Module      :  Bio.Metadata.Sequence.Block
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Bio.Sequence.Block.Builder
  ( PartialCharacterBlock(..)
  , continuousSingleton
  , discreteSingleton
  , dynamicSingleton
  ) where


import Data.TCM
import Data.Vector.Instances ()
import VectorBuilder.Builder (Builder)
import qualified VectorBuilder.Builder as VB



-- |
-- Represents a block of charcters which are optimized atomically together across
-- networks. The 'CharacterBlock' is polymorphic over static and dynamic character
-- definitions.
--
-- Use '(<>)' to construct larger blocks.
data  PartialCharacterBlock u v w x y z
    = PartialCharacterBlock
    { partialContinuousCharacterBins  :: Builder u
    , partialNonAdditiveCharacterBins :: Builder v
    , partialAdditiveCharacterBins    :: Builder w
    , partialMetricCharacterBins      :: Builder x
    , partialNonMetricCharacterBins   :: Builder y
    , partialDynamicCharacters        :: Builder z
    }


instance Semigroup (PartialCharacterBlock u v w x y z) where

    lhs <> rhs =
        PartialCharacterBlock
          { partialContinuousCharacterBins  = partialContinuousCharacterBins  lhs <> partialContinuousCharacterBins  rhs
          , partialNonAdditiveCharacterBins = partialNonAdditiveCharacterBins lhs <> partialNonAdditiveCharacterBins rhs
          , partialAdditiveCharacterBins    = partialAdditiveCharacterBins    lhs <> partialAdditiveCharacterBins    rhs
          , partialMetricCharacterBins      = partialMetricCharacterBins      lhs <> partialMetricCharacterBins      rhs
          , partialNonMetricCharacterBins   = partialNonMetricCharacterBins   lhs <> partialNonMetricCharacterBins   rhs
          , partialDynamicCharacters        = partialDynamicCharacters        lhs <> partialDynamicCharacters        rhs
          }


-- |
-- Construct a singleton block containing a /continuous/ character.
continuousSingleton :: c -> PartialCharacterBlock c v w x y z
continuousSingleton dec = PartialCharacterBlock (VB.singleton dec) mempty mempty mempty mempty mempty


-- |
-- Construct a singleton block containing a /discrete/ character.
discreteSingleton :: forall c s d . TCMStructure -> s -> PartialCharacterBlock c s s s s d
discreteSingleton struct dec =
    case struct of
      NonSymmetric -> PartialCharacterBlock mempty mempty mempty mempty bin    mempty
      Symmetric    -> PartialCharacterBlock mempty mempty mempty mempty bin    mempty
      Metric       -> PartialCharacterBlock mempty mempty mempty bin    mempty mempty
      UltraMetric  -> PartialCharacterBlock mempty mempty mempty bin    mempty mempty
      Additive     -> PartialCharacterBlock mempty mempty bin    mempty mempty mempty
      NonAdditive  -> PartialCharacterBlock mempty bin    mempty mempty mempty mempty
  where
    bin :: Builder s
    {-# INLINE bin #-}
    bin = VB.singleton dec


-- |
-- Construct a singleton block containing a /dynamic/ character.
dynamicSingleton :: d -> PartialCharacterBlock u v w x y d
dynamicSingleton = PartialCharacterBlock mempty mempty mempty mempty mempty . VB.singleton
