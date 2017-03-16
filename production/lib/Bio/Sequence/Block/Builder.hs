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

{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module Bio.Sequence.Block.Builder
  ( PartialCharacterBlock()
  , finalizeCharacterBlock
--  , toMissingCharacters
  , continuousSingleton
  , discreteSingleton
  , dynamicSingleton
  ) where


import           Bio.Character.Encodable
import           Bio.Character.Decoration.Continuous
import           Bio.Metadata.CharacterName
import           Bio.Sequence.Block.Internal
import           Control.Lens
import           Control.Parallel.Custom
import           Control.Parallel.Strategies
import           Data.DList                    hiding (toList)
import           Data.Foldable
import           Data.Key
import           Data.Monoid                          (mappend)
import           Data.Semigroup
--import           Data.Semigroup.Traversable
import           Data.TCM
import           Data.Vector                          (Vector)
import           Data.Vector.Instances                ()
import qualified Data.Vector                   as V
import           Prelude                       hiding (zipWith)
import           Safe                                 (headMay)


-- |
-- Represents a block of charcters which are optimized atomically together across
-- networks. The 'CharacterBlock' is polymorphic over static and dynamic character
-- definitions.
--
-- Use '(<>)' to construct larger blocks.
data PartialCharacterBlock m i c f a d
   = PartialCharacterBlock
   { partialContinuousCharacterBins  :: DList c
   , partialNonAdditiveCharacterBins :: DList f
   , partialAdditiveCharacterBins    :: DList a
   , partialMetricCharacterBins      :: DList m
   , partialNonMetricCharacterBins   :: DList i
   , partialDynamicCharacters        :: DList d
   } deriving (Eq)


instance Semigroup (PartialCharacterBlock m i c f a d) where

    lhs <> rhs =
        PartialCharacterBlock
          { partialContinuousCharacterBins  = partialContinuousCharacterBins  lhs <> partialContinuousCharacterBins  rhs
          , partialNonAdditiveCharacterBins = partialNonAdditiveCharacterBins lhs <> partialNonAdditiveCharacterBins rhs
          , partialAdditiveCharacterBins    = partialAdditiveCharacterBins    lhs <> partialAdditiveCharacterBins    rhs
          , partialMetricCharacterBins      = partialMetricCharacterBins      lhs <> partialMetricCharacterBins      rhs
          , partialNonMetricCharacterBins   = partialNonMetricCharacterBins   lhs <> partialNonMetricCharacterBins   rhs
          , partialDynamicCharacters        = partialDynamicCharacters        lhs <> partialDynamicCharacters        rhs
          }
          

finalizeCharacterBlock :: PartialCharacterBlock m i c f a d -> CharacterBlock m i c f a d
finalizeCharacterBlock =
    CharacterBlock
      <$> fromDList . partialContinuousCharacterBins 
      <*> fromDList . partialNonAdditiveCharacterBins
      <*> fromDList . partialAdditiveCharacterBins
      <*> fromDList . partialMetricCharacterBins
      <*> fromDList . partialNonMetricCharacterBins
      <*> fromDList . partialDynamicCharacters
  where
    fromDList = V.fromList . toList 


{-
-- |
-- Convert all characters contained in the block to thier missing value.
toMissingCharacters :: ( PossiblyMissingCharacter m
                       , PossiblyMissingCharacter i
                       , PossiblyMissingCharacter c
                       , PossiblyMissingCharacter f
                       , PossiblyMissingCharacter a
                       , PossiblyMissingCharacter d
                       )
                    => CharacterBlock m i c f a d
                    -> CharacterBlock m i c f a d
toMissingCharacters cb =
    CharacterBlock
    { continuousCharacterBins  = toMissing <$> continuousCharacterBins  cb
    , nonAdditiveCharacterBins = toMissing <$> nonAdditiveCharacterBins cb
    , additiveCharacterBins    = toMissing <$> additiveCharacterBins    cb
    , metricCharacterBins      = toMissing <$> metricCharacterBins      cb
    , nonMetricCharacterBins   = toMissing <$> nonMetricCharacterBins   cb
    , dynamicCharacters        = toMissing <$> dynamicCharacters        cb
    }
-}


-- TODO get rid of ContinuousDecorationInitial in signiture

-- |
-- Construct a singleton block containing a /continuous/ character.
continuousSingleton :: CharacterName -> (a -> c) -> a -> PartialCharacterBlock m i (ContinuousDecorationInitial c) f a d
continuousSingleton nameValue transformation continuousValue =
    PartialCharacterBlock (pure bin)  mempty  mempty  mempty mempty mempty
  where
    bin = continuousDecorationInitial nameValue transformation continuousValue


-- |
-- Construct a singleton block containing a /discrete/ character.
discreteSingleton :: TCMStructure -> s -> PartialCharacterBlock s s c s s d
discreteSingleton struct dec =
    case struct of
      NonSymmetric -> PartialCharacterBlock mempty mempty mempty mempty bin    mempty
      Symmetric    -> PartialCharacterBlock mempty mempty mempty mempty bin    mempty
      Metric       -> PartialCharacterBlock mempty mempty mempty bin    mempty mempty
      UltraMetric  -> PartialCharacterBlock mempty mempty mempty bin    mempty mempty
      Additive     -> PartialCharacterBlock mempty mempty bin    mempty mempty mempty
      NonAdditive  -> PartialCharacterBlock mempty bin    mempty mempty mempty mempty
  where
    bin = pure dec


-- |
-- Construct a singleton block containing a /dynamic/ character.
dynamicSingleton :: d -> PartialCharacterBlock m i c f a d
dynamicSingleton = PartialCharacterBlock mempty mempty mempty mempty mempty . pure

