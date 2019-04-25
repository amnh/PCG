-------------------------------------------------------------------------------
-- |
-- Module      :  Bio.Sequence.Metric
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Bio.Sequence.Metric (
--  characterSequenceDistance
  ) where

import Bio.Graph.Constructions
import Data.Coerce
import Bio.Metadata.Metric
import Data.Monoid
import Bio.Metadata.General
import Control.Applicative
import Bio.Sequence.Block
import Data.Foldable
import Bio.Metadata.Dynamic
import Bio.Character.Encodable
import Control.Lens
import Bio.Character.Decoration.Dynamic
import Bio.Metadata.DiscreteWithTCM
import Bio.Character.Encodable.Static
import Bio.Character.Decoration.Discrete

{-
characterSequenceDistance ::
  UnifiedMetadataSequence -> UnifiedCharacterSequence -> UnifiedCharacterSequence -> Sum Double
characterSequenceDistance meta charSeq1 charSeq2 = undefined
  --zipWithFold blockDistance charSeq1 charSeq2

blockDistance :: UnifiedMetadataBlock -> UnifiedCharacterBlock -> UnifiedCharacterBlock -> Sum Double
blockDistance meta block1 block2
  = hexFold $
    hexZip2WithMeta
      unifiedCharacterDistance
      unifiedCharacterDistance
      unifiedCharacterDistance
      undefined -- unifiedDiscreteWithTCMDistance
      mempty
      undefined -- unifiedDynamicCharacterDistance
      meta
      block1
      block2
      
  where
--    continuousDist
--      :: ContinuousCharacterMetadataDec
--      -> UnifiedContinuousCharacter
--      -> UnifiedContinuousCharacter
--      -> Double
--    continuousDist
--      = unifiedCharacterDistance
--
--    additiveDist = undefined
--    nonAdditiveDist = undefined
--    metricDist = undefined
--    nonMetricDist = undefined
--    dynamicDist = undefined


unifiedCharacterDistance
  :: forall s c n n'
   . ( Real n
     , Real n'
     , Fractional n'
     , GetPairwiseTransitionCostMatrix s c n
     , HasCharacterWeight s n'
     )
  => s -> (Maybe c -> Maybe c -> Sum n')
unifiedCharacterDistance s
  = \c1 c2 -> fold $  liftA2 (getPairwiseWeightedTransitionCost @s @c @n s) c1 c2 
-}

unifiedDynamicCharacterDistance = undefined
--  :: forall n
----  .  (Real n)
--  .  DynamicCharacterMetadataDec DynamicCharacterElement
--  -> DynamicDecorationInitial DynamicCharacter
--  -> DynamicDecorationInitial DynamicCharacter
--  -> Word
--unifiedDynamicCharacterDistance meta d1 d2 = 
--    snd $ (meta ^. pairwiseTransitionCostMatrix) dynDec1 dynDec2
--  where
--    dynDec1 = d1 ^. encoded
--    dynDec2 = d2 ^. encoded
---- d

{-
unifiedDiscreteWithTCMDistance
  :: Fractional a
  => DiscreteWithTCMCharacterMetadataDec StaticCharacter
  -> UnifiedDiscreteCharacter
  -> UnifiedDiscreteCharacter
  -> Sum a
unifiedDiscreteWithTCMDistance meta dc1 dc2 =
  (fmap (fromRational . toRational)) $ unifiedDiscreteWithTCMDistance' meta dc1 dc2

unifiedDiscreteWithTCMDistance'
  :: DiscreteWithTCMCharacterMetadataDec StaticCharacter
  -> UnifiedDiscreteCharacter
  -> UnifiedDiscreteCharacter
  -> Sum Double
unifiedDiscreteWithTCMDistance' = coerce
  $ unifiedCharacterDistance @(DiscreteWithTCMCharacterMetadataDec StaticCharacter) @StaticCharacter @Word @Double

  
-}
