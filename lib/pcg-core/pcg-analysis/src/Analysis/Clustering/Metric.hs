-------------------------------------------------------------------------------
-- |
-- Module      :  Analysis.Clustering.Metric
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

module Analysis.Clustering.Metric (
  characterSequenceDistance
  ) where

import Analysis.Parsimony.Dynamic.DirectOptimization                                       
import Data.Coerce
import Bio.Sequence
import Bio.Metadata.Metric
import Data.Monoid
import Bio.Metadata
import Control.Applicative
import Data.Foldable
import Bio.Character.Encodable
import Control.Lens
import Bio.Character.Decoration.Dynamic
import Bio.Metadata.DiscreteWithTCM
import Bio.Character.Decoration.Discrete
import Bio.Character.Decoration.Continuous
import Bio.Graph.Constructions
import qualified Bio.Sequence.Block as Blk




--type  UnifiedCharacterSequence
--    = CharacterSequence
--        UnifiedContinuousCharacter
--        UnifiedDiscreteCharacter
--        UnifiedDiscreteCharacter
--        UnifiedDiscreteCharacter
--        UnifiedDiscreteCharacter
--        UnifiedDynamicCharacter
--type UnifiedContinuousCharacter =  (ContinuousDecorationInitial ContinuousCharacter)
--type UnifiedDiscreteCharacter   =  (DiscreteDecoration StaticCharacter)
--type UnifiedDynamicCharacter    =  (DynamicDecorationInitial DynamicCharacter)
characterSequenceDistance ::
  UnifiedMetadataSequence -> UnifiedCharacterSequence -> UnifiedCharacterSequence -> Sum Double
characterSequenceDistance =
  foldZipWithMeta blockDistance
  --Sum . sequenceCost metaSeq . hexZipMeta
----    (const additivePostorderPairwise)                                                 
----    (const    fitchPostorderPairwise)                                                 
----    (const additivePostorderPairwise)                                                 
--    unifiedCharacterDistance
--    unifiedCharacterDistance
--    unifiedCharacterDistance
--    sankoffPostorderPairwise                                                          
--    sankoffPostorderPairwise                                                          
--    adaptiveDirectOptimizationPostorderPairwise                                         
--    metaSeq $ hexZip charSeq1 charSeq2
--  where
--    adaptiveDirectOptimizationPostorderPairwise meta = directOptimizationPostorderPairwise pairwiseAlignmentFunction
--      where
--        pairwiseAlignmentFunction = selectDynamicMetric meta
-- 
  --zipWithFold blockDistance charSeq1 charSeq2


blockDistance :: UnifiedMetadataBlock -> UnifiedCharacterBlock -> UnifiedCharacterBlock -> Sum Double
blockDistance meta block1 block2
  = hexFold $
    Blk.hexZipWithMeta
      unifiedCharacterDistance
      unifiedCharacterDistance
      unifiedCharacterDistance
      unifiedCharacterDistance -- unifiedDiscreteWithTCMDistance
      mempty
      unifiedDynamicCharacterDistance
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
--
 

-- Monoid m => (Element mono -> Element mono -> m) -> (mono -> mono -> m)
unifiedCharacterDistance
  :: forall s c n n'
   . ( Real n
     , Real n'
     , Fractional n'
     , GetPairwiseTransitionCostMatrix s c n
     , HasCharacterWeight s n'
     )
  => s -> Maybe c -> Maybe c -> Sum n'
unifiedCharacterDistance s
  = \c1 c2 -> fold $ liftA2 (getPairwiseWeightedTransitionCost @s @c @n s) c1 c2 

unifiedDynamicCharacterDistance
  :: DynamicCharacterMetadataDec DynamicCharacterElement
  -> UnifiedDynamicCharacter
  -> UnifiedDynamicCharacter
  -> Sum Double
unifiedDynamicCharacterDistance meta c1 c2
  = foldMap (Sum . fromIntegral) $ liftA2 (unifiedDynamicCharacterDistance' meta) c1 c2
  
unifiedDynamicCharacterDistance'
  :: DynamicCharacterMetadataDec DynamicCharacterElement
  -> DynamicDecorationInitial DynamicCharacter
  -> DynamicDecorationInitial DynamicCharacter
  -> Word
unifiedDynamicCharacterDistance' meta d1 d2 =  pr5_1 $ selectDynamicMetric meta c1 c2
  where
    c1 = d1 ^. encoded
    c2 = d2 ^. encoded
    pr5_1 = \(w,_,_,_,_) -> w
    
    adaptiveDirectOptimizationPostorderPairwise _ = directOptimizationPostorderPairwise pairwiseAlignmentFunction
      where
        pairwiseAlignmentFunction = selectDynamicMetric meta
-- 
  --zipWithFold blockDistance charSeq1 charSeq2
--  :: forall n
--  .  (Real n)
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


unifiedDiscreteWithTCMDistance
  :: forall s c n n'
   . ( Real n
     , Real n'
     , Fractional n'
     , GetPairwiseTransitionCostMatrix s c n
     , HasCharacterWeight s n'
     )
  => s -> Maybe c -> Maybe c -> Sum n'
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
