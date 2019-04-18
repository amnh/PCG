-----------------------------------------------------------------------------
-- |
-- Module      :  Analysis.Clustering.Hierarchical
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards  #-}

module Analysis.Clustering.Hierarchical where

import qualified Data.Vector as V
import Data.Monoid (Sum)
import Bio.Sequence
import Data.MetricRepresentation
import Bio.Character
import Data.Coerce
import Control.Lens.Operators
import Numeric.Extended.Real
import Bio.Character
import Bio.Graph 
import Bio.Character.Decoration.Continuous
import Bio.Character.Decoration.Dynamic
import Bio.Graph.LeafSet
import Bio.Graph.Node
import Bio.Graph.ReferenceDAG
import Bio.Graph.PhylogeneticDAG
import Control.Applicative
import Data.Foldable
import Data.Semigroup
import Bio.Metadata
import Bio.Sequence.Block



import AI.Clustering.Hierarchical ()


cluster
  :: PhylogeneticDAG m e n u v w x y z
  -> Int
  -> LeafSet (PhylogeneticNode (CharacterSequence u v w x y z) n)
cluster = undefined

data CharacterInfo m u v w x y z = CharacterInfo
  { characterSequence_  :: CharacterSequence u v w x y z
  , metadataSequence_   :: MetadataSequence m
  }

--getCharacterInfo
--  :: PhylogeneticDAG m e n u v w x y z
--  -> CharacterInfo m u v w x y z
--getCharacterInfo pdag = CharacterInfo{..}
--  where
--    characterSequence_
--      = (_nodeDecorationDatum ^.) <$> (pdag ^. (_phylogeneticForest . _references))
--    metadataSequence_
--      = columnMetadata ^. pdag


--type UnifiedDiscreteCharacter   = Maybe (DiscreteDecoration StaticCharacter)
--type UnifiedContinuousCharacter = Maybe (ContinuousDecorationInitial ContinuousCharacter)
--type UnifiedDynamicCharacter    = Maybe (DynamicDecorationInitial DynamicCharacter)
--type  UnifiedMetadataBlock = MetadataBlock ()
--type  UnifiedMetadataSequence = MetadataSequence ()
--type  UnifiedCharacterSequence
--    = CharacterSequence
--        UnifiedContinuousCharacter
--        UnifiedDiscreteCharacter
--        UnifiedDiscreteCharacter
--        UnifiedDiscreteCharacter
--        UnifiedDiscreteCharacter
--        UnifiedDynamicCharacter
--
--type  UnifiedCharacterBlock
--    = CharacterBlock
--        UnifiedContinuousCharacter
--        UnifiedDiscreteCharacter
--        UnifiedDiscreteCharacter
--        UnifiedDiscreteCharacter
--        UnifiedDiscreteCharacter
--        UnifiedDynamicCharacter
--  

charSequenceDistance ::
  UnifiedMetadataSequence -> UnifiedCharacterSequence -> UnifiedCharacterSequence -> Sum Double
charSequenceDistance meta charSeq1 charSeq2 = undefined
  --zipWithFold blockDistance charSeq1 charSeq2

blockDistance :: UnifiedMetadataBlock -> UnifiedCharacterBlock -> UnifiedCharacterBlock -> Sum Double
blockDistance meta block1 block2
  = hexFold $
    hexZip2WithMeta
      continuousDist
      additiveDist
      nonAdditiveDist
      metricDist
      nonMetricDist
      dynamicDist
      meta
      block1
      block2
      
  where
    continuousDist
      :: ContinuousCharacterMetadataDec
      -> UnifiedContinuousCharacter
      -> UnifiedContinuousCharacter
      -> Sum Double
    continuousDist cmeta char1 char2
      = let
          interval1 = fmap (^. intervalCharacter) char1
          interval2 = fmap (^. intervalCharacter) char2
          weight :: ExtendedReal
          weight    = coerce @Double @ExtendedReal $ cmeta ^. characterWeight
        in
            fold
          . coerce @(Maybe ExtendedReal) @(Maybe (Sum Double))
          . fmap (* weight)
          $ liftA2 continuousMetric interval1 interval2

    additiveDist = undefined
    nonAdditiveDist = undefined
    metricDist = undefined
    nonMetricDist = undefined
    dynamicDist = undefined


continuousMetric
  :: ContinuousCharacter
  -> ContinuousCharacter
  -> ExtendedReal
continuousMetric c1 c2 = snd $ firstLinearNormPairwiseLogic @_ @_ @ContinuousCharacter c1 c2

discreteMetric
  :: StaticCharacter
  -> StaticCharacter
  -> Double
discreteMetric s1 s2 = snd $ discreteMetricPairwiseLogic s1 s2
