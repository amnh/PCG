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

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-# LANGUAGE NoMonoLocalBinds    #-}

module Analysis.Clustering.Metric (
  characterSequenceDistance
  ) where

import           Analysis.Parsimony.Dynamic.DirectOptimization
import           Bio.Character
import           Bio.Character.Decoration.Continuous
import           Bio.Character.Decoration.Discrete
import           Bio.Character.Decoration.Dynamic
import           Bio.Character.Encodable
import           Bio.Metadata
import           Bio.Metadata.Metric
import           Bio.Sequence
import qualified Bio.Sequence.Block                            as Blk
import           Control.Applicative
import           Control.Lens
import           Data.Foldable
import           Data.Monoid
import           Data.MonoTraversable
import           Numeric.Extended.Real


characterSequenceDistance
  :: forall f u v w x y z m.
  ( (HasIntervalCharacter u ContinuousCharacter )
  , (HasDiscreteCharacter v StaticCharacter       )
  , (HasDiscreteCharacter w StaticCharacter       )
  , (HasDiscreteCharacter x StaticCharacter       )
  , (DirectOptimizationPostorderDecoration z DynamicCharacter)
  , Applicative f
  , Foldable f
  )
  => MetadataSequence m
  -> CharacterSequence (f u) (f v) (f  w) (f x) (f y) (f z)
  -> CharacterSequence (f u) (f v) (f  w) (f x) (f y) (f z)
  -> Sum Double
characterSequenceDistance =
  foldZipWithMeta blockDistance


blockDistance
  :: forall u v w x y z m f .
     ( (HasIntervalCharacter u ContinuousCharacter )
     , (HasDiscreteCharacter v StaticCharacter       )
     , (HasDiscreteCharacter w StaticCharacter       )
     , (HasDiscreteCharacter x StaticCharacter       )
     , (DirectOptimizationPostorderDecoration z DynamicCharacter)
     , Applicative f
     , Foldable f
     )
  => MetadataBlock m
  -> CharacterBlock (f u) (f v) (f w) (f x) (f y) (f z)
  -> CharacterBlock (f u) (f v) (f w) (f x) (f y) (f z)
  -> Sum Double
blockDistance meta block1 block2
  = hexFold $
    Blk.hexZipWithMeta
      (characterDistance @ExtendedReal (^. (intervalCharacter @u)))
      (characterDistance @Word         (^.   discreteCharacter))
      (characterDistance @Word         (^.   discreteCharacter))
      (characterDistance @Word         (^.   discreteCharacter))
      mempty
      dynamicCharacterDistance
      meta
      block1
      block2



characterDistance
  :: forall n m c d f .
     ( Real n
     , GetPairwiseTransitionCostMatrix m c n
     , HasCharacterWeight m Double
     , Applicative f
     , Foldable f
     )
  => (d -> c) -> m -> f d -> f d -> Sum Double
characterDistance f m c1 c2 = fold $
    liftA2 (getPairwiseWeightedTransitionCost @m @c @n m) (f <$> c1) (f <$> c2)


dynamicCharacterDistance
  :: forall m d c f .
     ( DirectOptimizationPostorderDecoration d c
     , Exportable c
     , GetDenseTransitionCostMatrix m (Maybe DenseTransitionCostMatrix)
     , GetPairwiseTransitionCostMatrix m (Element c) Word
     , HasCharacterWeight m Double
     , Ord (Element c)
     , Applicative f
     , Foldable f
     )
  => m -> f d -> f d -> Sum Double
dynamicCharacterDistance meta c1 c2
  = foldMap (Sum . (weight *) . fromIntegral) $ liftA2 (dynamicCharacterDistance' meta) c1 c2
  where
    weight = meta ^. characterWeight


dynamicCharacterDistance'
  :: forall m d c
   . ( DirectOptimizationPostorderDecoration d c
     , Exportable c
     , GetDenseTransitionCostMatrix m (Maybe DenseTransitionCostMatrix)
     , GetPairwiseTransitionCostMatrix m (Element c) Word
     , Ord (Element c)
     )
  => m -> d -> d -> Word
dynamicCharacterDistance' meta d1 d2 = (^. _1) $ selectDynamicMetric meta c1 c2
  where
    c1 = d1 ^. encoded
    c2 = d2 ^. encoded
