-------------------------------------------------------------------------------
-- |
-- Module      :  Analysis.Distance
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


module Analysis.Distance
  ( characterSequenceDistance
  , characterDistanceMatrix
  ) where

import           Analysis.Parsimony.Dynamic.DirectOptimization
import           Bio.Character
import           Bio.Character.Decoration.Continuous
import           Bio.Character.Decoration.Discrete
import           Bio.Character.Decoration.Dynamic
import           Bio.Metadata.Metric
import           Bio.Sequence
import qualified Bio.Sequence.Block                            as Blk
import           Control.Applicative
import           Control.Lens
import           Control.Parallel.Custom                       (parmap)
import           Control.Parallel.Strategies                   (rpar)
import           Data.Foldable
import           Data.Matrix.Unboxed                           (Matrix)
import qualified Data.Matrix.Unboxed                           as Matrix
import           Data.MonoTraversable
import           Data.Monoid
import           Data.TCM.Dense
import           Data.Vector                                   hiding (foldMap, length)
import           Numeric.Extended.Real


characterSequenceDistance
  :: forall f u v w x y z m.
  ( Applicative f
  , DirectOptimizationPostorderDecoration z DynamicCharacter
  , HasIntervalCharacter u ContinuousCharacter
  , HasDiscreteCharacter v StaticCharacter
  , HasDiscreteCharacter w StaticCharacter
  , HasDiscreteCharacter x StaticCharacter
  , HasDiscreteCharacter y StaticCharacter
  , Foldable f
  )
  => MetadataSequence m
  -> CharacterSequence (f u) (f v) (f  w) (f x) (f y) (f z)
  -> CharacterSequence (f u) (f v) (f  w) (f x) (f y) (f z)
  -> Sum Double
characterSequenceDistance = foldZipWithMeta blockDistance


characterDistanceMatrix
  :: forall f u v w x y z m .
  ( (HasIntervalCharacter u ContinuousCharacter )
  , (HasDiscreteCharacter v StaticCharacter       )
  , (HasDiscreteCharacter w StaticCharacter       )
  , (HasDiscreteCharacter x StaticCharacter       )
  , (HasDiscreteCharacter y StaticCharacter       )
  , (DirectOptimizationPostorderDecoration z DynamicCharacter)
  , Applicative f
  , Foldable f
  )
  => Vector (CharacterSequence (f u) (f v) (f w) (f x) (f y) (f z))
  -> MetadataSequence m
  -> Matrix Double
characterDistanceMatrix leaves meta =
  let numLeaves = length leaves
      distFn (i,j)  =  getSum $ characterSequenceDistance meta (leaves ! i) (leaves ! j)
      matrixEntries :: [Double]
      matrixEntries = parmap rpar distFn [(i,j) | i <- [0..(numLeaves - 1)], j <- [0..(numLeaves - 1)]]
  in  Matrix.fromList (numLeaves, numLeaves) matrixEntries


blockDistance
  :: forall u v w x y z m f .
     ( Applicative f
     , DirectOptimizationPostorderDecoration z DynamicCharacter
     , HasIntervalCharacter u ContinuousCharacter
     , HasDiscreteCharacter v StaticCharacter
     , HasDiscreteCharacter w StaticCharacter
     , HasDiscreteCharacter x StaticCharacter
     , HasDiscreteCharacter y StaticCharacter
     , Foldable f
     )
  => MetadataBlock m
  -> CharacterBlock (f u) (f v) (f w) (f x) (f y) (f z)
  -> CharacterBlock (f u) (f v) (f w) (f x) (f y) (f z)
  -> Sum Double
blockDistance meta block1 block2
  = hexFold $
    Blk.hexZipWithMeta
      (characterDistance @ExtendedReal (^.   intervalCharacter @u))
      (characterDistance @Word         (^.   discreteCharacter))
      (characterDistance @Word         (^.   discreteCharacter))
      (characterDistance @Word         (^.   discreteCharacter))
      (characterDistance @Word         (^.   discreteCharacter))
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
     ( Applicative f
     , DirectOptimizationPostorderDecoration d c
     , ExportableElements c
     , Foldable f
     , GetDenseTransitionCostMatrix m (Maybe DenseTransitionCostMatrix)
     , GetPairwiseTransitionCostMatrix m (Subcomponent (Element c)) Word
     , HasCharacterWeight m Double
     , Ord (Subcomponent (Element c))
     , Show c
     )
  => m -> f d -> f d -> Sum Double
dynamicCharacterDistance meta c1 c2 = foldMap (Sum . (weight *) . fromIntegral) $
                                        liftA2 (dynamicCharacterDistance' meta) c1 c2
  where
    weight = meta ^. characterWeight


dynamicCharacterDistance'
  :: forall m d c
   . ( DirectOptimizationPostorderDecoration d c
     , ExportableElements c
     , GetDenseTransitionCostMatrix m (Maybe DenseTransitionCostMatrix)
     , GetPairwiseTransitionCostMatrix m (Subcomponent (Element c)) Word
     , Ord (Subcomponent (Element c))
     , Show c
     )
  => m -> d -> d -> Word
dynamicCharacterDistance' meta d1 d2 = (^. _1) $ selectDynamicMetric meta c1 c2
  where
    c1 = d1 ^. encoded
    c2 = d2 ^. encoded
