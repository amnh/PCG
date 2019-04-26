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
{-# LANGUAGE GADTs #-}


{-# LANGUAGE NoMonoLocalBinds #-}

module Analysis.Clustering.Metric (
  characterSequenceDistance
  ) where

import Analysis.Parsimony.Dynamic.DirectOptimization                                       
import Bio.Sequence
import Bio.Metadata.Metric
import Data.Monoid
import Bio.Metadata
import Control.Applicative
import Data.Foldable
import Bio.Character.Encodable
import Control.Lens
import Bio.Character.Decoration.Dynamic
import Bio.Character.Decoration.Discrete
import Bio.Character.Decoration.Continuous
import qualified Bio.Sequence.Block as Blk
import Numeric.Extended.Real
import Data.MonoTraversable
import Bio.Character


characterSequenceDistance
  :: forall u v w x y z m.
  ( (HasIntervalCharacter u ContinuousCharacter )
  , (HasDiscreteCharacter v StaticCharacter       )
  , (HasDiscreteCharacter w StaticCharacter       )
  , (HasDiscreteCharacter x StaticCharacter       )
  , (DynamicCharacterDecoration z DynamicCharacter)
  )
  => MetadataSequence m
  -> CharacterSequence (Maybe u) (Maybe v) (Maybe  w) (Maybe x) (Maybe y) (Maybe z)
  -> CharacterSequence (Maybe u) (Maybe v) (Maybe  w) (Maybe x) (Maybe y) (Maybe z)
  -> Sum Double
characterSequenceDistance = 
  foldZipWithMeta blockDistance


blockDistance
  :: forall u v w x y z m .
  ( (HasIntervalCharacter u ContinuousCharacter )
  , (HasDiscreteCharacter v StaticCharacter       )
  , (HasDiscreteCharacter w StaticCharacter       )
  , (HasDiscreteCharacter x StaticCharacter       )
  , (DynamicCharacterDecoration z DynamicCharacter)
  )
  => MetadataBlock m
  -> CharacterBlock (Maybe u) (Maybe v) (Maybe w) (Maybe x) (Maybe y) (Maybe z)
  -> CharacterBlock (Maybe u) (Maybe v) (Maybe w) (Maybe x) (Maybe y) (Maybe z)
  -> Sum Double
blockDistance meta block1 block2
  = hexFold $
    Blk.hexZipWithMeta
      ((characterDistance @ExtendedReal) (^. (intervalCharacter @u)))
      ((characterDistance @Word        ) (^.   discreteCharacter))
      ((characterDistance @Word        ) (^.   discreteCharacter))
      ((characterDistance @Word        ) (^.   discreteCharacter))
      mempty
      dynamicCharacterDistance
      meta
      block1
      block2


    
characterDistance
  :: forall n m c d
   . ( Real n
     , GetPairwiseTransitionCostMatrix m c n
     , HasCharacterWeight m Double
     )
  => (d -> c) -> m -> Maybe d -> Maybe d -> Sum Double
characterDistance f m c1 c2 = fold $
    liftA2 (getPairwiseWeightedTransitionCost @m @c @n m) (f <$> c1) (f <$> c2)


dynamicCharacterDistance
  :: forall m d c
   . ( DynamicCharacterDecoration d c
     , Exportable c
     , GetDenseTransitionCostMatrix m (Maybe DenseTransitionCostMatrix)
     , GetPairwiseTransitionCostMatrix m (Element c) Word
     , HasCharacterWeight m Double
     , Ord (Element c)
     )
  => m -> Maybe d -> Maybe d -> Sum Double
dynamicCharacterDistance meta c1 c2
  = foldMap (Sum . (weight *) . fromIntegral) $ liftA2 (dynamicCharacterDistance' meta) c1 c2
  where
    weight = meta ^. characterWeight


dynamicCharacterDistance'
  :: forall m d c
   . ( DynamicCharacterDecoration d c
     , Exportable c
     , GetDenseTransitionCostMatrix m (Maybe DenseTransitionCostMatrix)
     , GetPairwiseTransitionCostMatrix m (Element c) Word
     , Ord (Element c)
     )
  => m -> d -> d -> Word
dynamicCharacterDistance' meta d1 d2 = pr5_1 $ selectDynamicMetric meta c1 c2
  where
    c1 = d1 ^. encoded
    c2 = d2 ^. encoded
    pr5_1 = \(w,_,_,_,_) -> w
