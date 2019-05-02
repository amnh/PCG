-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Metadata.Metric.Internal
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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Bio.Metadata.Metric.Internal
  ( getPairwiseTransitionCost
  , getPairwiseWeightedTransitionCost
  ) where

import Bio.Metadata.General
import Bio.Metadata.Metric.Class
import Control.Lens
import Data.Monoid


getPairwiseTransitionCost
  :: forall s c n . GetPairwiseTransitionCostMatrix s c n
  => s -> (c -> c -> n)
getPairwiseTransitionCost s = \c1 c2 ->
  snd $ (s ^. pairwiseTransitionCostMatrix) c1 c2


getPairwiseWeightedTransitionCost
  :: forall m c n n'.
     ( Real n
     , Real n'
     , Fractional n'
     , GetPairwiseTransitionCostMatrix m c n
     , HasCharacterWeight m n'
     )
  => m -> (c -> c -> Sum n')
getPairwiseWeightedTransitionCost m = \c1 c2 ->
  Sum $ (m ^. characterWeight) * (realToFrac $ getPairwiseTransitionCost @_ @_ @n m c1 c2)
