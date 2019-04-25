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

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
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
  :: forall s c n n'.
     ( Real n
     , Real n'
     , Fractional n'
     , GetPairwiseTransitionCostMatrix s c n
     , HasCharacterWeight s n'
     )
  => s -> (c -> c -> Sum n')
getPairwiseWeightedTransitionCost s = \c1 c2 -> 
  Sum $ (s ^. characterWeight) * (realToFrac $ getPairwiseTransitionCost @_ @_ @n s c1 c2)
