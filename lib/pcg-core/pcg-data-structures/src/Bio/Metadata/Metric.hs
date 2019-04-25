-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Metadata.Metric
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Bio.Metadata.Metric
  ( GetSymbolChangeMatrix(..)
  , GetPairwiseTransitionCostMatrix(..)
  , GetSparseTransitionCostMatrix(..)
  , getPairwiseTransitionCost
  , getPairwiseWeightedTransitionCost
  ) where

import Bio.Metadata.Metric.Class
import Bio.Metadata.Metric.Internal
