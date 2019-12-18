-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Metadata.Metric.Class
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

module Bio.Metadata.Metric.Class
  ( GetSymbolChangeMatrix(..)
  , GetPairwiseTransitionCostMatrix(..)
  , GetSparseTransitionCostMatrix(..)
  ) where

import Control.Lens


-- |
-- A 'Getter' for the 'symbolChangeMatrix' field
class GetSymbolChangeMatrix s a | s -> a where

    {-# MINIMAL symbolChangeMatrix #-}
    symbolChangeMatrix :: Getter s a


-- |
-- A 'Getter' for the 'pairwiseTransitionCostMatrix' field
class GetPairwiseTransitionCostMatrix s c w where

    {-# MINIMAL pairwiseTransitionCostMatrix #-}
    pairwiseTransitionCostMatrix  :: Getter s (c -> c -> (c, w))


-- |
-- A 'Getter' for the 'denseTransitionCostMatrix' field
class GetSparseTransitionCostMatrix s a | s -> a where

    {-# MINIMAL sparseTransitionCostMatrix #-}
    sparseTransitionCostMatrix  :: Getter s a
