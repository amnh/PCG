-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.TCM.Dense
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Data.TCM.Dense
  ( AlignmentStrategy(..)
  , DenseTransitionCostMatrix(..)
  , CostMatrix2d()
  , CostMatrix3d()
    -- * Construction
  , generateDenseTransitionCostMatrix
    -- * Lookup functions
  , lookupPairwise
  , lookupThreeway
    -- * Query
  , getAlignmentStrategy
  ) where

import Data.TCM.Dense.FFI
