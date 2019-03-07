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

module Bio.TCM.Dense
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

import Bio.TCM.Dense.FFI
