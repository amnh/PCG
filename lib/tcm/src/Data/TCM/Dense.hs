-----------------------------------------------------------------------------
-- |
-- Module      :  Data.TCM.Dense
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Data.TCM.Dense
  ( DenseTransitionCostMatrix()
    -- * Construction
  , generateDenseTransitionCostMatrix
    -- * Accessor functions
  , lookupPairwise
  , lookupThreeway
  ) where

import Data.TCM.Dense.FFI
