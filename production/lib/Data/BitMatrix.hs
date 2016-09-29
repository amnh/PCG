-----------------------------------------------------------------------------
-- |
-- Module      :  Data.BitMatrix
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- A matrix of bits with some useful operations.
-- Exposes row-based monomorphic maps, folds, and traversals.
-- Intended to be used by multiple datatypes for space efficient character
-- state encoding and packing.
-----------------------------------------------------------------------------

module Data.BitMatrix
  ( BitMatrix()
  , bitMatrix
  , collapseRows
  , expandVector
  , fromRows
  , isSet
  , isZeroMatrix
  , numCols
  , numRows
  , rows
  , row
  ) where

import Data.BitMatrix.Internal
