-----------------------------------------------------------------------------
-- |
-- Module      :  Data.TCM
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- A matrix of transition costs between alphabet symbols.
-- Exposes row-major monomorphic maps, folds, and traversals.
-----------------------------------------------------------------------------

module Data.TCM
  ( TCM()
    -- * Construction
  , fromCols
  , fromList
  , fromRows
  , generate
    -- * Indexing
  , (!)
  , (!?)
    -- * Specialization Utility
  , factorTCM
  , isAdditive
  , isNonAdditive
  ) where

import Data.TCM.Internal
