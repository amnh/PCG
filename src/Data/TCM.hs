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
  , TCMDiagnosis(..)
  , TCMStructure(..)
    -- * Construction
  , fromCols
  , fromList
  , fromRows
  , generate
    -- * Indexing
  , (!)
  , (!?)
    -- * Queries
  , size
    -- * Specialization Utility
  , diagnoseTcm
  , reduceTcm
  ) where

import           Data.TCM.Internal
