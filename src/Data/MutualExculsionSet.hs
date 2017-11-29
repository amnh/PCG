-----------------------------------------------------------------------------
-- |
-- Module      :  Data.MutualExculsionSet
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Set-like structures for collection of edges.
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric #-}

module Data.MutualExculsionSet
  ( MutualExculsionSet
  -- * Construction
  , singleton
  -- * Deconstruct
  , excludedSet
  , includedSet
  , mutuallyExclusivePairs
  -- * Manipulation
  , invert
  -- * Comparison / Queries
  , excludedLookup
  , includedLookup
  , isExcluded
  , isIncluded
  , isPermissible
  ) where

import Data.MutualExculsionSet.Internal
