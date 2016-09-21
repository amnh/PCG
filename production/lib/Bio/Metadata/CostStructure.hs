------------------------------------------------------------------------------
-- |
-- Module      :  Bio.Metadata.CostStructure
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Data types for metadata
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}

module Bio.Metadata.CostStructure
  ( CostStructureResult()
  , weight
  ) where

import Control.Monad.State.Lazy
import Data.Traversable
import Data.List       (isPrefixOf)
import Data.Map hiding (null)
import Data.Monoid
import Prelude  hiding (lookup)
import Text.Show       (showListWith, showString)

-- import Debug.Trace

data CostStructureResult
   = NonAdditive  Int
   | Additive     Int
   | NonMetricTCM Int TCM

weight :: CostStructureResult -> Int
weight (NonAdditive  w  ) = w
weight (Additive     w  ) = w
weight (NonMetricTCM w _) = w  

