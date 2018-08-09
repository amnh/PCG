-----------------------------------------------------------------------------
-- |
-- Module      :  Analysis.Parsimony.Dynamic.InsertionEvents
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Core types for representing and accumulating insertion events.
-----------------------------------------------------------------------------

module Analysis.Parsimony.Dynamic.DirectOptimization.InsertionEvents
  ( InsertionEvents()
  , coalesce
  , fromEdgeMapping
  , fromList
  , toInsertionCounts
  , unwrap
  , wrap
  )
  where

import Analysis.Parsimony.Dynamic.DirectOptimization.InsertionEvents.Internal
