-----------------------------------------------------------------------------
-- |
-- Module      :  Analysis.ImpliedAlignment.InsertionEvents
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Core types for representing and accumulating insertion events.
-----------------------------------------------------------------------------

-- TODO: Maybe we don't need these language extensions?
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}

module Analysis.ImpliedAlignment.InsertionEvents
  ( InsertionEvents()
  , (<^>)
  , coalesce
  , fromList
  , unwrap
  , wrap
  )
  where

import Analysis.ImpliedAlignment.InsertionEvents.Internal
