-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.PhyloGraph.Node.ImpliedAlign
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Class for nodes that work with the implied alignment
--
-----------------------------------------------------------------------------

{-# LANGUAGE FunctionalDependencies #-}

module Bio.PhyloGraph.Node.ImpliedAlign where

import Data.Vector

type HomologyTrace = Vector (Vector Int)

class IANode n where
    getHomologies :: n -> HomologyTrace
    setHomologies :: n -> HomologyTrace -> n