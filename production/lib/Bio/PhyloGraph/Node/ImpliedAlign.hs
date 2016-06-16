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

{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses #-}

module Bio.PhyloGraph.Node.ImpliedAlign where

import Data.Vector

type Homologies    = Vector Int        -- there's a homology trace for every character in the sequence
type HomologyTrace = Vector Homologies -- there's a Homologies vector for every character at the node

class IANode n where
    getHomologies  :: n -> HomologyTrace
    setHomologies  :: n -> HomologyTrace -> n

class IANode' n s | n -> s where
    getHomologies' :: n -> Vector s
    setHomologies' :: n -> Vector s -> n 
