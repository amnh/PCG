-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Phylogeny.Edge.Class
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Standard edge structure for phylogenetic trees
--
-----------------------------------------------------------------------------
{-# LANGUAGE AllowAmbiguousTypes, MultiParamTypeClasses #-}

module Bio.PhyloGraph.Edge.Class where

import Control.Arrow ((&&&))

-- | A standard edge allows you to get and set length as well as get the origin and terminal
class StandardEdge e n where
    getEdgeLen    :: e -> Double
    setEdgeLen    :: e -> Double -> e
    getOrigin     :: e -> n
    getTerminal   :: e -> n
    getConnection :: e -> (n, n)

    getConnection = getOrigin &&& getTerminal
    getOrigin     = fst . getConnection
    getTerminal   = snd . getConnection
