-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.PhyloGraph.Graph
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Types for the representation of a node
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Bio.PhyloGraph.Node (Node(..)
                                , module Bio.PhyloGraph.Node.Encoded
                                , module Bio.PhyloGraph.Node.Final
                                , module Bio.PhyloGraph.Node.Packed
                                , module Bio.PhyloGraph.Node.Preliminary
                                , module Bio.PhyloGraph.Node.Internal
                                , module Bio.PhyloGraph.Node.ImpliedAlign
                                ) where

import Bio.PhyloGraph.Node.Encoded 
import Bio.PhyloGraph.Node.Final 
import Bio.PhyloGraph.Node.Packed 
import Bio.PhyloGraph.Node.Preliminary 
import Bio.PhyloGraph.Node.Internal
import Bio.PhyloGraph.Node.ImpliedAlign
