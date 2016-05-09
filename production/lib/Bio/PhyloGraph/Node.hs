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

module Bio.PhyloGraph.Node ( Node(..)
                           , module X
                           ) where

import Bio.PhyloGraph.Node.Encoded      as X
import Bio.PhyloGraph.Node.Final        as X
import Bio.PhyloGraph.Node.Packed       as X
import Bio.PhyloGraph.Node.Preliminary  as X 
import Bio.PhyloGraph.Node.Internal     as X
import Bio.PhyloGraph.Node.ImpliedAlign as X
import Bio.PhyloGraph.Node.Referential  as X
