-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.PhyloGraph.Node.Class
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Just a collecting constraint kind for all the usual node typeclasses
--
-----------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds #-}

module Bio.PhyloGraph.Node.Class where

import Bio.PhyloGraph.Node.Preliminary
import Bio.PhyloGraph.Node.Encoded
import Bio.PhyloGraph.Node.Final

type StandardNode n s = (PreliminaryNode n s, EncodedNode n s, FinalNode n s)
