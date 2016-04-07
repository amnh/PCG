-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Phylogeny.Node.Class
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

module Bio.Phylogeny.Node.Class where

import Bio.Phylogeny.Node.Preliminary
import Bio.Phylogeny.Node.Encoded
import Bio.Phylogeny.Node.Final

type StandardNode n s = (PreliminaryNode n s, EncodedNode n s, FinalNode n s)