-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.PhyloGraphPrime.Node
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- The Phylogentic Graph types.
--
-- 
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveFunctor #-}

module Bio.PhyloGraphPrime.Node (PhylogeneticNode(..)) where


data PhylogeneticNode a
    = PNode
    { nodeName       :: String
    , nodeDecoration :: a
    } deriving (Eq, Functor)
