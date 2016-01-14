-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Megaparsec.Custom
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

{-# LANGUAGE FunctionalDependencies #-}

module Bio.Phylogeny.Tree.Edge.Standard where

-- | A standard edge allows you to get and set length as well as get the origin and terminal
class StandardEdge e n | n -> e where
    edgeLen :: e -> Double
    setEdgeLen :: e -> Double -> e
    origin :: e -> n
    terminal :: e -> n
    connection :: e -> (n, n)

    connection edge = (origin edge, terminal edge)
    origin edge = fst $ connection edge
    terminal edge = snd $ connection edge
