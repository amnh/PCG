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
-- Class with operations on a referential tree 
--
-----------------------------------------------------------------------------

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}


module Bio.PhyloGraph.Tree.Referential where

-- | The referential tree class just means that each node has a code you can use to get it from a iterable
class ReferentialTree t n | t -> n where
    code :: n -> t -> Maybe Int
    getNthNode :: t -> Int -> n