------------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) 2015-2021 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Creates examples of network transformations.
--
-----------------------------------------------------------------------------

module Main where

import           Data.Foldable    (traverse_)
import qualified DisplayTree      as DT
import qualified NetworkEdges     as NE
import qualified ProjectOverview  as PO
import           System.Directory (createDirectoryIfMissing, setCurrentDirectory)


-- |
-- The entry point for constructing example networks.
main :: IO ()
main = do
    createDirectoryIfMissing False "graphviz-examples"
    setCurrentDirectory "graphviz-examples"
    createDirectoryIfMissing False "networks"
    setCurrentDirectory "networks"
    traverse_ NE.makeDotFile NE.networks
    setCurrentDirectory ".."
    createDirectoryIfMissing False "display-trees"
    setCurrentDirectory "display-trees"
    traverse_ DT.makeDotFile DT.displayTrees
    setCurrentDirectory ".."
    createDirectoryIfMissing False "project"
    setCurrentDirectory "project"
    traverse_ PO.makeDotFile PO.overview

