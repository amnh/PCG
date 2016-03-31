-----------------------------------------------------------------------------
-- |
-- Module      :  PCG.Command.Types.Report.Newick
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Functionality to output a Newick format from a Graph
--
-----------------------------------------------------------------------------
{-# LANGUAGE BangPatterns #-}

module PCG.Command.Types.Report.Newick where

import Bio.Phylogeny.Solution
import Bio.Phylogeny.Tree.Node.Topological (TopoNode(..))
import Data.Monoid            ((<>))
import Data.List              (intercalate)

-- | Main fold over a graph
newickReport :: StandardSolution -> String
newickReport solution =
  case newickRenderForest <$> forests solution of
    []   -> "There were no trees. Check input data for inconsistencies?"
    strs -> intercalate ";\n" strs

newickRenderForest :: Forest DAG -> String
newickRenderForest []   = ""
newickRenderForest dags =
  case concat $ newickRenderDag <$> dags of
    ""  -> ""
    str -> "<" <> str <> ">"

-- Probably doesn't handle a tree with a single node well since Newick format sucks at specifying that...
newickRenderDag :: DAG -> String
newickRenderDag dag = concat [ newickRenderNode rootNode, "[", show treeCost, "];" ]
  where
    rootNode = structure $ toTopo dag
    treeCost = totalCost rootNode
    newickRenderNode :: Show b => TopoNode b -> String
    newickRenderNode node
      | null (children node) = name node
      | otherwise = (\x -> "(" <> x <> ")") . intercalate "," $ newickRenderNode <$> children node

-- | Wrapper function to output a graph to a newick
outPutNewick :: String -> StandardSolution -> IO ()
outPutNewick fileName = writeFile fileName . newickReport

