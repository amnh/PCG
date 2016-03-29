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

module PCG.Command.Types.Report.Newick where

import Bio.Phylogeny.Graph
import Bio.Phylogeny.Graph.Topological
import Bio.Phylogeny.Graph.Utilities
import Bio.Phylogeny.Tree.Node.Topological

-- | Main fold over a graph
newickReport :: Graph -> String
newickReport graph =
  case toTopoGraph graph of
    (TopoGraph []   ) -> "There were no trees. Check inoput data for inconsitencies?"
    (TopoGraph trees) -> concat ["<", foldr appendTree "" trees, ">"]
  where
    appendTree t str  = concat [init $ printNewick t', renderRootCost t', ";\n"]
      where
        t' = tree t
{-
        -- | Allows output of forests by wrapping up a tree
        terminateTree :: String -> String -> String
        terminateTree treeStr accStr = accStr ++ init treeStr
-}
        -- | Adds a root cost to the end of a tree
    renderRootCost :: TopoNode b -> String
    renderRootCost rootTopo = concat ["[", show $ totalCost rootTopo, "]"]

-- | Wrapper function to output a graph to a newick
outPutNewick :: String -> Graph -> IO ()
outPutNewick fileName = writeFile fileName . newickReport
            
-- | Most important functionality to turn a topoNode into a string
printNewick :: TopoNode b -> String
printNewick curNode
    | isLeaf curNode = name curNode ++ ","
    | otherwise = 
        let childStr = foldr (\c acc -> acc ++ printNewick c) mempty (children curNode)
        in "(" ++ init childStr ++ ")," 
