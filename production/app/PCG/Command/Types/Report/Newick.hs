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

-- | Wrapper function to output a graph to a newick
outPutNewick :: String -> Graph -> IO ()
outPutNewick fileName inGraph = writeFile fileName (toNewick inGraph)
    where
        -- | Main fold over a graph
        toNewick :: Graph -> String
        toNewick graph = 
            let (TopoGraph trees) = toTopoGraph graph
            in "<" ++ init (foldr (\g acc -> withRootCost (tree g) acc . printNewick $ tree g) mempty trees) ++ ">"

{-
        -- | Allows output of forests by wrapping up a tree
        terminateTree :: String -> String -> String
        terminateTree treeStr accStr = accStr ++ init treeStr
-}
        -- | Adds a root cost to the end of a tree
        withRootCost :: TopoNode b -> String -> String -> String
        withRootCost rootTopo accStr treeStr = accStr ++ init treeStr ++ "[" ++ rCost ++ "];\n" 
            where rCost = show $ totalCost rootTopo
            
-- | Most important functionality to turn a topoNode into a string
printNewick :: TopoNode b -> String
printNewick curNode
    | isLeaf curNode = name curNode ++ ","
    | otherwise = 
        let childStr = foldr (\c acc -> acc ++ printNewick c) mempty (children curNode)
        in "(" ++ init childStr ++ ")," 
