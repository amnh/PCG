-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Phylogeny.Graph.Output
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Functionality to output a graph
--
-----------------------------------------------------------------------------

module Bio.Phylogeny.Graph.Output where

import Bio.Phylogeny.Graph.Data
import Bio.Phylogeny.Tree.Node

import qualified Data.IntMap as IM (elems, (!))
import System.IO ()

outPutDot :: String -> Graph -> IO ()
outPutDot fileName inGraph = writeFile fileName (toDot inGraph)
    
    where
        toDot :: Graph -> String
        toDot (Graph trees) = header ++ foldr treeToDot "" trees ++ footer
            where
                header = "digraph G { \n" ++ "\trankdir = LR;\n" ++ "\tnode [shape = rect];"
                footer = "}"

        treeToDot :: DAG -> String -> String
        treeToDot inTree curString = foldr printEdge curString (edges inTree)
            where 
                printEdge :: EdgeSet -> String -> String
                printEdge curEdge accum = foldr (++) accum (zipWith printOne origins terminals)
                    where 
                        getName n = nodeNames inTree IM.! code n
                        origins = map (getName . origin) (IM.elems $ outNodes curEdge)
                        terminals = map (getName . terminal) (IM.elems $ outNodes curEdge)
                        printOne o t = o ++ " -> " ++ t ++ ";\n"


                        