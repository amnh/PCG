-----------------------------------------------------------------------------
-- |
-- Module      :  PCG.Command.Types.Report.Graphviz
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Functionality to output a graphviz format from a Graph
--
-----------------------------------------------------------------------------

module PCG.Command.Types.Report.GraphViz where

import Bio.Phylogeny.Graph.Data
import Bio.Phylogeny.Tree.Node
import Data.Char

import qualified Data.IntMap as IM (elems, (!))
import System.IO ()

outPutDot :: String -> Graph -> IO ()
outPutDot fileName inGraph = writeFile fileName (toDot inGraph)
    
    where
        toDot :: Graph -> String
        toDot (Graph trees) = header ++ foldr treeToDot "" trees ++ footer
            where
                header = "digraph G { \n" ++ "\trankdir = LR;\n" ++ "\tnode [shape = rect];\n"
                footer = "}"

        treeToDot :: DAG -> String -> String
        treeToDot inTree curString = foldr printEdge curString (edges inTree)
            where 
                printEdge :: EdgeSet -> String -> String
                printEdge curEdge accum = foldr (++) accum (zipWith printOne origins terminals)
                    where 
                        getName n = nodeNames inTree IM.! code n
                        origins = map (replaceSpaces . getName . origin) (IM.elems $ outNodes curEdge)
                        terminals = map (replaceSpaces . getName . terminal) (IM.elems $ outNodes curEdge)
                        printOne o t = "\t" ++ o ++ " -> " ++ t ++ ";\n"
                        replaceSpaces = map (\c -> if isSpace c then '_' else c)


                        