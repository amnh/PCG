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

import Bio.PhyloGraph.DAG
import Bio.PhyloGraph.Edge
import Bio.PhyloGraph.Solution
import Bio.PhyloGraph.Node
import Data.Char
--import Data.Vector

import qualified Data.IntMap as IM (elems)

import Debug.Trace

dotOutput :: StandardSolution -> String
dotOutput solution = header ++ foldr (\f acc -> acc ++ foldr treeToDot mempty f) mempty (forests solution) ++ footer
    where
        header = "digraph G { \n" ++ "\trankdir = LR;\n" ++ "\tnode [shape = rect];\n"
        footer = "}"

        treeToDot :: DAG -> String -> String
        treeToDot inTree curString = foldr printEdge curString (edges inTree)
            where 
                printEdge :: EdgeSet -> String -> String
                --printEdge curEdge accum | trace ("printEdge " ++ show curEdge) False = undefined
                printEdge curEdge accum = foldr (++) accum (zipWith printOne origins terminals)
                    where 
                        origins = replaceSpaces . name . origin <$> IM.elems (outNodes curEdge)
                        terminals = replaceSpaces . name . terminal <$> IM.elems (outNodes curEdge)
                        printOne o t = "\t\"" ++ o ++ "\" -> \"" ++ t ++ "\";\n"
                        replaceSpaces = fmap (\c -> if isSpace c then '_' else c)

outPutDot :: String -> StandardSolution -> IO ()
outPutDot fileName = writeFile fileName . dotOutput
