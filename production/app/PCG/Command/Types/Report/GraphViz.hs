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
import Data.Foldable
import Data.Key hiding (zipWith)
import Data.Monoid
import qualified Data.Vector as V

import qualified Data.IntMap as IM (keys)

--import Debug.Trace

dotOutput :: StandardSolution -> String
--dotOutput solution | trace (show solution) False = undefined
dotOutput solution = header ++ foldr (\f acc -> acc ++ foldr treeToDot mempty f) mempty (forests solution) ++ footer
    where
        header = "digraph G { \n" ++ "\trankdir = LR;\n" ++ "\tnode [shape = rect];\n"
        footer = "}"

        treeToDot :: DAG -> String -> String
        treeToDot inTree curString = edgesStr
            where
              nodeValues = nodes inTree
              
              name' = replaceSpaces . name
                where
                  replaceSpaces = fmap (\c -> if isSpace c then '_' else c)

              edgesStr = foldrWithKey printEdge curString . toList $ edges inTree
                where
                  printEdge :: Int -> EdgeSet -> String -> String
                  --printEdge curEdge accum | trace ("printEdge " ++ show curEdge) False = undefined
                  printEdge i curEdge accum = foldr (<>) accum (zipWith printOne origins terminals)
                    where 
                      origins   = repeat . printNode $ nodeValues V.! i
                      terminals = printNode . (nodeValues V.!) <$> IM.keys (outNodes curEdge)
                      printOne o t = "\t\"" ++ o ++ "\" -> \"" ++ t ++ "\";\n"
                      printNode x
                        | c == 0    = n
                        | otherwise = unwords [n, ": cost =", show' c]
                        where
                          n = name' x
                          c = totalCost x
                          show' x
                            | fromIntegral x' == x = show x'
                            | otherwise = show x
                            where
                              x' = floor x :: Int

outPutDot :: String -> StandardSolution -> IO ()
outPutDot fileName = writeFile fileName . dotOutput
