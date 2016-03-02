-----------------------------------------------------------------------------
-- |
-- Module      :  File.Format.VertexEdgeRoot.Parser
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Functionality for converting VER internal format into Graph format
--
-----------------------------------------------------------------------------

module File.Format.VertexEdgeRoot.Converter where

import Bio.Phylogeny.Graph
import qualified File.Format.VertexEdgeRoot.Parser as VER
import Bio.Phylogeny.Tree.Node
import Bio.Phylogeny.Graph.Utilities

import qualified Data.Set       as S  (Set, elemAt, toList, size, filter)
import qualified Data.HashMap.Lazy   as HM (HashMap, insert, (!))
import qualified Data.IntMap    as IM (IntMap, insert, (!))
import qualified Data.Vector    as V  (Vector, fromList, (!))
import qualified Data.IntSet    as IS (fromList)

--import Debug.Trace

data IntEdge = IntEdge (Int, Int) VER.EdgeLength deriving (Eq, Show, Ord)

convert :: VER.VertexEdgeRoot -> Graph
convert inVer = splitConnected outTree
    where
        nameDicts = accumNames 0 (VER.vertices inVer)
        rootList = S.toList $ VER.roots inVer
        finalNodes = accumNodes (VER.edges inVer) nameDicts
        outEdges = accumEdges finalNodes (VER.edges inVer) (fst nameDicts)
        outTree = DAG (fst nameDicts) mempty mempty finalNodes outEdges 0

        -- | First we correspond the names to indices in both directions
        accumNames :: Int -> S.Set VER.VertexLabel -> (IM.IntMap Identifier, HM.HashMap Identifier Int)
        --accumNames curPos labels | trace ("accumNames at pos " ++ show curPos ++ " with labels " ++ show labels) False = undefined
        accumNames curPos labels 
            | curPos == S.size labels = (mempty, mempty)
            | otherwise = 
                let 
                    recursion = accumNames (curPos + 1) labels
                    fromInt = IM.insert curPos (S.elemAt curPos labels) (fst recursion)
                    toInt = HM.insert (S.elemAt curPos labels) curPos (snd recursion)
                in (fromInt, toInt)

        -- | Now we accumulate as many nodes as we had names
        accumNodes :: S.Set VER.EdgeInfo -> (IM.IntMap Identifier, HM.HashMap Identifier Int) -> V.Vector NodeInfo
        --accumNodes inEdges (toNames, fromNames) | trace ("accumNodes with names " ++ show toNames) False = undefined
        accumNodes inEdges (toNames, fromNames) = fmap makeNode (V.fromList [0..S.size (VER.vertices inVer) - 1])
            where
                makeNode :: Int -> NodeInfo
                makeNode index = 
                    let
                        myName = toNames IM.! index
                        myParents = foldr (\(VER.EdgeInfo (o, t) _) acc -> if t == myName then (fromNames HM.! o) : acc else acc) mempty inEdges
                        myChildren = foldr (\(VER.EdgeInfo (o, t) _) acc -> if o == myName then (fromNames HM.! t) : acc else acc) mempty inEdges
                        atRoot = myName `elem` rootList
                    in Node index atRoot (null myChildren) myParents myChildren mempty mempty mempty mempty mempty mempty 0

        -- | Now we can generate the edges
        accumEdges :: V.Vector NodeInfo -> S.Set VER.EdgeInfo -> IM.IntMap Identifier -> V.Vector EdgeSet
        accumEdges madeNodes origEdges nameMap = fmap makeEdge madeNodes
            where
                getLen :: Int -> Int -> Double
                getLen start stop = 
                    let findIt = S.toList $ S.filter (\(VER.EdgeInfo (i, o) _) -> i == (nameMap IM.! start) && o == (nameMap IM.! stop)) origEdges
                    in case findIt of
                        [VER.EdgeInfo _ (Just l)] -> l
                        _ -> 0

                makeEdge :: NodeInfo -> EdgeSet
                makeEdge inNode = 
                    let
                        myI = code inNode
                        inE = IS.fromList $ parents inNode
                        outInfo = map (\i -> EdgeInfo (getLen myI i) (madeNodes V.! myI) (madeNodes V.! i) Nothing) (children inNode)
                        outMap = foldr (\info acc -> IM.insert (code $ terminal info) info acc) mempty outInfo
                    in EdgeSet inE outMap
