{- |
Module      :  Functions for reading graph formats (Newick, Extended Newick, Vertex-Edge-Root)
Description :  Reads input graph file and returns basic graph structure  
Copyright   :  (c) 2014 Ward C. Wheeler, Division of Invertebrate Zoology, AMNH. All rights reserved.
License     :  

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met: 

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer. 
2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution. 

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

The views and conclusions contained in the software and documentation are those
of the authors and should not be interpreted as representing official policies, 
either expressed or implied, of the FreeBSD Project.

Maintainer  :  Ward Wheeler <wheeler@amnh.org>
Stability   :  unstable
Portability :  portable (I hope)

-}

--TO DO 
-- 1) Bug in dot generation/ancestor list generation--comes out when edge
-- between forest components (prob should be forbidden, but still...)

module ReadGraphs
( processVertexEdgeRoot
, printGraph
, printGraphVizDot
, GenPhyNetNode
, GenPhyNet
, GenForest
) where

import Data.List
import Data.Maybe
import Debug.Trace
import Data.List.Split
import qualified Data.Set as Set
import Data.Tuple
import System.IO
import System.Process
import Control.Exception
import System.Directory

--these types are for a general graph (forest) with isolated nodes, trees, and networks allowing
--for any number of parents and children as lists. Assumes no edges between
--GeneralPhylogenetic Networks--"gauled" in that sense

--definition of non-recusive node type based on String labels or node, desc
--list, and anc list -> (Name, DescendentLIst, AncestorList)
type GenPhyNetNode = (String, [String], [String])

--non-recursive list definition of General Phylogenetic Network
--should be changed to Map or better Array for better ranomd access during
--optimization
type GenPhyNet = [GenPhyNetNode]

--data GenPhyNet a = Node a [GenPhyNet a]  [GraphNode a] 
--                 | EmptyGenPhyNet
--                 deriving (Show)
--need to add a Float for cost to this--perhaps make a tuple
type GenForest = [GenPhyNet]

simpleGenPhyNet = ("root", [], [])

simpleForest = [simpleGenPhyNet]

--hasVertexSet checks input for keyword "vertexSet"
hasVertexSet :: [String] -> Bool
hasVertexSet x =
    "vertexSet" `elem` x 

--hasVertexSet checks input for keyword "edgeSet"
hasEdgeSet :: [String] -> Bool
hasEdgeSet x =
    "edgeSet" `elem` x 

--hasRootSet checks input for keywors "rootSet"
hasRootSet :: [String] -> Bool
hasRootSet x =
    "rootSet" `elem` x 

--hasCurlies checks input for 3 pairs {}
--should and could check for ordering of {} to make sure not out of place.
hasCurlies :: String -> Bool
hasCurlies x =
    let leftCurly = elemIndices '{' x 
        rightCurly = elemIndices '}' x
        equalSigns = elemIndices '=' x
    in
    not ((length leftCurly /= 3) || (length rightCurly /= 3) || (length equalSigns /=3)) 


--deleteAll filters out char Char y from String x
deleteAll :: Eq a => [a]  -> [[a]] -> [[a]]
deleteAll y = map (filter (`notElem` y))

--getVertices idenitifies which of three String lists are the vertex set and
--returns list of vertex identifiers (Strings) after removing head word, { and =
getVertices :: [String] -> [String] -> [String] -> [String]
getVertices (x:xs) (y:ys) (z:zs)    
    | x == "vertexSet" = filter (not . null) $ deleteAll ",={" (tail xs) 
    | y == "vertexSet" = filter (not . null) $ deleteAll ",={" (tail ys)
    | z == "vertexSet" = filter (not . null) $ deleteAll ",={" (tail zs)
    | otherwise          = error "This can't happen"
    

--getEdges idenitifies which of three String lists are the edge set and
--returns list of edge identifiers (Strings) after removing head word, { and =
getEdges :: [String] -> [String] -> [String] -> [String]
getEdges (x:xs) (y:ys) (z:zs)    
    | x == "edgeSet" = filter (not . null) $ deleteAll ",={" (tail xs)
    | y == "edgeSet" = filter (not . null) $ deleteAll ",={" (tail ys)
    | z == "edgeSet" = filter (not . null) $ deleteAll ",={" (tail zs)
    | otherwise        = error "This can't happen"
    
--getRoots idenitifies which of three String lists are the root set and
--returns list of root identifiers (Strings) after removing head word, { and =
getRoots :: [String] -> [String] -> [String] -> [String]
getRoots (x:xs) (y:ys) (z:zs)    
    | x == "rootSet" = filter (not . null) $ deleteAll ",={" (tail xs)
    | y == "rootSet" = filter (not . null) $ deleteAll ",={" (tail ys)
    | z == "rootSet" = filter (not . null) $ deleteAll ",={" (tail zs)
    | otherwise        = error "This can't happen"
    
--splityAndCreatSets inputs valid (checked) graph string and returns tuple of
--vertex, edge and root strings
splitAndCreateSets :: String -> ([String], [String], [String])
splitAndCreateSets x =
    let y = endBy "}" x
        first = words (head y)
        second = words (y !! 1)
        third = words (y !! 2)
        vertexList = getVertices first second third
        edgeList = getEdges first second third
        rootList = getRoots first second third
    in 
        if null vertexList  then error "Empty vertex list"        
        else if null edgeList then error "Empty edge list"        
        else if null rootList then error "Empty root list"        
        else 
            --trace ("pieces "  ++ (show (length y))) 
            (vertexList, edgeList, rootList)  

--isValidGraph checks that all roots and vertices in edge list are in vertex
--list
isValidGraph :: Set.Set String -> Set.Set String -> Set.Set String -> Bool
isValidGraph vSet eSet rSet
    | not (rSet `Set.isSubsetOf` vSet) =
      trace "Root(s) specification error " False
    | not (eSet `Set.isSubsetOf` vSet) =
      trace ("Edge specification error " ++ show (Set.size vSet) ++ " vertices and " ++ show (Set.size eSet) 
        ++ " edge vertices with difference " ++ show (Set.difference eSet vSet)) False
    | otherwise = trace "Graph is set valid." True

--swapIfSecondRoot swpas tuple if second is root
swapIfSecond :: String -> (String, String) -> (String, String)
swapIfSecond x y =
    if x == snd y then swap y
    else y

--divideSet uses partition to split set into one with root and one wiithout
--and orders the tuples in the set with the root
divideSet :: Set.Set (String, String) -> String -> (Set.Set (String, String) , Set.Set (String, String))
divideSet inSet root =
    let (rSet, nonRSet) =  Set.partition (\x -> (fst x == root) || (snd x == root)) inSet
        orderedRootSet = Set.map (swapIfSecond root) rSet
    in
    --trace ("RS: " ++ show orderedRootSet ++ "Rest " ++ show nonRSet)
    (orderedRootSet, nonRSet)

--getNonRoot extract a list of vertices on edges with input root
getNonRoot :: String -> [(String, String)] -> [String]
getNonRoot root edgeList =
    if null edgeList then []
    else 
        let b = snd (head edgeList)
            retList = b : getNonRoot root (tail edgeList)
        in
        retList

--getRootDesc returns list of nodes that are descendents of 'root' based
--on directed edges
getRootDesc :: String -> [(String, String)] -> [String]
getRootDesc root edgeList =
    if null edgeList then []
    else 
        let b = snd (head edgeList)
        in
        if b /= root then b : getRootDesc root (tail edgeList)
        else getRootDesc root (tail edgeList)

--getRootAnc returns list of nodes that are ancestors of 'root' based
--on directed edges
getRootAnc :: String -> [(String, String)] -> [String]
getRootAnc root edgeList =
    if null edgeList then []
    else 
        let (a, b) =  head edgeList
        in
        if b == root then a : getRootAnc root (tail edgeList)
        else getRootAnc root (tail edgeList)

--orderEdges creates ordered tuple set based on root set and edge set 
--adds non-root vertices to root list, calling on remainder till none left
orderEdges :: Set.Set (String, String) -> [String] -> Set.Set (String, String)
orderEdges x y = 
    if null y || Set.null x then Set.empty
    else
        let curRoot = head y
            (setWithRoot, setWithoutRoot) = divideSet x curRoot
            nextRoots = getNonRoot curRoot (Set.toList setWithRoot)
            returnEdges = Set.union setWithRoot (orderEdges setWithoutRoot (tail y ++ nextRoots))
        in
    returnEdges

--getEdgeSet takes string of vertices ordered by "take 2" from list of edge
--vertices and recurses down the line--does not check that the lenght is even
getEdgeSet :: [String] -> [(String, String)]
getEdgeSet x =
    if null x then []
    else 
        let y = take 2 x
            yEdge = (head y,last y)
            yEdgeList = yEdge  : getEdgeSet (drop 2 x)
        in
        yEdgeList

--buildRestGenPhyNet builds on root list of GenPhyNodes
--added "nub" to descendent list because netowrk nodes were being set multiple
--times
buildRestGenPhyNet :: [String] -> Set.Set (String, String) -> [(String, String)] -> GenPhyNet 
buildRestGenPhyNet nodeList directedEdgeSet allDirectedEdgeList = 
    if null nodeList then []
    else 
        let root = head nodeList
            (rootEdges, restEdges) = divideSet directedEdgeSet root
            rootEdgeList = Set.toList rootEdges
            descendentList = getRootDesc root rootEdgeList
            ancestorList = getRootAnc root allDirectedEdgeList
        in 
            --trace ("EdgeList: "++ show rootEdgeList ++ "DescList: " ++ show descendentList ++ "AncdList: " ++ show ancestorList)
            --trace ("\nmaking node " ++ show root ++ " desc: " ++ show descendentList ++ " anc: " ++ show ancestorList)
            (root, descendentList, ancestorList) : buildRestGenPhyNet (nub (tail nodeList ++ descendentList)) restEdges allDirectedEdgeList

--buildGenPhyNet builds a general phylogenetic network starting with a root via
--directed edges
buildGenPhyNet :: String  -> Set.Set (String, String) -> GenPhyNet
buildGenPhyNet root directedEdges =
    let (rootEdges, restEdges) = divideSet directedEdges root
        rootEdgeList = Set.toList rootEdges
        allDirectedEdgeList = Set.toList directedEdges
        descendentList = getRootDesc root rootEdgeList
        ancestorList = getRootAnc root rootEdgeList --[EmptygraphNode]
    in    
        (root, descendentList, ancestorList) : buildRestGenPhyNet descendentList restEdges allDirectedEdgeList 

--buildForest takes list of roots to initate Forest/Tree/Network contruction
buildForest :: [String] -> Set.Set (String, String) -> GenForest 
buildForest r directedEdges =
    if null r then []
    else 
        let returnForest = buildGenPhyNet (head r) directedEdges :  buildForest (tail r) directedEdges
        in
        returnForest


--stringTupleToForest converts vertex, edge, and root string tuples to
--GenForest type
--networks are build tree recusively from roots untill done or error
stringTupleToForest :: ([String], [String], [String]) -> GenForest 
stringTupleToForest (v, e, r) = 
    let e2 = filter (not . null) $ deleteAll ",()" e  --need to replace these with " " or cause problems with "words" later
        vSet = Set.fromList v
        eSet = Set.fromList e2
        rSet = Set.fromList r
    in
    if not (isValidGraph vSet eSet rSet) then error "Error in graph specification"
    else
    --Convert edge string to ordered tuples based on roots
        let inputEdgeSet = Set.fromList (getEdgeSet e2) 
            orderedEdgeSet = orderEdges inputEdgeSet r
            returnForest = buildForest r orderedEdgeSet
        in
        --trace ("\nedge pairs " ++ show inputEdgeSet ++ " roots " ++ show r ++ " ordered " ++ show orderedEdgeSet)
        --Recurse through roots to build networks
        returnForest --simpleForest

--checkandSplitInput checks validity of input graph and split parts
checkAndSplitInput :: String -> ([String], [String], [String])
checkAndSplitInput x =
    let y = words x
    in
    if null x then error "Input Graph empty"
    else if not (hasVertexSet y) then error "Missing vertex set" 
    else if not (hasEdgeSet y) then error "Missing edge set" 
    else if not (hasRootSet y) then error "Missing root set"
    else if not (hasCurlies x) then error "Unbalanced/missing/superfluous '{}' and or '='"
    else 
         --trace ("Input looks OK: " ++ (show y))
         splitAndCreateSets x 

--insertSpecesAfterDelimiters inserts spaces on input string to allow for easier
--parsing of forest
insertSpacesBeforeAndAfterDelimiters :: String -> String -> String
insertSpacesBeforeAndAfterDelimiters y x 
    | null x =  []
    | head x  `elem` y  =  ' ' : head x : ' ' : insertSpacesBeforeAndAfterDelimiters y (tail x)
    | otherwise =  head x : insertSpacesBeforeAndAfterDelimiters y (tail x)

--processVertexEdgeRoot takes input string of graph descrition from a set of
--vertices, edges, and roots and returns a base forest data structure
processVertexEdgeRoot :: String -> GenForest 
processVertexEdgeRoot x =
    if null x then error "Graph string empty."
    else
        let 
            x1 = insertSpacesBeforeAndAfterDelimiters "={}()," x --this to help format with tuples
            y = checkAndSplitInput x1
            z = stringTupleToForest y
        in
            --trace ("\nInstring " ++ (show x) ++ " to " ++ show z)
            z

--printGraph prints ascii graph representation
printGraph :: GenForest -> IO ()
printGraph x = 
    putStrLn "Output graph :"

--putEdges gets edges from nodes and list of descendents
hPutEdges :: Handle -> String -> [String] -> IO ()
hPutEdges myHandle root desc =
    if null desc then hPutStr myHandle ""
    else
        do
            let outString = "\t" ++ "\"" ++ root ++ "\"  -> \"" ++ head desc ++ "\";"
            hPutStrLn myHandle outString
            hPutEdges myHandle root (tail desc)

--putNode puts cretes and prints edges from node
hPutNode :: Handle -> GenPhyNetNode -> IO ()
hPutNode myHandle x =
    --if  (x == ())  then putStr ""
    --else 
        do
            let (label, desc, _) = x
            hPutEdges myHandle label desc

--putGenPhyNet prints edges from root on in pairs
hPutGenPhyNet ::  Handle -> GenPhyNet -> IO ()
hPutGenPhyNet myHandle x =
    if null x then hPutStr myHandle ""
    else 
        do
            hPutNode myHandle (head x)
            hPutGenPhyNet myHandle (tail x)

--putForest prints GenPhyNet in Forest
hPutForest :: Handle -> GenForest -> IO ()
hPutForest myHandle x = 
    if null x then  hPutStr myHandle ""
    else 
        do
            hPutGenPhyNet myHandle (head x)
            hPutForest myHandle (tail x)

--printGraph graphviz simple dot file of graph
--execute with "dot -Tps test.dot -o test.ps"
--need to add output to argument filename and call 
--graphviz via System.Process.runprocess
--also, reorder GenForest so smalles (num leaves) is either first or
--last so can print small to large all the way so easier to read
printGraphVizDot :: GenForest -> String -> IO ()
printGraphVizDot x dotFile =
    if null x then error "No graph to report"
    else do
        myHandle <- openFile dotFile WriteMode
        hPutStrLn  stderr ("Outputting graphviz to " ++ dotFile ++ ".pdf.")
        hPutStrLn myHandle "digraph G {"
        hPutStrLn myHandle "\trankdir = LR;"
        hPutStrLn myHandle "\tnode [ shape = rect];"
        hPutForest myHandle x
        hPutStrLn myHandle "}"
        hClose myHandle
        pCode <- findExecutable "dot" --system "dot" --check for Graphviz
        r <- createProcess (proc "dot" ["-Teps", dotFile, "-O"])
        hPutStrLn stderr
            (if isJust pCode then --pCode /= Nothing then
                "executed dot " ++ "-Teps " ++ dotFile ++ " -O " else
                "Graphviz call failed (not installed or found).  Dot file still created.")

