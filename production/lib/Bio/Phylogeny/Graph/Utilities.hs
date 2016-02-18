{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, NoImplicitPrelude, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Bio.Phylogeny.Graph.Utilities where

import Bio.Phylogeny.Graph.Data
import qualified Bio.Phylogeny.Graph.Topological as TG
import qualified Bio.Phylogeny.Tree.Node.Topological as TN
import qualified Bio.Phylogeny.Network as N
import Bio.Phylogeny.Tree.Node

import Prelude      hiding          (filter)
import Data.Vector                  (filter, (!), toList, (//), cons, singleton, Vector, ifoldr)
import qualified Data.Vector as V   (map, fromList, (++), replicate)
import Data.Monoid
import Data.BitVector               (BitVector)
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.HashMap.Lazy as HM

-- | Splits a tree into connected components, forming a graph
splitConnected :: Tree -> Graph
splitConnected inTree = 
    let roots = filter isRoot (nodes inTree)
    in Graph $ toList $ V.map (grabConnected inTree) roots

-- | Grabs connected nodes, assuming that they are isolated 
-- from the rest of the network.
grabConnected :: Tree -> NodeInfo -> Tree
grabConnected inTree curNode = fromTopo $ nodeToTopo inTree curNode

-- | Conversion function from a TopoTree to an indexed tree
fromTopo :: TG.TopoTree -> Tree
fromTopo (TG.TopoTree inTopo _) = internalFromTopo inTopo
  where
    internalFromTopo :: TN.TopoNode BitVector -> Tree
    internalFromTopo topo
      | TN.isLeaf topo = myTree
      | otherwise = foldr (\n acc -> acc <> internalFromTopo n) myTree (TN.children topo)

          where
              myNode = Node 0 (TN.isRoot topo) (TN.isLeaf topo) [] [] (TN.encoded topo) (TN.packed topo) (TN.preliminary topo) 
                          (TN.final topo) (TN.temporary topo) (TN.aligned topo) (TN.cost topo)
              myTree = mempty `N.addNode` myNode

-- | Conversion function from an indexed tree to a TopoTree
toTopo :: Tree -> TG.TopoTree
toTopo tree = nodeToTopo tree (nodes tree ! root tree)

-- | Conversion from an indexed tree to a TopoTree, starting at the given node
nodeToTopo :: Tree -> NodeInfo -> TG.TopoTree
nodeToTopo topTree topNode = TG.TopoTree (internalFromTopo topTree topNode) (characters topTree)
  where
    internalFromTopo :: Tree -> NodeInfo -> TN.TopoNode BitVector
    internalFromTopo inTree curNode 
      | isLeaf curNode = leaf
      | otherwise = 
          let childTrees = map (\i -> internalFromTopo inTree (nodes inTree ! i)) (children curNode)
          in leaf {TN.children = childTrees}
          where
              leaf = TN.TopoNode (isRoot curNode) (isLeaf curNode) (safeName) [] (encoded curNode) (packed curNode) (preliminary curNode) 
                      (final curNode) (temporary curNode) (aligned curNode) (cost curNode)
              safeName = if (code curNode) `IM.member` nodeNames inTree then nodeNames inTree IM.! (code curNode)
                          else ""

-- | Function to append two trees at a given node
-- Properly updates all of the edges to connect the two there
appendAt :: Tree -> Tree -> NodeInfo -> Tree
--appendAt (Tree _ _ _ n _ r) (Tree _ _ _ _ _ r') _ | trace ("appendAt " P.++ show r P.++ show r' P.++ show (length n)) False = undefined
appendAt t1@(Tree names seqs chars n e r) t2@(Tree names' seqs' chars' n' e' r') hangNode 
  | null n  = t2
  | null n' = t1
  | r > length n - 1 || r' > length n' - 1 = error "Root out of bounds when trying to append trees"
  | otherwise = 
    let
      shift = length n
      hCode = code hangNode
      -- First, update the nodes to hold the new characters
      (charNodes1, charNodes2, newChars) = reCodeChars (n, n') (chars, chars')
      -- Then update nodes to recode them and re-hang
      recodeFun node = node {code = code node + shift, children = map (shift +) (children node), parents = map (shift +) (parents node)}
      recodeNew = V.map recodeFun charNodes2
      hungNodes = recodeNew // [(r', (recodeNew ! r') {isRoot = False, parents = [hCode]})]
      connectN = charNodes1 // [(code hangNode, (charNodes1 ! hCode) {children = (shift + r') : children hangNode, isLeaf = False})]
      allNodes = --trace ("finished nodes " P.++ show hungNodes P.++ show connectN) $ 
                  connectN V.++ hungNodes
      -- Now update the names and sequences
      shiftNames = IM.foldWithKey (\k val acc -> IM.insert (k + shift) (val ++ "a" ++ show shift) acc) mempty names'
      allNames = names <> shiftNames
      shiftSeqs = HM.foldrWithKey (\k val acc -> HM.insert (k ++ "a" ++ show shift) val acc) mempty seqs'
      allSeqs = --trace ("finished seqs " P.++ show shiftSeqs) $ 
                  seqs <> shiftSeqs
      -- Finally update the edges and add a connecting edge to old nodes
      shiftEdge edge = edge {inNodes = IS.map (shift +) (inNodes edge), outNodes = reMapOut (outNodes edge)}
      reMapOut outEdge = IM.foldWithKey (\k val acc -> IM.insert (k + shift) (reMapInfo val) acc) mempty outEdge
      reMapInfo eInfo = eInfo {origin = allNodes ! code (origin eInfo), terminal = allNodes ! code (terminal eInfo)}
      newEdges = V.map shiftEdge e'
      newRootEdge = EdgeSet (IS.singleton hCode) mempty
      oldRootEdge = EdgeSet mempty (IM.singleton (r' + shift) (EdgeInfo 0 (allNodes ! hCode) (allNodes ! (r' + shift))))
      oldRootUpdate = EdgeSet mempty (IM.insert (r' + shift) (EdgeInfo 0 (allNodes ! hCode) (allNodes ! (r' + shift))) (updateOrigin $ outNodes $ e ! hCode))
      updateOrigin outs = IM.map (\eInfo -> eInfo {origin = allNodes ! hCode}) outs
      newRootUpdate = EdgeSet (IS.insert hCode (inNodes $ newEdges ! r')) mempty
      connectEdges =  let 
                        outE  | null e && null newEdges = V.fromList [oldRootEdge, newRootEdge]
                              | null e || (length e < r - 1) = oldRootEdge `cons` (newEdges // [(r', newRootUpdate)])
                              | null newEdges || (length e' < r' - 1) = (e // [(hCode, oldRootUpdate)]) V.++ singleton newRootEdge
                              | otherwise = (e V.++ newEdges) // [(hCode, oldRootUpdate), (r', newRootUpdate)]
                      in outE 
    in --trace ("edges on tree join " P.++ show connectEdges)
        Tree allNames allSeqs newChars allNodes connectEdges r

-- | Minor functions to help with appendAt include: reCodeChars, makeEdges, and resetPos
-- which takes two sets of nodes and chars, then recodes both sets of nodes to the unified set of chars
reCodeChars :: (Vector NodeInfo, Vector NodeInfo) -> (Vector CharInfo, Vector CharInfo) -> (Vector NodeInfo, Vector NodeInfo, Vector CharInfo)
reCodeChars (nodes1, nodes2) (chars1, chars2)
  | chars1 == chars2 = (nodes1, nodes2, chars1)
  | otherwise = 
    let 
      intersectChars1 = ifoldr (\i c acc -> if c `elem` chars2 then i `cons` acc else acc) mempty chars1
      complement1 = ifoldr (\i c acc -> if c `elem` chars2 then acc else i `cons` acc) mempty chars1
      complement2 = ifoldr (\i c acc -> if c `elem` chars1 then acc else i `cons` acc) mempty chars2
      justFill items indices = foldr (\i acc-> items ! i `cons` acc) mempty indices
      reorderFill items indices = foldr (\i acc -> if (length items - 1) < i then Nothing `cons` acc else items ! i `cons` acc) mempty indices
      fill1 seqs = if null seqs then seqs
                    else reorderFill seqs intersectChars1 V.++ reorderFill seqs complement1 V.++ V.replicate (length complement2) Nothing
      fill2 seqs = if null seqs then seqs
                    else reorderFill seqs intersectChars1 V.++ V.replicate (length complement1) Nothing V.++ reorderFill seqs complement2
      update1 node = node {encoded = fill1 (encoded node), packed = fill1 (packed node), preliminary = fill1 (preliminary node), final = fill1 (final node), temporary = fill1 (temporary node), aligned = fill1 (aligned node)}
      update2 node = node {encoded = fill2 (encoded node), packed = fill2 (packed node), preliminary = fill2 (preliminary node), final = fill2 (final node), temporary = fill2 (temporary node), aligned = fill2 (aligned node)}
      allChars = justFill chars1 intersectChars1 V.++ justFill chars1 complement1 V.++ justFill chars2 complement2
    in --trace ("finish " P.++ show complement1 P.++ show (V.map update1 nodes1))
        (V.map update1 nodes1, V.map update2 nodes2, allChars)

-- | makeEdges is a small function assisting appendAt
-- it creates the edge set for a given node in the given tree
makeEdges :: NodeInfo -> Tree -> EdgeSet
makeEdges node inTree = EdgeSet (IS.fromList $ parents node) out
  where
    out  = foldr (\i acc -> IM.insert i (info $ nodes inTree ! i) acc) mempty (children node)
    info = EdgeInfo 0 node 

-- | resetPos is a small function assisting the joining of two subtrees
-- simple function to reset positioning of a node
resetPos :: NodeInfo -> Tree -> Int -> NodeInfo
resetPos node prevTree index =
  let
    leaf = null $ children node
    nroot = null (parents node) && null (nodes prevTree)
  in node {code = index, isLeaf = leaf, isRoot = nroot}

-- | addConnections is a small function assiting subtree joins
-- it adds edges between a new node and an existing tree
addConnections :: NodeInfo -> Vector NodeInfo -> Vector NodeInfo
addConnections newNode myNodes = 
  let 
    setIn curPos curNodes = curNodes // [(curPos, (curNodes ! curPos) {children = code newNode : children (curNodes ! curPos), isLeaf = False})]
    withIn = foldr setIn myNodes (parents newNode)
    setOut curPos curNodes = curNodes // [(curPos, (curNodes ! curPos) {parents = code newNode : parents (curNodes ! curPos), isRoot = False})]
    withOut = foldr setOut withIn (children newNode)
  in withOut 

instance Monoid Tree where
  mempty = Tree mempty mempty mempty mempty mempty 0
  mappend tree1 tree2 = appendAt tree1 tree2 (N.root tree1)

instance Monoid Graph where
  mempty = Graph mempty
  mappend (Graph ts1) (Graph ts2) = Graph (ts1 <> ts2)

instance N.Network Tree NodeInfo where
  parents n t   = map (\i -> nodes t ! i) (parents n)
  root t        = nodes t ! root t
  children n t  = map (\i -> nodes t ! i) (children n)
  isLeaf n _    = isLeaf n
  isRoot n _    = isRoot n
  update t new  = t {nodes = nodes t // map (\n -> (code n, n)) new}
  numNodes      = length . nodes 
  addNode t n   = Tree names2 seqs2 (characters t) nodes2 edges2 reroot
    where
      addPos = length $ nodes t
      names2 = IM.insert addPos (show addPos) (nodeNames t)
      seqs2  = HM.insert (show addPos) mempty (parsedSeqs t)
      newNode = resetPos n t addPos
      newEdge = makeEdges newNode t
      edges2 = edges t V.++ singleton newEdge
      nodes2 = addConnections newNode (nodes t) V.++ singleton newNode
      reroot = if isRoot n && null (nodes t) then addPos else root t