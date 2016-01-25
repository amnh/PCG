-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Phylogeny.Graph
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Instances and other stuff for a Graph representation
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, NoImplicitPrelude, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Bio.Phylogeny.Graph (Graph(..), Tree(..), EdgeSet(..), EdgeInfo(..), Identifier, Sequence, CharInfo, NodeInfo) where

import           Bio.Phylogeny.Forest
import           Bio.Phylogeny.Graph.Class
import           Bio.Phylogeny.Tree.Binary
import qualified Bio.Phylogeny.Tree.CharacterAware as CT
import qualified Bio.Phylogeny.Tree.Edge.Standard  as E
import qualified Bio.Phylogeny.Tree.EdgeAware      as ET
import           Bio.Phylogeny.Tree.Node
import qualified Bio.Phylogeny.Tree.Referential    as RT
import           Bio.Phylogeny.Tree.Rose
import qualified Bio.Phylogeny.Network             as N
import qualified Bio.Phylogeny.Network.Subsettable as SN

import qualified Data.HashMap.Strict               as H  (insert, foldrWithKey, (!))
import           Data.IntMap                             (insert, foldWithKey, IntMap, member)
import qualified Data.IntMap                       as IM (singleton, map, (!))
import           Data.IntSet                             (fromList)
import qualified Data.IntSet                       as IS (singleton, map, insert, foldr)
import           Data.Key                                (lookup)
import           Data.Monoid
import           Data.Vector                             ((++),(//), length, singleton, elemIndex, (!), Vector, replicate, ifoldr, replicate, cons)
import qualified Data.Vector                       as V  (map, fromList)

import           Prelude                       hiding    ((++),length,lookup, replicate)
import qualified Prelude                           as P  ((++))              
import           Safe



--import Debug.Trace

type KeyMap = IntMap Int

-- | Make all types instances of monoid to allow for mempty and mappend usage
instance Monoid Graph where
  mempty = Graph []
  mappend (Graph g1) (Graph g2) = Graph (g1 <> g2)

-- | Trees are monoids
instance Monoid Tree where
  mempty = Tree mempty mempty mempty mempty mempty 0
  mappend tree1 tree2 = appendAt tree1 tree2 (N.root tree1)

instance SN.SubsettableNetwork Tree NodeInfo where
  appendSubtree = appendAt
  accessSubtree = grabAt

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
                  connectN ++ hungNodes
      -- Now update the names and sequences
      shiftNames = foldWithKey (\k val acc -> insert (k + shift) (val P.++ "a" P.++ show shift) acc) mempty names'
      allNames = names <> shiftNames
      shiftSeqs = H.foldrWithKey (\k val acc -> H.insert (k P.++ "a" P.++ show shift) val acc) mempty seqs'
      allSeqs = --trace ("finished seqs " P.++ show shiftSeqs) $ 
                  seqs <> shiftSeqs
      -- Finally update the edges and add a connecting edge to old nodes
      shiftEdge edge = edge {inNodes = IS.map (shift +) (inNodes edge), outNodes = reMapOut (outNodes edge)}
      reMapOut outEdge = foldWithKey (\k val acc -> insert (k + shift) (reMapInfo val) acc) mempty outEdge
      reMapInfo eInfo = eInfo {origin = allNodes ! code (origin eInfo), terminal = allNodes ! code (terminal eInfo)}
      newEdges = V.map shiftEdge e'
      newRootEdge = EdgeSet (IS.singleton hCode) mempty
      oldRootEdge = EdgeSet mempty (IM.singleton (r' + shift) (EdgeInfo 0 (allNodes ! hCode) (allNodes ! (r' + shift))))
      oldRootUpdate = EdgeSet mempty (insert (r' + shift) (EdgeInfo 0 (allNodes ! hCode) (allNodes ! (r' + shift))) (updateOrigin $ outNodes $ e ! hCode))
      updateOrigin outs = IM.map (\eInfo -> eInfo {origin = allNodes ! hCode}) outs
      newRootUpdate = EdgeSet (IS.insert hCode (inNodes $ newEdges ! r')) mempty
      connectEdges =  let 
                        outE  | null e && null newEdges = V.fromList [oldRootEdge, newRootEdge]
                              | null e || (length e < r - 1) = oldRootEdge `cons` (newEdges // [(r', newRootUpdate)])
                              | null newEdges || (length e' < r' - 1) = (e // [(hCode, oldRootUpdate)]) ++ singleton newRootEdge
                              | otherwise = (e ++ newEdges) // [(hCode, oldRootUpdate), (r', newRootUpdate)]
                      in outE 
    in --trace ("edges on tree join " P.++ show connectEdges)
        Tree allNames allSeqs newChars allNodes connectEdges r

-- | Simply add in the offending nodes without updating edges or characters
simpleAppend :: Tree -> Tree -> NodeInfo -> Tree
simpleAppend (Tree names seqs chars n e r) (Tree names' seqs' _ n' e' r') hangNode = 
  let
    resetRoot = n' // [(r', (n' ! r') {isRoot = False, parents = [code hangNode]})]
  in Tree (names <> names') (seqs <> seqs') chars (n ++ resetRoot) (e ++ e') r

grabAt :: Tree -> NodeInfo -> Tree
grabAt inTree1 rootNode = fst $ internalGrab inTree1 rootNode mempty mempty 0
  where
    internalGrab :: Tree -> NodeInfo -> KeyMap -> Tree -> Int -> (Tree, KeyMap)
    --internalGrab (Tree _ _ _ n _ _) _ _ _ _ | trace ("internalGrab " P.++ show n) False = undefined
    internalGrab inTree@(Tree names seqs chars n e _) curNode keyShift soFar@(Tree names' seqs' _ n' _ _) curKey
      | isLeaf curNode && curKey == 0 = 
        let
          breakNode = curNode {parents = []} 
          brokenNodes = n' ++ (singleton breakNode)
        in (recodeEdges (newTree {nodes = brokenNodes}) insertShift, insertShift)
      | isLeaf curNode = (newTree, insertShift)
      | curKey == 0 = 
        let
          breakNode = curNode {parents = []} 
          brokenNodes = n' ++ (singleton breakNode)
          foldBreak = foldr (\(child, _) acc -> simpleAppend acc child curNode) (newTree {nodes = brokenNodes}) childResults
        in (recodeEdges foldBreak insertShift, mempty)
      | otherwise = (foldWithMe, insertShift)
        where
          prevCode = code curNode
          newNames = case null names of
                      True -> mempty
                      False -> insert curKey (names IM.! prevCode) names'
          newSeqs = case null seqs || null names of
                      True -> mempty
                      False -> H.insert (names IM.! prevCode) (seqs H.! (names IM.! prevCode)) seqs'
          newNodes = n' ++ (singleton curNode)
          newTree = Tree newNames newSeqs chars newNodes e 0
          childResults = zipWith (\c i -> internalGrab inTree (n ! c) keyShift soFar i) (children curNode) [curKey + 1..]
          foldWithMe = --trace ("fold with me " P.++ show childResults) 
                        foldr (\(child, _) acc -> simpleAppend acc child curNode) newTree childResults
          foldMaps = foldr (\(_, m) acc -> m <> acc) keyShift childResults
          insertShift = insert prevCode curKey foldMaps

          recodeEdges :: Tree -> KeyMap -> Tree
          --recodeEdges (Tree _ _ _ inN _ _) reMap | trace ("recodeEdges " P.++ show reMap P.++ show (length inN)) False = undefined
          recodeEdges rTree@(Tree _ _ _ inN inEdges _) reMap 
            | length inN <= 1 = rTree {edges = mempty, nodes = recodeNodes}
            | otherwise = rTree {edges = newEdges, nodes = recodeNodes}
              where
                newEdges = foldr (\n0 acc -> (codeOne $ inEdges ! (code n0)) `cons` acc) mempty inN
                recodeNodes = V.map (\n0 -> n0 {code = reMap IM.! (code n0)}) inN
                codeOne :: EdgeSet -> EdgeSet
                codeOne inSet = 
                  let
                    newIn = IS.foldr (\i acc -> if i `member` reMap then IS.insert (reMap IM.! i) acc else acc) mempty (inNodes inSet)
                    oCode = code . origin
                    tCode = code . terminal
                    oneInfo info = EdgeInfo (len info) ((inN ! (oCode info)) {code = reMap IM.! oCode info}) ((inN ! (tCode info)) {code = reMap IM.! tCode info})
                    newOut = foldWithKey (\i curInfo acc -> if i `member` reMap then insert (reMap IM.! i) (oneInfo curInfo) acc else acc) mempty (outNodes inSet)
                  in EdgeSet newIn newOut


---- | Create a subtree matrix to find all sub nodes
--getSubtrees :: TreeConstraint t n s b => t -> Subtrees
--getSubtrees tree = subtreeMatrix
--  where
--    n = numNodes tree
--    subtreeMatrix = matrix n n omega
--    omega :: (Int, Int) -> Int
--    omega (i,j)
--      | nodeJ `elem` childs = 1 -- Node `j` is a direct child of node `i` (base case)
--      | any indexSet childs = 1 -- Node `j` is in a subtree of node `i`   (memoization)
--      | otherwise           = 0 -- Node `j` is not in a subtree of node `i`
--      where
--        nodeI      = getNthNode tree i
--        nodeJ      = getNthNode tree j
--        childs     = children nodeI tree
--        pointer    = fromJust . flip code tree
--        indexSet n = subtreeMatrix ! (pointer n, j) /= 0

reCodeChars :: (Vector NodeInfo, Vector NodeInfo) -> (Vector CharInfo, Vector CharInfo) -> (Vector NodeInfo, Vector NodeInfo, Vector CharInfo)
reCodeChars (nodes1, nodes2) (chars1, chars2)
  | chars1 == chars2 = (nodes1, nodes2, chars1)
  | otherwise = 
    let 
      intersectChars1 = ifoldr (\i c acc -> if c `elem` chars2 then i `cons` acc else acc) mempty chars1
      complement1 = ifoldr (\i c acc -> if c `elem` chars2 then acc else i `cons` acc) mempty chars1
      complement2 = ifoldr (\i c acc -> if c `elem` chars1 then acc else i `cons` acc) mempty chars2
      reorderFill items indices = foldr (\i acc -> if (length items - 1) < i then Nothing `cons` acc else items ! i `cons` acc) mempty indices
      justFill items indices = foldr (\i acc-> items ! i `cons` acc) mempty indices
      fill1 seqs = if null seqs then seqs
                    else reorderFill seqs intersectChars1 ++ reorderFill seqs complement1 ++ replicate (length complement2) Nothing
      fill2 seqs = if null seqs then seqs
                    else reorderFill seqs intersectChars1 ++ replicate (length complement1) Nothing ++ reorderFill seqs complement2
      allChars = justFill chars1 intersectChars1 ++ justFill chars1 complement1 ++ justFill chars2 complement2
      update1 node = node {encoded = fill1 (encoded node), packed = fill1 (packed node), preliminary = fill1 (preliminary node), final = fill1 (final node), temporary = fill1 (temporary node), aligned = fill1 (aligned node)}
      update2 node = node {encoded = fill2 (encoded node), packed = fill2 (packed node), preliminary = fill2 (preliminary node), final = fill2 (final node), temporary = fill2 (temporary node), aligned = fill2 (aligned node)}
    in --trace ("finish " P.++ show complement1 P.++ show (V.map update1 nodes1))
        (V.map update1 nodes1, V.map update2 nodes2, allChars)

-- | Edge Sets are monoids
instance Monoid EdgeSet where
  mempty = EdgeSet mempty mempty
  mappend (EdgeSet in1 out1) (EdgeSet in2 out2) = EdgeSet (in1 <> in2) (out1 <> out2)

-- | Make this tree structure an instance of the tree types
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
      names2 = insert addPos (show addPos) (nodeNames t)
      seqs2  = H.insert (show addPos) mempty (parsedSeqs t)
      newNode = resetPos n t addPos
      newEdge = makeEdges newNode t
      edges2 = edges t ++ singleton newEdge
      nodes2 = addConnections newNode (nodes t) ++ singleton newNode
      reroot = if isRoot n && null (nodes t) then addPos else root t

makeEdges :: NodeInfo -> Tree -> EdgeSet
makeEdges node inTree = EdgeSet (fromList $ parents node) out
  where
    out  = foldr (\i acc -> insert i (info $ nodes inTree ! i) acc) mempty (children node)
    info = EdgeInfo 0 node 

resetPos :: NodeInfo -> Tree -> Int -> NodeInfo
resetPos node prevTree index =
  let
    leaf = null $ children node
    nroot = null (parents node) && null (nodes prevTree)
  in node {code = index, isLeaf = leaf, isRoot = nroot}

addConnections :: NodeInfo -> Vector NodeInfo -> Vector NodeInfo
addConnections newNode myNodes = 
  let 
    setIn curPos curNodes = curNodes // [(curPos, (curNodes ! curPos) {children = code newNode : children (curNodes ! curPos), isLeaf = False})]
    withIn = foldr setIn myNodes (parents newNode)
    setOut curPos curNodes = curNodes // [(curPos, (curNodes ! curPos) {parents = code newNode : parents (curNodes ! curPos), isRoot = False})]
    withOut = foldr setOut withIn (children newNode)
  in withOut 

-- | This tree can be a binary tree
instance BinaryTree Tree NodeInfo where
  parent     n t = headMay $ map (\i -> nodes t ! i) (parents n)
  leftChild  n t = lookup 0 $ (\i -> nodes t ! i) <$> children n
  rightChild n t = lookup 1 $ (\i -> nodes t ! i) <$> children n

-- | Or this tree can be a rose tree
instance RoseTree Tree NodeInfo where
  parent n t = headMay $ map (\i -> nodes t ! i) (parents n)

-- | Make the graph structure an instance of a forest
instance Forest Graph Tree where
  trees (Graph f) = f
  setTrees _ = Graph
  filterTrees (Graph f) func = Graph $ filter func f

-- | Make it an instance of data storage type classes
instance E.StandardEdge EdgeInfo NodeInfo where
  edgeLen  = len
  setEdgeLen e f = e {len = f}
  origin   = origin
  terminal = terminal

-- | This tree knows its edges
instance ET.EdgedTree Tree NodeInfo EdgeSet where
  edges    n t   = edges t ! code n
  setEdges n t e = t {edges = edges t // [(code n, e)]}

-- | And the tree is aware of its character info
instance CT.CharacterTree Tree CharInfo where
  characters = characters
  setCharacters t c = t {characters = c} 

-- | This particular tree is referential
instance RT.ReferentialTree Tree NodeInfo where
  code node tree = elemIndex node (nodes tree)
  getNthNode tree pos = nodes tree ! pos
