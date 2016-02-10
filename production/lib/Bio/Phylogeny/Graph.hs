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

module Bio.Phylogeny.Graph (Graph(..), Tree(..), EdgeSet(..), EdgeInfo(..), Identifier, Sequence, CharInfo, NodeInfo, toTopo, simpleAppend, fromTopo) where

import           Bio.Phylogeny.Forest
import           Bio.Phylogeny.Graph.Class
import           Bio.Phylogeny.Tree.Binary
import qualified Bio.Phylogeny.Tree.CharacterAware as CT
import qualified Bio.Phylogeny.Tree.Edge.Standard  as E
import qualified Bio.Phylogeny.Tree.EdgeAware      as ET
import           Bio.Phylogeny.Tree.Node
import qualified Bio.Phylogeny.Tree.Referential    as RT
import           Bio.Phylogeny.Tree.Rose
import qualified Bio.Phylogeny.Network.Subsettable as SN
import           Bio.Phylogeny.Graph.Utilities

import qualified Data.HashMap.Strict               as H  (insert, (!), member)
import           Data.IntMap                             (insert, foldWithKey, member, IntMap)
import qualified Data.IntMap                       as IM ((!))
import qualified Data.IntSet                       as IS (map)
import           Data.Key                                (lookup)
import           Data.Monoid
import           Data.Vector                             ((++),(//), length, singleton, elemIndex, (!))
import qualified Data.Vector                       as V  (map)

import           Prelude                       hiding    ((++), length, lookup, replicate)          
import           Safe


--import Debug.Trace

type KeyMap = IntMap Int

-- | Edge Sets are monoids
instance Monoid EdgeSet where
  mempty = EdgeSet mempty mempty
  mappend (EdgeSet in1 out1) (EdgeSet in2 out2) = EdgeSet (in1 <> in2) (out1 <> out2)

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

-- | Simply add in the offending nodes without updating edges or characters
simpleAppend :: Tree -> Tree -> NodeInfo -> Tree
simpleAppend (Tree names seqs chars n e r) (Tree names' seqs' _ n' e' r') hangNode = 
  let resetRoot = n' // [(r', (n' ! r') {isRoot = False, parents = [code hangNode]})]
  in Tree (names <> names') (seqs <> seqs') chars (n ++ resetRoot) (e ++ e') r

grabAt :: Tree -> NodeInfo -> Tree
grabAt inTree1 rootNode = recodeEdges2 $ internalGrab2 inTree1 rootNode mempty
                          --fromTopo $ nodeToTopo inTree1 rootNode
   --where
   -- internalGrab :: Tree -> NodeInfo -> KeyMap -> Tree -> (Int, Int) -> (Tree, KeyMap)
   -- --internalGrab (Tree _ _ _ _ _ _) _ _ _ _ | trace ("internalGrab ") False = undefined
   -- internalGrab inTree@(Tree names seqs chars n e _) curNode keyShift soFar@(Tree names' seqs' _ n' _ _) (curKey, nextKey)
   --   | curKey == 0 = --trace "start not leaf" $
   --     let
   --       breakNode = curNode {parents = [], isRoot = True} 
   --       brokenNodes = singleton breakNode
   --       foldBreak = foldr (\(child, _) acc -> simpleAppend acc child curNode) (newTree {nodes = brokenNodes}) childResults
   --     in (recodeEdges foldBreak insertShift, mempty)
   --   | otherwise = (foldWithMe, insertShift)
   --     where
   --       prevCode = code curNode
   --       newNames = case null names of
   --                   True -> mempty
   --                   False -> insert curKey (names IM.! prevCode) names'
   --       newSeqs = case null seqs || null names of
   --                   True -> mempty
   --                   False -> H.insert (names IM.! prevCode) (seqs H.! (names IM.! prevCode)) seqs'
   --       newNodes = curNode `cons` n'
   --       newTree = Tree newNames newSeqs chars newNodes e 0
   --       nextKey' = nextKey + (P.length $ children curNode)
   --       childResults = --trace ("get child results " P.++ show (children curNode)) 
   --                       zipWith (\c i -> internalGrab inTree (n ! c) keyShift soFar (i, nextKey')) (children curNode) [nextKey..]
   --       foldWithMe = --trace ("fold with me " P.++ show childResults) 
   --                     foldl (\acc (child, _) -> simpleAppend acc child curNode) newTree childResults
   --       foldMaps = foldr (\(_, m) acc -> m <> acc) keyShift childResults
   --       insertShift = insert prevCode curKey foldMaps

   --       recodeEdges :: Tree -> KeyMap -> Tree
   --       --recodeEdges (Tree _ _ _ inN _ _) reMap | trace ("recodeEdges " P.++ show reMap P.++ show (length inN)) False = undefined
   --       recodeEdges rTree@(Tree _ _ _ inN inEdges _) reMap 
   --         | length inN <= 1 = rTree {edges = mempty, nodes = recodeNodes}
   --         | otherwise = rTree {edges = newEdges, nodes = recodeNodes}
   --           where
   --             recodeNodes = V.map changeNode inN
   --             newEdges = foldr (\n0 acc -> (codeOne $ inEdges ! (code n0)) `cons` acc) mempty inN
   --             changeNode n0 = n0 {code = reMap IM.! (code n0), children = recodeConnect (children n0), parents = recodeConnect (parents n0)}
   --             recodeConnect = foldr (\c acc -> if c `member` reMap then reMap IM.! c : acc else acc) []
   --             codeOne :: EdgeSet -> EdgeSet
   --             codeOne inSet = 
   --               let
   --                 newIn = IS.foldr (\i acc -> if i `member` reMap then IS.insert (reMap IM.! i) acc else acc) mempty (inNodes inSet)
   --                 oCode info = reMap IM.! (code $ origin info)
   --                 tCode info = reMap IM.! (code $ terminal info)
   --                 oneInfo info = EdgeInfo (len info) (changeNode (inN ! (oCode info))) (changeNode (inN ! (tCode info)))
   --                 newOut = foldWithKey (\i curInfo acc -> if i `member` reMap then insert (reMap IM.! i) (oneInfo curInfo) acc else acc) mempty (outNodes inSet)
   --               in EdgeSet newIn newOut

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



instance Monoid Graph where
  mempty = Graph []
  mappend (Graph g1) (Graph g2) = Graph (g1 <> g2)

instance SN.SubsettableNetwork Tree NodeInfo where
  appendSubtree = appendAt
  accessSubtree = grabAt




internalGrab2 :: Tree -> NodeInfo -> Tree -> (Tree, KeyMap)
internalGrab2 inTree@(Tree names seqs chars n e _) curNode (Tree names' seqs' _ n' _ _)
  | null n' =
    let 
      breakNode = curNode {parents = [], isRoot = True, code = curPos}
      newNBreak = n' ++ singleton breakNode
      newTBreak = Tree newNames newSeqs chars newNBreak e 0 
    in foldResults newTBreak
  | otherwise = 
    let
      newNodes = n' ++ (singleton $ curNode {code = curPos})
      newTree = Tree newNames newSeqs chars newNodes e 0
    in foldResults newTree

    where
      curPos = length n'
      prevCode = code curNode
      (newNames, newSeqs) = case prevCode `member` names of
                  True -> let myName = names IM.! prevCode
                          in case myName `H.member` seqs of
                            True -> (insert curPos myName names', H.insert myName (seqs H.! myName) seqs')
                            False -> (insert curPos myName names', seqs')
                  False -> (names', seqs')
      kMap' = insert prevCode curPos mempty
      foldOne (curTree, curMap) (accTree, accMap) = (simpleAppend2 curTree accTree curNode, curMap <> accMap)
      foldResults curT = foldr (\c acc -> foldOne (internalGrab2 inTree (n ! c) curT) acc) (curT, kMap') (children curNode)

recodeEdges2 :: (Tree, KeyMap) -> Tree
recodeEdges2 (inTree@(Tree _ _ _ n es _), kMap) = 
  let
    doList = map (\i -> kMap IM.! i)
    recodeConnect = V.map (\n0 -> n0 {children = doList $ children n0, parents = doList $ parents n0}) n
    recodeIn = IS.map (\i -> kMap IM.! i)
    recodeRecord r = r {origin = n ! (kMap IM.! (code $ origin r)), terminal = n ! (kMap IM.! (code $ terminal r))}
    recodeOut = foldWithKey (\i record acc -> insert i (recodeRecord record) acc) mempty
    recodeESet = V.map (\e -> EdgeSet (recodeIn $ inNodes e) (recodeOut $ outNodes e)) es
  in inTree {nodes = recodeConnect, edges = recodeESet}

simpleAppend2 :: Tree -> Tree -> NodeInfo -> Tree
simpleAppend2 (Tree names' seqs' _ n' e' r') (Tree names seqs chars n e r) curNode = 
  let hangIt = n' // [(r', (n' ! r') {isRoot = False, parents = [code curNode]})]
  in Tree (names <> names') (seqs <> seqs') chars (n ++ hangIt) (e ++ e') r


