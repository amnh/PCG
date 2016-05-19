-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.PhyloGraph.DAG
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Instances and other stuff for a DAG
--
-----------------------------------------------------------------------------
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Bio.PhyloGraph.DAG
  ( StandardDAG(..)
  , NodeInfo
  , Topo
  , DAG(..)
  , TopoDAG(..)
  , fromNewick
  , fromTopo
  , toTopo
  , arbitraryDAGGS
  ) where

import           Bio.Character.Parsed
import           Bio.Metadata.Internal              hiding (name)
import           Bio.Character.Dynamic.Coded
import           Bio.PhyloGraph.DAG.Internal
import           Bio.PhyloGraph.DAG.Class
import           Bio.PhyloGraph.Edge
import           Bio.PhyloGraph.Forest
import qualified Bio.PhyloGraph.Network             as N
import qualified Bio.PhyloGraph.Network.Subsettable as SN
import           Bio.PhyloGraph.Node
import qualified Bio.PhyloGraph.Node.Topological    as TN
import qualified Bio.PhyloGraph.Tree.EdgeAware      as ET
import           Bio.PhyloGraph.Tree.Binary
import qualified Bio.PhyloGraph.Tree.Referential    as RT
import           Bio.PhyloGraph.Tree.Rose

import           Data.Alphabet
import           Data.Bifunctor
import           Data.Bits
import           Data.BitVector                     hiding (foldr)
import           Data.HashMap.Lazy                         (HashMap)
import qualified Data.HashMap.Lazy                  as H   (toList)
import           Data.IntSet                               (IntSet)
import qualified Data.IntSet                        as IS
import           Data.IntMap                               (IntMap)
import qualified Data.IntMap                        as IM
import           Data.Key                                  ((!),lookup)
import           Data.List                                 (delete)
import           Data.Maybe
import           Data.Monoid
import           Data.Vector                               ((//), Vector, elemIndex)
import qualified Data.Vector                        as V
import qualified File.Format.Newick                 as New
import           Prelude                            hiding (lookup)
import           Safe
import           Test.Tasty.QuickCheck

instance StandardDAG DAG NodeInfo EdgeSet where
    getNodes       = nodes
    setNodes inD n = inD {nodes = n}
    getEdges       = edges
    setEdges inD e = inD {edges = e}
    getRoot  inD   = nodes inD V.! root inD

instance Arbitrary DAG where
  arbitrary = binaryTreeToDAG <$> (arbitrary :: Gen (TestingBinaryTree Node))


instance Arbitrary (Positive Int, Positive Int, Alphabet String, [BitVector]) where
  arbitrary = do
    alphabet   <- (arbitrary :: Gen (Alphabet String))
    numTaxa    <- (arbitrary :: Gen (Positive Int)) -- this should be limited to <= 10
    numChars   <- (arbitrary :: Gen (Positive Int))
    let bvGen  =  fromBits <$> vectorOf ((getPositive numChars) * (length alphabet)) (arbitrary :: Gen Bool)
    bitVectors <- vectorOf (getPositive numTaxa) bvGen
    pure (numTaxa, numChars, alphabet, bitVectors)
    

data TestingBinaryTree a 
   = Leaf a
   | Internal (TestingBinaryTree a) (TestingBinaryTree a)
   deriving (Eq,Show)

type Counter = Int
type Accumulator = (IM.IntMap NodeInfo, IM.IntMap EdgeSet, Counter)

binaryTreeToDAG :: TestingBinaryTree Node -> DAG
binaryTreeToDAG root = DAG 
                     { nodes = V.generate (length totalNodeMap) (\i -> totalNodeMap ! i)
                     , edges = V.generate (length totalEdgeMap) (\i -> totalEdgeMap ! i)
                     , root  = 0
                     }
   where
       (totalNodeMap, totalEdgeMap, _) = f Nothing root (mempty, mempty, 0)
       f :: Maybe Int -> TestingBinaryTree Node -> Accumulator -> Accumulator
       f parentMay (Leaf node) (nodeMap, edgeMap, counter) = 
           ( IM.insert counter node nodeMap
           , IM.insert counter (EdgeSet (inNodeSet parentMay) mempty) edgeMap
           , counter + 1
           )
           
       f parentMay (Internal left right) (nodeMap, edgeMap, counter) =
           ( IM.insert counter internalNode nodeMap
           , IM.insert counter (EdgeSet (inNodeSet parentMay) 
                                        ( IM.insert counter' (EdgeInfo 0 internalNode (nodeMap' ! counter') Nothing) 
                                            (IM.insert (counter+1) (EdgeInfo 0 internalNode (nodeMap' ! (counter+1)) Nothing) mempty)
                                        )
                               ) edgeMap'
           , counter''
           )
         where
           leftAccumulator@(_, _, counter') = f (Just $ counter) left  (nodeMap, edgeMap, counter + 1)
           (nodeMap', edgeMap', counter'')  = f (Just $ counter) right leftAccumulator
           internalNode = Node 
                        { code        = counter
                        , name        = "HTU: " <> show counter
                        , isRoot      = null $ maybe [] pure parentMay
                        , isLeaf      = False
                        , children    = [counter+1, counter']
                        , parents     = maybe [] pure parentMay
                        , encoded     = mempty
                        , packed      = mempty
                        , preliminary = mempty
                        , final       = mempty
                        , temporary   = mempty
                        , aligned     = mempty
                        , random      = mempty
                        , union       = mempty
                        , single      = mempty
                        , gapped      = mempty
                        , iaHomology  = mempty
                        , localCost   = 0
                        , totalCost   = 0
                        }
       inNodeSet :: Maybe Int -> IntSet
       inNodeSet (Just parent) = IS.insert parent mempty
       inNodeSet  Nothing      =  mempty

{-
instance (Arbitrary a, Eq a) => Arbitrary (TestingBinaryTree a) where
    arbitrary = do
        leafCount <- (getPositive <$> (arbitrary :: Gen (Positive Int))) `suchThat` (<=10)
        leaves    <- (fmap Leaf) <$> vectorOf leafCount arbitrary
        f leaves
      where
        f :: Eq a => [TestingBinaryTree a] -> Gen (TestingBinaryTree a)
        f [x] = pure x
        f subTrees  = do
            left:right:remaining <- shuffle subTrees 
            f (Internal left right : remaining)
-}
instance Arbitrary (TestingBinaryTree Node) where
    arbitrary = do
        leafCount <- (getPositive <$> (arbitrary :: Gen (Positive Int))) `suchThat` (\x -> 2 <= x && x <=10)
        alphabet  <- arbitrary :: Gen (Alphabet String)
        leaves    <- generateLeavesDO alphabet leafCount
        generateBinaryTree leaves
      where
        generateBinaryTree :: [a] -> Gen (TestingBinaryTree a)
        generateBinaryTree = f . fmap Leaf
          where
            f :: [TestingBinaryTree a] -> Gen (TestingBinaryTree a)
            f [x] = pure x
            f subTrees  = do
                left:right:remaining <- shuffle subTrees 
                f (Internal left right : remaining)

-- TODO: For DAGS, we'll need a testing flag to set the maximum number of taxa and number of children
-- for now we default to 10 taxa
maxTaxa :: Int
maxTaxa = 10

maxChildren :: Int
maxChildren = 2 -- it's a binary tree.

-- | Generate an arbitrary TopoDAG given an alphabet
arbitraryTopoDAGGA :: Alphabet String -> Gen TopoDAG 
arbitraryTopoDAGGA inAlph = TopoDAG <$> TN.arbitraryTopoGivenCAL maxChildren inAlph (0, maxTaxa)

-- | Generate an arbitrary DAG given sequences
arbitraryDAGGS :: HashMap String ParsedChars -> Vector (CharacterMetadata DynamicChar) -> Gen DAG
arbitraryDAGGS allSeqs metadata = fromTopo <$> TopoDAG <$> TN.arbitraryTopoGivenCSNA maxChildren (H.toList allSeqs) metadata (0, maxTaxa)

-- TODO: I'm pretty sure this is also an improper use of monoid, as graphs can't be "appended". Rather, the
-- can be joined, in various ways.
instance Monoid DAG where
    mempty = DAG mempty mempty 0
    -- append is adding dag2 to dag1 just below the root---which isn't actually appending, of course.
    mappend dag1 dag2 = attachAt dag1 dag2 (N.root dag1)

instance Monoid TopoDAG where
    mempty = TopoDAG mempty
    mappend (TopoDAG topo1) (TopoDAG topo2) = TopoDAG $ topo1 { TN.children = topo2 : TN.children topo1 }

instance SN.SubsettableNetwork DAG NodeInfo where
  appendSubtree = attachAt
  accessSubtree = grabAt

-- | This tree knows its edges
instance ET.EdgedTree DAG NodeInfo EdgeSet where
  edges    n t   = edges t V.! code n
  setEdges n t e = t {edges = edges t // [(code n, e)]}

-- | This particular tree is referential
instance RT.ReferentialTree DAG NodeInfo where
  code node tree = elemIndex node (nodes tree)
  getNthNode tree pos = nodes tree V.! pos

instance BinaryTree DAG NodeInfo where
    leftChild  n t = lookup 0 $ (\i -> nodes t V.! i) <$> children n
    rightChild n t = lookup 1 $ (\i -> nodes t V.! i) <$> children n
    verifyBinary   = all ((2 >=) . length . children) . nodes

instance RoseTree DAG NodeInfo where
    parent n t = headMay $ fmap (\i -> nodes t V.! i) (parents n)

instance N.Network DAG NodeInfo where
    parents n t   = fmap (\i -> nodes t V.! i) (parents n)
    root t        = nodes t V.! root t
    children n t  = fmap (\i -> nodes t V.! i) (children n)
    nodeIsLeaf n _    = isLeaf n
    nodeIsRoot n _    = isRoot n
    update t new  = t {nodes = nodes t // (fmap (\n -> (code n, n)) new)}
    numNodes      = length . nodes 
    addNode t n   = DAG nodes2 edges2 reroot
      where
          addPos = length $ nodes t
          newNode = resetPos n t addPos
          newEdge = makeEdges newNode t
          edges2 = edges t V.++ pure newEdge
          nodes2 = addConnections newNode (nodes t) V.++ pure newNode
          reroot = if isRoot n && null (nodes t) then addPos else root t

-- | attachAt is used to build arbitrary trees. It takes two DAGs, d_1, d_2 and a node, node_11 and node_12, that must exist in the first DAG, 
-- and that must, furthermore, be connected by an edge. 
-- Previously, it combined d_1 and d_2 by creating a new (directed) edge between the root of d_2, 
-- i.e. root_2, and node_11.
-- I'm not sure that this was a mathematically valid operation: It's possible that root_2 should actually
-- be replaced by node_11.
-- More important, at this point---5/16/16---DAGs are only used to represent binary trees, thus I'm amending 
-- this operation to only allow binary trees. The amended function will add anode, node_1new, between 
-- node_11 and node_12, and root_1 will be attached to node_1new by a new edge, e_new. node_1new will 
-- thus be the parent of both root_2 and node_12, and a child of node_11.
-- TODO: generalize this to DAGs. Move this (binary tree) operation off to Tree.Binary.Internal.
attachAt :: DAG -> DAG -> NodeInfo -> DAG
attachAt d1@(DAG nodes_1 edges_1 root_1) d2@(DAG nodes_2 edges_2 root_2) node_11
    | null nodes_1 = d2
    | null nodes_2 = d1
    | root_1 > length nodes_1 - 1 || root_2 > length nodes_2 - 1 = error "Root out of bounds when trying to append trees"
    | otherwise = DAG allNodes connectEdges root_1
        where
            shift           = length nodes_1 + 1 -- how much to add to the code of each node in DAG_2. Adding one because a new node, node_1new is added to nodes_1
            hCode           = code node_11

            -- hang and shift the nodes
            hungNodes       = nodes_2 // [( root_2
                                          , (nodes_2 V.! root_2) { isRoot = False
                                                               , parents = [hCode]
                                                               }
                                          )]
    
            connectN        = nodes_1  // [( hCode
                                           , node_11 { children = (shift + root_2) : children node_11
                                                     , isLeaf = False
                                                     }
                                           )]
    
            recodeNew       = fmap recodeFun hungNodes -- this changes the parents of the root, which has already been set in hungNodes
            recodeFun m     = m { code     = code m + shift
                                , children = fmap (shift +) (children m)
                                , parents  = fmap (shift +) (parents m) 
                                }
            allNodes        = connectN V.++ recodeNew

            -- update edges and add connecting edge
            reMapOut        = IM.foldWithKey (\k val acc -> IM.insert (k + shift) (reMapInfo val) acc) mempty
            reMapInfo eInfo = eInfo { origin   = allNodes V.! (code (origin eInfo) + shift)
                                    , terminal = allNodes V.! (code (terminal eInfo) + shift)
                                    }

            shiftEdge edge  = edge { inNodes  = IS.map (shift +) (inNodes edge)
                                   , outNodes = reMapOut (outNodes edge)
                                   }

            newEdges        = fmap shiftEdge edges_2
            allEdges        = edges_1 V.++ newEdges
            hangUpdate      = (allEdges V.! hCode) { outNodes = fmap (\info -> info { origin = allNodes V.! hCode }) (outNodes (allEdges V.! hCode)) }
            hangAdd         = hangUpdate <> EdgeSet (inNodes $ edges_1 V.! hCode) (IM.insert (root_2 + shift) (EdgeInfo 0 (allNodes V.! hCode) (allNodes V.! (root_2 + shift)) Nothing) (outNodes $ edges_1 V.! hCode))
            hangedUpdate    = (allEdges V.! (root_2 + shift)) <> EdgeSet (IS.singleton hCode) mempty
            connectEdges    = allEdges // [(hCode, hangAdd), (root_2 + shift, hangedUpdate)]


-- | Function to grab from a DAG
grabAt :: DAG -> NodeInfo -> DAG
grabAt inTree hangNode = fromTopo rootedTopo
  where 
    topo = nodeToTopo inTree hangNode
    rootedTopo = TopoDAG topo

-- | Function to go from referential to topo
-- TODO define all these conversion functions
fromTopo :: TopoDAG -> DAG
fromTopo (TopoDAG inTopo) = internalFromTopo inTopo
    where
        internalFromTopo :: Topo -> DAG
        internalFromTopo topo
            | TN.isLeaf topo = singletonDAG topo 
            | otherwise = foldr (\n acc -> acc <> internalFromTopo n) (singletonDAG topo) (TN.children topo)
                where
                    -- | Function to convert a node to a tree for folding
                    singletonDAG :: Topo -> DAG
                    singletonDAG topoNode = 
                      let myNode = Node 0 (TN.name topoNode) (TN.isRoot topoNode) (TN.isLeaf topoNode) [] [] (TN.encoded topoNode) (TN.packed topoNode) (TN.preliminary topoNode) 
                                              (TN.final topoNode) (TN.temporary topoNode) (TN.aligned topoNode) mempty mempty mempty mempty mempty (TN.localCost topoNode) (TN.totalCost topoNode)
                      in DAG (pure myNode) (pure mempty) 0 

-- | Function to go from topo to referential
toTopo :: DAG -> TopoDAG
toTopo tree = TopoDAG $ nodeToTopo tree (nodes tree V.! root tree)

-- | convert a given node to topo
nodeToTopo :: DAG -> NodeInfo -> Topo
nodeToTopo inDAG curNode
    | isLeaf curNode = leaf
    | otherwise = leaf {TN.children = childDAGs}
      where
          childDAGs = fmap (\i -> nodeToTopo inDAG (nodes inDAG V.! i)) (children curNode)
          leaf = TN.TopoNode (isRoot curNode) (isLeaf curNode) (name curNode) mempty (encoded curNode) (packed curNode) (preliminary curNode) 
                  (final curNode) (temporary curNode) (aligned curNode) (random curNode) (union curNode) (single curNode) (gapped curNode) (localCost curNode) (totalCost curNode)

-- | makeEdges is a small function assisting attachAt
-- it creates the edge set for a given node in the given tree
makeEdges :: NodeInfo -> DAG -> EdgeSet
makeEdges node inDAG = EdgeSet (IS.fromList $ parents node) out
  where
    out  = foldr (\i acc -> IM.insert i (info $ nodes inDAG V.! i) acc) mempty (children node)
    info input = EdgeInfo 0 node input Nothing

-- | resetPos is a small function assisting the joining of two subtrees
-- simple function to reset positioning of a node
resetPos :: NodeInfo -> DAG -> Int -> NodeInfo
resetPos node prevDAG index =
  let
    leaf = null $ children node
    nroot = null (parents node) && null (nodes prevDAG)
  in node {code = index, isLeaf = leaf, isRoot = nroot}

-- | addConnections is a small function assiting subtree joins
-- it adds edges between a new node and an existing tree
addConnections :: NodeInfo -> Vector NodeInfo -> Vector NodeInfo
addConnections newNode myNodes = 
  let 
    setIn curPos curNodes = curNodes // [(curPos, (curNodes V.! curPos) {children = code newNode : children (curNodes V.! curPos), isLeaf = False})]
    withIn = foldr setIn myNodes (parents newNode)
    setOut curPos curNodes = curNodes // [(curPos, (curNodes V.! curPos) {parents = code newNode : parents (curNodes V.! curPos), isRoot = False})]
    withOut = foldr setOut withIn (children newNode)
  in withOut 

-- | Convert from a Newick format to a current DAG
fromNewick :: New.NewickForest -> Forest DAG
--fromNewick forest | trace ("fromNewick on forest " ++ show forest) False = undefined
fromNewick forest = fst $ foldr (\d (acc, counter) -> first (: acc) $ oneNewick counter d) ([], 0) forest
  where
    oneNewick :: Int -> New.NewickNode -> (DAG, Int)
    --oneNewick new | trace ("oneNewick on tree " ++ show new) False = undefined
    oneNewick count new = first fromTopo $ newickTopo count new
    
    newickTopo :: Int -> New.NewickNode -> (TopoDAG, Int)
    --newickTopo tree0 | trace ("newickTopo on tree " ++ show tree0) False = undefined
    newickTopo count tree0 = first (TopoDAG . (\x -> x {TN.isRoot = True})) $ internalNewick count tree0 
      where
        internalNewick :: Int -> New.NewickNode -> (Topo, Int)
        internalNewick nameCount inTree = (outNode, nextNameCount)
          where
            myName = fromMaybe ("HTU " ++ show nameCount) (New.newickLabel inTree)
            baseCase = ([], if isNothing $ New.newickLabel inTree then nameCount + 1 else nameCount)
            myCost = fromMaybe 0 (New.branchLength inTree)
            --recurse = V.toList $ V.imap (\i n -> internalNewick n (nameCount + i + 1)) (V.fromList $ New.descendants inTree) 
            (recurse,nextNameCount) = foldr (\n (acc,i) -> first (: acc) $ internalNewick i n) baseCase (New.descendants inTree) 
            outNode = TN.TopoNode False (null $ New.descendants inTree) myName recurse mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty myCost 0
