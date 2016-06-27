-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.PhyloGraph.DAG.Internal
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Types for DAG representation
--
-----------------------------------------------------------------------------
{-# LANGUAGE BangPatterns, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module Bio.PhyloGraph.DAG.Internal where

import           Bio.Character.Dynamic.Coded
import           Bio.Character.Parsed
import           Bio.Metadata.Internal                     (CharacterMetadata)
import           Bio.PhyloGraph.DAG.Class
import           Bio.PhyloGraph.Edge
import           Bio.PhyloGraph.Forest
import qualified Bio.PhyloGraph.Network             as N
import qualified Bio.PhyloGraph.Network.Subsettable as SN
import           Bio.PhyloGraph.Node
--import           Bio.PhyloGraph.Node.Referential          ()
import           Bio.PhyloGraph.Node.Topological          (TopoNode)
import qualified Bio.PhyloGraph.Node.Topological    as TN
import qualified Bio.PhyloGraph.Tree.EdgeAware      as ET
import           Bio.PhyloGraph.Tree.Binary
import qualified Bio.PhyloGraph.Tree.Referential    as RT
import           Bio.PhyloGraph.Tree.Rose
import           Control.Arrow                             ((&&&))
import           Data.Alphabet
import           Data.Bifunctor
import           Data.BitVector                     hiding (foldr,index)
import           Data.Foldable
import           Data.HashMap.Lazy                         (HashMap)
import qualified Data.HashMap.Lazy                  as H   (toList)
import           Data.IntSet                               (IntSet)
import qualified Data.IntSet                        as IS
import           Data.IntMap                               (IntMap)
import qualified Data.IntMap                        as IM
import           Data.Key
import           Data.List                                 (elemIndex,sort)
import           Data.Map                                  (Map)
import qualified Data.Map                           as M
import           Data.Maybe
import           Data.Monoid
import           Data.MonoTraversable
import           Data.Vector                               ((//), Vector)
import qualified Data.Vector                        as V
import qualified File.Format.Newick                 as New
import           Prelude                            hiding (lookup, zip)
import           Test.Tasty.QuickCheck


-- | Alias for Node used in 'DAG'
type NodeInfo = Node

--TODO: This shouldn't be tightly bound to DynamicChar
-- | Alias for Node used in 'TopoDAG'
type Topo = TopoNode DynamicChar

-- | A dag is an element of a forest, stored referentially
data DAG 
   = DAG
   { nodes :: Vector NodeInfo 
   , edges :: Vector EdgeSet
   , root  :: Int
   } deriving (Eq, Show)

-- | A topodag is an alternative forest element stored topologically
data TopoDAG 
   = TopoDAG 
   { structure :: Topo
   }

-- | (✔)
instance Arbitrary DAG where
  arbitrary = binaryTreeToDAG <$> (arbitrary :: Gen (TestingBinaryTree Node))

-- | (✔)
instance Arbitrary (Positive Int, Positive Int, Alphabet String, [BitVector]) where
  arbitrary = do
    alphabet   <- arbitrary :: Gen (Alphabet String)
    taxaCount  <- getPositive <$> (arbitrary :: Gen (Positive Int)) -- this should be limited to <= 10
    charCount  <- getPositive <$> (arbitrary :: Gen (Positive Int))
    let bvGen  =  fromBits    <$> vectorOf (charCount * length alphabet) (arbitrary :: Gen Bool)
    bitVectors <- vectorOf taxaCount bvGen
    pure (Positive taxaCount, Positive charCount, alphabet, bitVectors)
    
-- | A well typed tree used for generating a binary tree. An intermidiate type
--   representation used in 'DAG' generation.
data TestingBinaryTree a 
   = Leaf a
   | Internal (TestingBinaryTree a) (TestingBinaryTree a)
   deriving (Eq,Show)

-- | Type synonym for a counter used when enumerating nodes in a tree.
type Counter = Int

-- | An accumulator type used when traversing the intermidate type tree. 
type Accumulator = (IntMap NodeInfo, IntMap EdgeSet, Counter)

-- | Convert between intermidate type 'TestingBinaryTree' to the resulting type
--   DAG.
binaryTreeToDAG :: TestingBinaryTree Node -> DAG
binaryTreeToDAG binaryRoot = DAG 
                           { nodes = V.generate (length totalNodeMap) (totalNodeMap !)
                           , edges = V.generate (length totalEdgeMap) (totalEdgeMap !)
                           , root  = 0
                           }
   where
       (totalNodeMap, totalEdgeMap, _) = f Nothing binaryRoot (mempty, mempty, 0)
       f :: Maybe Int -> TestingBinaryTree Node -> Accumulator -> Accumulator
       f parentMay (Leaf node) (nodeMap, edgeMap, counter) = 
           ( IM.insert counter (node { nodeIdx = counter
                                     , name = "Taxon: " <> show (nodeIdx node)
                                     , parents = otoList (inNodeSet parentMay)
                                     } ) nodeMap
           , IM.insert counter (EdgeSet (inNodeSet parentMay) mempty) edgeMap
           , counter + 1
           )
           
       f parentMay (Internal left right) (nodeMap, edgeMap, counter) =
           ( IM.insert counter internalNode nodeMap'
           , IM.insert counter (EdgeSet resultingInNodes resultingOutNodes) edgeMap'
           , counter''
           )
         where
           leftAccumulator@(_, _, counter') = f (Just counter) left  (nodeMap, edgeMap, counter + 1)
           (nodeMap', edgeMap', counter'')  = f (Just counter) right leftAccumulator
           resultingInNodes  = inNodeSet parentMay
           resultingOutNodes = IM.insert  counter'   (EdgeInfo 0 internalNode (nodeMap' ! counter'   ) Nothing)
                              (IM.insert (counter+1) (EdgeInfo 0 internalNode (nodeMap' ! (counter+1)) Nothing) mempty)
           internalNode = Node 
                        { nodeIdx             = counter
                        , name                = "HTU: " <> show counter
                        , isRoot              = null $ maybe [] pure parentMay
                        , isLeaf              = False
                        , children            = [counter+1, counter']
                        , parents             = maybe [] pure parentMay
                        , encoded             = mempty
                        , packed              = mempty
                        , preliminaryUngapped = mempty
                        , finalUngapped       = mempty
                        --, temporary   = mempty
                        , preliminaryGapped   = mempty
                        , random              = mempty
                        , union               = mempty
                        , single              = mempty
                        , finalGapped         = mempty
                        , iaHomology          = mempty
                        , impliedAlignment    = mempty
                        , localCost           = 0
                        , totalCost           = 0
                        }
       inNodeSet :: Maybe Int -> IntSet
       inNodeSet (Just parentReference) = IS.insert parentReference mempty
       inNodeSet  Nothing               = mempty

-- | (✔)
instance Arbitrary (TestingBinaryTree Node) where
    arbitrary = do
        leafCount <- (getPositive <$> (arbitrary :: Gen (Positive Int))) `suchThat` (\x -> 2 <= x && x <= 10)
        alphabet  <- arbitrary `suchThat` ((<= 63) . length) :: Gen (Alphabet String)
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

maxTaxa, maxChildren :: Int
-- | Default value for specified child count when generating trees.
maxChildren = 2 -- it's a binary tree.
-- | Default value for leaf node (taxa) count when generating trees.
maxTaxa     = 10

-- | Generate an arbitrary TopoDAG given an alphabet
--arbitraryTopoDAGGA :: Alphabet String -> Gen TopoDAG 
--arbitraryTopoDAGGA inAlph = TopoDAG <$> TN.arbitraryTopoGivenCAL maxChildren inAlph (0, maxTaxa)

-- | Generate an arbitrary DAG given sequences
-- TODO: When you delete this, delete maxChildren, above.
arbitraryDAGGS :: HashMap String ParsedChars -> Vector (CharacterMetadata DynamicChar) -> Gen DAG
arbitraryDAGGS allSeqs metadata = fromTopo . TopoDAG <$> TN.arbitraryTopoGivenCSNA maxChildren (H.toList allSeqs) metadata (0, maxTaxa)

-- TODO: is this really a Monoid?
-- | (✔)
instance Monoid TopoDAG where
    mempty = TopoDAG mempty
    mappend (TopoDAG topo1) (TopoDAG topo2) = TopoDAG $ topo1 { TN.children = topo2 : TN.children topo1 }

-- | (✔)
instance SN.SubsettableNetwork DAG NodeInfo where
    appendSubtree = attachAt
    accessSubtree = grabAt

-- | (✔)
instance ET.EdgedTree DAG NodeInfo EdgeSet where
    edges    n t   = edges t V.! nodeIdx n
    setEdges n t e = t {edges = edges t // [(nodeIdx n, e)]}

-- | (✔)
instance RT.ReferentialTree DAG NodeInfo where
    getNodeIdx node tree = elemIndex (getCode node) . toList $ getCode <$> nodes tree
    getNthNode tree pos  = nodes tree V.! pos

-- | (✔)
instance BinaryTree DAG NodeInfo where
    leftChild  n t = lookup 0 $ (\i -> nodes t V.! i) <$> children n
    rightChild n t = lookup 1 $ (\i -> nodes t V.! i) <$> children n
    verifyBinary   = all ((2 >=) . length . children) . nodes

-- | (✔)
instance RoseTree DAG NodeInfo where
    parent n t = headMay $ fmap (\i -> nodes t V.! i) (parents n)

-- | (✔)
instance N.Network DAG NodeInfo where
    parents node dag    = fmap (\i -> nodes dag V.! i) (parents node)
    root dag            = nodes dag V.! root dag
    children node dag   = fmap (\i -> nodes dag V.! i) (children node)
    update dag newNodes = dag { nodes = nodes dag // updatedNodes }
        where
            updatedNodes = fmap (\n -> (nodeIdx n, n)) newNodes
    numNodes            = length . nodes 
    addNode dag node    = DAG nodes2 edges2 reroot
      where
          addPos  = length $ nodes dag
          newNode = resetPos node dag addPos
          newEdge = makeEdges newNode dag
          edges2  = edges dag <> pure newEdge
          nodes2  = addConnections newNode (nodes dag) <> pure newNode
          reroot  = if   isRoot node && null (nodes dag) 
                    then addPos 
                    else root dag

-- | (✔)
instance StandardDAG DAG NodeInfo EdgeSet where
    getNodes       = nodes
    setNodes inD n = inD {nodes = n}
    getEdges       = edges
    setEdges inD e = inD {edges = e}
    getRoot  inD   = nodes inD V.! root inD

type instance Element DAG = NodeInfo

-- | (✔)
instance MonoFoldable DAG where
    ofoldMap f = foldr (mappend . f) mempty . nodes
    {-# INLINE ofoldMap #-}

    ofoldr f e = foldr f e . nodes
    {-# INLINE ofoldr #-}

    ofoldl' f e = foldl' f e . nodes
    {-# INLINE ofoldl' #-}

    ofoldr1Ex f = foldr1 f . nodes
    {-# INLINE ofoldr1Ex #-}

    ofoldl1Ex' f = foldl1 f . nodes
    {-# INLINE ofoldl1Ex' #-}

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
            shiftNum        = length nodes_1 + 1 -- how much to add to the nodeIdx of each node in DAG_2. Adding one because a new node, node_1new is added to nodes_1
            hCode           = nodeIdx node_11

            -- hang and shift the nodes
            hungNodes       = nodes_2 // [( root_2
                                          , (nodes_2 V.! root_2) { isRoot = False
                                                                 , parents = [hCode]
                                                                 }
                                          )]
    
            connectN        = nodes_1  // [( hCode
                                           , node_11 { children = (shiftNum + root_2) : children node_11
                                                     , isLeaf = False
                                                     }
                                           )]
    
            recodeNew       = fmap recodeFun hungNodes -- this changes the parents of the root, which has already been set in hungNodes
            recodeFun m     = m { nodeIdx  = nodeIdx m + shiftNum
                                , children = fmap (shiftNum +) (children m)
                                , parents  = fmap (shiftNum +) (parents m) 
                                }
            allNodes        = connectN V.++ recodeNew

            -- update edges and add connecting edge
            reMapOut        = IM.foldWithKey (\k val acc -> IM.insert (k + shiftNum) (reMapInfo val) acc) mempty
            reMapInfo eInfo = eInfo { origin   = allNodes V.! (nodeIdx (origin   eInfo) + shiftNum)
                                    , terminal = allNodes V.! (nodeIdx (terminal eInfo) + shiftNum)
                                   }

            shiftEdge edge  = edge { inNodes  = IS.map (shiftNum +) (inNodes edge)
                                   , outNodes = reMapOut (outNodes edge)
                                   }

            newEdges        = fmap shiftEdge edges_2
            allEdges        = edges_1 <> newEdges
            hangUpdate      = (allEdges V.! hCode) { outNodes = fmap (\info -> info { origin = allNodes V.! hCode }) (outNodes (allEdges V.! hCode)) }
            hangAdd         = hangUpdate <> EdgeSet (inNodes $ edges_1 V.! hCode) (IM.insert (root_2 + shiftNum) (EdgeInfo 0 (allNodes V.! hCode) (allNodes V.! (root_2 + shiftNum)) Nothing) (outNodes $ edges_1 V.! hCode))
            hangedUpdate    = (allEdges V.! (root_2 + shiftNum)) <> EdgeSet (IS.singleton hCode) mempty
            connectEdges    = allEdges // [(hCode, hangAdd), (root_2 + shiftNum, hangedUpdate)]


-- | Function to grab from a DAG
grabAt :: DAG -> NodeInfo -> DAG
grabAt inTree hangNode = fromTopo rootedTopo
  where 
    topo = nodeToTopo inTree hangNode
    rootedTopo = TopoDAG topo

-- | Function to go from a topological structure to a DAG
--   Conversion is performed /strictly/.
fromTopo :: TopoDAG -> DAG
fromTopo topoDag = DAG
               { nodes = nodeVector
               , edges = edgeVector
               , root  = rootRef
               }
  where
    rootNode = structure topoDag
    !rootRef = maybe 0 nodeIdx . find isRoot $ toList nodeVector
    
    -- Step 1: We assume that each node in the TopoDAG has a unique 'name' field.
    -- We collect the names and assign each unique name a unique index in the range [0,|T|-1].
    nameEnumeration :: Map String Int
    nameEnumeration = M.fromList $ zip (sort $ getNames rootNode) [0..]
      where
        getNames n = (TN.name n :) . concatMap getNames $ TN.children n

    -- Step 2: We create an referential lookup table using the name based indicies.
    reference :: IntMap (IntSet, Topo, IntSet)
    reference = f Nothing rootNode
      where
        f :: Maybe Int -> Topo -> IntMap (IntSet, Topo, IntSet)
        f parentMay e = IM.insert nodeIndex (parentSet, e, childSet) $ foldMap (f (Just nodeIndex)) children'
          where
            nodeIndex = getIndex e
            getIndex  = (nameEnumeration !) . TN.name 
            childSet  = IS.fromList $ getIndex <$> children'
            children' = TN.children e
            parentSet = maybe mempty IS.singleton parentMay

    --Step 3: We generate the node vector
    nodeVector :: Vector NodeInfo
    !nodeVector = V.generate (length reference) f
      where
        f i = Node { nodeIdx             = i
                   , name                = TN.name topoRef
                   , isRoot              = null parents'
                   , isLeaf              = null children'
                   , children            = children'
                   , parents             = parents'
                   , encoded             = TN.encoded     topoRef
                   , packed              = TN.packed      topoRef
                   , preliminaryUngapped = TN.preliminary topoRef
                   , finalUngapped       = TN.final       topoRef
                   --, temporary           = TN.temporary   topoRef
                   , preliminaryGapped   = TN.aligned     topoRef
                   , random              = TN.random      topoRef
                   , union               = TN.union       topoRef
                   , single              = TN.single      topoRef
                   , finalGapped         = TN.gapped      topoRef
                   , iaHomology          = mempty
                   , impliedAlignment    = mempty
                   , localCost           = TN.localCost   topoRef
                   , totalCost           = TN.totalCost   topoRef
                   }
            where
                (parentRefs, topoRef, childRefs) = reference ! i
                children' = otoList childRefs
                parents'  = otoList parentRefs

    -- Step 4: We generate the edge set vector
    edgeVector :: Vector EdgeSet
    !edgeVector = V.generate (length reference) f
      where
        f i = EdgeSet
            { inNodes  = IS.fromList $ parents nodeData
            , outNodes = IM.fromList $ (id &&& g) <$> children nodeData
            }
          where
            nodeData = nodeVector V.! i
            g j = EdgeInfo
                { len         = localCost nodeData
                , origin      = nodeData
                , terminal    = childData
                , virtualNode = Nothing
                }
              where
                childData = nodeVector V.! j


-- | Function to go from topo to referential
toTopo :: DAG -> TopoDAG
toTopo tree = TopoDAG $ nodeToTopo tree (nodes tree V.! root tree)

-- | convert a given node to topo
nodeToTopo :: DAG -> NodeInfo -> Topo
nodeToTopo inDAG curNode
    | N.nodeIsLeaf curNode inDAG = leaf
    | otherwise = leaf {TN.children = childDAGs}
      where
          childDAGs = fmap (\i -> nodeToTopo inDAG (nodes inDAG V.! i)) (children curNode)
          leaf = TN.TopoNode
                   (isRoot                 curNode)
                   (N.nodeIsLeaf           curNode inDAG)
                   (name                   curNode)
                   mempty
                   (encoded                curNode)
                   (packed                 curNode)
                   (getPreliminaryUngapped curNode) 
                   (getFinal               curNode)
                   --(temporary   curNode)
                   mempty
                   (getPreliminaryGapped   curNode)
                   (random                 curNode)
                   (union                  curNode)
                   (single                 curNode)
                   (getFinalGapped         curNode)
                   (localCost              curNode)
                   (totalCost              curNode)

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
resetPos node prevDAG i =
  let
    leaf  = null $ children node
    nroot = null (parents node) && null (nodes prevDAG)
  in node {nodeIdx = i, isLeaf = leaf, isRoot = nroot}

-- | addConnections is a small function assiting subtree joins
-- it adds edges between a new node and an existing tree
addConnections :: NodeInfo -> Vector NodeInfo -> Vector NodeInfo
addConnections newNode myNodes = 
  let 
    setIn curPos curNodes = curNodes // [(curPos, (curNodes V.! curPos) { children = nodeIdx newNode : 
                                                                              children (curNodes V.! curPos)
                                                                        , isLeaf = False}
                                        )]
    withIn = foldr setIn myNodes (parents newNode)
    setOut curPos curNodes = curNodes // [(curPos, (curNodes V.! curPos) {parents = nodeIdx newNode : parents (curNodes V.! curPos), isRoot = False})]
    withOut = foldr setOut withIn (children newNode)
  in withOut 

-- | Convert from a Newick format to a current DAG
fromNewick :: New.NewickForest -> Forest DAG
--fromNewick forest | trace ("fromNewick on forest " ++ show forest) False = undefined
fromNewick forest = fst $ foldr convertNewickForest ([], 1) forest
  where
    convertNewickForest newickRoot (acc, counter) = (newickToDAG counter newickRoot : acc, counter + 1)
    newickToDAG :: Int            -- ^ The one-indexed location of the DAG in the Forest of DAGs
                -> New.NewickNode -- ^ The root node of the topological representation of the DAG
                -> DAG            -- ^ The resulting DAG
    --newickToDAG tree0 | trace ("newickTopo on tree " ++ show tree0) False = undefined
    newickToDAG forestCount tree0 = fromTopo . TopoDAG . (\x -> x {TN.isRoot = True}) . fst $ internalNewick 1 tree0 
      where
        internalNewick :: Int -> New.NewickNode -> (Topo, Int)
        internalNewick nameCount inTree = (outNode, nextNameCount)
          where
            myName      = fromMaybe defaultName (New.newickLabel inTree)
            defaultName = "HTU " <> show forestCount <> ":" <> show nameCount
            baseCase    = ([], if isNothing $ New.newickLabel inTree then nameCount + 1 else nameCount)
            myCost      = fromMaybe 0 (New.branchLength inTree)
            --recurse = V.toList $ V.imap (\i n -> internalNewick n (nameCount + i + 1)) (V.fromList $ New.descendants inTree) 
            (recurse, nextNameCount) = foldr (\n (acc,i) -> first (: acc) $ internalNewick i n) baseCase (New.descendants inTree) 
            outNode     = TN.TopoNode False (null $ New.descendants inTree) myName recurse mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty myCost 0
