-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Graph.ReferenceDAG.Utility
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Bio.Graph.ReferenceDAG.Utility where

import           Bio.Graph.ReferenceDAG.Internal
import           Bio.Graph.ReferenceDAG.Network
import           Control.Lens
import           Control.Monad.Loops
import           Data.Foldable
import           Data.IntMap                     (IntMap)
import qualified Data.IntMap                     as IM (mapKeys)
import           Data.IntSet                     (IntSet)
import qualified Data.IntSet                     as IS (map, singleton)
import           Data.Key                        ((!))
import qualified Data.List.NonEmpty              as NE
import           Data.Set                        (Set, (\\))
import qualified Data.Set                        as Set
import           Data.Vector                     (Vector)
import           Test.QuickCheck


data NetworkInformation
  = NetworkInformation
  { _candidateNetworkEdges :: Set ((Int, Int), (Int, Int))
  , _networkAdjacentEdges  :: Set (Int, Int)
  , _edgeSet               :: Set (Int, Int)
  , _rootNode              :: Int
  }

emptyNetworkInfo :: NetworkInformation
emptyNetworkInfo = NetworkInformation mempty mempty mempty 0



instance Show NetworkInformation where
  show = const "[Network Information]"

-- |
-- This function takes valid Networks n0 and n1 and forms the network:
--
-- >                    r
-- >                ┌───┴───┐
-- >                │       │
-- >                │       │
-- >               n0       n1
-- > also returning the network information for n0 and n1 (with the correct internal indices)
makeBranchedNetworkWithInfo
  ::  (Monoid d, Monoid n)
  =>   ReferenceDAG d () n   -- ^ n0
  ->   ReferenceDAG d () n   -- ^ n1
  -> ( ReferenceDAG d () n
     , NetworkInformation
     , NetworkInformation
     )
makeBranchedNetworkWithInfo n0 n1 = (ReferenceDAG{..}, networkInfoN0, networkInfoN1')
  where
    newNode    = IndexData {..}
      where
        nodeDecoration = nodeDec0 <> nodeDec1
        parentRefs     = []
        childRefs      = [(rootIndex0, ()), (rootIndex1', ())]

 -- n1' with the corrected internal indices with respect to the new network
    n1' = incrementNodeIndices (length references0) n1

    references = references0WithRoot <> references1'WithRoot <> pure newNode
    rootRefs   = pure rootNodeIndex
    graphData  = (n0 ^. _graphData) <> (n1 ^. _graphData)

    nodeDec0 = (references0 ! rootIndex0)  ^. _nodeDecoration
    nodeDec1 = (references1 ! rootIndex1)  ^. _nodeDecoration

    rootIndex0     = NE.head (n0  ^. _rootRefs)
    rootIndex1     = NE.head (n1  ^. _rootRefs)
    rootIndex1'    = NE.head (n1' ^. _rootRefs)

    references0  = n0  ^. _references
    references1  = n1  ^. _references
    references1' = n1' ^. _references

 -- Add the new root node as a parent to the old root nodes
    references0WithRoot
      = references0  & ix rootIndex0 . _parentRefs .~ IS.singleton rootNodeIndex
    references1'WithRoot
      = references1' & ix rootIndex1 . _parentRefs .~ IS.singleton rootNodeIndex

  -- since vectors are zero-indexed this is the entry after the n0 and n1 node data.
    rootNodeIndex = length references0 + length references1

  -- Gets the network information about n0 and n1'
    networkInfoN0  = getNetworkInformation n0
    networkInfoN1  = getNetworkInformation n1
    networkInfoN1' = incrementNetworkInformation (length references0) networkInfoN1


makeBranchedNetwork
  :: (Monoid d, Monoid n)
  => ReferenceDAG d () n   -- ^ n0
  -> ReferenceDAG d () n   -- ^ n1
  -> ReferenceDAG d () n
makeBranchedNetwork n0 n1 = (^. _1) $ makeBranchedNetworkWithInfo n0 n1

-- |
-- This function takes valid networks n0, n1 and n2 and forms the network:
--
-- >                    r
-- >                ┌───┴───┐
-- >                │       │
-- >                │       │
-- >               n2       x
-- >                    ┌───┴───┐
-- >                    │       │
-- >                    │       │
-- >                   n0       n1
-- This also returns network information about n0, n1, n2 and the index
-- of x in the above diagram.
makeDoublyBranchedNetwork
  ::  (Monoid d, Monoid n)
  => ReferenceDAG d () n   -- ^ n0
  -> ReferenceDAG d () n   -- ^ n1
  -> ReferenceDAG d () n   -- ^ n2
  -> ReferenceDAG d () n
makeDoublyBranchedNetwork n0 n1 n2 = makeBranchedNetwork n2 n0n1Branched
  where
    n0n1Branched = makeBranchedNetwork n0 n1

makeDoublyBranchedNetworkWithInfo
  ::  forall d n . (Monoid d, Monoid n)
  =>   ReferenceDAG d () n   -- ^ n0
  ->   ReferenceDAG d () n   -- ^ n1
  ->   ReferenceDAG d () n   -- ^ n2
  -> ( ReferenceDAG d () n
     , NetworkInformation
     , NetworkInformation
     , NetworkInformation
     , Int
     )
makeDoublyBranchedNetworkWithInfo n0 n1 n2 = (network, n0NetInfo, n1NetInfo, n2NetInfo, xIndex)
  where
 -- the index of the root of the non1 branch
    xIndex = length n0 + length n1 + length n2
    (n0n1Branched, n0NetInfo, n1NetInfo) = makeBranchedNetworkWithInfo n0 n1
    (network     , n2NetInfo, _        ) = makeBranchedNetworkWithInfo n2 n0n1Branched

-- |
-- This function takes valid networks n0, n1, n2 and n3 and forms the network:
--
-- >                    r
-- >                ┌───┴───┐
-- >                │       │
-- >                │       │
-- >                n3      a
-- >                   ┌────┴────┐
-- >                   │         │
-- >                   │         │
-- >                   b         c
-- >               ┌───┴───┐ ┌───┴───┐
-- >               │       │ │       │
-- >               │       │ │       │
-- >               │       └┬┘       │
-- >              n0        d       n2
-- >                        │
-- >                        n1
--
-- This also returns the network information about n0, n1 and n2.
makeBranchedNetworkWithNetworkEventWithInfo
  ::  forall d n . (Monoid d, Monoid n)
  => ReferenceDAG d () n    -- ^ n0
  -> ReferenceDAG d () n    -- ^ n1
  -> ReferenceDAG d () n    -- ^ n2
  -> ReferenceDAG d () n    -- ^ n3
  -> ( ReferenceDAG d () n
     , NetworkInformation
     , NetworkInformation
     , NetworkInformation
     )
makeBranchedNetworkWithNetworkEventWithInfo  n0 n1 n2 n3
    = (network, n0NetworkInfo, n1NetworkInfo, n2NetworkInfo)
  where
    (network, _, _ ) = makeBranchedNetworkWithInfo n3 internalNetwork

    internalNetwork :: ReferenceDAG d () n
    internalNetwork = ReferenceDAG{..}

    references :: Vector (IndexData () n)
    references = fold vs
      where
        vs :: [Vector (IndexData () n)]
        vs = [ referencesN0WithRoot
             , referencesN1WithRoot
             , referencesN2WithRoot
             , pure dIndexData
             , pure bIndexData
             , pure cIndexData
             , pure aIndexData
             ]

    rootRefs = pure aIndex
    graphData = (n1 ^. _graphData) <> (n2 ^. _graphData) <> (n3 ^. _graphData)

    aIndexData :: IndexData () n
    aIndexData = IndexData {..}
      where
        nodeDecoration = (bIndexData ^. _nodeDecoration) <> (cIndexData ^. _nodeDecoration)
        parentRefs     = []
        childRefs      = [(bIndex, ()), (cIndex, ())]

    bIndexData :: IndexData () n
    bIndexData = IndexData{..}
      where
        nodeDecoration = rootNodeDec0 <> rootNodeDec1
        parentRefs :: IntSet
        parentRefs     = [aIndex]
        childRefs :: IntMap ()
        childRefs      = [(n0RootIndex, ()), (dIndex, ())]

    cIndexData :: IndexData () n
    cIndexData = IndexData{..}
      where
        nodeDecoration = rootNodeDec1 <> rootNodeDec2
        parentRefs :: IntSet
        parentRefs     = [aIndex]
        childRefs :: IntMap ()
        childRefs      = [(dIndex, ()), (n2RootIndex', ())]

    dIndexData :: IndexData () n
    dIndexData = IndexData{..}
      where
        nodeDecoration = rootNodeDec1
        parentRefs :: IntSet
        parentRefs     = [bIndex, cIndex]
        childRefs :: IntMap ()
        childRefs      = [(n1RootIndex', ())]

    aIndex, bIndex, cIndex, n0RootIndex, n1RootIndex', n2RootIndex' :: Int
    aIndex = cIndex + 1
    bIndex = dIndex + 1
    dIndex = n0n1n2Nodes
    cIndex = bIndex + 1

    n0RootIndex  = NE.head (n0  ^. _rootRefs)
    n1RootIndex  = NE.head (n1  ^. _rootRefs)
    n1RootIndex' = NE.head (n1' ^. _rootRefs)
    n2RootIndex' = NE.head (n2' ^. _rootRefs)
    n2RootIndex  = NE.head (n2  ^. _rootRefs)

    rootNodeDec0 = (referencesN0  ! n0RootIndex) ^. _nodeDecoration
    rootNodeDec1 = (referencesN1' ! n1RootIndex) ^. _nodeDecoration
    rootNodeDec2 = (referencesN2' ! n2RootIndex) ^. _nodeDecoration

    n1', n2' :: ReferenceDAG d () n
    n1' = incrementNodeIndices n0Nodes   n1
    n2' = incrementNodeIndices n0n1Nodes n2

    n0NetworkInfo, n1NetworkInfo, n2NetworkInfo :: NetworkInformation
    n0NetworkInfo = incrementNetworkInformation (length n3) $ getNetworkInformation n0
    n1NetworkInfo = incrementNetworkInformation (n0Nodes + length n3) $ getNetworkInformation n1
    n2NetworkInfo = incrementNetworkInformation (n0n1Nodes + length n3) $ getNetworkInformation n2

    referencesN0, referencesN1', referencesN2' :: Vector (IndexData () n)
    referencesN0   = n0  ^. _references
    referencesN1'  = n1' ^. _references
    referencesN2'  = n2' ^. _references

    referencesN0WithRoot
      = referencesN0
      & ix n0RootIndex . _parentRefs .~ IS.singleton bIndex
    referencesN1WithRoot
      = referencesN1'
      & ix n1RootIndex . _parentRefs .~ IS.singleton dIndex
    referencesN2WithRoot
      = referencesN2'
      & ix n2RootIndex . _parentRefs .~ IS.singleton cIndex

    n0Nodes, n0n1Nodes, n0n1n2Nodes :: Int
    n0Nodes        = length referencesN0
    n0n1Nodes      = n0Nodes   + length referencesN1'
    n0n1n2Nodes    = n0n1Nodes + length referencesN2'


makeBranchedNetworkWithNetworkEvent
  ::  forall d n . (Monoid d, Monoid n)
  => ReferenceDAG d () n   -- ^ n0
  -> ReferenceDAG d () n   -- ^ n1
  -> ReferenceDAG d () n   -- ^ n2
  -> ReferenceDAG d () n   -- ^ n3
  -> ReferenceDAG d () n
makeBranchedNetworkWithNetworkEvent n0 n1 n2 n3
  = (^. _1) $ makeBranchedNetworkWithNetworkEventWithInfo n0 n1 n2 n3


-- |
-- Make a binary tree of a given depth.
makeBinaryTree  :: forall d n . (Monoid d, Monoid n)
  =>  Int                    -- ^ depth of binary tree
  -> ReferenceDAG d () n
makeBinaryTree depth = go depth (singletonRefDAG mempty)
  where
    go :: Int -> ReferenceDAG d () n -> ReferenceDAG d () n
    go 0 subtree = subtree
    go n subtree = go (n - 1) (makeBranchedNetwork subtree subtree)

-- |
-- Make a pectinate tree of a given depth.
makePectinateTree  :: forall d n . (Monoid d, Monoid n)
  =>  Int                    -- ^ depth of pectinate tree
  -> ReferenceDAG d () n
makePectinateTree depth = go depth (singletonRefDAG mempty)
  where
    go :: Int -> ReferenceDAG d () n -> ReferenceDAG d () n
    go 0 subtree = subtree
    go n subtree = go (n - 1) (makeBranchedNetwork (singletonRefDAG mempty) subtree)
   

-- |
-- Generate a random binary tree of some depth.
generateBinaryTree ::  (Monoid d, Monoid n)
  => Gen (ReferenceDAG d () n)
generateBinaryTree = do
  depth <- choose (1, 8)
  pure $ makeBinaryTree depth


-- |
-- Generate a tree with potential network edges.
generateNetwork :: forall d n . (Monoid d, Monoid n) => Gen (ReferenceDAG d () n)
generateNetwork  = do
  binTree  <- generateBinaryTree @d @n
  (refDAG, _) <- iterateUntilM stoppingCondition addNetworkEdge (binTree, False)
  pure refDAG
    where
      stoppingCondition :: (ReferenceDAG d () n, Bool) -> Bool
      stoppingCondition = \case
        (_,    False) -> True
        (rdag, True ) -> null $ candidateNetworkEdges rdag

      addNetworkEdge :: (ReferenceDAG d () n, Bool) -> Gen (ReferenceDAG d () n, Bool)
      addNetworkEdge (dag, _) = do
          (prob :: Int) <- choose (1, 100)
          if prob > 90 then
            pure (dag, False)
          else
            do
              let networkEdges = toList . candidateNetworkEdges $ dag
              ind <- choose (0, length networkEdges - 1)
              let (sourceEdge, targetEdge) = networkEdges ! ind
              let newRefDAG = connectEdge dag combine (<>) sourceEdge targetEdge
              pure (newRefDAG, False)
        where
          combine n1 n2 n3 = n1 <> n2 <> n3


-- |
-- Generate a random network in the same shape as 'makeBranchedNetworkWithInfo'.
generateBranchedNetwork
  :: forall d n . (Monoid d, Monoid n)
  => Gen
      ( ReferenceDAG d () n
      , NetworkInformation
      , NetworkInformation
      )
generateBranchedNetwork =
  do
    n0 <- generateNetwork
    n1 <- generateNetwork
    pure $ makeBranchedNetworkWithInfo n0 n1

-- |
-- Generate a random network in the same shape as 'makeBranchedNetworkWithNetworkEventWithInfo'
-- returning also the candidate network information of n0, n1 and n2 and the index of the nodes
-- called a, b and c (in the above diagram).
generateBranchedNetworkWithNetworkEvent
  :: (Monoid d, Monoid n)
  => Gen
       ( ReferenceDAG d () n
       , NetworkInformation
       , NetworkInformation
       , NetworkInformation
       , Int
       , Int
       , Int
       )
generateBranchedNetworkWithNetworkEvent =
  do
    n0 <- generateNetwork
    n1 <- generateNetwork
    n2 <- generateNetwork
    n3 <- generateNetwork
    let (dag, n0NetInfo, n1NetInfo, n2NetInfo)
          = makeBranchedNetworkWithNetworkEventWithInfo n0 n1 n2 n3

    let lengthNets = length n0 + length n1 + length n2 + length n3
    let aIndex = lengthNets + 3
    let bIndex = lengthNets + 1
    let cIndex = lengthNets + 2
    pure ( dag
         , n0NetInfo
         , n1NetInfo
         , n2NetInfo
         , aIndex
         , bIndex
         , cIndex
         )

-- |
-- Generate a random network in the same shape as 'makeDoublyBranchedNetwork'
-- also returning 'NetworkInformation' about the sub-networks and the internal
-- index of the x node.
generateDoublyBranchedNetwork
  :: (Monoid d, Monoid n)
  => Gen
      ( ReferenceDAG d () n
      , NetworkInformation
      , NetworkInformation
      , NetworkInformation
      , Int
      )
generateDoublyBranchedNetwork =
  do
    n0 <- generateNetwork
    n1 <- generateNetwork
    n2 <- generateNetwork
    let n0NetworkInfo = getNetworkInformation n0
    let n1NetworkInfo = getNetworkInformation n1
    let n2NetworkInfo = getNetworkInformation n2
    let xInd          = length n0 + length n1 + length n2
    pure ( makeDoublyBranchedNetwork n0 n1 n2
         , n0NetworkInfo
         , n1NetworkInfo
         , n2NetworkInfo
         , xInd
         )

-- |
-- This function takes an `Int` and increments the internal names of nodes
-- uniformly by that amount.
incrementNodeIndices :: forall d e n . Int -> ReferenceDAG d e n -> ReferenceDAG d e n
incrementNodeIndices n refDAG = refDAG & _rootRefs   %~ fmap (+ n)
                                       & _references %~ incrementRefVector n

-- |
-- Increment all of the index data in a reference vector.
incrementRefVector :: Int ->  Vector (IndexData e n) -> Vector (IndexData e n)
incrementRefVector n = fmap incrementIndexData
  where
    incrementIndexData :: IndexData e n -> IndexData e n
    incrementIndexData ind = ind & _parentRefs %~ IS.map     (+ n)
                                 & _childRefs  %~ IM.mapKeys (+ n)

-- |
-- Increment all index data in 'NetworkInformation'
incrementNetworkInformation :: Int -> NetworkInformation -> NetworkInformation
incrementNetworkInformation n NetworkInformation{..}
  = NetworkInformation
    { _candidateNetworkEdges = _candidateNetworkEdges'
    , _networkAdjacentEdges  = _networkAdjacentEdges'
    , _edgeSet               = _edgeSet'
    , _rootNode              = _rootNode'
    }

  where
    _candidateNetworkEdges'
      = Set.map (bimap incrementEdge incrementEdge) _candidateNetworkEdges
    _networkAdjacentEdges'
      = Set.map incrementEdge _networkAdjacentEdges
    _edgeSet'
      = Set.map incrementEdge _edgeSet

    _rootNode' = _rootNode + n

    incrementEdge :: (Int, Int) -> (Int, Int)
    incrementEdge (src, tgt) = (src + n, tgt + n)

-- |
-- Constructs a singleton `ReferenceDAG`.
singletonRefDAG :: forall d e n . (Monoid d) => n -> ReferenceDAG d e n
singletonRefDAG nodeDec = ReferenceDAG{..}
  where
    nodeData :: IndexData e n
    nodeData = IndexData{..}
      where
        nodeDecoration = nodeDec
        parentRefs     = []
        childRefs      = []

    references = pure nodeData
    rootRefs   = pure 0
    graphData  = mempty


-- |
-- Gets 'NetworkInformation' from a 'ReferenceDAG'.
getNetworkInformation :: ReferenceDAG d e n -> NetworkInformation
getNetworkInformation dag = NetworkInformation{..}
  where
    _candidateNetworkEdges = candidateNetworkEdges' IncludeRoot dag
    _networkAdjacentEdges  = getNetworkEdges       dag
    _edgeSet               = getEdgeSet dag
    _rootNode              = NE.head  $ dag ^. _rootRefs


-- |
-- Gets all edges which are not adjacent to a network node
getNonNetworkEdges :: NetworkInformation -> Set (Int, Int)
getNonNetworkEdges NetworkInformation{..} = _edgeSet \\ _networkAdjacentEdges

