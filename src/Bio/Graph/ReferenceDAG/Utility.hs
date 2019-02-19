{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE TypeApplications    #-}


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

module Bio.Graph.ReferenceDAG.Utility where

import           Bio.Graph.ReferenceDAG.Internal
import           Control.Lens
import qualified Data.IntMap                     as IM (mapKeys)
import qualified Data.IntSet                     as IS (map, singleton)
import Data.IntSet (IntSet)
import Data.IntMap (IntMap)
import Data.EdgeSet
import           Data.Key                        ((!))
import qualified Data.List.NonEmpty              as NE
import           Data.Vector                     (Vector)
import           Test.QuickCheck
import Control.Monad.Loops
import Data.Foldable
import Data.Set (Set, (\\))
import Data.Tuple.Utility


import Debug.Trace


data NetworkInformation
  = NetworkInformation
  { _candidateNetworkEdges :: Set ((Int, Int), (Int, Int))
  , _networkAdjacentEdges  :: Set (Int, Int)
  , _edgeSet               :: Set (Int, Int)
  }

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
makeBranchedNetworkWithInfo'
  ::  (Semigroup d, Show n)
  =>  (n -> n -> n)        -- ^ Function for combining node decorations.
  ->   ReferenceDAG d () n   -- ^ n0
  ->   ReferenceDAG d () n   -- ^ n1
  -> ( ReferenceDAG d () n
     , NetworkInformation  -- ^ Network information for n0
     , NetworkInformation  -- ^ Network information for n1 (with correct indicies)
     )
makeBranchedNetworkWithInfo' fnn n0 n1 = (ReferenceDAG{..}, networkInfoN0, networkInfoN1')
  where
    newNode    = IndexData {..}
      where
        nodeDecoration = fnn nodeDec0 nodeDec1
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
    networkInfoN1' = getNetworkInformation n1'

makeBranchedNetwork'
  ::  (Semigroup d, Show n)
  =>  (n -> n -> n)        -- ^ Function for combining node decorations.
  -> ReferenceDAG d () n   -- ^ n0
  -> ReferenceDAG d () n   -- ^ n1
  -> ReferenceDAG d () n
makeBranchedNetwork' fnn n0 n1 = proj3_1 $ makeBranchedNetworkWithInfo' fnn n0 n1


makeBranchedNetwork
  ::  (Semigroup d, Monoid n, Show n)
  => ReferenceDAG d () n   -- ^ n0
  -> ReferenceDAG d () n   -- ^ n1
  -> ReferenceDAG d () n
makeBranchedNetwork = makeBranchedNetwork' (<>)


makeBranchedNetwork
  ::  (Semigroup d, Monoid n, Show n)
  =>   ReferenceDAG d () n   -- ^ n0
  ->   ReferenceDAG d () n   -- ^ n1
  -> ( ReferenceDAG d () n
     , NetworkInformation    -- ^ Network Information for n0
     , NetworkInformation    -- ^ Network Information for n1 (with correct indices)
     )
makeBranchedNetwork = makeBranchedNetwork' (<>)


-- |
-- This function takes valid networks n0, n1 and n2 and forms the network:
--
-- >                    r
-- >                ┌───┴───┐
-- >                │       │
-- >                │       │
-- >               n0       x
-- >                    ┌───┴───┐
-- >                    │       │
-- >                    │       │
-- >                   n1       n2
-- |
makeDoublyBranchedNetwork'
  ::  (Monoid d, Show n)
  =>  (n -> n -> n)        -- ^ Function for combining node decorations.
  -> ReferenceDAG d () n   -- ^ n0
  -> ReferenceDAG d () n   -- ^ n1
  -> ReferenceDAG d () n   -- ^ n2
  -> ReferenceDAG d () n
makeDoublyBranchedNetwork' fnn n0 n1 n2 = makeBranchedNetwork' fnn n0 n0n1Branched
  where
    n0n1Branched = makeBranchedNetwork' fnn n1 n2


makeDoublyBranchedNetwork
  ::  forall d n . (Monoid d, Monoid n, Show n)
  => ReferenceDAG d () n   -- ^ n0
  -> ReferenceDAG d () n   -- ^ n1
  -> ReferenceDAG d () n   -- ^ n2
  -> ReferenceDAG d () n
makeDoublyBranchedNetwork = makeDoublyBranchedNetwork' (<>)

-- |
-- This function takes valid networks n0, n1, n2 and n3 and forms the network:
--
-- >                    r
-- >                ┌───┴───┐
-- >                │       │
-- >                │       │
-- >                n0      a
-- >                   ┌────┴────┐
-- >                   │         │
-- >                   │         │
-- >                   b         c
-- >               ┌───┴───┐ ┌───┴───┐
-- >               │       │ │       │
-- >               │       │ │       │
-- >               │       └┬┘       │
-- >              n1        n2       n3
-- >
-- >
-- >
makeBranchedNetworkWithNetworkEvent'
  ::  forall d n . (Monoid d, Show n)
  =>  (n -> n -> n)        -- ^ Function for combining node decorations.
  -> ReferenceDAG d () n   -- ^ n0
  -> ReferenceDAG d () n   -- ^ n1
  -> ReferenceDAG d () n   -- ^ n2
  -> ReferenceDAG d () n   -- ^ n3
  -> ReferenceDAG d () n
makeBranchedNetworkWithNetworkEvent' fnn n0 n1 n2 n3
    = makeBranchedNetwork' fnn n0 internalNetwork
  where
    internalNetwork :: ReferenceDAG d () n
    internalNetwork = ReferenceDAG{..}

    references :: Vector (IndexData () n)
    references = mconcat
                   [ referencesN1
                   , referencesN2
                   , referencesN3
                   , pure bIndexData
                   , pure cIndexData
                   , pure aIndexData
                   ]
    rootRefs = pure aIndex
    graphData = (n1 ^. _graphData) <> (n2 ^. _graphData) <> (n3 ^. _graphData)

    aIndexData :: IndexData () n
    aIndexData = IndexData {..}
      where
        nodeDecoration = fnn (bIndexData ^. _nodeDecoration) (cIndexData ^. _nodeDecoration)
        parentRefs     = []
        childRefs      = [(bIndex, ()), (cIndex, ())]

    bIndexData :: IndexData () n
    bIndexData = IndexData{..}
      where
        nodeDecoration = fnn rootNodeDec1 rootNodeDec2
        parentRefs :: IntSet
        parentRefs     = [aIndex]
        childRefs :: IntMap ()
        childRefs      = [(n1RootIndex, ()), (n2RootIndex, ())]

    cIndexData :: IndexData () n
    cIndexData = IndexData{..}
      where
        nodeDecoration = fnn rootNodeDec2 rootNodeDec3
        parentRefs :: IntSet
        parentRefs     = [aIndex]
        childRefs :: IntMap ()
        childRefs      = [(n2RootIndex, ()), (n3RootIndex, ())]

    aIndex, bIndex, cIndex, n1RootIndex, n2RootIndex, n3RootIndex :: Int
    aIndex = cIndex + 1
    bIndex = n1n2n3Nodes
    cIndex = bIndex + 1

--  TODO: fix node indexing
    n1RootIndex = NE.head (n1 ^. _rootRefs)
    n2RootIndex = NE.head (n2 ^. _rootRefs)
    n3RootIndex = NE.head (n3 ^. _rootRefs)

    rootNodeDec1 = (referencesN1 ! n1RootIndex) ^. _nodeDecoration
    rootNodeDec2 = (referencesN2 ! n1RootIndex) ^. _nodeDecoration
    rootNodeDec3 = (referencesN3 ! n1RootIndex) ^. _nodeDecoration

--  TODO: fix vector indexing
    referencesN1, referencesN2, referencesN3 :: Vector (IndexData () n)
    referencesN1 = n1 ^. _references
    referencesN2 = n2 ^. _references
    referencesN3 = n3 ^. _references
    n1n2n3Nodes  = (length referencesN1) + (length referencesN2) + (length referencesN3)


makeBranchedNetworkWithNetworkEvent
  ::  forall d n . (Monoid d, Monoid n, Show n)
  => ReferenceDAG d () n   -- ^ n0
  -> ReferenceDAG d () n   -- ^ n1
  -> ReferenceDAG d () n   -- ^ n2
  -> ReferenceDAG d () n   -- ^ n3
  -> ReferenceDAG d () n
makeBranchedNetworkWithNetworkEvent = makeBranchedNetworkWithNetworkEvent' (<>)


makeBinaryTree
  :: forall d n . (Monoid d, Monoid n, Show n)
  =>  Int                    -- ^ depth of binary tree
  -> (ReferenceDAG d () n)   
makeBinaryTree 0 = singletonRefDAG mempty
makeBinaryTree n = let subtree = makeBinaryTree (n - 1) in
                     makeBranchedNetwork subtree subtree



generateBinaryTree' ::  forall d n . (Monoid d, Monoid n, Show n)
  => Gen
       ( (ReferenceDAG d () n)
       , NetworkInformation
       , NetworkInformation
       )
generateBinaryTree' = do
  depth <- choose (1, 4)
  let binTree = makeBinaryTree @d @n depth
  let netInfo = getNetworkInformation binTree
  pure $ (binTree, netInfo, netInfo)

generateBinaryTree ::  (Monoid d, Monoid n, Show n)
  => Gen (ReferenceDAG d () n)
generateBinaryTree = do
  depth <- choose (1, 10)
  pure $ (makeBinaryTree depth)



generateNetwork :: forall d n . (Monoid d, Monoid n, Show n) => Gen (ReferenceDAG d () n)
generateNetwork  = do
  (binTree :: ReferenceDAG d () n)  <- generateBinaryTree
  (refDAG, _) <- iterateUntilM stoppingCondition addNetworkEdge (binTree, True)
  pure refDAG
    where
      stoppingCondition :: (ReferenceDAG d () n, Bool) -> Bool
      stoppingCondition = \case
        (_, False)   -> False
        (rdag, True) -> not . null $ candidateNetworkEdges rdag

      addNetworkEdge :: (ReferenceDAG d () n, Bool) -> Gen ((ReferenceDAG d () n), Bool)
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
              pure (newRefDAG, True)
        where
          combine n1 n2 n3 = n1 <> n2 <> n3
       
          

generateBranchedNetwork
  :: (Monoid d, Monoid n, Show n)
  => Gen
      ( ReferenceDAG d () n -- ^ Branched network
      , NetworkInformation  -- ^ Candidate network information of n0
      , NetworkInformation  -- ^ Candidate network information of n1
      )
generateBranchedNetwork =
  do
    n0 <- generateNetwork
    n1 <- generateNetwork
    let n0NetworkInfo = getNetworkInformation n0
    let n1NetworkInfo = getNetworkInformation n1
    pure $ (makeBranchedNetwork n0 n1
           , n0NetworkInfo
           , n1NetworkInfo
           )

generateBranchedNetworkWithNetworkEvent
  :: (Monoid d, Monoid n, Show n)
  => Gen
       ( (ReferenceDAG d () n)
       , NetworkInformation  -- ^ Candidate network information of n0
       , NetworkInformation  -- ^ Candidate network information of n1
       , NetworkInformation  -- ^ Candidate network information of n2
       , NetworkInformation  -- ^ Candidate network information of n3
       )
generateBranchedNetworkWithNetworkEvent =
  do
    n0 <- generateNetwork
    n1 <- generateNetwork
    n2 <- generateNetwork
    n3 <- generateNetwork
    let n0NetworkInfo = getNetworkInformation n0
    let n1NetworkInfo = getNetworkInformation n1
    let n2NetworkInfo = getNetworkInformation n2
    let n3NetworkInfo = getNetworkInformation n3
    pure $ ( makeBranchedNetworkWithNetworkEvent n0 n1 n2 n3
           , n0NetworkInfo
           , n1NetworkInfo
           , n2NetworkInfo
           , n3NetworkInfo
           )

generateDoublyBranchedNetwork
  :: (Monoid d, Monoid n, Show n)
  => Gen
      ( (ReferenceDAG d () n)
      , NetworkInformation  -- ^ Candidate network information of n0
      , NetworkInformation  -- ^ Candidate network information of n1
      , NetworkInformation  -- ^ Candidate network information of n2
      )
generateDoublyBranchedNetwork =
  do
    n0 <- generateNetwork
    n1 <- generateNetwork
    n2 <- generateNetwork
    let n0NetworkInfo = getNetworkInformation n0
    let n1NetworkInfo = getNetworkInformation n1
    let n2NetworkInfo = getNetworkInformation n2
    pure $ ( makeDoublyBranchedNetwork n0 n1 n2
           , n0NetworkInfo
           , n1NetworkInfo
           , n2NetworkInfo
           )

-- |
-- This function takes an `Int` and increments the internal names of nodes
-- uniformly by that amount.
incrementNodeIndices :: forall d e n . Int -> ReferenceDAG d e n -> ReferenceDAG d e n
incrementNodeIndices n refDAG = refDAG & _rootRefs   %~ (fmap (+ n))
                                       & _references %~ incrementRefVector
  where
    incrementRefVector :: Vector (IndexData e n) -> Vector (IndexData e n)
    incrementRefVector = fmap incrementIndexData

    incrementIndexData :: IndexData e n -> IndexData e n
    incrementIndexData ind = ind & _parentRefs %~ IS.map     (+ n)
                                 & _childRefs  %~ IM.mapKeys (+ n)

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
    _candidateNetworkEdges = candidateNetworkEdges dag
    _networkAdjacentEdges  = getNetworkEdges       dag
    _edgeSet               = getUnderlyingEdgeSet . referenceEdgeSet $ dag
    

-- |
-- Gets all edges which are not adjacent to a network node
getNonNetworkEdges :: NetworkInformation -> Set ((Int, Int))
getNonNetworkEdges NetworkInformation{..} = _edgeSet \\ _networkAdjacentEdges

