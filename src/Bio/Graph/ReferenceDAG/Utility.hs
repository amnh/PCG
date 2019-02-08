{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}


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
import qualified Data.IntSet                     as IS (map)
import           Data.Key                        ((!))
import qualified Data.List.NonEmpty              as NE
import           Data.Vector                     (Vector)
import           Test.QuickCheck


-- |
-- This function takes valid Networks n0 and n1 and forms the network:
--
-- >                    r
-- >                ┌───┴───┐
-- >                │       │
-- >                │       │
-- >               n0       n1
makeBranchedNetwork
  ::  (Semigroup d)
  =>  (n -> n -> n)        -- ^ Function for combining node decorations.
  -> ReferenceDAG d () n   -- ^ n0
  -> ReferenceDAG d () n   -- ^ n1
  -> ReferenceDAG d () n
makeBranchedNetwork fn n0 n1 = ReferenceDAG{..}
  where
    newNode    = IndexData {..}
      where
        nodeDecoration = fn nodeDec0 nodeDec1
        parentRefs     = []
        childRefs      = [(rootIndex0, ()), (rootIndex1, ())]

    nodeDec0 = rootIndexData0  ^. _nodeDecoration
    nodeDec1 = rootIndexData1  ^. _nodeDecoration

    rootIndex0      = NE.head  (n0  ^. _rootRefs)
    rootIndex1      = NE.head  (n1  ^. _rootRefs)
    rootIndexData0 = references0 ! rootIndex0
    rootIndexData1 = references1 ! rootIndex1

    references0 = n0 ^. _references
    references1 = n1 ^. _references

    reindexedReferences1 = incrementNodeIndices (length references0) references0

  -- since vectors are zero-indexed this is the entry after the n0 and n1 node data.
    rootNodeIndex = length references0 + length references1


    references = references0 <> reindexedReferences1 <> pure newNode
    rootRefs   = pure rootNodeIndex
    graphData  = (n0 ^. _graphData) <> (n1 ^. _graphData)

-- |
-- This function takes valid networks n0, n1 and n2 and forms the network:
--
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
makeDoublyBranchedNetwork
  ::  (Monoid d)
  =>  (n -> n -> n)        -- ^ Function for combining node decorations.
  -> ReferenceDAG d () n   -- ^ n0
  -> ReferenceDAG d () n   -- ^ n1
  -> ReferenceDAG d () n   -- ^ n2
  -> ReferenceDAG d () n
makeDoublyBranchedNetwork fnn n0 n1 n2 = makeBranchedNetwork fnn n0 n0n1Branched
  where
    n0n1Branched = makeBranchedNetwork fnn n1 n2

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
makeDoublyBranchedNetworkWithNetworkEvent
  ::  forall d n. (Monoid d)
  =>  (n -> n -> n)        -- ^ Function for combining node decorations.
  -> ReferenceDAG d () n   -- ^ n0
  -> ReferenceDAG d () n   -- ^ n1
  -> ReferenceDAG d () n   -- ^ n2
  -> ReferenceDAG d () n   -- ^ n3
  -> ReferenceDAG d () n
makeDoublyBranchedNetworkWithNetworkEvent fnn n0 n1 n2 n3
    = makeBranchedNetwork fnn n0 internalNetwork
  where
    internalNetwork :: ReferenceDAG d () n
    internalNetwork= undefined


makeBinaryTree
  :: (Monoid d, Monoid n)
  =>  Int                    -- ^ depth of binary tree
  -> (ReferenceDAG d () n)   
makeBinaryTree 0 = singletonRefDAG mempty
makeBinaryTree n =
  let subtree = makeBinaryTree (n - 1) in
    makeBranchedNetwork (<>) subtree subtree

generateBinaryTree ::  (Monoid d, Monoid n) => Gen (ReferenceDAG d () n)
generateBinaryTree = do
  depth <- choose (1, 10)
  pure $ makeBinaryTree depth

generateNetwork :: Gen (ReferenceDAG d () n)
generateNetwork  = undefined


constructBranchedNetwork :: Gen (ReferenceDAG d () n)
constructBranchedNetwork = undefined

constructBranchedNetworkWithNetworkEvent :: Gen (ReferenceDAG d () n)
constructBranchedNetworkWithNetworkEvent = undefined


-- This function takes an `Int` and increments the internal names of nodes
-- uniformly by that amount.
incrementNodeIndices :: Int -> Vector (IndexData e n) -> Vector (IndexData e n)
incrementNodeIndices n = fmap incrementIndexData
  where
    incrementIndexData :: IndexData e n -> IndexData e n
    incrementIndexData ind = ind & _parentRefs %~ IS.map     (+ n)
                                 & _childRefs  %~ IM.mapKeys (+ n)



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
