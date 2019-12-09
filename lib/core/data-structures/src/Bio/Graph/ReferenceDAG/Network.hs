-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Graph.ReferenceDAG.Network
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bio.Graph.ReferenceDAG.Network where

import           Bio.Graph.Node.Context
import           Bio.Graph.ReferenceDAG.Internal
import           Bio.Graph.ReferenceDAG.Traversal
import           Control.Applicative              as Alt (Alternative (empty, (<|>)))
import           Control.DeepSeq
import           Control.Lens
import           Control.Monad                    (guard)
import           Data.EdgeSet
import           Data.Foldable
import           Data.IntMap                      (keys)
import           Data.IntSet                      (IntSet, notMember)
import qualified Data.IntSet                      as IS
import           Data.Key
import           Data.List                        (tails)
import           Data.Monoid                      hiding ((<>))
import           Data.MonoTraversable
import           Data.Set                         (Set)
import qualified Data.Set                         as S
import           Data.Vector                      (Vector)
import           Data.Vector.Memo                 as Memo
import           GHC.Generics


data  NetworkContext
    = NetworkContext
    { netNode    :: Int
    , netParent1 :: Int
    , netParent2 :: Int
    }
    deriving stock    (Eq, Ord, Generic, Show)
    deriving anyclass (NFData)


getNetworkContextParents :: NetworkContext -> IntSet
getNetworkContextParents NetworkContext{..} = IS.fromList [netParent1, netParent2]

-- |
-- This computes, in a nodal context, the set of ancestral
-- Network contexts from the parent edge sets and current node data.
ancestralNetworkContextContextFn
  :: ParentContext (Set NetworkContext, Int, IndexData e n)  -- ^ Parent ancestral network contexts
  -> ChildContext Int
  -> (Int, IndexData e n)                                    -- ^ Current node data
  -> Set NetworkContext                                     -- ^ Current ancestral network contexts
ancestralNetworkContextContextFn parentNetContexts _ (currInd, _) =
    case parentNetContexts of
      NoParent                              -> mempty
      OneParent (ancestralNetContext, _, _) -> ancestralNetContext
      TwoParents (ancestralNetContext1, parInd1, _) (ancestralNetContext2, parInd2, _) ->
        let
          currNetContext = NetworkContext
                          { netNode    = currInd
                          , netParent1 = parInd1
                          , netParent2 = parInd2
                          }
        in
               S.singleton currNetContext
            <> ancestralNetContext1
            <> ancestralNetContext2


-- |
-- This computes, in a nodal context, the set of ancestral
-- edges from the parent edge sets and current node data.
ancestralEdgeSetContextFn
  :: ParentContext (EdgeSet (Int, Int))  -- ^ Parent ancestral edge sets
  -> (Int, IndexData e n)                -- ^ Current node data
  -> EdgeSet (Int, Int)                  -- ^ Current node edge sets
ancestralEdgeSetContextFn ancestralEdgeSets (currInd, nodeDatum) =
    case ancestralEdgeSets of
      NoParent                   -> mempty
      OneParent ancestralEdgeSet -> currentEdgeSet <> ancestralEdgeSet

      TwoParents ancestralEdgeSet1 ancestralEdgeSet2
        -> currentEdgeSet <> ancestralEdgeSet1 <> ancestralEdgeSet2
  where
    parRefs        = nodeDatum ^. _parentRefs
    currentEdgeSet = makeParentEdgeSet currInd parRefs

-- |
-- This computes, in a nodal context, the set of ancestral
-- nodes from the parent edge sets and current node data.
ancestralNodeSetContextFn
  :: ParentContext IntSet  -- ^ Parent ancestral nodes
  -> (Int, IndexData e n)      -- ^ Current node data
  -> IntSet                  -- ^ Current ancestral nodes
ancestralNodeSetContextFn ancestralNodes (currInd, _) =
    case ancestralNodes of
      NoParent              -> IS.singleton currInd
      OneParent parentAncestralNodes -> IS.singleton currInd <> parentAncestralNodes

      TwoParents ancestralEdgeSet1 ancestralEdgeSet2
        -> IS.singleton currInd <> ancestralEdgeSet1 <> ancestralEdgeSet2


-- |
-- This computes, in a nodal context, the set of descendent
-- network nodes from the child descendent network sets and current node data.
-- It does this via a 'traversal with state' which keeps track of whether a child
-- node has only a single child and so is a descendant network node.
descendantNetworkNodesContextFn
  :: ChildContext IntSet  -- ^ Child descendent network node set
  -> (Int, IndexData e n) -- ^ Current node data
  ->  IntSet              -- ^ Current descendant network node sets
descendantNetworkNodesContextFn descendantNetworkNodes (currInd, _) =
    case descendantNetworkNodes of
    NoChildren                              -> mempty
 -- If a node has a single child then it is a network node and so is added
 -- to the set of nodes to be included in the parent descendant sets.
    OneChild descNetworkNodes               ->     IS.singleton currInd
                                                <> descNetworkNodes
    TwoChildren networkNodes1 networkNodes2 -> networkNodes1 <> networkNodes2

-- |
-- This computes, in a nodal context, the set of descendent
-- edges from the child descendent edge sets and current node data.
descendantEdgesContextFn
  :: ChildContext (Set (Int, Int))  -- ^ Child descendent edge set
  -> (Int, IndexData e n) -- ^ Current node data
  ->  Set (Int, Int)              -- ^ Current edge set
descendantEdgesContextFn childDescendantEdges (currInd, indData) =
    case childDescendantEdges of
    NoChildren                  -> mempty

    OneChild descendantEdges    ->
      let
        childrenInds = keys $ childRefs indData
        newEdges = (\c -> (currInd, c)) <$> childrenInds
      in
        S.fromList newEdges <> descendantEdges

    TwoChildren descendantEdges1 descendantEdges2 ->
      let
        childrenInds = keys $ childRefs indData
        newEdges = (\c -> (currInd, c)) <$> childrenInds
      in
        S.fromList newEdges <> descendantEdges1 <> descendantEdges2


-- |
-- This computes, in a nodal context, the set of descendent
-- network edges from the child descendent network sets and current node data.
-- It does this via a 'traversal with state' which keeps track of whether a child
-- node has only a single child and so is a descendant network node.
descendantNetworkEdgesContextFn
  ::  ChildContext (Set (Int, Int), Int, IndexData e n) -- ^ Child descendent network edge set and indexInfo
  -> ParentContext Int
  -> (Int, IndexData e n)                               -- ^ Current node data
  -> Set (Int, Int)                                     -- ^ Current descendant network edge sets
descendantNetworkEdgesContextFn descendantNetworkNodes parentContext (currInd, _) =
   case descendantNetworkNodes of
    NoChildren                  -> mempty
 -- If a node has a single child then it is a network node and so is added
 -- to the set of network nodes.
    OneChild (networkNodes, _, _)  ->
      case parentContext of
        TwoParents parInd1 parInd2 ->
              S.singleton (parInd1, currInd)
          <>  S.singleton (parInd2, currInd)
          <> networkNodes
       -- This case should never happen as it violates the network invariants
        _  ->
          error "This graph contains a node with out-degree 1 and in-degree other than 2"
    TwoChildren (networkNodes1, _, _) (networkNodes2, _, _)
      -> networkNodes1 <> networkNodes2



-- |
-- This computes, in a nodal context, the set of ancestral
-- nodes which are incident to a root node set. It does this
-- via a 'traversal with state' passing a boolean value of
-- whether a node is a root node.
ancestralRootIncidentNodesContextFn
  :: ParentContext (IntSet,Bool)  -- ^ parent root node set and state information
  -> (Int, IndexData e n)         -- ^ Current node data
  -> (IntSet, Bool)               -- ^ Current node edge sets
ancestralRootIncidentNodesContextFn ancestralRootNodes (currInd, _) =
  case ancestralRootNodes of
    NoParent
      -> (mempty, True)
    OneParent (parAncestralSet, incidentToRoot)
      -> if incidentToRoot
           then (IS.singleton currInd, False)
           else (parAncestralSet, False)
    TwoParents
      (parAncestralSet1, incidentToRoot1)
      (parAncestralSet2, incidentToRoot2)
        -> if incidentToRoot1 && incidentToRoot2
             then (IS.singleton currInd <> parAncestralSet1 <> parAncestralSet2, False)
             else (parAncestralSet1 <> parAncestralSet2, False)

-- |
-- Generate a vector of graph data for finding the candidate network edges in a memoized
-- fashion.
tabulateNetworkInformation
  :: ReferenceDAG d e n
  -> Vector
        ( EdgeSet (Int, Int)  -- Ancestral Edge Set
        , IntSet              -- Descendant Network Nodes
        , IntSet              -- Ancestral Node Set
        , Set NetworkContext  -- Ancestral Network Contexts
        )
tabulateNetworkInformation dag =
  let
    dVectorAncestralEdge    = dVectorPreorder  ancestralEdgeSetContextFn       dag
    dVectorDescendantNet    = dVectorPostorder descendantNetworkNodesContextFn dag
    dVectorAncestralNodes   = dVectorPreorder  ancestralNodeSetContextFn       dag
    dVectorAncestralNetCtxt = dVectorPreorderWithContext ancestralNetworkContextContextFn dag


    dVectorNetInfo
      = Memo.zip4
          dVectorAncestralEdge
          dVectorDescendantNet
          dVectorAncestralNodes
          dVectorAncestralNetCtxt
  in
    generateMemo lengthRefs dVectorNetInfo
  where
    lengthRefs = length $ dag ^. _references





data RootStatus = IncludeRoot | ExcludeRoot

-- |
-- Find all candidate network edges in a DAG.
candidateNetworkEdges'
  :: RootStatus     -- ^ Whether we include nodes adjacent to the root
  -> ReferenceDAG d e n
  -> Set ((Int, Int), (Int,Int))
candidateNetworkEdges' rootStatus dag = S.fromList candidateEdgesList
  where
    completeEdges      = toList $ getEdges dag
    rootIndices        = IS.fromList . toList . rootRefs $ dag
    leafInds           = leafIndices dag
    networkNodes       = gatherDescendantNetworkNodes rootIndices networkInformation
    networkContexts    = gatherAncestralNetworkContexts leafInds networkInformation

  -- This vector contains all the information needed for the various edge
 -- compatibility criteria.
    networkInformation = tabulateNetworkInformation dag



 -- Gets all pairs of distinct edges from the edge set that can be
 -- compatably added to the network.
    candidateEdgesList :: [((Int, Int), (Int, Int))]
    candidateEdgesList =
        do
       -- collect distinct edge pairs.
          e1 : es <- tails completeEdges
          e2      <- es
          guard $ rootTest e1 e2
          let
                       -- Check the target edge is not incident to a network
                       -- node as this leads to a display trees with illegal
                       -- incident structure.
            e1e2Bool =    not (hasIncidentNetworkNode e2)
                       && ancestralTest  e1 e2
                       && descendantTest e1 e2
            e2e1Bool =    not (hasIncidentNetworkNode e1)
                       && ancestralTest  e2 e1
                       && descendantTest e2 e1
          case (e1e2Bool, e2e1Bool) of
                (True , True ) -> pure (e1,e2) <|> pure (e2,e1)
                (True , False) -> pure (e1, e2)
                (False, True ) -> pure (e2, e1)
                (False, False) -> Alt.empty

      where
     -- Checks ancestral compatability i.e. we do not form a network event
     -- with nodes ancestral to one another.
        ancestralTest :: (Int, Int) -> (Int, Int) -> Bool
        ancestralTest (src1,tgt1) (src2, tgt2) =
     -- Network Info
            let

              getAncestralNodes :: Int -> IntSet
              getAncestralNodes node = (^. _3) $ networkInformation ! node

              getAncestralNetworkPairs :: IntSet -> IntSet
              getAncestralNetworkPairs =
                ofoldMap
                  (\node ->
                     foldMap
                       (\NetworkContext{..} ->
                           if  netParent1 == node then IS.singleton netParent2
                           else
                             if netParent2 == node then IS.singleton netParent1
                             else mempty
                       ) networkContexts
                  )


              e1SrcAncestralNodes, e2SrcAncestralNodes :: IntSet
              e1SrcAncestralNodes = getAncestralNodes src1
              e2SrcAncestralNodes = getAncestralNodes src2

              e1SrcAncestralNetworkContexts = getAncestralNetworkPairs e1SrcAncestralNodes
              e2SrcAncestralNetworkContexts = getAncestralNetworkPairs e2SrcAncestralNodes
              e2SrcImpliedHistoricalNodes
                = ofoldMap
                    getAncestralNodes
                    e2SrcAncestralNetworkContexts

              e1SrcImpliedHistoricalNodes
                = ofoldMap
                    getAncestralNodes
                    e1SrcAncestralNetworkContexts

--
            in
                -- First check if the two edges are from the same parent to short circuit
              -- faster in this case.
                  src1 /= src2
              -- or if either edge is ancestral to the other.
              && (tgt1 `notMember` e2SrcAncestralNodes)
              && (src2 `notMember` e1SrcAncestralNodes)
              && (tgt1 `notMember` e2SrcImpliedHistoricalNodes)
              && (tgt2 `notMember` e1SrcImpliedHistoricalNodes)
             -- && (singletonEdgeSet e2 `disjoint` e1AncestralEdges)


        descendantTest :: (Int, Int) -> (Int, Int) -> Bool
        descendantTest e1@(_, tgt1) (src2, tgt2) =
          let
         -- Network Information

            e1TgtAncestralNodes = (^. _3) $ networkInformation ! tgt1
            e2TgtAncestralNodes = (^. _3) $ networkInformation ! tgt2

            networkParentSource2Test :: Int -> IntSet -> Bool
            networkParentSource2Test _src1 e2TgtAncNodes =
              case getOtherNetworkParentFromNode _src1 networkContexts of
                Just netPar -> netPar `IS.notMember` e2TgtAncNodes
                Nothing     -> True

            networkEdgeSource1Test :: (Int, Int) -> IntSet -> Bool
            networkEdgeSource1Test e e2TgtAncNodes =
              case getOtherNetworkParentFromEdge e networkContexts of
                Just netPar -> netPar `IS.notMember` e2TgtAncNodes
                Nothing     -> True

         -- TODO: better name and document  -- networkParentSource2Test?
         -- checks if src2 is already a network parent node with a node ancestral to newSrc
            e2NetworkEdgeTest = networkParentSource2Test src2 e1TgtAncestralNodes

         -- TODO: same   --networkEdgeSource1Test
         -- checks if (src1, tgt1) is a network edge which is in a context which is ancestral
         -- to src2
            e1NetworkEdgeTest = networkEdgeSource1Test e1 e2TgtAncestralNodes

          in
               e1NetworkEdgeTest
            && e2NetworkEdgeTest

        rootTest :: (Int, Int) -> (Int, Int) -> Bool
        rootTest (src1, _) (src2, _) =
          let
        -- Tests
            e1HasRootSource, e2HasRootSource :: Bool
            e1HasRootSource = src1 `IS.member` rootIndices
            e2HasRootSource = src2 `IS.member` rootIndices
          in
            case rootStatus of
              ExcludeRoot ->    not e1HasRootSource
                             && not e2HasRootSource
              IncludeRoot ->
                case (e1HasRootSource, e2HasRootSource) of
                  (True, True) -> False
                  _            -> True


     -- helper functions
        hasIncidentNetworkNode :: (Int, Int) -> Bool
        hasIncidentNetworkNode (src,tgt) =
             tgt `IS.member` networkNodes
          || src `IS.member` networkNodes

        getOtherNetworkParentFromNode :: Int -> Set NetworkContext -> Maybe Int
        getOtherNetworkParentFromNode src = getFirst . foldMap getNetPar
          where
            getNetPar :: NetworkContext -> First Int
            getNetPar (NetworkContext _ netPar1 netPar2)
              | src == netPar1  = pure netPar2
              | src == netPar2  = pure netPar1
              | otherwise       = mempty

        getOtherNetworkParentFromEdge :: (Int, Int) -> Set NetworkContext -> Maybe Int
        getOtherNetworkParentFromEdge (src, tgt) = getFirst . foldMap getNetPar
          where
            getNetPar :: NetworkContext -> First Int
            getNetPar (NetworkContext networkNode netPar1 netPar2)
              | src == netPar1 && tgt == networkNode  = pure netPar2
              | src == netPar2 && tgt == networkNode  = pure netPar1
              | otherwise       = mempty


candidateNetworkEdges :: ReferenceDAG d e n -> Set ((Int, Int), (Int,Int))
candidateNetworkEdges = candidateNetworkEdges' ExcludeRoot



-- |
-- Helper function to get all descendent network nodes from an `IntSet` of nodes.
gatherDescendantNetworkNodes
  :: IntSet                    -- ^ Node set
  -> Vector (a, IntSet, c, d)  -- ^ Vector tuple with network node indices
  -> IntSet                    -- ^ All descendant network nodes
gatherDescendantNetworkNodes inds vect
  = ofoldMap (\ind -> (^. _2) $ vect ! ind) inds

-- |
-- Helper function to get all descendent network contexts from an `IntSet` of nodes.
gatherAncestralNetworkContexts
  :: IntSet                                -- ^ Node set
  -> Vector (a, b, c, Set NetworkContext)  -- ^ Vector tuple with network node indices
  -> Set NetworkContext                    -- ^ All descendant network nodes
gatherAncestralNetworkContexts inds vect
  = ofoldMap (\ind -> (^. _4) $ vect ! ind) inds



-- |
-- Gets all edges from a `ReferenceDAG` which are incident to a network
-- node.
getNetworkEdges :: ReferenceDAG d e n -> Set (Int, Int)
getNetworkEdges dag = fold descendantNetworkEdges
  where
    descendantNetworkEdges = generateMemo numberOfNodes dVectorDescendantNet
    dVectorDescendantNet   = dVectorPostorderWithContext descendantNetworkEdgesContextFn dag
    numberOfNodes          = length $ dag ^. _references


getEdgeSet :: ReferenceDAG d e n -> Set (Int, Int)
getEdgeSet dag =  fold descendantEdges
  where
    descendantEdges        = generateMemo numberOfNodes dVectorDescendantEdges
    dVectorDescendantEdges = dVectorPostorder descendantEdgesContextFn dag
    numberOfNodes          = length $ dag ^. _references


