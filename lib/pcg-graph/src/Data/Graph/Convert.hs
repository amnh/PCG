{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MagicHash     #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.Graph.Convert where
  

import Control.Lens              hiding (index)
import Data.Graph.Type
import Data.Graph.Indices
import Data.Graph.NodeContext
import Data.Pair.Strict
import Control.Monad.State.Strict
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Hashable
import Data.Foldable
import Data.Bifunctor
import Data.Monoid
import qualified Data.Vector.Mutable as MV
import Control.Monad.ST
import Data.Coerce
import Control.Monad.ST


import Data.Map (Map)
import qualified Data.Map as M

type AdjacencyState t e = State (Int :!: AdjacencyGraph (Either Int t) e) ()


data AdjacencyRelations t e =
  AdjacencyRelations
  { adjacencyParents  :: {-# UNPACK #-} ![(t, e)]
  , adjacencyChildren :: {-# UNPACK #-} ![(t, e)]
  }

type AdjacencyGraph t e = Map t (AdjacencyRelations t e)

-- |
-- This function takes an unfold function and gives a list of all of the distinct
-- vertex seed names.
getVertexNames
  :: forall a e t . (Ord a, Hashable a)
  => (a -> ([(a,e)], t, [(a,e)]))
  -> a
  -> [a]
getVertexNames unfoldFn start = nodes
  where
    nodes :: [a]
    nodes = view _right (execState (tryAddNode start) (mempty :!: []))

    tryAddNode
      :: a
      -> State (HashSet a :!: [a]) ()
    tryAddNode node =
      do
        nodesSeen :!: _ <- get
        if node `HS.member` nodesSeen then
          pure ()
        else
          do
            let updatedNodesSeen = (HS.insert node)
            let addToNodes      = (node :)
            modify (bimap updatedNodesSeen addToNodes)
            let
              parEdges, childEdges :: [(a,e)]
              (parEdges, _, childEdges) = unfoldFn node

              nodeParents, nodeChildren :: [a]
              nodeParents  = fst <$> parEdges
              nodeChildren = fst <$> childEdges
            traverse_ tryAddNode (nodeParents <> nodeChildren)

-- |
-- This function takes an unfold function and a list of vertex seeds
-- and checks they satisfy the valency condition for a phylogenetic netowrk namely,
-- a node should be one of the following:
--
-- * A root node which has no parents and one or two children.
--
-- * A leaf node which has one parent and no children.
--
-- * A tree node which has one parent and two children.
--
-- * A network node which has two parents and one child.
hasNetworkValence
  :: forall a e t . ()
  => (a -> ([(a,e)], t, [(a,e)]))
  -> [a]
  -> Bool
hasNetworkValence unfoldFn = getAll . foldMap checkNode
  where
    checkNode :: a -> All
    checkNode n =
      let
        (parSeeds, _, childSeeds) = unfoldFn n
        numberOfParents  = length parSeeds
        numberOfChildren = length childSeeds
      in
        case (numberOfParents, numberOfChildren) of
          (0, 1) -> All True
          (0, 2) -> All True
          (1, 2) -> All True
          (2, 1) -> All True
          (1, 0) -> All True
          _      -> All False

addIndicesFromUnfold
  :: forall a e t . (Ord t)
  => (a -> ([(a,e)], t, [(a,e)]))
  -> [a]
  -> ((Int, Int, Int, Int), Map t ([(t, e)], [(t, e)], TaggedIndex))
addIndicesFromUnfold unfoldFn input =
    case go (# 0, 0, 0, 0 #) input mempty of
      (# (# numL, numT, numN, numR #), taggedNodes #) -> ((numL, numT, numN, numR), taggedNodes)
  where
    go
      :: (# Int, Int, Int, Int #)
      -> [a]
      -> Map t ([(t, e)], [(t, e)], TaggedIndex)
      -> (# (# Int, Int, Int, Int #), Map t ([(t, e)], [(t, e)], TaggedIndex) #)
    go lengths [] res = (# lengths, res #)
    go (# l, t, n, r #) (a:as) res =
      let
        (parSeeds, nodeVal, childSeeds) = unfoldFn a
        parentVals, childVals :: [(t, e)]
        parentVals = first ((view _2) . unfoldFn) <$> parSeeds
        childVals  = first ((view _2) . unfoldFn) <$> childSeeds
        parLen   = length parentVals
        childLen = length childVals
      in
        case (# parLen, childLen #) of
          (# 0, 1 #)
            -> go
                (# l, t, n, (r + 1) #)
                as
                (M.insert nodeVal (parentVals, childVals, (TaggedIndex r RootTag)) res)
          (# 0, 2 #)
            -> go
                 (# l, t, n, (r + 1) #)
                 as
                 (M.insert nodeVal (parentVals, childVals, (TaggedIndex r RootTag)) res)
          (# 1, 2 #)
            -> go
                 (# l + 1, t, n, r   #)
                 as
                 (M.insert nodeVal (parentVals, childVals, (TaggedIndex r TreeTag)) res)
          (# 2, 1 #)
            -> go
                 (# l + 1, t, n, r   #)
                 as
                 (M.insert nodeVal (parentVals, childVals, (TaggedIndex r NetworkTag)) res)
          (# 1, 0 #)
            -> go
                 (# l + 1, t, n, r   #)
                 as
                 (M.insert nodeVal (parentVals, childVals, (TaggedIndex r LeafTag   )) res)
          _          ->
            error $
              unlines
                [ "Data.Graph.Convert.addIndices :"
                , "Whilst adding indices we encountered an index with number of parents:"
                , "parent length: " <> show parLen
                , "and children length:"
                , "children length: " <> show childLen
                , "This violates the valency conditions of a phylogenetic network."
                ]


indexAdjacencyGraphToGraph
  :: forall f e t . (Applicative f, Ord t)
  => ((Int, Int, Int, Int), Map t ([(t, e)], [(t, e)], TaggedIndex))
  -> Graph f () e t t
indexAdjacencyGraphToGraph ((numL, numT, numN, numR), adjGraph) =
    runST $
      do
        graph <- buildGraphM
        unsafeFreezeGraph graph () 
  where
    lookupTagInd :: t -> TaggedIndex
    lookupTagInd =
      (view _3) . (adjGraph M.!)

    getTwoChildren :: [(t, e)] -> ChildInfo e :!: ChildInfo e
    getTwoChildren cVals =
      let
        cEdges    = getPair cVals
        cTagIndL  = lookupTagInd . (view (_left . _1)) $ cEdges
        ctagIndR  = lookupTagInd . (view (_left . _1)) $ cEdges
        cInfo1 = childInfoTag cTagIndL (view (_left  . _2) cEdges)
        cInfo2 = childInfoTag cTagIndL (view (_right . _2) cEdges)
      in
        cInfo1 :!: cInfo2

    getOneChild :: [(t, e)] -> ChildInfo e
    getOneChild cVals =
      let
        cEdge    = head $ cVals
        cTagInd  = lookupTagInd . (view _1) $ cEdge
      in
        childInfoTag cTagInd (view _2 cEdge)

    getParentIndex :: (t, e) -> ParentIndex
    getParentIndex = coerce . (view _3) . (adjGraph M.!) . (view _1)

    getOneParent :: [(t, e)] -> ParentIndex
    getOneParent = getParentIndex . head

    getTwoParents :: [(t, e)] -> ParentIndex :!: ParentIndex
    getTwoParents = bimap getParentIndex getParentIndex . (getPair)

    toLeafData :: t -> ([(t, e)], [(t, e)], TaggedIndex) -> LeafIndexData t
    toLeafData name (parVals, _, untaggedIndex)
      = leafIndexData name (getOneParent parVals)

    toRootData :: t -> ([(t, e)], [(t, e)], TaggedIndex) -> RootIndexData (f t) e
    toRootData name (_, childVals, untaggedIndex)
      = let
          cInfo :: Either (ChildInfo e) (ChildInfo e :!: ChildInfo e)
          cInfo =
            case length childVals of
              1 -> Left  (getOneChild childVals)
              2 -> Right (getTwoChildren childVals)
              _ -> error
                     $ fold
                        [ "In Data.Graph.Convert.indexAdjacencyGraphToGraph.toRootData"
                        , "found root with more than 2 children"
                        ]
        in
          rootIndexDataA name cInfo

    toTreeData :: t -> ([(t, e)], [(t, e)], TaggedIndex) -> TreeIndexData (f t) e
    toTreeData name (parVals, childVals, untaggedIndex)
      = treeIndexDataA name (getOneParent parVals) (getTwoChildren childVals)

    toNetworkData :: t -> ([(t, e)], [(t, e)], TaggedIndex) -> NetworkIndexData (f t) e
    toNetworkData name (parVals, childVals, untaggedIndex)
      = networkIndexDataA name (getTwoParents parVals) (getOneChild childVals)

    addNodeContext
      :: MGraph s f e t t
      -> t -> ([(t, e)], [(t, e)], TaggedIndex) -> ST s ()
    addNodeContext mgraph name rels@(parVals, childVals, nodeInd) =
      let ind = untaggedIndex nodeInd in
      case getTag nodeInd of
        LeafTag    -> writeL mgraph ind (toLeafData name rels)
        RootTag    -> writeR mgraph ind (toRootData name rels)
        TreeTag    -> writeT mgraph ind (toTreeData name rels)
        NetworkTag -> writeN mgraph ind (toNetworkData name rels)
    
    buildGraphM :: forall s . ST s (MGraph s f e t t)
    buildGraphM =
      do
        mgraph <- newMGraph (numL, numT, numN, numR)
        void (M.traverseWithKey (addNodeContext mgraph) adjGraph)
        pure mgraph
        
directConversion
  :: forall a f c e t . (Applicative f, Ord t, Ord a, Hashable a)
  => (a -> ([(a,e)], t, [(a,e)]))
  -> a
  -> Graph f () e t t
directConversion unfoldFn seed = indexAdjacencyGraphToGraph graphIndexInfo
  where
    vertices :: [a]
    vertices = getVertexNames unfoldFn seed

    graphIndexInfo = addIndicesFromUnfold unfoldFn vertices
                

addLegalNode
  :: forall t e . (Ord t)
  => Either Int t
  -> [(Either Int t, e)]
  -> [(Either Int t, e)]
  -> AdjacencyState t e
addLegalNode rightV p c = modify (second (M.insert (rightV) (AdjacencyRelations p c)))


addNewNode
  :: forall t e . (Ord t)
  => Either Int t
  -> [(Either Int t, e)]
  -> [(Either Int t, e)]
  -> AdjacencyState t e
addNewNode leftFr p c = modify (second (M.insert (leftFr) (AdjacencyRelations p c)))


incrementFresh :: Int -> AdjacencyState t e
incrementFresh n = modify (first (+ n))


getPair :: [a] -> a :!: a
getPair (x:y:xs) = x :!: y
getPair _ = error "Called getPair on list of length less than 2"


buildAdjacencyGraph
  :: forall a e t.
  (Monoid e, Ord t)
  => (a -> ([(a,e)], t, [(a,e)]))
  -> [a]
  -> AdjacencyGraph (Either Int t) e
buildAdjacencyGraph unfoldFn nodes = adjacencyGraph
  where
    adjacencyGraph :: AdjacencyGraph (Either Int t) e
    adjacencyGraph = view _right (go `execState` (0 :!: mempty))

    go :: State (Int :!: AdjacencyGraph (Either Int t) e) ()
    go = traverse_ attemptAddNode nodes

    attemptAddNode :: a -> State (Int :!: AdjacencyGraph (Either Int t) e) ()
    attemptAddNode node =
      do
        let
          (nodeParents, val, nodeChildren) = unfoldFn node
          parentEdges, childrenEdges :: [(Either Int t, e)]
          parentEdges   = first (Right . (view _2) . unfoldFn) <$> nodeParents
          childrenEdges = first (Right . (view _2) . unfoldFn) <$> nodeChildren

        addNode (Right val) parentEdges childrenEdges

addNode
  :: forall t e . (Monoid e, Ord t)
  => Either Int t
  -> [(Either Int t, e)]
  -> [(Either Int t, e)]
  -> AdjacencyState t e
addNode currVal parentEdges childrenEdges = do
  fresh :!: _ <- get
  let numPars   = length parentEdges
  let numChilds = length childrenEdges
  case (numPars, numChilds) of
    (0, 0) -> addLegalNode currVal parentEdges childrenEdges
    (1, 1) -> pure ()
    (2, 1) -> addLegalNode currVal parentEdges childrenEdges
    (1, 2) -> addLegalNode currVal parentEdges childrenEdges
    (3, 1) ->
      do
        let (leftPars, rightPars) = splitAt 1 parentEdges
        let
          valEdge :: (Either Int t, e)
          valEdge = (currVal, mempty)
          newPars :: [(Either Int t, e)]
          newPars = (Left fresh, mempty) : leftPars
        addNewNode (Left fresh) rightPars (pure valEdge)
        addLegalNode currVal newPars childrenEdges
   --     updatePreviousParentNodes rightPars valEdge newValPar
        incrementFresh 1
    (2, 3) ->
      do
        let (leftChilds, rightChilds) = splitAt 1 childrenEdges
        let
          valEdge :: (Either Int t, e)
          valEdge = (currVal, mempty)
          newChilds :: [(Either Int t, e)]
          newChilds = (Left fresh, mempty) : leftChilds
        addNewNode (Left fresh) (pure valEdge) rightChilds
        addLegalNode currVal parentEdges newChilds
--        updatePreviousChildNodes rightChilds valEdge newValChild
        incrementFresh 1
    (p, c) | p == 1 || p == 2 ->
      do
        bifurcateChildren numChilds currVal parentEdges childrenEdges
    (p, c) | c == 1 || c == 2 ->
      do
        bifurcateParents numPars currVal parentEdges childrenEdges
    (p, c) ->
      do
        fresh <- gets (view _left)
        incrementFresh 1
        let newNode = Left fresh
        let newChildEdges  = [(newNode, mempty)]
        let newParentEdges = [(currVal, mempty)]
        bifurcateParents numPars currVal parentEdges newChildEdges
        bifurcateChildren numChilds newNode newParentEdges childrenEdges
        

bifurcateParents
  :: forall t e . (Monoid e, Ord t)
  => Int
  -> Either Int t         -- ^ Number of parents
  -> [(Either Int t, e)]  -- ^ Current Node value
  -> [(Either Int t, e)]  -- ^ Parent edge value
  -> AdjacencyState t e   -- ^ Child edge values
bifurcateParents n currVal parentEdges childrenEdges = do
  fresh <- gets (view _left)
  let
    half = n `div` 2
    (leftParents, rightParents) = splitAt half parentEdges
    valEdge = (currVal, mempty)
    newPar1, newPar2 :: (Either Int t, e)
    newPar1 = (Left fresh      , mempty)
    newPar2 = (Left (fresh + 1), mempty)
    newPars :: [(Either Int t, e)]
    newPars = [(Left fresh, mempty), (Left (fresh + 1), mempty)]
  addLegalNode currVal newPars childrenEdges
  incrementFresh 2
  addNode (Left fresh)       leftParents  (pure valEdge)
  addNode (Left (fresh + 1)) rightParents (pure valEdge)


bifurcateChildren
  :: forall t e . (Monoid e, Ord t)
  => Int                  -- ^ Number of children
  -> Either Int t         -- ^ Current Node value
  -> [(Either Int t, e)]  -- ^ Parent edge values
  -> [(Either Int t, e)]  -- ^ Child edge values
  -> AdjacencyState t e
bifurcateChildren n currVal parentEdges childrenEdges = do
  fresh <- gets (view _left)
  let
    half = n `div` 2
    (leftChildren, rightChildren) = splitAt half childrenEdges
    valEdge = (currVal, mempty)
    newChild1, newChild2 :: (Either Int t, e)
    newChild1 = (Left fresh      , mempty)
    newChild2 = (Left (fresh + 1), mempty)
    newChilds :: [(Either Int t, e)]
    newChilds = [(Left fresh, mempty), (Left (fresh + 1), mempty)]
  addLegalNode currVal parentEdges newChilds
  incrementFresh 2
  addNode (Left fresh)       (pure valEdge) leftChildren
  addNode (Left (fresh + 1)) (pure valEdge) rightChildren


updateChild
  :: forall t e . (Ord t)
  => AdjacencyRelations (Either Int t) e
  -> Either Int t
  -> [(Either Int t, e)]
  -> AdjacencyState t e
updateChild prevNode parInd newCh =
  do
    let
      f :: Int :!: AdjacencyGraph (Either Int t) e
        -> Int :!: AdjacencyGraph (Either Int t) e
      f =
        second
          (\adj ->
             M.insert parInd (prevNode {adjacencyChildren = newCh}) adj)
    modify f

updateParent
  :: forall t e . (Ord t)
  => AdjacencyRelations (Either Int t) e
  -> Either Int t
  -> [(Either Int t, e)]
  -> AdjacencyState t e
updateParent prevNode childInd newPar =
  do
    let
      f :: Int :!: AdjacencyGraph (Either Int t) e
        -> Int :!: AdjacencyGraph (Either Int t) e
      f =
        second
          (\adj ->
             M.insert childInd (prevNode {adjacencyParents = newPar}) adj)
    modify f



updatePreviousChildNodes
  :: forall t e . (Monoid e, Ord t, Eq e, Show t, Show e)
  => [(Either Int t, e)]
  -> (Either Int t, e)
  -> (Either Int t, e)
  -> AdjacencyState t e
updatePreviousChildNodes cs oldParent newParent =
            do
              currMap :: AdjacencyGraph (Either Int t) e  <- gets (view _right)
              for_ cs $ \c ->
                let
                  childName     = fst c
                  noParError child op =  error $ unlines
                                               ["Found node: "
                                               ,"   " <>  (show child)
                                               , "without original parent"
                                               , "   " <> (show op)
                                               ]
                in
                case childName `M.lookup` currMap of
                  Nothing        -> pure ()
                  Just prevEntry ->
                    let
                      oldParents = adjacencyParents prevEntry
                    in
                      case oldParents of
                        [par] | par == oldParent ->
                                    updateParent prevEntry childName [newParent]
                                | otherwise -> noParError c oldParent
                        [child1, child2] | child1 == oldParent ->
                                             updateParent
                                               prevEntry childName [newParent, child2]
                                         | child2 == oldParent ->
                                             updateParent
                                               prevEntry childName [child1, newParent]
                                         | otherwise ->
                                              noParError c oldParent
                        cs ->
                          error $
                            unlines
                              ["Found node: "
                              ,"   " <>  (show c)
                              , "with more than two children: "
                              , "   " <> (show cs)
                              ]

-- type AdjacencyGraph t e = Map t (AdjacencyRelations t e)

addIndicesFromAdjacencyGraph
  :: forall e t . (Ord t)
  => AdjacencyGraph (Either Int t) e
  -> ( (Int, Int, Int, Int)
     , Map (Either Int t) ([(Either Int t, e)], [(Either Int t, e)], TaggedIndex))
addIndicesFromAdjacencyGraph = M.foldrWithKey acc ((0,0,0,0), mempty)
  where
    acc
      :: (Either Int t)
      -> AdjacencyRelations (Either Int t) e
      -> ( (Int, Int, Int, Int)
         , Map (Either Int t) ([(Either Int t, e)], [(Either Int t, e)], TaggedIndex))
      -> ( (Int, Int, Int, Int)
         , Map (Either Int t) ([(Either Int t, e)], [(Either Int t, e)], TaggedIndex))
    acc nodeLabel adjacencyRelations = undefined
--      let
--        (parSeeds, nodeVal, childSeeds) = unfoldFn a
--        parentVals, childVals :: [(t, e)]
--        parentVals = first ((view _2) . unfoldFn) <$> parSeeds
--        childVals  = first ((view _2) . unfoldFn) <$> childSeeds
--        parLen   = length parentVals
--        childLen = length childVals
--      in
--        case (# parLen, childLen #) of
--          (# 0, 1 #)
--            -> go
--                (# l, t, n, (r + 1) #)
--                as
--                (M.insert nodeVal (parentVals, childVals, (TaggedIndex r RootTag)) res)
--          (# 0, 2 #)
--            -> go
--                 (# l, t, n, (r + 1) #)
--                 as
--                 (M.insert nodeVal (parentVals, childVals, (TaggedIndex r RootTag)) res)
--          (# 1, 2 #)
--            -> go
--                 (# l + 1, t, n, r   #)
--                 as
--                 (M.insert nodeVal (parentVals, childVals, (TaggedIndex r TreeTag)) res)
--          (# 2, 1 #)
--            -> go
--                 (# l + 1, t, n, r   #)
--                 as
--                 (M.insert nodeVal (parentVals, childVals, (TaggedIndex r NetworkTag)) res)
--          (# 1, 0 #)
--            -> go
--                 (# l + 1, t, n, r   #)
--                 as
--                 (M.insert nodeVal (parentVals, childVals, (TaggedIndex r LeafTag   )) res)
--          _          ->
--            error $
--              unlines
--                [ "Data.Graph.Convert.addIndices :"
--                , "Whilst adding indices we encountered an index with number of parents:"
--                , "parent length: " <> show parLen
--                , "and children length:"
--                , "children length: " <> show childLen
--                , "This violates the valency conditions of a phylogenetic network."
--                ]
--
-- |
-- This function is intended as a way to convert from unstructured
-- external tree formats to our *internal* phylogenetic binary networks.
-- It is not intended to be used for internal logic.
unfoldGraph
  :: forall a e t. -- (Eq a, Show a, Monoid e) =>
     (a -> ([(a,e)], t, [(a,e)]))
  -> a
  -> Graph Identity () e t t
unfoldGraph _unfoldFn _seed = undefined
