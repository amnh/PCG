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

module Data.Graph.Type
  ( -----------
    -- Types --
    -----------
    Graph(..)
  , GraphShape(..)
  , GraphShape'
  , GraphBuilder(..)
  , MGraph(..)
    -----------------------
    -- Mutable interface --
    -----------------------
  , newMGraph
  , writeL
  , writeR
  , writeN
  , writeT
  , readL
  , readR
  , readN
  , readT
  , unsafeFreezeGraph
  , unsafeThawGraph
    ------------------------------
    -- construct builder graphs --
    ------------------------------
  , leafGB
  , treeGB
  , rootGB
  , networkGB
    ---------------------------
    -- classy lens accessors --
    ---------------------------
  , HasTreeReferences(..)
  , HasNetworkReferences(..)
  , HasRootReferences(..)
  , HasLeafReferences(..)
  , HasCachedData(..)
  , HasLeafData(..)
  , HasTreeData(..)
  , HasNetworkData(..)
  , HasRootData(..)
    -------------------------------
    -- graph with specified root --
    -------------------------------
  , RootFocusGraph
  , Focus
  , makeRootFocusGraphs
    --------------
    -- Indexing --
    --------------
  , index
  , indexRoot
  , indexTree
  , indexNetwork
  , indexLeaf
    ----------------------
    -- utility functions--
    ----------------------
  , buildGraph
  , lookupTreeNode
  , getRootInds
  , numberOfNodes
  , getTreeEdges
  , getTreeEdgeSet
  , getNetworkEdges
  , getNetworkEdgeSet
  , getRootEdges
  , getRootEdgeSet
  , getEdgeGraphShape
  , getEdges
  , hasRootParent
  , getSibling
  , getParents
  )where

import Prelude hiding (read, lookup)
import Control.Lens              hiding (index)
import Data.Graph.Indices
import Data.Graph.NodeContext
import Data.Kind                 (Type)
import Data.Pair.Strict
import qualified VectorBuilder.Vector as Builder
import Data.Vector               (Vector, generate, unsafeFreeze, unsafeThaw, (!))
import Data.Vector.Instances     ()
import Test.QuickCheck.Arbitrary
import TextShow                  hiding (Builder)
import qualified VectorBuilder.Builder as Builder
import VectorBuilder.Builder (Builder)
import Data.Vector.Mutable (MVector, new, write, read)
import Control.Monad.Primitive
import           Data.Vector.Instances ()
import Data.Key (foldMapWithKey, lookup)
import Data.Coerce
import Data.Set (Set)
import qualified Data.Set as Set


--      ┌─────────────┐
--      │    Types    │
--      └─────────────┘

-- |
-- The GraphShape type is for storing data in the same `shape` as our graph.
data GraphShape tree net root leaf
  = GraphShape
  { leafData    :: Vector leaf
  , treeData    :: Vector tree
  , networkData :: Vector net
  , rootData    :: Vector root
  }

type GraphShape' tree leaf = GraphShape tree tree tree leaf

lookupTreeNode
  :: TaggedIndex
  -> GraphShape internal internal root leaf -> Maybe internal
lookupTreeNode taggedIndex GraphShape{..} =
  case getTag taggedIndex of
    -- to do : improve error message
    LeafTag    -> Nothing
    TreeTag    -> (getIndex taggedIndex) `lookup` treeData
    NetworkTag -> (getIndex taggedIndex) `lookup` networkData
    RootTag    -> Nothing


data  Graph
        (f :: Type -> Type)
        (c :: Type)
        (e :: Type)
        (n :: Type)
        (t :: Type)
   = Graph
   { leafReferences    :: {-# UNPACK #-} !(Vector (LeafIndexData       t   ))
   , treeReferences    :: {-# UNPACK #-} !(Vector (TreeIndexData    (f n) e))
   , networkReferences :: {-# UNPACK #-} !(Vector (NetworkIndexData (f n) e))
   , rootReferences    :: {-# UNPACK #-} !(Vector (RootIndexData    (f n) e))
   , cachedData        :: c
   }
   deriving stock Show

-- |
-- Returns the total number of nodes in the graph
numberOfNodes :: Graph f c e n t -> Int
numberOfNodes (Graph lRef tRef netRef rootRef _) =
  length lRef + length tRef + length netRef + length rootRef


data  MGraph
        (s :: Type)
        (f :: Type -> Type)
        (e :: Type)
        (n :: Type)
        (t :: Type)
   = MGraph
   { leafReferencesM    :: {-# UNPACK #-} !(MVector s (LeafIndexData       t   ))
   , treeReferencesM    :: {-# UNPACK #-} !(MVector s (TreeIndexData    (f n) e))
   , networkReferencesM :: {-# UNPACK #-} !(MVector s (NetworkIndexData (f n) e))
   , rootReferencesM    :: {-# UNPACK #-} !(MVector s (RootIndexData    (f n) e))
   }

newMGraph :: PrimMonad m => (Int, Int, Int, Int) -> m (MGraph (PrimState m) f e n t)
newMGraph (numL, numT, numN, numR) =
  do
    leafReferencesM     <- new numL
    treeReferencesM     <- new numT
    networkReferencesM  <- new numN
    rootReferencesM     <- new numR
    pure MGraph{..}

writeL :: PrimMonad m => MGraph (PrimState m) f e n t -> Int -> LeafIndexData t -> m ()
writeL MGraph{..} ind li = write leafReferencesM ind li

writeT :: PrimMonad m => MGraph (PrimState m) f e n t -> Int -> TreeIndexData (f n) e -> m ()
writeT MGraph{..} ind ti = write treeReferencesM ind ti

writeN :: PrimMonad m => MGraph (PrimState m) f e n t -> Int -> NetworkIndexData (f n) e -> m ()
writeN MGraph{..} ind ni = write networkReferencesM ind ni

writeR :: PrimMonad m => MGraph (PrimState m) f e n t -> Int -> RootIndexData (f n) e  -> m ()
writeR MGraph{..} ind ri = write rootReferencesM ind ri

readL :: PrimMonad m => MGraph (PrimState m) f e n t -> Int -> m (LeafIndexData t)
readL MGraph{..} ind = read leafReferencesM ind

readT :: PrimMonad m => MGraph (PrimState m) f e n t -> Int -> m (TreeIndexData (f n) e)
readT MGraph{..} ind = read treeReferencesM ind

readN :: PrimMonad m => MGraph (PrimState m) f e n t -> Int -> m (NetworkIndexData (f n) e)
readN MGraph{..} ind = read networkReferencesM ind

readR :: PrimMonad m => MGraph (PrimState m) f e n t -> Int -> m (RootIndexData (f n) e)
readR MGraph{..} ind = read rootReferencesM ind

unsafeFreezeGraph :: PrimMonad m => MGraph (PrimState m) f e n t -> c -> m (Graph f c e n t)
unsafeFreezeGraph MGraph{..} c =
  do
    l <- unsafeFreeze leafReferencesM
    t <- unsafeFreeze treeReferencesM
    n <- unsafeFreeze networkReferencesM
    r <- unsafeFreeze rootReferencesM
    pure $ Graph l t n r c

unsafeThawGraph :: PrimMonad m => Graph f c e n t -> m (MGraph (PrimState m) f e n t)
unsafeThawGraph Graph{..} =
  do
    l <- unsafeThaw leafReferences
    t <- unsafeThaw treeReferences
    n <- unsafeThaw networkReferences
    r <- unsafeThaw rootReferences
    pure $ MGraph l t n r


data  GraphBuilder
        (f :: Type -> Type)
        (e :: Type)
        (n :: Type)
        (t :: Type)
   = GraphBuilder
   { leafReferencesBuilder    :: {-# UNPACK #-} !(Builder.Builder (LeafIndexData       t   ))
   , treeReferencesBuilder    :: {-# UNPACK #-} !(Builder.Builder (TreeIndexData    (f n) e))
   , networkReferencesBuilder :: {-# UNPACK #-} !(Builder.Builder (NetworkIndexData (f n) e))
   , rootReferencesBuilder    :: {-# UNPACK #-} !(Builder.Builder (RootIndexData    (f n) e))
   }

instance Semigroup (GraphBuilder f e n t) where
  (<>) (GraphBuilder l1 t1 n1 r1) (GraphBuilder l2 t2 n2 r2)
    = GraphBuilder
        (l1 <> l2)
        (t1 <> t2)
        (n1 <> n2)
        (r1 <> r2)

instance Monoid (GraphBuilder f e n t) where
  mempty = GraphBuilder mempty mempty mempty mempty


buildGraph :: GraphBuilder f e n t -> c -> Graph f c e n t
buildGraph GraphBuilder{..} cachedData =
    let
      leafReferences    = Builder.build leafReferencesBuilder
      treeReferences    = Builder.build treeReferencesBuilder
      networkReferences = Builder.build networkReferencesBuilder
      rootReferences    = Builder.build rootReferencesBuilder
    in
      Graph{..}

leafGB :: LeafIndexData t -> GraphBuilder f e n t
leafGB leafInd =
  GraphBuilder (Builder.singleton leafInd) mempty mempty mempty

treeGB :: TreeIndexData (f n) e -> GraphBuilder f e n t
treeGB treeInd =
  GraphBuilder mempty (Builder.singleton treeInd) mempty mempty

networkGB :: NetworkIndexData (f n) e -> GraphBuilder f e n t
networkGB networkInd =
  GraphBuilder mempty mempty (Builder.singleton networkInd) mempty

rootGB :: RootIndexData (f n) e -> GraphBuilder f e n t
rootGB rootInd =
  GraphBuilder mempty mempty mempty (Builder.singleton rootInd)


type  Focus = Pair IndexType UntaggedIndex
type  RootFocusGraph f c e n t = Pair Focus (Graph f c e n t)

-- |
-- This makes a list of graphs along with the roots to focus upon.
makeRootFocusGraphs :: Graph f c e n t -> [RootFocusGraph f c e n t]
makeRootFocusGraphs graph =
  let
    rootLength = length (view _rootReferences graph)
    rootNames :: [Focus]
    rootNames  =
      let
        rootInds = case rootLength of
            0 -> []
            1 -> [0]
            n -> [0..(n - 1)]
      in
        fmap (Pair RootTag) rootInds
  in
    fmap (:!: graph) rootNames


--      ┌─────────────────┐
--      │    Instances    │
--      └─────────────────┘

instance Functor f => Bifunctor (Graph f c e) where

    bimap f g graph@Graph{..} = graph
        { leafReferences     = (fmap . fmap        $ g) leafReferences
        , treeReferences     = (fmap . fmap . fmap $ f) treeReferences
        , networkReferences  = (fmap . fmap . fmap $ f) networkReferences
        , rootReferences     = (fmap . fmap . fmap $ f) rootReferences
        }


instance Arbitrary (Graph f c e n t) where

    arbitrary = pure undefined


instance TextShow (Graph f c e n t) where

    showb = undefined


--instance Show (Graph f c e n t) where
--  show = toString . showb



class HasLeafData s t a b | s -> a, t -> b, s b -> t, t a -> s where
    _leafData :: Lens s t a b


instance HasLeafData
           (GraphShape i n r t )
           (GraphShape i n r t')
           (Vector  t)
           (Vector  t') where

    _leafData = lens leafData (\g l -> g { leafData = l})



class HasTreeData s a | s -> a where
    _treeData :: Lens' s a


instance HasTreeData
           (GraphShape i n r t)
           (Vector i) where
    _treeData = lens treeData (\g fn -> g {treeData = fn})


class HasNetworkData s a | s -> a where
    _networkData :: Lens' s a


instance HasNetworkData
           (GraphShape i n r t)
           (Vector n) where
    _networkData = lens networkData (\g fn -> g {networkData = fn})


class HasRootData s a | s -> a where

    _rootData :: Lens' s a


instance HasRootData
           (GraphShape i n r t)
           (Vector r) where
    _rootData = lens rootData (\g fn -> g {rootData = fn})


class HasLeafReferences s t a b | s -> a, t -> b, s b -> t, t a -> s where

    _leafReferences :: Lens s t a b


instance HasLeafReferences
           (Graph f c e n t)
           (Graph f c e n t')
           (Vector (IndexData LeafContext t))
           (Vector (IndexData LeafContext t')) where

    _leafReferences = lens leafReferences (\g l -> g { leafReferences = l})


class HasTreeReferences s a | s -> a where

    _treeReferences :: Lens' s a


instance HasTreeReferences
           (Graph f c e n t)
           (Vector (IndexData (TreeContext e)  (f n))) where

    _treeReferences = lens treeReferences (\g fn -> g {treeReferences = fn})


class HasNetworkReferences s a | s -> a where

    _networkReferences :: Lens' s a


instance HasNetworkReferences
           (Graph f c e n t)
           (Vector (IndexData (NetworkContext e) (f n))) where

    _networkReferences = lens networkReferences (\g fn -> g {networkReferences = fn})


class HasRootReferences s a | s -> a where

    _rootReferences :: Lens' s a


instance HasRootReferences
           (Graph f c e n t)
           (Vector (IndexData (RootContext e) (f n))) where

    _rootReferences = lens rootReferences (\g fn -> g {rootReferences = fn})


class HasCachedData s t a b | s -> a, t -> b, s b -> t, t a -> b where

    _cachedData :: Lens s t a b


instance HasCachedData
           (Graph f c1 e n t)
           (Graph f c2 e n t)
           c1
           c2 where

    _cachedData = lens cachedData (\g c2 -> g {cachedData = c2})


--      ┌────────────────┐
--      │    Indexing    │
--      └────────────────┘

-- Note: Each of these index functions is as unsafe as indexing into a vector.
-- They are intended to be used when working with the underlying graph representation.

-- |
-- This function indexes into a graph using the tag to correctly index into the
-- node type. As we can't know what this returns we use a new sum type which
-- may be inconvenient and so we also offer index variants for the specific node type.
index :: Tagged taggedInd => Graph f c e n t -> taggedInd -> NodeIndexData (f n) e t
index graph taggedIndex =
  let
    ind = getIndex taggedIndex
  in
  case getTag taggedIndex of
    LeafTag    -> LeafNodeIndexData $
                     graph ^.
                      _leafReferences
                    . singular (ix ind)

    TreeTag -> TreeNodeIndexData $
                     graph ^.
                       _treeReferences
                     . singular (ix ind)

    NetworkTag  -> NetworkNodeIndexData $
                    graph ^.
                       _networkReferences
                     . singular (ix ind)

    RootTag     -> RootNodeIndexData $
                    graph ^.
                       _rootReferences
                     . singular (ix ind)

-- |
-- Index into the root reference vector.
indexRoot :: Graph f c e n t -> Int -> RootIndexData (f n) e
indexRoot graph untaggedIndex =
  view (_rootReferences . singular (ix untaggedIndex)) graph

-- |
-- Index into the tree reference vector.
indexTree :: Graph f c e n t -> Int -> TreeIndexData (f n) e
indexTree graph untaggedIndex =
  view (_treeReferences . singular (ix untaggedIndex)) graph

-- |
-- Index into the network reference vector.
indexNetwork :: Graph f c e n t -> Int -> NetworkIndexData (f n) e
indexNetwork graph untaggedIndex =
  view (_networkReferences . singular (ix untaggedIndex)) graph

-- |
-- Index into the leaf reference vector.
indexLeaf :: Graph f c e n t -> Int -> LeafIndexData t
indexLeaf graph untaggedIndex =
  view (_leafReferences . singular (ix untaggedIndex)) graph




--      ┌───────────────┐
--      │    Utility    │
--      └───────────────┘

getRootInds :: Graph f c e n t -> Vector TaggedIndex
getRootInds graph =
  let
    numberOfRoots = length (view _rootReferences graph)
    roots = generate numberOfRoots (`TaggedIndex` RootTag)
  in
    roots

--      ┌─────────────────┐
--      │    Rendering    │
--      └─────────────────┘

{-
topologyRendering :: Graph f e c n t -> Text
topologyRendering = undefined


horizontalRendering :: _
horizontalRendering = undefined


referenceRendering :: Graph f e c n t -> Text
referenceRendering = undefined


toBinaryRenderingTree :: (n -> String) -> Graph f e c n t -> NonEmpty BinaryRenderingTree
toBinaryRenderingTree = undefined
-}

--      ┌─────────────────┐
--      │    Edges        │
--      └─────────────────┘



getTreeEdges :: forall f c e n t . Graph f c e n t -> Vector EdgeIndex
getTreeEdges graph = Builder.build treeEdgesB
  where
    treeVec :: Vector (TreeIndexData (f n) e)
    treeVec = view _treeReferences graph

    treeEdgesB :: Builder EdgeIndex
    treeEdgesB = foldMapWithKey buildEdges treeVec

    buildEdges :: Int -> TreeIndexData (f n) e -> Builder EdgeIndex
    buildEdges ind treeData =
      let
        sourceTaggedIndex :: TaggedIndex
        sourceTaggedIndex = TaggedIndex ind TreeTag

        childTaggedIndices :: TaggedIndex :!: TaggedIndex
        childTaggedIndices = coerce $ view _childInds treeData

        addTwoEdges source target =
          let
            e1 = Builder.singleton
               $ EdgeIndex {edgeSource = source, edgeTarget = view _left target}
            e2 = Builder.singleton
               $ EdgeIndex {edgeSource = source, edgeTarget = view _right target}
          in
            e1 <> e2
      in
        addTwoEdges sourceTaggedIndex childTaggedIndices

getTreeEdgeSet :: forall f c e n t . Graph f c e n t -> Set EdgeIndex
getTreeEdgeSet graph = edgeSet
  where
    treeVec :: Vector (TreeIndexData (f n) e)
    treeVec = view _treeReferences graph

    edgeSet :: Set EdgeIndex
    edgeSet = foldMapWithKey buildEdges treeVec

    buildEdges :: Int -> TreeIndexData (f n) e -> Set EdgeIndex
    buildEdges ind treeData =
      let
        sourceTaggedIndex :: TaggedIndex
        sourceTaggedIndex = TaggedIndex ind TreeTag

        childTaggedIndices :: TaggedIndex :!: TaggedIndex
        childTaggedIndices = coerce $ view _childInds treeData

        addTwoEdges source target =
          let
            e1 = Set.singleton
               $ EdgeIndex {edgeSource = source, edgeTarget = view _left target}
            e2 = Set.singleton
               $ EdgeIndex {edgeSource = source, edgeTarget = view _right target}
          in
            e1 <> e2
      in
        addTwoEdges sourceTaggedIndex childTaggedIndices


getNetworkEdges :: forall f c e n t . Graph f c e n t -> Vector EdgeIndex
getNetworkEdges graph = Builder.build netEdgesB
  where
    netVec :: Vector (NetworkIndexData (f n) e)
    netVec = view _networkReferences graph

    netEdgesB :: Builder EdgeIndex
    netEdgesB = foldMapWithKey buildEdges netVec

    buildEdges :: Int -> NetworkIndexData (f n) e -> Builder.Builder EdgeIndex
    buildEdges ind netData =
      let
        sourceTaggedIndex :: TaggedIndex
        sourceTaggedIndex = TaggedIndex ind NetworkTag

        childTaggedIndex :: TaggedIndex
        childTaggedIndex = coerce $ view _childInds netData
      in
        Builder.singleton $
          EdgeIndex {edgeSource = sourceTaggedIndex, edgeTarget = childTaggedIndex}

getNetworkEdgeSet :: forall f c e n t . Graph f c e n t -> Set EdgeIndex
getNetworkEdgeSet graph = edgeSet
  where
    netVec :: Vector (NetworkIndexData (f n) e)
    netVec = view _networkReferences graph

    edgeSet :: Set EdgeIndex
    edgeSet = foldMapWithKey buildEdges netVec

    buildEdges :: Int -> NetworkIndexData (f n) e -> Set EdgeIndex
    buildEdges ind netData =
      let
        sourceTaggedIndex :: TaggedIndex
        sourceTaggedIndex = TaggedIndex ind NetworkTag

        childTaggedIndex :: TaggedIndex
        childTaggedIndex = coerce $ view _childInds netData
      in
        Set.singleton $
          EdgeIndex {edgeSource = sourceTaggedIndex, edgeTarget = childTaggedIndex}


getRootEdges :: forall f c e n t . Graph f c e n t -> Vector EdgeIndex
getRootEdges graph = Builder.build rootEdgesB
  where
    rootVec :: Vector (RootIndexData (f n) e)
    rootVec = view _rootReferences graph

    rootEdgesB :: Builder EdgeIndex
    rootEdgesB = foldMapWithKey buildEdges rootVec

    buildEdges :: Int -> RootIndexData (f n) e -> Builder EdgeIndex
    buildEdges ind treeData =
      let
        sourceTaggedIndex :: TaggedIndex
        sourceTaggedIndex = TaggedIndex ind RootTag

        childTaggedIndices :: Either TaggedIndex (TaggedIndex :!: TaggedIndex)
        childTaggedIndices = coerce $ view _childInds treeData

        addOneEdge source target =
          Builder.singleton $ EdgeIndex {edgeSource = source, edgeTarget = target}

        addTwoEdges source target =
          let
            e1 = Builder.singleton
               $ EdgeIndex {edgeSource = source, edgeTarget = view _left target}
            e2 = Builder.singleton
               $ EdgeIndex {edgeSource = source, edgeTarget = view _right target}
          in
            e1 <> e2
      in
        either (addOneEdge sourceTaggedIndex) (addTwoEdges sourceTaggedIndex)
          $ childTaggedIndices

getRootEdgeSet :: forall f c e n t . Graph f c e n t -> Set EdgeIndex
getRootEdgeSet graph = edgeSet
  where
    rootVec :: Vector (RootIndexData (f n) e)
    rootVec = view _rootReferences graph

    edgeSet :: Set EdgeIndex
    edgeSet = foldMapWithKey buildEdges rootVec

    buildEdges :: Int -> RootIndexData (f n) e -> Set EdgeIndex
    buildEdges ind treeData =
      let
        sourceTaggedIndex :: TaggedIndex
        sourceTaggedIndex = TaggedIndex ind RootTag

        childTaggedIndices :: Either TaggedIndex (TaggedIndex :!: TaggedIndex)
        childTaggedIndices = coerce $ view _childInds treeData

        addOneEdge source target =
          Set.singleton $ EdgeIndex {edgeSource = source, edgeTarget = target}

        addTwoEdges source target =
          let
            e1 = Set.singleton
               $ EdgeIndex {edgeSource = source, edgeTarget = view _left target}
            e2 = Set.singleton
               $ EdgeIndex {edgeSource = source, edgeTarget = view _right target}
          in
            e1 <> e2
      in
        either (addOneEdge sourceTaggedIndex) (addTwoEdges sourceTaggedIndex)
          $ childTaggedIndices


-- |
-- This returns a graph shape where each data bucket contains
-- those edges with that node as _parent_.
--
-- Note: This means the leafData is empty as there are no edges with leaves as parents.
getEdgeGraphShape :: Graph f c e n t -> GraphShape EdgeIndex EdgeIndex EdgeIndex EdgeIndex
getEdgeGraphShape graph = GraphShape{..}
  where
    leafData    = mempty
    treeData    = getTreeEdges    graph
    networkData = getNetworkEdges graph
    rootData    = getRootEdges    graph


-- |
-- This returns all of the edges from parents to children in the graph.
getEdges :: Graph f c e n t -> Vector EdgeIndex
getEdges graph =
  let
    GraphShape{..} = getEdgeGraphShape graph
  in
    leafData <> (coerce $ treeData <> networkData <> rootData)



-- |
-- Get sibling of a node or return Nothing if one doesn't exist.
getSibling :: TaggedIndex -> Graph f c e n t -> Maybe TaggedIndex
getSibling taggedIndex graph =
  case view _indexType taggedIndex of
    RootTag    -> Nothing
    NetworkTag -> Nothing
    LeafTag    -> f taggedIndex
    TreeTag    -> f taggedIndex
  where
    f index = 
      let
        nodeInfo = indexLeaf graph (view _untaggedIndex index)
        parIndex :: TaggedIndex
        parIndex = coerce $ view _parentInds nodeInfo
        untaggedPar = view _untaggedIndex parIndex
        parTag = view _indexType parIndex
      in
        case parTag of
          TreeTag ->
            let
              l :!: r = coerce $ view _childInds (indexTree graph untaggedPar)
            in
              if index == l then pure r else pure l
          _       -> Nothing


hasRootParent :: TaggedIndex -> Graph f c e n t -> Bool
hasRootParent taggedIndex graph =
  case view _indexType taggedIndex of
    RootTag    -> False
    NetworkTag -> False
    LeafTag    -> f taggedIndex
    RootTag    -> f taggedIndex
  where
    f index =
      let
        nodeInfo = indexLeaf graph (view _untaggedIndex index)
        parTag :: IndexType
        parTag = view (_parentInds . _indexType) nodeInfo
      in
        parTag == RootTag


getParents :: TaggedIndex -> Graph f c e n t -> [TaggedIndex]
getParents taggedIndex graph =
  case view _indexType taggedIndex of
    RootTag -> []
    NetworkTag ->
      let
        nodeInfo = indexNetwork graph (view _untaggedIndex taggedIndex)
        par1 :!: par2 = coerce $ view _parentInds nodeInfo
      in
        [par1, par2]
    LeafTag ->
      let
        nodeInfo = indexLeaf graph (view _untaggedIndex taggedIndex)
        par = coerce $ view _parentInds nodeInfo
      in
        pure par
    TreeTag ->
      let
        nodeInfo = indexTree graph (view _untaggedIndex taggedIndex)
        par = coerce $ view _parentInds nodeInfo
      in
        pure par
