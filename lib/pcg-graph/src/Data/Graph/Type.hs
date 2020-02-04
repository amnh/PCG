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
  ( Graph(..)
  , GraphShape(..)
  , GraphBuilder(..)
  , MGraph(..)
  , numberOfNodes
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
  , leafGB
  , treeGB
  , rootGB
  , networkGB
  , HasTreeReferences(..)
  , HasNetworkReferences(..)
  , HasRootReferences(..)
  , HasLeafReferences(..)
  , HasCachedData(..)
  , RootFocusGraph
  , Focus
  , makeRootFocusGraphs
  , buildGraph
  , index
  , getRootInds
  , unsafeLeafInd
  , unsafeRootInd
  , unsafeTreeInd
  , unsafeNetworkInd
  )where

import Prelude hiding (read)
import Control.Lens              hiding (index)
import Data.Graph.Indices
import Data.Graph.NodeContext
import Data.Kind                 (Type)
import Data.Pair.Strict
import Data.Hashable
import VectorBuilder.Vector
import VectorBuilder.Builder
import Data.Set (Set)
import qualified Data.Set as S
import Control.Monad.State.Strict
import Data.Maybe (catMaybes)
import Control.Arrow (first)
import Control.Lens.Tuple
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Hashable
import Data.List.Extra (maximumOn)
import Data.Foldable (toList)
import Data.Vector               (Vector, generate, unsafeFreeze, unsafeThaw)
import Data.Vector.Instances     ()
import Test.QuickCheck.Arbitrary
import TextShow                  hiding (Builder)
import VectorBuilder.Builder as VB
import VectorBuilder.Vector as VB
import Data.Vector.Mutable (MVector, new, write, read)
import Control.Monad.Primitive


--      ┌─────────────┐
--      │    Types    │
--      └─────────────┘

-- |
-- The GraphShape type is for storing data in the same `shape` as our graph.
data GraphShape i n r t
  = GraphShape
  { leafData    :: Vector t
  , treeData    :: Vector i
  , networkData :: Vector n
  , rootData    :: Vector r
  }



data  Graph
        (f :: Type -> Type)
        (c :: Type)
        (e :: Type)
        (n :: Type)
        (t :: Type)
   = Graph
   { leafReferences    :: {-# unpack #-} !(Vector (LeafIndexData       t   ))
   , treeReferences    :: {-# unpack #-} !(Vector (TreeIndexData    (f n) e))
   , networkReferences :: {-# unpack #-} !(Vector (NetworkIndexData (f n) e))
   , rootReferences    :: {-# unpack #-} !(Vector (RootIndexData    (f n) e))
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
   { leafReferencesBuilder    :: {-# UNPACK #-} !(Builder (LeafIndexData       t   ))
   , treeReferencesBuilder    :: {-# UNPACK #-} !(Builder (TreeIndexData    (f n) e))
   , networkReferencesBuilder :: {-# UNPACK #-} !(Builder (NetworkIndexData (f n) e))
   , rootReferencesBuilder    :: {-# UNPACK #-} !(Builder (RootIndexData    (f n) e))
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
      leafReferences    = build leafReferencesBuilder
      treeReferences    = build treeReferencesBuilder
      networkReferences = build networkReferencesBuilder
      rootReferences    = build rootReferencesBuilder
    in
      Graph{..}

leafGB :: LeafIndexData t -> GraphBuilder f e n t
leafGB leafInd =
  GraphBuilder (VB.singleton leafInd) mempty mempty mempty

treeGB :: TreeIndexData (f n) e -> GraphBuilder f e n t
treeGB treeInd =
  GraphBuilder mempty (VB.singleton treeInd) mempty mempty

networkGB :: NetworkIndexData (f n) e -> GraphBuilder f e n t
networkGB networkInd =
  GraphBuilder mempty mempty (VB.singleton networkInd) mempty

rootGB :: RootIndexData (f n) e -> GraphBuilder f e n t
rootGB rootInd =
  GraphBuilder mempty mempty mempty (VB.singleton rootInd)


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


--      ┌───────────────┐
--      │    Utility    │
--      └───────────────┘

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


getRootInds :: Graph f c e n t -> Vector TaggedIndex
getRootInds graph =
  let
    numberOfRoots = length (view _rootReferences graph)
    roots = generate numberOfRoots (`TaggedIndex` RootTag)
  in
    roots


size :: Builder a -> Int
size = length . build @Vector


data RoseTree e t = RoseTree
  { root      :: t
  , subForest :: [(RoseTree e t, e)]
  }

roseBranch :: (Monoid t, Monoid e) => RoseForest e t -> RoseTree e t
roseBranch forests =
  let
    labelledForests = fmap (\f -> (f,mempty)) forests
  in
    RoseTree mempty labelledForests

type RoseForest e t = [RoseTree e t]


class (Hashable t) => Fresh t where
  -- We ask that `hash . fromInt` gives the identity
  fromInt :: Int -> t
  fresh   :: HashSet t -> t
  fresh hs =
    let
      m = maximumOn hash . toList $ hs
    in
      fromInt . (+ 1) . hash $ m
                
unfoldToRoseForest
  :: forall a e t. (Eq a, Show a, Monoid e, Ord a, Hashable t, Eq t)
  => (a -> ([(a,e)], t, [(a,e)]))
  -> a
  -> (RoseForest e t, HashSet t)
unfoldToRoseForest unfoldFn seed = (unfoldFromRoots roots, nodeIDs)
  where
    (roots, nodeIDs) = rootsAndNodes

    rootsAndNodes :: ([a], HashSet t)
    rootsAndNodes = findRootsFrom seed `runState` mempty

    isRoot :: a -> [b] -> Maybe a
    isRoot start pars =
        if null pars
          then (Just start)
          else Nothing

    addRoot :: a -> State (HashSet t) (Maybe a)
    addRoot start = do
      let (pars, val, childs) = unfoldFn start
      seenNodes <- get
      put (val `HS.insert` seenNodes)
      case val `HS.member` seenNodes of
        True  -> pure Nothing
        False ->
          do
            pure $ isRoot start pars

    findRootsFrom :: a -> State (HashSet t) [a]
    findRootsFrom start = do
      let (pars, val, childs) = unfoldFn start
      let otherNodes = fst <$> pars <> childs
      startRoot <- addRoot start
      let
        otherRootNodes :: State (HashSet t) [a]
        otherRootNodes = catMaybes <$> traverse addRoot otherNodes

      case startRoot of
        Nothing -> otherRootNodes
        Just r  -> (r :) <$> otherRootNodes  


    unfoldFromRoots :: [a] -> RoseForest e t
    unfoldFromRoots = fmap unfoldFromRoot

    unfoldFromRoot :: a -> RoseTree e t
    unfoldFromRoot root =
      let
        (pars, val, childs) = unfoldFn root
        subtrees = first unfoldFromRoot <$> childs
        parVals  = first ((view _2) . unfoldFn) <$> pars
      in
        RoseTree
          { root = val
          , subForest = undefined --(subtrees, parVals)
          }

-- |
-- This function will convert a rose tree into a binary tree which we then convert to our internal format.
normaliseRoseTree
  :: forall e t
  .  (Ord t, Fresh t, Monoid e)
  => (RoseTree e t, HashSet t) -> RoseTree e t
normaliseRoseTree (rt, nodeIDs) = undefined


createBinaryForest :: (Monoid t, Monoid e) => RoseForest e t -> RoseForest e t
createBinaryForest forest =
  case length forest of
    1 -> forest
    2 -> forest
    3 ->
      let
        (left : right) = forest
      in
        [left, roseBranch right]
    n ->
      let
        lenHalf = n `div` 2
        (leftTrees, rightTrees) = splitAt lenHalf forest
        leftBin  = createBinaryForest leftTrees
        rightBin = createBinaryForest rightTrees
      in
        [roseBranch leftBin, roseBranch rightBin]
        

fromRoseTree :: RoseTree e t -> Graph Identity () e t t
fromRoseTree = undefined
  
      
      
-- |
-- This function is intended as a way to convert from unstructured
-- external tree formats to our *internal* phylogenetic binary networks.
-- It is not intended to be used for internal logic.
unfoldGraph
  :: forall a e t. (Eq a, Show a, Monoid e)
  => (a -> ([(a,e)], t, [(a,e)]))
  -> a
  -> Graph Identity () e t t
unfoldGraph unfoldFn seed = undefined
  where
    go :: a -> GraphBuilder Identity e t t -> GraphBuilder Identity e t t
    go a (GraphBuilder currTreeRefs currLeafRefs currNetRefs currRootRefs)
      = case unfoldFn  a of
          ([], t, [(par, e)]) ->
            let
              newInd = undefined
            in
              undefined
    
  
--      ┌───────────────────────┐
--      │    Unsafe Indexing    │
--      └───────────────────────┘

{-# INLINE unsafeLeafInd #-}
unsafeLeafInd    :: Graph f c e n t -> LeafInd -> LeafIndexData t
unsafeLeafInd graph (LeafInd i) = graph ^. _leafReferences . singular (ix i)

{-# INLINE unsafeTreeInd #-}
unsafeTreeInd    :: Graph f c e n t -> TreeInd -> TreeIndexData (f n) e
unsafeTreeInd graph (TreeInd i) = graph ^. _treeReferences . singular (ix i)

{-# INLINE unsafeRootInd #-}
unsafeRootInd    :: Graph f c e n t -> RootInd -> RootIndexData (f n) e
unsafeRootInd graph (RootInd i) = graph ^. _rootReferences . singular (ix i)

{-# INLINE unsafeNetworkInd #-}
unsafeNetworkInd :: Graph f c e n t -> NetworkInd -> NetworkIndexData (f n) e
unsafeNetworkInd graph (NetworkInd i) = graph ^. _networkReferences . singular (ix i)



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
