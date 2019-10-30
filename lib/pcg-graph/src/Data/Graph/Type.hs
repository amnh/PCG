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
  , GraphBuilder(..)
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

import Control.Lens              hiding (index)
import Data.Graph.Indices
import Data.Graph.NodeContext
import Data.Kind                 (Type)
import Data.Pair.Strict
import Data.Vector               (Vector, generate)
import Data.Vector.Instances     ()
import Test.QuickCheck.Arbitrary
import TextShow                  hiding (Builder)
import VectorBuilder.Builder as VB
import VectorBuilder.Vector as VB
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as M

--      ┌─────────────┐
--      │    Types    │
--      └─────────────┘

{-
data GraphShape i n r t
  = GraphShape
  { leafData    :: Vector t
  , treeData    :: Vector i
  , networkData :: Vector n
  , rootData    :: Vector r
  }
-}


data  Graph
        (f :: Type -> Type)
        (c :: Type)
        (e :: Type)
        (n :: Type)
        (t :: Type)
   = Graph
   { leafReferences    :: Vector (LeafIndexData       t   )
   , treeReferences    :: Vector (TreeIndexData    (f n) e)
   , networkReferences :: Vector (NetworkIndexData (f n) e)
   , rootReferences    :: Vector (RootIndexData    (f n) e)
   , cachedData        :: c
   }
   deriving stock Show


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
