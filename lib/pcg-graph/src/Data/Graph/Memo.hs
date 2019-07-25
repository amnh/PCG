{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}
module Data.Graph.Memo where

import Data.Graph.Type
import Data.Graph.NodeContext
import Control.Lens
import Data.Key
import Data.Vector.Instances ()
import Data.Graph.Indices
import Data.Coerce
import Control.Applicative

import qualified Data.Vector as V

type Endo a = (a -> a)

data MemoGen i n r l = MemoGen
  { leafGen     :: Int -> l
  , internalGen :: Int -> i
  , networkGen  :: Int -> n
  , rootGen     :: Int -> r
  }

type MemoGen' i l = MemoGen i i i l

type MemoGenGraph i l =
  MemoGen
    (InternalIndexData i)
    (NetworkIndexData  i)
    (RootIndexData     i)
    (LeafIndexData     l)

generateMemoGraph
  :: forall f c e n t
  .  c
  -> Int
  -> Int
  -> Int
  -> Int
  -> Endo (MemoGenGraph (f n) t)
  -> Graph f c e n t
generateMemoGraph cache numL numI numN numR recursiveFuns = memoGraph
  where
    memoGraph :: Graph f c e n t
    memoGraph = generateGraph cache numL numI numN numR (recursiveFuns memoizedFunction)

    memoizedFunction :: MemoGenGraph (f n) t
    memoizedFunction = MemoGen
      { leafGen     = ((memoGraph ^. _leafReferences    ) !)
      , internalGen = ((memoGraph ^. _internalReferences) !)
      , networkGen  = ((memoGraph ^. _networkReferences ) !)
      , rootGen     = ((memoGraph ^. _rootReferences    ) !)
      }


generateGraph
  :: c
  -> Int
  -> Int
  -> Int
  -> Int
  -> MemoGenGraph (f n) t
  -> Graph f c e n t
generateGraph cache numL numI numN numR MemoGen{..} =
  Graph
  { leafReferences     = V.generate numL leafGen
  , internalReferences = V.generate numI internalGen
  , networkReferences  = V.generate numN networkGen
  , rootReferences     = V.generate numR rootGen
  , cachedData         = cache
  }

memoPostorder
  :: forall g f c e n t val . (Applicative g)
  =>  (t    -> val)
  -> (val -> val -> val)
  -> (val -> val)
  -> Graph f c e n t -> Endo (MemoGen' (g val) val)
memoPostorder leafFn internalFn netFn graph = f
  where
    f :: MemoGen' (g val) val -> MemoGen' (g val) val
    f MemoGen{..} = MemoGen leafGen' internalGen' networkGen' rootGen'
      where
        childVal :: TaggedIndex -> g val
        childVal i  = case getTag i of
              LeafTag     -> pure . leafGen  $ (untagValue i)
              InternalTag -> internalGen (untagValue i)
              NetworkTag  -> networkGen  (untagValue i)
              RootTag     -> rootGen     (untagValue i)


        leafGen' :: Int -> val
        leafGen' i = leafFn $
                       graph ^.
                         _leafReferences
                        . singular (ix i)
                        . (_nodeData)

        fromTwoChildren  :: Pair ChildIndex -> g val
        fromTwoChildren c =
            (liftA2 internalFn)
              (childVal (coerce $ c ^. _left))
              (childVal (coerce $ c ^. _right))

        fromOneChild :: ChildIndex -> g val
        fromOneChild c =
          netFn <$> (childVal . coerce $  c)

        getChildInd refs i = graph
                           ^. refs
                            . singular (ix i)
                            . _nodeContext
                            . _childInds

        internalGen' :: Int -> g val
        internalGen' i = fromTwoChildren (getChildInd _internalReferences i)

        networkGen' :: Int -> g val
        networkGen' i = fromOneChild (getChildInd _networkReferences i)

        rootGen' :: Int -> g val
        rootGen' i =
          either
            fromOneChild
            fromTwoChildren
            (getChildInd _rootReferences i)


memoGraphPostorder
  :: forall g f c e n t val . (Applicative g)
  =>  (t    -> val)
  -> (val -> val -> val)
  -> (val -> val)
  -> Graph f c e n t -> Endo (MemoGenGraph (g val) val)
memoGraphPostorder leafFn internalFn netFn graph = f
  where
    f :: MemoGenGraph (g val) val -> MemoGenGraph (g val) val
    f MemoGen{..} = MemoGen leafGen' internalGen' networkGen' rootGen'
      where
        childVal :: TaggedIndex -> g val
        childVal i  = case getTag i of
              LeafTag     -> pure . (^. _nodeData) . leafGen  $ (untagValue i)
              InternalTag -> (^. _nodeData) . internalGen $ (untagValue i)
              NetworkTag  -> (^. _nodeData) . networkGen  $ (untagValue i)
              RootTag     -> (^. _nodeData) . rootGen     $ (untagValue i)


        leafGen' :: Int -> LeafIndexData val
        leafGen' i = leafFn <$>
                       graph ^.
                         _leafReferences
                        . singular (ix i)

        fromTwoChildren  :: Pair ChildIndex -> g val
        fromTwoChildren c =
            (liftA2 internalFn)
              (childVal (coerce $ c ^. _left))
              (childVal (coerce $ c ^. _right))

        fromOneChild :: ChildIndex -> g val
        fromOneChild c =
          netFn <$> (childVal . coerce $  c)

        getChildInd refs i = graph
                           ^. refs
                            . singular (ix i)
                            . _nodeContext
                            . _childInds

        internalGen' :: Int -> InternalIndexData (g val)
        internalGen' i =
          (graph ^. _internalReferences . singular (ix i))
          & _nodeData
          .~ (fromTwoChildren (getChildInd _internalReferences i))

        networkGen' :: Int -> NetworkIndexData (g val)
        networkGen' i =
           (graph ^. _networkReferences . singular (ix i))
           & _nodeData
           .~ fromOneChild (getChildInd _networkReferences i)

        rootGen' :: Int -> RootIndexData (g val)
        rootGen' i =
          (graph ^. _rootReferences . singular (ix i))
          & _nodeData
          .~
            either
              fromOneChild
              fromTwoChildren
              (getChildInd _rootReferences i)
{-
memoPostorder
  :: forall f c e n t val
  .  (t    -> val)
  -> (val -> val -> val)
  -> (val -> val)
  -> Graph f c e n t -> Endo (MemoGen val val val val)
memoPostorder leafFn internalFn netFn graph = f
  where
    f :: MemoGen val val val val -> MemoGen val val val val
    f MemoGen{..} = MemoGen leafGen' internalGen' networkGen' rootGen'
      where
        childVal :: TaggedIndex -> val
        childVal i  = case getTag i of
              LeafTag     -> leafGen     (untagValue i)
              InternalTag -> internalGen (untagValue i)
              NetworkTag  -> networkGen  (untagValue i)
              RootTag     -> rootGen     (untagValue i)

        leafGen' :: Int -> val
        leafGen' i = leafFn $
                       graph ^.
                         _leafReferences
                        . singular (ix i)
                        . (_nodeData)

        fromTwoChildren  :: Pair ChildIndex -> val
        fromTwoChildren c =
            internalFn
              (childVal (coerce $ c ^. _left))
              (childVal (coerce $ c ^. _right))

        fromOneChild :: ChildIndex -> val
        fromOneChild c =
          netFn (childVal . coerce $  c)

        getChildInd refs i = graph
                           ^. refs
                            . singular (ix i)
                            . _nodeContext
                            . _childInds

        internalGen' :: Int -> val
        internalGen' i = fromTwoChildren (getChildInd _internalReferences i)

        networkGen' :: Int -> val
        networkGen' i = fromOneChild (getChildInd _networkReferences i)

        rootGen' i =
          either
            fromOneChild
            fromTwoChildren
            (getChildInd _rootReferences i)

--}
