------------------------------------------------------------------------------
-- |
-- Module      :  Data.Graph.Memo
-- Copyright   :  (c) 2015-2021 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Graph.Memo
  ( MemoGen
  , MemoGenGraph
  , generateMemoGraph
  , memoGraphPostorder
  ) where

import           Control.Lens
import           Data.Coerce
import           Data.Graph.Indices
import           Data.Graph.NodeContext
import           Data.Graph.Type
import           Data.Key
import           Data.Maybe             (fromJust)
import           Data.Monoid            (Endo(..))
import           Data.Pair.Strict
import qualified Data.Vector            as V
import           Data.Vector.Instances  ()


-- |
-- Memoization generator to improve graph traversal performance.
data  MemoGen i n r l
    = MemoGen
    { leafGen    :: Int -> l
    , treeGen    :: Int -> i
    , networkGen :: Int -> n
    , rootGen    :: Int -> r
    }


-- |
-- Memoization of one or more graph traversals.
type  MemoGenGraph i l e
    = MemoGen
        (TreeIndexData     i e)
        (NetworkIndexData  i e)
        (RootIndexData     i e)
        (LeafIndexData     l)


-- type MemoGen' i l = MemoGen i i i l


-- |
-- Perform a memoization at the specified indices of the 4 node vectors.
generateMemoGraph
  :: forall f c e n t
  .  c
  -> Int
  -> Int
  -> Int
  -> Int
  -> Endo (MemoGenGraph (f n) t e)
  -> Graph f c e n t
generateMemoGraph cache numL numI numN numR (Endo recursiveFuns) = memoGraph
  where
    memoGraph :: Graph f c e n t
    memoGraph = generateGraph cache numL numI numN numR (recursiveFuns memoizedFunction)

    memoizedFunction :: MemoGenGraph (f n) t e
    memoizedFunction = MemoGen
      { leafGen     = ((memoGraph ^. _leafReferences   ) !)
      , treeGen     = ((memoGraph ^. _treeReferences   ) !)
      , networkGen  = ((memoGraph ^. _networkReferences) !)
      , rootGen     = ((memoGraph ^. _rootReferences   ) !)
      }


generateGraph
  :: c
  -> Int
  -> Int
  -> Int
  -> Int
  -> MemoGenGraph (f n) t e
  -> Graph f c e n t
generateGraph cache numL numI numN numR MemoGen{..} =
  Graph
  { leafReferences     = V.generate numL leafGen
  , treeReferences     = V.generate numI treeGen
  , networkReferences  = V.generate numN networkGen
  , rootReferences     = V.generate numR rootGen
  , cachedData         = cache
  }


-- |
-- Perform a memoized a post-order graph traversal.
--
-- Multiple traversals are composable via the 'Endo' return result.
memoGraphPostorder
  :: forall g f c e n t val . (Applicative g)
  => (t    -> val)
  -> (g val -> g val -> g val)
  -> (val -> val)
  -> Graph f c e n t -> Endo (MemoGenGraph (g val) val e)
memoGraphPostorder leafFn treeFn netFn graph = Endo f
  where
    f :: MemoGenGraph (g val) val e -> MemoGenGraph (g val) val e
    f MemoGen{..} = MemoGen leafGen' treeGen' networkGen' rootGen'
      where
        childVal :: TaggedIndex -> g val
        childVal i  = case getTag i of
              LeafTag    -> pure . (^. _nodeData) . leafGen     $ getIndex i
              TreeTag    ->        (^. _nodeData) . treeGen     $ getIndex i
              NetworkTag ->        (^. _nodeData) . networkGen  $ getIndex i
              RootTag    ->        (^. _nodeData) . rootGen     $ getIndex i

        leafGen' :: Int -> LeafIndexData val
        leafGen' i = leafFn <$>
                       fromJust (preview (_leafReferences . ix i) graph)

        fromTwoChildren  :: ChildIndex :!: ChildIndex -> g val
        fromTwoChildren c =
            treeFn
              (childVal (coerce $ c ^. _left))
              (childVal (coerce $ c ^. _right))

        fromOneChild :: ChildIndex -> g val
        fromOneChild c =
          netFn <$> (childVal . coerce $  c)

        getChildInd refs i = fromJust $
                               preview
                                 ( refs
                                 . ix i
                                 . _nodeContext
                                 . _childInds)
                                 graph


        treeGen' :: Int -> TreeIndexData (g val) e
        treeGen' i =
          fromJust (preview ( _treeReferences . ix i) graph)
          & _nodeData
          .~ fromTwoChildren (getChildInd _treeReferences i)

        networkGen' :: Int -> NetworkIndexData (g val) e
        networkGen' i =
           fromJust (preview (_networkReferences . ix i) graph)
           & _nodeData
           .~ fromOneChild (getChildInd _networkReferences i)

        rootGen' :: Int -> RootIndexData (g val) e
        rootGen' i =
          fromJust (preview (_rootReferences . ix i) graph)
          & _nodeData
          .~
            either
              fromOneChild
              fromTwoChildren
              (getChildInd _rootReferences i)


{-
memoPostorder
  :: forall g f c e n t val . (Applicative g)
  => (t  -> val)
  -> (g val -> g val -> g val)
  -> (val -> val)
  -> Graph f c e n t -> Endo (MemoGen' (g val) val)
memoPostorder leafFn treeFn netFn graph = Endo f
  where
    f :: MemoGen' (g val) val -> MemoGen' (g val) val
    f MemoGen{..} = MemoGen leafGen' treeGen' networkGen' rootGen'
      where
        childVal :: TaggedIndex -> g val
        childVal i  = case getTag i of
              LeafTag    -> pure . leafGen     $ getIndex i
              TreeTag    ->        treeGen     $ getIndex i
              NetworkTag ->        networkGen  $ getIndex i
              RootTag    ->        rootGen     $ getIndex i


        leafGen' :: Int -> val
        leafGen' i = leafFn (graph ^?! (_leafReferences . ix i . _nodeData))
--        leafGen' i = leafFn . fromJust $
--                       graph ^?
--                         _leafReferences
--                        . ix i
--                        . _nodeData

        fromTwoChildren  :: ChildIndex :!: ChildIndex -> g val
        fromTwoChildren c =
            treeFn
              (childVal (coerce $ c ^. _left))
              (childVal (coerce $ c ^. _right))

        fromOneChild :: ChildIndex -> g val
        fromOneChild c =
          netFn <$> (childVal . coerce $  c)

        getChildInd refs i = graph ^?! (refs . ix i . _nodeContext . _childInds)

        treeGen' :: Int -> g val
        treeGen' i = fromTwoChildren (getChildInd _treeReferences i)

        networkGen' :: Int -> g val
        networkGen' i = fromOneChild (getChildInd _networkReferences i)

        rootGen' :: Int -> g val
        rootGen' i =
          either
            fromOneChild
            fromTwoChildren
            (getChildInd _rootReferences i)
-}


{-
memoPostorder
  :: forall f c e n t val
  .  (t    -> val)
  -> (val -> val -> val)
  -> (val -> val)
  -> Graph f c e n t -> Endo (MemoGen val val val val)
memoPostorder leafFn treeFn netFn graph = Endo f
  where
    f :: MemoGen val val val val -> MemoGen val val val val
    f MemoGen{..} = MemoGen leafGen' treeGen' networkGen' rootGen'
      where
        childVal :: TaggedIndex -> val
        childVal i  = case getTag i of
              LeafTag     -> leafGen     (getIndex i)
              TreeTag -> treeGen (getIndex i)
              NetworkTag  -> networkGen  (getIndex i)
              RootTag     -> rootGen     (getIndex i)

        leafGen' :: Int -> val
        leafGen' i = leafFn $
                       graph ^.
                         _leafReferences
                        . singular (ix i)
                        . (_nodeData)

        fromTwoChildren  :: Pair ChildIndex -> val
        fromTwoChildren c =
            treeFn
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

        treeGen' :: Int -> val
        treeGen' i = fromTwoChildren (getChildInd _treeReferences i)

        networkGen' :: Int -> val
        networkGen' i = fromOneChild (getChildInd _networkReferences i)

        rootGen' i =
          either
            fromOneChild
            fromTwoChildren
            (getChildInd _rootReferences i)
--}
