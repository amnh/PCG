-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.PhyloGraphPrime.ZipperDAG.Internal
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- The Phylogentic Graph types.
--
-- 
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveTraversable, GeneralizedNewtypeDeriving, TypeFamilies #-}

module Bio.PhyloGraphPrime.ZipperDAG.Internal where

import Data.Bifunctor
import Data.Monoid 

-- |
-- A node in the zipper structure.
--
-- * The node's decoration value can be accessed through the 'Cursor' instance functions.
-- * The node's edges can bew extracted with 'zipperEdges'.
data ZipperNode e n = ZNode n [InternalEdge n e]


-- |
-- An edge of the zipper structure.
--
-- * The edge's decoration value can be accessed through the 'Cursor' instance functions.
-- * The edge's parent node can be accessed with 'zipperEdgeParent'.
-- * The edge's child  node can be accessed with 'zipperEdgeChild'.
data ZipperEdge n e = ZEdge EdgeDirection e (ZipperNode e n) (ZipperNode e n)


-- WARN: Do not export
-- |
-- This edge does not have bidirectional pointers.
-- 
-- If bidirectional pointers to parent and child existed, then the entire graph
-- would be reconstructed on an modification of any node or edge data!
--
-- When 'zipperEdges' is called, 'ZipperEdge' values containing bidirectional
-- references for the parent and child 'ZipperNode's of the edge are created.
-- By only creating bidirectional references on demand while zipping locally,
-- we circumvebnt the problem of reconstructing the entire graph upon local
-- cursor modification that is present with a more naive data definition.
data InternalEdge n e = IEdge EdgeDirection e (ZipperNode e n)


-- WARN: Do not export
-- |
-- The direction of the edge in a ZipperDAG.
data EdgeDirection = Leftward | Rightward


-- | (✔)
instance Bifunctor ZipperNode where

    bimap f g (ZNode nDatum iEdges) = ZNode (g nDatum) $ h <$> iEdges
      where
        h (IEdge dir eDatum node) = IEdge dir (f eDatum) $ bimap f g node


-- | (✔)
instance Cursor (ZipperNode e) where

    adjustCursor f (ZNode datum edges) = f datum `ZNode` edges

    getCursor (ZNode datum _) = datum

    setCursor e (ZNode _ edges) = e `ZNode` edges


-- | (✔)
instance Functor (ZipperNode e) where

    -- We use the more general Bifunctor instance to define the Functor instance
    fmap = second


-- | (✔)
instance Bifunctor ZipperEdge where

    bimap f g (ZEdge dir datum lhs rhs) = ZEdge dir (g datum) (bimap g f lhs) (bimap g f rhs)


-- | (✔)
instance Cursor (ZipperEdge n) where

    adjustCursor f (ZEdge dir datum lhs rhs) = ZEdge dir (f datum) lhs rhs

    getCursor (ZEdge _ datum _ _) = datum

    setCursor e (ZEdge dir _ lhs rhs) = ZEdge dir e lhs rhs


-- | (✔)
instance Functor (ZipperEdge n) where

    -- We use the more general Bifunctor instance to define the Functor instance
    fmap = second


-- |
-- Zipper types with an accessible cursor. The instance methods should be
-- implemented in constant time for this type-class to be useful.
class Cursor f where

    {-# MINIMAL getCursor, (setCursor | adjustCursor) #-}

    -- |
    -- Monomorphically modify the value at the zipper's current cursor position
    -- with the supplied function.
    adjustCursor :: (a -> a) -> f a -> f a
    adjustCursor f c = setCursor e c
      where
        e = f $ getCursor c

    -- |
    -- Retreive the value at the zipper's current cursor position.
    getCursor    ::  f a -> a

    -- |
    -- Overwrite the value at the zipper's current cursor position.
    setCursor    ::  a -> f a -> f a
    setCursor e = adjustCursor (const e)


-- |
-- /O(e)/
--
-- Retrieves each 'ZipperEdge' of a 'ZipperNode'.
zipperEdges :: ZipperNode e n -> [ZipperEdge n e]
zipperEdges origin@(ZNode _ internalEdges) = f <$> internalEdges
  where
    f (IEdge dir datum node) = ZEdge dir datum origin node 


-- |
-- /O(1)/
--
-- Retrieves the parent 'ZipperNode' of a 'ZipperEdge'
zipperEdgeParent :: ZipperEdge n e -> ZipperNode e n
zipperEdgeParent (ZEdge dir _ lhs rhs) =
    case dir of
      Leftward  -> rhs
      Rightward -> lhs


-- |
--
-- /O(1)/
-- Retrieves the /child/ 'ZipperNode' of a 'ZipperEdge'
zipperEdgeChild :: ZipperEdge n e -> ZipperNode e n
zipperEdgeChild (ZEdge dir _ lhs rhs) =
    case dir of
      Leftward  -> lhs
      Rightward -> rhs


-- | Build the graph functionally from a generating function.
unfoldDAG :: (b -> ([(e,b)], n, [(e,b)])) -> b -> ZipperNode e n
unfoldDAG f origin = ZNode nDatum $ (pApply <$> parents) <> (cApply <$> children)
  where
    (parents, nDatum, children) = f origin
    pApply (eDatum, seed) = IEdge Leftward  eDatum (unfoldDAG f seed)
    cApply (eDatum, seed) = IEdge Rightward eDatum (unfoldDAG f seed)
