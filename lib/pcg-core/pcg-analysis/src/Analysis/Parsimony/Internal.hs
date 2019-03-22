-----------------------------------------------------------------------------
-- |
-- Module      :  Analysis.Parsimony.Internal
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RecordWildCards       #-}

module Analysis.Parsimony.Internal
  ( PostorderContext(..)
  , PreorderContext (..)
  , ChildContext(..)
  , ParentContext(..)
  , extractNode
  , extractPreNode
  , leafFunction
  , otoChildContext
  , otoParentContext
  , postBinaryFunction
  , postorderContext
  , preBinaryFunction
  , preorderContext
  , preorderContextSym
  , rootFunction
  ) where

import Bio.Graph.Node.Context
import Data.MonoTraversable


{-
-- |
-- A node context for performing a postorder traversal on a binary
-- network with potential in-degree 2, out-degree 1 network nodes.
data  PostorderContext n c
    = LeafContext n
    | PostNetworkContext c
    | PostBinaryContext
      { leftChild  :: c
      , rightChild :: c
      }

-- |
-- Smart elimination principle for a 'PostorderContext' where we do not change
-- the network context.
postorderContext
  :: (n -> c)      -- ^ Leaf context function.
  -> ((c, c) -> c) -- ^ Binary context function.
  -> PostorderContext n c
  -> c
postorderContext leafFn binaryFn = \case
    LeafContext leafNode        -> leafFn leafNode
    PostNetworkContext netChild -> netChild
    PostBinaryContext {..}      -> binaryFn (leftChild, rightChild)


-- |
-- Extract the function on leaves from a function on a 'PostorderContext'.
leafFunction :: (PostorderContext n c -> e) -> (n -> e)
leafFunction postFn = postFn . LeafContext


-- |
-- Extract the function on an internal binary context from a function on a
-- 'PostorderContext'.
postBinaryFunction :: (PostorderContext n c -> e) -> ((c, c) -> e)
postBinaryFunction postFn (leftChild, rightChild) = postFn $ PostBinaryContext{..}


-- |
-- Extracts the node data from a 'PostorderContext'.
extractNode :: PostorderContext c c -> c
extractNode = \case
    LeafContext        leafNode -> leafNode
    PostNetworkContext netChild -> netChild
    PostBinaryContext  {..}     -> leftChild


-- |
-- A data type for the child contexts that can occur in graphs.
data  ChildContext c
    = NoChildren
    | OneChild c
    | TwoChildren c c
    deriving Functor


-- |
-- Construct a 'ChildContext' from a monoTraversable structure ignoring
-- any elements beyond the first two.
otoChildContext :: MonoFoldable t =>  t -> ChildContext (Element t)
otoChildContext xs =
    case otoList xs of
      []      -> NoChildren
      [c]     -> OneChild c
      (l:r:_) -> TwoChildren l r


-- |
-- A node context for performing a preorder traversal on a binary
-- network with possible in-degree 2, out-degree 1 nodes
data  PreorderContext c p
    = RootContext c
    | PreInternalContext
      { preParent       :: p
      , preChildContext :: Either c c
      }


-- |
-- Elimination principle for 'PreorderContext'
preorderContext
  :: (c -> e)               -- ^ Root context function
  -> (Either c c -> p -> e) -- ^ Binary Context function
  -> PreorderContext c p
  -> e
preorderContext rootFn internalFn = \case
  RootContext    rootNode -> rootFn rootNode
  PreInternalContext {..} -> internalFn preChildContext preParent


-- |
-- Elimination principle for 'PreorderContext' that is symmetric in
-- the child.
preorderContextSym
  :: (c -> e)      -- ^ Root context function
  -> (c -> p -> e) -- ^ Binary Context function
  -> PreorderContext c p
  -> e
preorderContextSym rootFn symInternalFn =
    preorderContext rootFn internalFn
  where
    internalFn optN = symInternalFn (either id id optN)


-- |
-- Extract the function on a root context from a function on a
-- 'PreorderContext'.
rootFunction :: (PreorderContext c p -> e) -> (c -> e)
rootFunction preFn = preFn . RootContext


-- |
-- Extract the function on an internal binary context from a function on a
-- 'PreorderContext'.
preBinaryFunction :: (PreorderContext c p -> e) -> (Either c c -> p -> e)
preBinaryFunction preFn optC p = preFn $ PreInternalContext p optC


-- |
-- Extracts the node data from a 'PreorderContext'.
extractPreNode :: PreorderContext c c -> c
extractPreNode = \case
    RootContext        c    -> c
    PreInternalContext {..} -> preParent


-- |
-- A data type for the parent contexts that can occur in graphs.
data  ParentContext p
    = NoParent
    | OneParent p
    | TwoParents p p
    deriving Functor

-- |
-- Construct a 'ParentContext' from a monoTraversable structure ignoring
-- any elements beyond the first two.
otoParentContext :: MonoFoldable t =>  t -> ParentContext (Element t)
otoParentContext xs =
    case otoList xs of
      []      -> NoParent
      [p]     -> OneParent p
      (l:r:_) -> TwoParents l r

-}
