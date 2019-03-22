-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Graph.ReferenceDAG.Traversal
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}

module Bio.Graph.ReferenceDAG.Traversal where

import           Bio.Graph.Node.Context
import           Bio.Graph.ReferenceDAG.Internal
import           Control.Lens.Operators          ((^.))
import qualified Data.IntMap                     as IM
import qualified Data.IntSet                     as IS
import           Data.Key
import           Data.Vector.Memo                as Memo


-- |
-- A function that recursively builds  a generating function
-- to be consumed as reference data. The function returned uses open recursion
-- (for memoization purposes), in the form of a 'DVector'.
dVectorPostorderWithContext
  :: forall a d e n
   . (  ChildContext (a , Int, IndexData e n) --  Child values and index information
     -> ParentContext Int                     --  Parent indices
     -> (Int, IndexData e n)                  --  Current index information
     -> a                                     --  Index data
     )
  -> ReferenceDAG d e n -> Memo.DVector a
dVectorPostorderWithContext indexFn dag = Memo.DVector f
  where
    refs        = dag ^. _references
    leafInds    = leafIndices dag

 -- A generate function with open recursion
    f :: (Int -> a) -> Int -> a
    f recurseFn ind =
      let parentContext = getParentContext refs ind in
        if ind `IS.member` leafInds
            -- inductive case updating leaves
          then indexFn NoChildren (getParentContext refs ind) (ind, refs ! ind)
          else
            case getChildContext refs ind of
              NoChildren
                -> error "Non-leaf node without children!"
           -- Recursively apply the function
              OneChild childInd
                -> indexFn (OneChild (g childInd)) parentContext (ind, refs ! ind)
              TwoChildren childInd1 childInd2
                -> indexFn
                     (TwoChildren (g childInd1) (g childInd2))
                     parentContext
                     (ind, refs ! ind)
      where
        g :: Int -> (a, Int, IndexData e n)
        g n = (recurseFn n, n, refs ! n)
-- |
-- A function that recursively builds (in a postorder fashion) a generating function
-- to be consumed as reference data. The function returned uses open recursion
-- (for memoization purposes), in the form of a 'Memoector'.
dVectorPostorder
  :: forall a d e n
   . (  ChildContext a       --  Child values
     -> (Int, IndexData e n) --  Current index information
     -> a                    --  Index data
     )
  -> ReferenceDAG d e n -> Memo.DVector a
dVectorPostorder indexFn dag = Memo.DVector f
  where
    refs        = dag ^. _references
    leafInds    = leafIndices dag

 -- A generate function with open recursion
    f :: (Int -> a) -> Int -> a
    f recurseFn ind =
      if ind `IS.member` leafInds
        then indexFn NoChildren (ind, refs ! ind)  -- inductive case updating leaves
        else
          case otoChildContext . IM.keysSet $ (refs ! ind) ^. _childRefs of
            NoChildren
              -> error "Non-leaf node without children!"
            OneChild childInd                           -- recursively apply the function
              -> indexFn
                   (OneChild (recurseFn childInd))
                   (ind, refs ! ind)
            TwoChildren childInd1 childInd2                 -- Same as above.
              -> indexFn
                   (TwoChildren (recurseFn childInd1) (recurseFn childInd2))
                   (ind, refs ! ind)




-- |
-- A function that recursively builds (in a preorder fashion) a generating function
-- to be consumed as reference data. The function returned uses open recursion
-- (for memoization purposes), in the form of a 'Memo.DVector'.
dVectorPreorder
  :: forall a d e n
   . (  ParentContext a      --  Parent data
     -> (Int, IndexData e n) --  Current index information
     -> a                    --  Index data
     )
  -> ReferenceDAG d e n -> Memo.DVector a
dVectorPreorder indexFn dag = Memo.DVector f
  where
    refs        = dag ^. _references
    rootInds    = rootRefs dag

 -- A generate function with open recursion
    f :: (Int -> a) -> Int -> a
    f recurseFn ind =
      if ind `elem` rootInds
        then indexFn NoParent (ind, refs ! ind)  -- base case updating roots
        else
          case otoParentContext $ (refs ! ind) ^. _parentRefs of
            NoParent
              -> error "Non-root node without parents!"
            OneParent parInd                           -- recursively apply the function
              -> indexFn
                   (OneParent (recurseFn parInd))
                   (ind, refs ! ind)
            TwoParents parInd1 parInd2                 -- Same as above.
              -> indexFn
                   (TwoParents (recurseFn parInd1) (recurseFn parInd2))
                   (ind, refs ! ind)

-- |
-- A function that recursively builds (in a preorder fashion) a generating function
-- to be consumed as reference data. The function returned uses open recursion
-- (for memoization purposes), in the form of a 'Memo.DVector'.
dVectorPreorderWithContext
  :: forall a d e n
   . (  ParentContext (a, Int, IndexData e n)  -- Parent data with their index information
     -> ChildContext Int                       -- Child indicies
     -> (Int, IndexData e n)                   -- Current index information
     -> a                                      -- Index data
     )
  -> ReferenceDAG d e n -> Memo.DVector a
dVectorPreorderWithContext indexFn dag = Memo.DVector f
  where
    refs        = dag ^. _references
    rootInds    = rootRefs dag

 -- A generate function with open recursion
    f :: (Int -> a) -> Int -> a
    f recurseFn ind =
      let childContext = getChildContext refs ind in
        if ind `elem` rootInds
            -- base case updating roots
          then indexFn NoParent childContext (ind, refs ! ind)
          else
              -- apply openly recursive argument
            case getParentContext refs ind of
              NoParent
                -> error "Non-root node without parents!"
              OneParent parInd
                -> indexFn
                     (OneParent (g parInd)) childContext (ind, refs ! ind)
              TwoParents parInd1 parInd2
                -> indexFn (TwoParents (g parInd1) (g parInd2)) childContext (ind, refs ! ind)

      where
        g :: Int -> (a, Int, IndexData e n)
        g n = (recurseFn n, n, refs ! n)
