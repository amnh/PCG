{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

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

module Analysis.Parsimony.Internal
  ( PostorderBinaryContext(..)
  , PreorderBinaryContext(..)
  , postorderBinaryContext
  , leafFunction
  , postInternalFunction
  )
  where


-- |
-- A node context for performing a postorder traversal on a binary
-- tree.
data PostorderBinaryContext n c
  = LeafContext n
  | PostInternalContext
    { node       :: n
    , leftChild  :: c
    , rightChild :: c
    }

-- |
-- Elimination for 'PostorderBinaryContext'.
postorderBinaryContext :: (n -> e) -> (n -> (c, c) -> e) -> PostorderBinaryContext n c -> e
postorderBinaryContext leafFn internalFn = \case
  LeafContext d            -> leafFn d
  PostInternalContext {..} -> internalFn node (leftChild, rightChild)

leafFunction :: (PostorderBinaryContext n c -> e) -> (n -> e)
leafFunction postFn = postFn . LeafContext

postInternalFunction :: (PostorderBinaryContext n c -> e) -> (n -> (c, c) -> e)
postInternalFunction postFn = \node (leftChild, rightChild) -> postFn $ PostInternalContext{..}


-- |
-- A node context for performing a preorder traversal on a binary
-- tree.
data PreorderBinaryContext d d'
  = RootContext d
  | PreInternalContext d' (Either d d)
