{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable #-}

module Resolve where

import Data.Foldable
import Data.List.NonEmpty
import Data.Semigroup
import Data.Tree

data  BinaryTree a
    = Leaf a
    | Branch (BinaryTree a) (BinaryTree a)
    deriving (Eq, Foldable, Functor, Traversable, Ord, Show)


unfoldr
  :: (a -> [a]) -- ^ Generating function (extract children)
  -> a          -- ^ Seed value (root node)
  -> NonEmpty (BinaryTree a)
unfoldr kids = resolveNode
  where
    -- |
    -- Given a node of a tree, return a non-empty set of all the unique
    -- ways that node's subtrees can be resolved as binary trees.
    resolveNode node =
        case kids node of
          []   -> pure $ Leaf node
          [x]  -> resolveNode x
          x:xs -> do
              -- What are all the unique combinations of ways
              -- I can split my children into two non-empty sets.
              (lhs, rhs)  <- splittings $ x:|xs
              -- Construct all possible binary trees where
              -- the subtrees in each partition are the leaf set
              lhsJoined   <- joinSubtrees lhs
              rhsJoined   <- joinSubtrees rhs
              -- Recursively resolve the nodes of the subtrees
              lhsResolved <- traverse resolveNode lhsJoined
              rhsResolved <- traverse resolveNode rhsJoined
              -- Splice partitions together
              pure $ Branch (collapse lhsResolved) (collapse rhsResolved)


-- |
-- Given a binary tree with leaves of binary trees, construct binary tree.
collapse :: BinaryTree (BinaryTree a) -> BinaryTree a
collapse (Leaf x) = x
collapse (Branch lhs rhs) = Branch (collapse lhs) $ collapse rhs


-- |
-- Given a non-empty set of subtrees, this function returns a list of
-- all the ways to join the subtrees together into a single tree.
--
-- Note, that the newly created internal verticies are decorated with
-- the 'mempty' value of the monoidal decoration type.
joinSubtrees :: NonEmpty a -> NonEmpty (BinaryTree a)
joinSubtrees (x:|[])  = pure $ Leaf x
joinSubtrees subtrees = do
    (lhs, rhs)  <- splittings subtrees
    lhsResolved <- joinSubtrees lhs
    rhsResolved <- joinSubtrees rhs
    pure $ Branch lhsResolved rhsResolved


-- |
-- Given a non-empty set of elements, returns all the unique ways
-- to split the input set into  two non-empty sets
splittings :: NonEmpty a -> NonEmpty (NonEmpty a, NonEmpty a)
splittings (x:|[])   = error "Singleton list of children supplied"
splittings (x:|[y])  = pure (pure x, pure y)
splittings (x:|y:ys) = pure (pure x, y:|ys) <> do
    e@(lhs, rhs) <- splittings $ y:|ys
    (pure x <> lhs, rhs) :| [(lhs, pure x <> rhs)]
