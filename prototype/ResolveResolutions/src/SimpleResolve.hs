module Resolve where

import Data.List.NonEmpty
import Data.Semigroup
import Data.Tree


-- |
-- Given a node of a tree, return a non-empty set of all the unique
-- ways that node's subtrees can be resolved as binary trees.
resolveNode :: Monoid a => Tree a -> NonEmpty (Tree a)
resolveNode node =
    case subForest node of
      []   -> pure node
      [x]  -> resolveNode x >>= (\y -> pure $ node { subForest = [y] })
      x:xs -> do
          (lhs, rhs)  <- splittings $ x:|xs
          lhsJoined   <- joinSubtrees lhs
          rhsJoined   <- joinSubtrees rhs
          lhsResolved <- resolveNode lhsJoined
          rhsResolved <- resolveNode rhsJoined
          pure $ node { subForest = [lhsResolved, rhsResolved] }


-- |
-- Given a non-empty set of elements, returns all the unique ways
-- to split the input set into  two non-empty sets
splittings :: NonEmpty a -> NonEmpty (NonEmpty a, NonEmpty a)
splittings (x:|[])   = error "Singleton list of chilren supplied"
splittings (x:|[y])  = pure (pure x, pure y)
splittings (x:|y:ys) = pure (pure x, y:|ys) <> do
    e@(lhs, rhs) <- splittings $ y:|ys
    (pure x <> lhs, rhs) :| [(lhs, pure x <> rhs)]


-- |
-- Given a non-empty set of subtrees, this function returns a list of
-- all the ways to join the subtrees together into a single tree.
--
-- Note, that the newly created internal verticies are decorated with
-- the 'mempty' value of the monoidal decoration type.
joinSubtrees :: Monoid a => NonEmpty (Tree a) -> NonEmpty (Tree a)
joinSubtrees e@(x:|[]) = e
joinSubtrees subtrees = do
    (lhs, rhs)  <- splittings subtrees
    lhsResolved <- joinSubtrees lhs
    rhsResolved <- joinSubtrees rhs
    pure $ Node mempty [lhsResolved, rhsResolved]
