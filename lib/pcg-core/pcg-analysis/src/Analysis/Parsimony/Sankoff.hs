-----------------------------------------------------------------------------
-- |
-- Module      :  Analysis.Parsimony.Sankoff
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Sankoff character analysis (cost and median)
--
-- This only works on static characters, and due to the traversal only one
-- character will be received at a time.
--
-- Goloboff’s algorithm relies on computing the “extra cost” for each non-
-- optimal state assignment for each node. There’s a preliminary extra
-- cost, which is the difference between the assignment cost for this state
-- on this node, and a final extra cost, which is the total extra cost
-- when recursing over the whole tree that this state assignment implies.
-- This involves, then, computing extra costs even for states that are non-
-- optimal.
--
-- Assumes binary trees.
--
-----------------------------------------------------------------------------

module Analysis.Parsimony.Sankoff
  ( sankoffPreorder
  , sankoffPostorder
  , sankoffPostorderPairwise
  ) where

import Analysis.Parsimony.Sankoff.Internal
