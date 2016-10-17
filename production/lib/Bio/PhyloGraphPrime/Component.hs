-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.PhyloGraphPrime.Component
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Bio.PhyloGraphPrime.Component where

import Data.List.NonEmpty

-- newtype PhylogeneticComponent a = PhylogeneticComponent a


class PhylogeneticComponent t i e n | t -> i, t -> n, t -> e where

    parents   :: i -> t -> [i]

    children  :: i -> t -> [i]

    neighbors :: i -> t -> [i]

    roots     :: t -> [i]

    leaves    :: t -> [i]

    nodeCount :: t -> Int

    nodeDatum :: i -> t -> n

    edgeDatum :: (i,i) -> t -> Maybe e

    isComponentNode, isNetworkNode, isTreeNode, isLeafNode, isRootNode :: i -> t -> Bool

    networks :: t -> NonEmpty t


class PhylogeneticNetwork t i e n | t -> i, t -> n, t -> e where

    root  :: t -> i

    trees :: t -> NonEmpty t


class PhylogeneticTree t i e n | t -> i, t -> n, t -> e where

    parent :: i -> t -> Maybe i

    binaryTrees :: t -> NonEmpty t


class PhylogeneticBinaryTree t i e n | t -> i, t -> n, t -> e where

    bifurcation :: i -> t -> Maybe (i,i)
