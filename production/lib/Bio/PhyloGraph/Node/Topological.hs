-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.PhyloGraph.Node.Topological
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Module making a topological node similar to an ordinary node
-- allows for good random generation behavior
--
-----------------------------------------------------------------------------

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}

module Bio.PhyloGraph.Node.Topological (TopoNode(..), arbitraryTopoGivenCAL) where

import Bio.Character.Dynamic.Coded
import Bio.Character.Dynamic.Coded.Internal
import Bio.Character.Parsed
import Data.Vector hiding (head)
import Test.Tasty.QuickCheck

-- | A tree construction which stores it's children as pointers. Tree traversal
--   must start from the root node.
data TopoNode b
   = TopoNode
   { isRoot       :: Bool
   , isLeaf       :: Bool
   , name         :: String
   , children     :: [TopoNode b]
   , encoded      :: Vector DynamicChar -- | Encoded version of original assignment.
   , packed       :: Vector DynamicChar -- | Packed version of the sequence.
   , preliminary  :: Vector DynamicChar -- | Preliminary assignment at a node.
   , final        :: Vector DynamicChar -- | Final assignment at a node.
   , temporary    :: Vector DynamicChar -- | Multipurpose temporary assignment.
   , aligned      :: Vector DynamicChar -- | The alignment between the children.
   , random       :: Vector DynamicChar -- | The assignment with a single state randomly selected to remove ambiguity.
   , union        :: Vector DynamicChar -- | The union assignment.
   , single       :: Vector DynamicChar -- | The single assignment.
   , gapped       :: Vector DynamicChar -- | The final assignment with gaps for alignment.
   , localCost    :: Double
   , totalCost    :: Double
   } deriving (Eq, Show)


-- | In a monoid instance, we take mappend to mean a joining of the two subtrees
-- where the second subtree passed becomes a child of the first
instance Monoid (TopoNode b) where
     mempty = TopoNode False False mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty 0 0
     mappend n1 n2 = n1 {children = n2 : children n1}

instance Arbitrary (TopoNode b) where
   arbitrary = do
    arbAlph <- arbitrary :: Gen Alphabet
    nc <- arbitrary :: Gen Int
    arbitraryTopoGivenCAL nc arbAlph (0, 1)

arbitraryTopoGivenCAL :: Int -> Alphabet -> (Int, Int) -> Gen (TopoNode b)
arbitraryTopoGivenCAL maxChildren inAlph (curLevel, maxLevel) = do
     n        <- arbitrary :: Gen String
     root     <- arbitrary :: Gen Bool
     leaf     <- arbitrary :: Gen Bool
     nc <- (arbitrary :: Gen Int) `suchThat` (<= maxChildren)
     let ncFinal = if curLevel == maxLevel then 0 else nc
     chillens <- vectorOf ncFinal (arbitraryTopoGivenCAL maxChildren inAlph (curLevel + 1, maxLevel))
     seqs     <- vectorOf 10 (arbitraryDynamicsGA inAlph)
     c2       <- arbitrary :: Gen Double
     c3       <- arbitrary :: Gen Double
     pure $ TopoNode root leaf n chillens (head seqs) (seqs !! 1) (seqs !! 2) (seqs !! 3) (seqs !! 4) (seqs !! 5) (seqs !! 6) (seqs !! 7) (seqs !! 8) (seqs !! 9) c2 c3
