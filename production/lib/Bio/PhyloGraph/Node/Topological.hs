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

module Bio.PhyloGraph.Node.Topological (TopoNode(..), arbitraryTopoGivenCAL, arbitraryTopoGivenCSNA) where

import Bio.Character.Dynamic.Coded
import Bio.Character.Dynamic.Coded.Internal
import Bio.Character.Parsed
import Bio.Metadata.Internal
import Data.List.Utility
import Data.Vector (Vector, toList)
import qualified Data.Vector as V (zipWith)
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
     let root = curLevel == 0
     n        <- arbitrary :: Gen String
     nc <- (arbitrary :: Gen Int) `suchThat` (<= maxChildren)
     let ncFinal = if curLevel == maxLevel then 0 else nc
     chillens <- vectorOf ncFinal (arbitraryTopoGivenCAL maxChildren inAlph (curLevel + 1, maxLevel))
     let leaf = ncFinal == 0
     seqs     <- vectorOf 10 (arbitraryDynamicsGA inAlph)
     c2       <- arbitrary :: Gen Double
     c3       <- arbitrary :: Gen Double
     pure $ TopoNode root leaf n chillens (seqs !! 0) (seqs !! 1) (seqs !! 2) (seqs !! 3) (seqs !! 4) (seqs !! 5) (seqs !! 6) (seqs !! 7) (seqs !! 8) (seqs !! 9) c2 c3

arbitraryTopoGivenCSNA :: Int -> [(String, ParsedChars)] -> Vector (CharacterMetadata DynamicChar) -> (Int, Int) -> Gen (TopoNode b)
arbitraryTopoGivenCSNA maxChildren namesAndSeqs inMeta (curLevel, maxLevel) 
  | length namesAndSeqs <= 1 = do
      c2       <- arbitrary :: Gen Double
      c3       <- arbitrary :: Gen Double
      pure $ TopoNode root False myName mempty coded coded mempty mempty mempty mempty mempty mempty mempty mempty c2 c3
  | otherwise = do
      nc <- (arbitrary :: Gen Int) `suchThat` (<= maxChildren)
      let ncFinal = if curLevel == maxLevel then 0 else nc
      let forChildren = chunksOf ncFinal (tail namesAndSeqs)
      chillens <- sequence $ map (\ns -> arbitraryTopoGivenCSNA maxChildren ns inMeta (curLevel + 1, maxLevel)) forChildren
      --chillens <- vectorOf ncFinal (arbitraryTopoGivenCSNA maxChildren (tail namesAndSeqs) inMeta (curLevel + 1, maxLevel))
      let leaf = ncFinal == 0
      c2       <- arbitrary :: Gen Double
      c3       <- arbitrary :: Gen Double
      pure $ TopoNode root leaf myName chillens coded coded mempty mempty mempty mempty mempty mempty mempty mempty c2 c3
    where
      (myName, mySeqs) = head namesAndSeqs
      root = curLevel == 0
      coded = V.zipWith encodeIt inMeta mySeqs
      encodeIt m s = case s of 
                      Nothing -> emptyChar
                      Just c  -> encodeOverAlphabet (alphabet m) c 
