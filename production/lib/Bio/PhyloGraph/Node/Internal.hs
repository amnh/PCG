-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.PhyloGraph.Node.Internal
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Type for a node
--
-----------------------------------------------------------------------------
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}

module Bio.PhyloGraph.Node.Internal where

import Bio.Sequence.Coded
import qualified Bio.PhyloGraph.Node.Encoded as EN
import qualified Bio.PhyloGraph.Node.Final as FN
import qualified Bio.PhyloGraph.Node.Packed as PN
import qualified Bio.PhyloGraph.Node.Preliminary as RN

import Data.Vector (Vector)
import Data.Monoid
import Data.Ord ()

-- | A node data structure holding all the necessary info (add verbose statement about what each field is)
data Node = Node  { code        :: Int
                  , name        :: String
                  , isRoot      :: Bool
                  , isLeaf      :: Bool
                  , parents     :: [Int]
                  , children    :: [Int]
                  , encoded     :: Vector EncodedSeq -- encoded version of original assignment
                  , packed      :: Vector EncodedSeq -- packed version of the sequence
                  , preliminary :: Vector EncodedSeq -- preliminary assignment at a node
                  , final       :: Vector EncodedSeq -- final assignment at a node
                  , temporary   :: Vector EncodedSeq -- multipurpose temporary assignment 
                  , aligned     :: Vector EncodedSeq -- the alignment between the children
                  , random      :: Vector EncodedSeq -- the assignment with a single state randomly selected to remove ambiguity
                  , union       :: Vector EncodedSeq -- the union assignment
                  , single      :: Vector EncodedSeq -- the single assignment
                  , gapped      :: Vector EncodedSeq -- the final assignment with gaps for alignment
                  , localCost   :: Double            -- cost of assignment at this node alone
                  , totalCost   :: Double            -- sum cost of this node and its subtree
                  } deriving (Eq, Show)

instance Monoid Node where
  mempty = Node 0 mempty False False mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty 0 0
  mappend n1 n2 = Node { code        = code n1
                       , name        = name n1 ++ " joinedTo " ++ name n2
                       , isRoot      = isRoot n1 || isRoot n2
                       , isLeaf      = isLeaf n1 || isLeaf n2
                       , parents     = parents     n1 <> parents     n2
                       , children    = children    n1 <> children    n2
                       , encoded     = encoded     n1 <> encoded     n2 
                       , packed      = packed      n1 <> packed      n2
                       , preliminary = preliminary n1 <> preliminary n2
                       , final       = final       n1 <> final       n2 
                       , temporary   = temporary   n1 <> temporary   n2
                       , aligned     = aligned     n1 <> aligned     n2
                       , localCost   = localCost n1 + localCost n2
                       , totalCost   = totalCost n1 + totalCost n2
                       }

-- | Make it an instance of encoded, final, packed, and preliminary
instance EN.EncodedNode Node EncodedSeq where
    getEncoded = encoded
    setEncoded n s = n {encoded = s}

-- | Nodes can hold final assignment
instance FN.FinalNode Node EncodedSeq where
    getFinal = final
    setFinal f n = n {final = f}

-- | Nodes can hold packed data
instance PN.PackedNode Node EncodedSeq where
    getPacked = packed
    setPacked n s = n {packed = s}

-- | Nodes hold all preliminary info
instance RN.PreliminaryNode Node EncodedSeq where
    getPreliminary = preliminary
    setPreliminary s n = n {preliminary = s}
    getPreliminaryAlign = aligned
    setAlign s n = n {aligned = s}
    getTemporary = temporary
    setTemporary s n = n {temporary = s}
    getLocalCost = localCost
    setLocalCost c n = n {localCost = c}
    getTotalCost = totalCost
    setTotalCost c n = n {totalCost = c}

instance Ord Node where
    compare n1 n2 = compare (code n1) (code n2)