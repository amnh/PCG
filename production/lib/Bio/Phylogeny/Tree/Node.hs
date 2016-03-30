-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Phylogeny.Graph
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Types for the representation of a node
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Bio.Phylogeny.Tree.Node where

import Bio.Sequence.Coded
import qualified Bio.Phylogeny.Tree.Node.Encoded as EN
import qualified Bio.Phylogeny.Tree.Node.Final as FN
import qualified Bio.Phylogeny.Tree.Node.Packed as PN
import qualified Bio.Phylogeny.Tree.Node.Preliminary as RN

import Data.Vector
import Data.Monoid
import Data.Ord ()

-- | A node data structure holding all the necessary info (add verbose statement about what each field is)
data Node b = Node  { code        :: Int
                    , isRoot      :: Bool
                    , isLeaf      :: Bool
                    , parents     :: [Int]
                    , children    :: [Int]
                    , encoded     :: Vector EncodedSeq
                    , packed      :: Vector EncodedSeq
                    , preliminary :: Vector EncodedSeq
                    , final       :: Vector EncodedSeq
                    , temporary   :: Vector EncodedSeq -- TODO: is this necessary? rename to fitch scratch? 
                    , aligned     :: Vector EncodedSeq -- TODO: rename to implied alignment
                    , localCost   :: Double
                    , totalCost   :: Double
                    } deriving (Eq, Show)
                    -- TODO: subtree representation to say if something is done
                    -- TODO: add a union labeling
                    -- TODO: add a single labeling
                    -- TODO: annotate fields with purpose

instance Monoid (Node b) where
    mempty = Node 0 False False mempty mempty mempty mempty mempty mempty mempty mempty 0 0
    mappend n1 n2 = Node { code        = code n1
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
instance EN.EncodedNode (Node b) EncodedSeq where
    encoded = encoded
    setEncoded n s = n {encoded = s}

-- | Nodes can hold final assignment
instance FN.FinalNode (Node b) EncodedSeq where
    final = final
    setFinal f n = n {final = f}

-- | Nodes can hold packed data
instance PN.PackedNode (Node b) EncodedSeq where
    packed = packed
    setPacked n s = n {packed = s}

-- | Nodes hold all preliminary info
instance RN.PreliminaryNode (Node b) EncodedSeq where
    preliminary = preliminary
    setPreliminary s n = n {preliminary = s}
    preliminaryAlign = aligned
    setAlign s n = n {aligned = s}
    temporary = temporary
    setTemporary s n = n {temporary = s}
    localCost = localCost
    setLocalCost c n = n {localCost = c}
    totalCost = totalCost
    setTotalCost c n = n {totalCost = c}

instance Eq b => Ord (Node b) where
    compare n1 n2 = compare (code n1) (code n2)
