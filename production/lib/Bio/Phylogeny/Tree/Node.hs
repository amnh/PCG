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

-- | A node data structure holding all the necessary info (add verbose statement about what each field is)
data Node b = Node  { code :: Int
                    , isRoot :: Bool
                    , isLeaf :: Bool
                    , children :: [Int]
                    , parents :: [Int]
                    , encoded :: Vector (EncodedSeq b)
                    , packed :: Vector (EncodedSeq b)
                    , preliminary :: Vector (EncodedSeq b)
                    , final :: Vector (EncodedSeq b)
                    , temporary :: Vector (EncodedSeq b) -- is this necessary? rename to fitch scratch? 
                    , aligned :: Vector (EncodedSeq b) -- rename to implied alignment
                    , cost :: Double} deriving (Eq, Show)
                    -- subtree representation?
                    -- add a current 

-- | Make it an instance of encoded, final, packed, and preliminary
instance EN.EncodedNode (Node b) (EncodedSeq b) where
    encoded = encoded
    setEncoded n s = n {encoded = s}

-- | Nodes can hold final assignment
instance FN.FinalNode (Node b) (EncodedSeq b) where
    final = final
    setFinal f n = n {final = f}

-- | Nodes can hold packed data
instance PN.PackedNode (Node b) (EncodedSeq b) where
    packed = packed
    setPacked n s = n {packed = s}

-- | Nodes hold all preliminary info
instance RN.PreliminaryNode (Node b) (EncodedSeq b) where
    preliminary = preliminary
    setPreliminary s n = n {preliminary = s}
    preliminaryAlign = aligned
    setAlign s n = n {aligned = s}
    temporary = temporary
    setTemporary s n = n {temporary = s}
    cost = cost
    setCost c n = n {cost = c}
