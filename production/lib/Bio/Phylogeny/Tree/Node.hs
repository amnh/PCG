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

-- | A node data structure holding all the necessary info (add verbose statement about what each field is)
data Node b = Node  { code :: Int
                    , isRoot :: Bool
                    , isLeaf :: Bool
                    , parents :: [Int]
                    , children :: [Int]
                    , encoded :: Vector (EncodedSeq b)
                    , packed :: Vector (EncodedSeq b)
                    , preliminary :: Vector (EncodedSeq b)
                    , final :: Vector (EncodedSeq b)
                    , temporary :: Vector (EncodedSeq b) -- is this necessary? rename to fitch scratch? 
                    , aligned :: Vector (EncodedSeq b) -- rename to implied alignment
                    , cost :: Double} deriving (Eq, Show)
                    -- subtree representation?
                    -- add a current 

instance Monoid (Node b) where
    mempty = Node 0 False False mempty mempty mempty mempty mempty mempty mempty mempty 0
    mappend n1 n2 = Node (code n1) (isRoot n1 || isRoot n2) (isLeaf n1 || isLeaf n2) (children n1 <> children n2) (parents n1 <> parents n2) (encoded n1 <> encoded n2) 
                        (packed n1 <> packed n2) (preliminary n1 <> preliminary n2) (final n1 <> final n2) 
                        (temporary n1 <> temporary n2) (aligned n1 <> aligned n2) (cost n1 + cost n2)

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
