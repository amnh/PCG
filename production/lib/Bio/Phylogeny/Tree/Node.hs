{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Bio.Phylogeny.Tree.Node where

import Bio.Sequence.Coded
import qualified Bio.Phylogeny.Tree.Node.Encoded as EN
import qualified Bio.Phylogeny.Tree.Node.Final as FN
import qualified Bio.Phylogeny.Tree.Node.Packed as PN
import qualified Bio.Phylogeny.Tree.Node.Preliminary as RN

data Node b = Node  { code :: Int
                    , isRoot :: Bool
                    , isLeaf :: Bool
                    , children :: [Int]
                    , parents :: [Int]
                    , encoded :: EncodedSeq b
                    , packed :: EncodedSeq b
                    , preliminary :: EncodedSeq b
                    , final :: EncodedSeq b
                    , temporary :: EncodedSeq b
                    , aligned :: EncodedSeq b
                    , cost :: Float} deriving (Eq, Show)

instance EN.EncodedNode (Node b) b where
    encoded = encoded
    setEncoded n s = n {encoded = s}

instance FN.FinalNode (Node b) b where
    final = final
    setFinal f n = n {final = f}

instance PN.PackedNode (Node b) b where
    packed = packed
    setPacked n s = n {packed = s}

instance RN.PreliminaryNode (Node b) b where
    preliminary = preliminary
    setPreliminary s n = n {preliminary = s}
    preliminaryAlign = aligned
    setAlign s n = n {aligned = s}
    temporary = temporary
    setTemporary s n = n {temporary = s}
    cost = cost
    setCost c n = n {cost = c}
