{-# LANGUAGE ConstraintKinds #-}

module Analysis.DirectOptimization where

import Prelude hiding (length,foldr, zipWith)
import Data.Maybe
import Data.Vector (Vector, singleton, length, zipWith, foldr, cons, empty)
import Data.Bits
import Data.Monoid ((<>))
import Data.Matrix

import Bio.Phylogeny.Tree.Node.Preliminary
import Bio.Phylogeny.Network
import Bio.Phylogeny.Tree.Node.Encoded

import Bio.Sequence.Coded

type TreeConstraint t n s b = (Network t n, NodeConstraint n s b)
type NodeConstraint n s b = (PreliminaryNode n s, EncodedNode n s, SeqConstraint s b)
type SeqConstraint s b = (Bits b, CodedSequence s b, Bits s)
data Direction = LeftDir | RightDir | DiagDir deriving (Eq, Show)

type AlignRow s = (Vector Float, s, Vector Direction)
data AlignMatrix s = AlignMatrix {costs :: Matrix Float, seqs :: Matrix s, traversal :: Matrix Direction} deriving (Eq, Show)

indelCost = 1
subCost = 1

-- | Performs a naive direct optimization on sequences of first two nodes and saves to third node
naiveDONode :: NodeConstraint n s b => n -> n -> n -> n
naiveDONode align1 align2 save = 
    let 
        result = zipWith naiveDO (encoded align1) (encoded align2) 
        gapped = foldr (\(_, _, g, _, _) acc -> g `cons` acc) empty result
    in setAlign gapped save

-- | Performs a naive direct optimization
naiveDO :: SeqConstraint s b => s -> s -> (s, Float, s, s, s)
naiveDO seq1 seq2 
    | isEmpty seq1 || isEmpty seq2 = (emptySeq, 0, emptySeq, emptySeq, emptySeq)
    | otherwise = 
        let
            seq1Len = numChars seq1
            seq2Len = numChars seq2
            (shorter, shortlen, longer, longlen) = case seq1Len > seq2Len of
                False -> (seq1, seq1Len, seq2, seq2Len)
                True -> (seq2, seq2Len, seq1, seq1Len)
            firstMatRow = firstAlignRow indelCost shorter shortlen 0 0 
            traversalMat = firstMatRow `joinMat` getAlignRows seq1 seq2 indelCost subCost 1 firstMatRow
            cost = getMatrixCost traversalMat
            (gapped, left, right) = traceback traversalMat seq1 seq2
            ungapped = filterGaps gapped
        in (ungapped, cost, gapped, left, right)

        where
            getMatrixCost :: SeqConstraint s b => AlignMatrix s -> Float
            getMatrixCost = undefined

            filterGaps :: SeqConstraint s b => s -> s
            filterGaps = undefined

            joinMat :: SeqConstraint s b => AlignRow s -> AlignMatrix s -> AlignMatrix s
            joinMat = undefined

-- | Gets the initial row of a naive alignment matrix
firstAlignRow :: SeqConstraint s b => Float -> s -> Int -> Int -> Float -> AlignRow s
firstAlignRow indelCost inSeq rowLength position prevCost
    | position == (rowLength + 1) = (mempty, mempty, mempty)
    | position == 0 = (singleton 0, gapChar, singleton DiagDir) <> firstAlignRow indelCost inSeq rowLength (position + 1) 0
    | newState /= gapChar = -- if there's no indel overlap
        (singleton $ prevCost + indelCost, newState, singleton LeftDir) <> firstAlignRow indelCost inSeq rowLength (position + 1) (prevCost + indelCost)
    | otherwise = -- matching indel so no cost
        (singleton prevCost, newState, singleton LeftDir) <> firstAlignRow indelCost inSeq rowLength (position + 1) prevCost
        where
            newState = getOverlapState gapChar inSeq (position - 1)

getOverlapState :: SeqConstraint s b => s -> s -> Int -> s
getOverlapState = undefined

getAlignRows :: SeqConstraint s b => s -> s -> Float -> Float -> Int -> AlignRow s -> AlignMatrix s
getAlignRows = undefined

traceback :: SeqConstraint s b => AlignMatrix s -> s -> s -> (s, s, s)
traceback = undefined


