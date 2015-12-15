{-# LANGUAGE ConstraintKinds #-}

module Analysis.DirectOptimization where

import Prelude hiding (length,foldr, zipWith)
import Data.Maybe
import Data.Vector (Vector, singleton, length, zipWith, foldr, cons, empty, toList, (!))
import Data.Bits
import Data.Monoid ((<>))
import Data.Matrix (fromList, Matrix, (<->), nrows, ncols, getElem, zero, matrix)

import Control.Applicative (liftA2)

import Bio.Phylogeny.Tree.Node.Preliminary
import Bio.Phylogeny.Network
import Bio.Phylogeny.Tree.Node.Encoded

import Bio.Sequence.Coded

type TreeConstraint t n s b = (Network t n, NodeConstraint n s b)
type NodeConstraint n s b = (PreliminaryNode n s, EncodedNode n s, SeqConstraint s b)
type SeqConstraint s b = (CodedSequence s b, Eq s, CharConstraint b)
type CharConstraint b = (Bits b, Eq b, CodedChar b)
data Direction = LeftDir | RightDir | DiagDir | DownDir deriving (Eq, Show)

type AlignRow s = (Vector Float, s, Vector Direction)
data AlignMatrix s = AlignMatrix {costs :: Matrix Float, seqs :: Vector s, traversal :: Matrix Direction} deriving (Eq, Show)

type Costs = (Float, Float)

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
            (shorter, shortlen, longer, longlen) = if seq1Len > seq2Len
                                                   then (seq2, seq2Len, seq1, seq1Len)
                                                   else (seq1, seq1Len, seq2, seq2Len)
            firstMatRow = firstAlignRow indelCost shorter shortlen 0 0 
            traversalMat = firstMatRow `joinMat` getAlignRows seq1 seq2 (indelCost, subCost) 1 firstMatRow
            cost = getMatrixCost traversalMat
            (gapped, left, right) = traceback traversalMat seq1 seq2
            ungapped = filterSeq gapped (gapChar /=)
        in (ungapped, cost, gapped, left, right)

        where
            getMatrixCost :: SeqConstraint s b => AlignMatrix s -> Float
            getMatrixCost inAlign = 
                let mat = costs inAlign
                in getElem (nrows mat - 1) (ncols mat - 1) mat

-- | Joins an alignment row to the rest of a matrix
joinMat :: SeqConstraint s b => AlignRow s -> AlignMatrix s -> AlignMatrix s
joinMat (inCosts, inSeq, directions) inMat = AlignMatrix (inCosts `joinRow` costs inMat) (inSeq `cons` seqs inMat) (directions `joinRow` traversal inMat) 
    where
        joinRow vec mat = fromList 1 (length vec) (toList vec) <-> mat

-- | Gets the initial row of a naive alignment matrix
firstAlignRow :: SeqConstraint s b => Float -> s -> Int -> Int -> Float -> AlignRow s
firstAlignRow indelCost inSeq rowLength position prevCost
    | position == (rowLength + 1) = (mempty, mempty, mempty)
    | position == 0 = (singleton 0, charToSeq gapChar, singleton DiagDir) <> firstAlignRow indelCost inSeq rowLength (position + 1) 0
    | newState /= gapChar = -- if there's no indel overlap
        (singleton $ prevCost + indelCost, charToSeq newState, singleton LeftDir) <> firstAlignRow indelCost inSeq rowLength (position + 1) (prevCost + indelCost)
    | otherwise = -- matching indel so no cost
        (singleton prevCost, charToSeq newState, singleton LeftDir) <> firstAlignRow indelCost inSeq rowLength (position + 1) prevCost
        where
            newState = getOverlapState gapChar (inSeq `grabSubChar` (position - 1))

-- | Gets the overlap state: intersect if possible and union if that's empty
getOverlapState :: CharConstraint b => b -> Maybe b -> b
getOverlapState compChar maybeChar
    | isNothing maybeChar = zeroBits
    | compChar .&. fromJust maybeChar == zeroBits = compChar .|. fromJust maybeChar
    | otherwise = compChar .&. fromJust maybeChar

-- | Main recursive function to get alignment rows
getAlignRows :: SeqConstraint s b => s -> s -> Costs -> Int -> AlignRow s -> AlignMatrix s
getAlignRows seq1 seq2 costs rowNum prevRow
    | rowNum == numChars seq2 + 1 = AlignMatrix (zero 0 0) empty (matrix 0 0 (const LeftDir))
    | otherwise = 
        let thisRow = generateRow seq1 seq2 costs rowNum prevRow (0, 0)
        in thisRow `joinMat` getAlignRows seq1 seq2 costs (rowNum + 1) thisRow

-- | Generates a single alignment row
generateRow :: SeqConstraint s b => s -> s -> Costs -> Int -> AlignRow s -> (Int, Float) -> AlignRow s
generateRow seq1 seq2 costvals@(indelCost, subCost) rowNum prevRow@(costs, _, _) (position, prevCost) 
    | position == (numChars seq1 + 1) = (empty, emptySeq, empty)
    | position == 0 && newState /= gapChar = (singleton $ upValue + indelCost, charToSeq newState, singleton DownDir) <> generateRow seq1 seq2 costvals rowNum prevRow (position + 1, upValue + indelCost)
    | otherwise = undefined
        where
            newState = getOverlapState gapChar (seq2 `grabSubChar` (rowNum - 1))
            upValue = costs ! position

-- | Performs the traceback of an alignment matrix
traceback :: SeqConstraint s b => AlignMatrix s -> s -> s -> (s, s, s)
traceback = undefined


