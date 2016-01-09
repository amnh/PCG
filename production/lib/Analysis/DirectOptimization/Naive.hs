{-# LANGUAGE ConstraintKinds #-}

module Analysis.DirectOptimization.Naive (naiveDOThree, naiveDOTwo, naiveDO) where

import Prelude hiding (length, zipWith, or)
import Data.Maybe
import Data.Ord      (comparing)
import Data.Foldable (foldl1,minimumBy)
import Data.Vector (Vector, singleton, length, zipWith, cons, empty, toList, (!), or, generate)
import qualified Data.Vector as V (foldr)
import Data.Bits
import Data.Monoid ((<>))
import Data.Matrix (fromList, Matrix, (<->), nrows, ncols, getElem, getRow, zero, matrix)

import Control.Applicative (liftA2)

import Bio.Phylogeny.Tree.Node.Preliminary
import Bio.Phylogeny.Network
import Bio.Phylogeny.Tree.Node.Encoded
import Bio.Sequence.Coded

import Analysis.DirectOptimization.Utilities

import Debug.Trace


data Direction = LeftDir | RightDir | DiagDir | DownDir deriving (Eq, Show)

type AlignRow s = (Vector Float, s, Vector Direction)
data AlignMatrix s = AlignMatrix {costs :: Matrix Float, seqs :: Vector s, traversal :: Matrix Direction} deriving (Eq, Show)

type Costs = (Float, Float)

indelCost = 1
subCost = 1

-- | Performs a 2-way alignment between node 3 with nodes 1 and 2
naiveDOThree :: NodeConstraint n s b => n -> n -> n -> (n, n, n, Bool)
naiveDOThree node1 node2 node3 = 
    let
        (newNode1, newNode3a, gapped, isLonger) = naiveDOTwo node1 node3
        (newNode2, newNode3b, gappedb, isLongerb) = naiveDOTwo node2 newNode3a
    in (newNode1, newNode2, newNode3b, isLonger || isLongerb)

-- | Performs a two-way alignment, assigns to both nodes, and returns those and whether it is longer
naiveDOTwo :: NodeConstraint n s b => n -> n -> (n, n, Vector s, Bool)
naiveDOTwo node1 node2 =
    let
        operateSeqs = checkForAlign node1 node2
        result = zipWith naiveDO (fst operateSeqs) (snd operateSeqs)
        (gapped, left, right) = foldr (\(_, _, g, l, r) (a, b, c) -> (g `cons` a, l `cons` b, r `cons` c)) mempty result
        checkLen = zipWith (\align preAlign -> if numChars align > numChars preAlign then True else False) gapped (getForAlign node2)
        foldCheck = or checkLen
    in (setAlign left node1, setAlign right node2, gapped, foldCheck)

-- | Performs a naive direct optimization
naiveDO :: SeqConstraint s b => s -> s -> (s, Float, s, s, s)
--naiveDO s1 s2 | trace ("Sequences of length " ++ show (numChars s1) ++ show (numChars s2)) False = undefined
naiveDO seq1 seq2 
    | isEmpty seq1 || isEmpty seq2 || numChars seq1 == 0 || numChars seq2 == 0 = (emptySeq, 0, emptySeq, emptySeq, emptySeq)
    | otherwise = 
        let
            seq1Len = numChars seq1
            seq2Len = numChars seq2
            (shorter, shortlen, longer, longlen) = if seq1Len > seq2Len
                                                   then (seq2, seq2Len, seq1, seq1Len)
                                                   else (seq1, seq1Len, seq2, seq2Len)
            firstMatRow = firstAlignRow indelCost longer longlen 0 0 
            traversalMat = firstMatRow `joinMat` getAlignRows longer shorter (indelCost, subCost) 1 firstMatRow
-- Alex's Experiment
--            traversalMat = optMat longer shorter (indelCost, subCost)
            cost = getMatrixCost traversalMat
            (gapped, left, right) = --trace ("get seqs " ++ show traversalMat)
                                    traceback traversalMat shorter longer
            ungapped = filterSeq gapped (gapChar /=)
            (out1, out2) = if seq1Len > seq2Len
                                then (right, left)
                                else (left, right)
        in (ungapped, cost, gapped, out1, out2)

        where
            getMatrixCost :: SeqConstraint s b => AlignMatrix s -> Float
            --getMatrixCost inAlign | trace ("Get cost " ++ show (nrows $ costs inAlign) ++ " " ++ show (ncols $ costs inAlign)) False = undefined
            getMatrixCost inAlign = 
                let mat = costs inAlign
                in getElem ((nrows mat) - 1) ((ncols mat) - 1) mat

-- | Joins an alignment row to the rest of a matrix
joinMat :: SeqConstraint s b => AlignRow s -> AlignMatrix s -> AlignMatrix s
joinMat (inCosts, inSeq, directions) inMat = AlignMatrix (inCosts `joinRow` costs inMat) (inSeq `cons` seqs inMat) (directions `joinRow` traversal inMat) 
    where
        joinRow vec mat = fromList 1 (length vec) (toList vec) <-> mat

-- | Gets the initial row of a naive alignment matrix
firstAlignRow :: SeqConstraint s b => Float -> s -> Int -> Int -> Float -> AlignRow s
--firstAlignRow indelCost inSeq rowLength position prevCost | trace ("firstAlignRow " ++ show inSeq) False = undefined
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
--generateRow seq1 seq2 costvals@(indelCost, subCost) rowNum prevRow@(costs, _, _) (position, prevCost)  | trace ("generateRow " ++ show position) False = undefined
generateRow seq1 seq2 costvals@(indelCost, subCost) rowNum prevRow@(costs, _, _) (position, prevCost) 
    | length costs < (position - 1) = error "Problem with row generation, previous costs not generated"
    | position == (numChars seq1 + 1) = (empty, emptySeq, empty)
    | position == 0 && newState /= gapChar = (singleton $ upValue + indelCost, charToSeq newState, singleton DownDir) <> nextCall (upValue + indelCost)
    | position == 0 = (singleton upValue, charToSeq newState, singleton DownDir) <> nextCall upValue
    | otherwise = --trace "minimal case" $ 
        (singleton minCost, charToSeq minState, singleton minDir) <> nextCall minCost
        where
            newState      = getOverlapState gapChar (seq2 `grabSubChar` (rowNum - 1))
            upValue       = costs ! position
            nextCall cost = generateRow seq1 seq2 costvals rowNum prevRow (position + 1, cost)
            char1         = unwrapSub $ seq1 `grabSubChar` (position - 1)
            char2         = unwrapSub $ seq2 `grabSubChar` (rowNum - 1)
            iuChar1       = getOverlapState gapChar (Just char1)
            iuChar2       = getOverlapState gapChar (Just char2)
            leftCost      = (overlapCost char1 indelCost) + prevCost
            downCost      = (overlapCost char2 indelCost) + upValue
            diagVal       = costs ! (position - 1)
            intersect     = char1 .&. char2
            union         = char1 .|. char2

            (diagCost, diagState) = if intersect == zeroBits 
                                    then (diagVal + subCost, union)
                                    else (diagVal, intersect)
            (minCost, minState, minDir) = --trace ("get minimum choice " ++ show [(leftCost, char1, LeftDir), (diagCost, diagState, DiagDir), (downCost, char2, DownDir)])
                                           minimumBy (comparing (\(a,b,c) -> a))
                                                [(leftCost, iuChar1, LeftDir), (downCost, iuChar2, DownDir), (diagCost, diagState, DiagDir)]

            overlapCost :: CharConstraint b => b -> Float -> Float
            overlapCost char cost 
                | gapChar .&. char == zeroBits = cost
                | otherwise = 0 

            unwrapSub :: CharConstraint b => Maybe b -> b
            unwrapSub val = case val of
                              Just v  -> v
                              Nothing -> error "Cannot access sequence at given position for matrix generation"


-- | Performs the traceback of an alignment matrix
traceback :: SeqConstraint s b => AlignMatrix s -> s -> s -> (s, s, s)
traceback alignMat seq1 seq2 | trace ("traceback with matrix " ++ show alignMat) False = undefined
traceback alignMat seq1 seq2 = tracebackInternal alignMat seq1 seq2 (numChars seq1, numChars seq2)
    where
        tracebackInternal :: SeqConstraint s b => AlignMatrix s -> s -> s -> (Int, Int) -> (s, s, s)
        --tracebackInternal alignMat seq1 seq2 (row, col)  | trace ("traceback " ++ show (nrows $ traversal alignMat) ++ show row ++ show col) False = undefined
        tracebackInternal alignMat seq1 seq2 (row, col) 
            | (length $ seqs alignMat) < row - 1 || (nrows $ traversal alignMat) < row - 1 || (ncols $ traversal alignMat) < col - 1 = error "Traceback cannot function because matrix is incomplete"
            | row == 0 && col == 0 = (emptySeq, emptySeq, emptySeq)
            | curDirect == LeftDir = tracebackInternal alignMat seq1 seq2 (row, col - 1) <> (curState, charToSeq gapChar, charToUnMaybe $ seq2 `grabSubChar` (col - 1)) 
            | curDirect == DownDir = tracebackInternal alignMat seq1 seq2 (row - 1, col) <> (curState, charToUnMaybe $ seq1 `grabSubChar` (row - 1), charToSeq gapChar) 
            | curDirect == DiagDir = tracebackInternal alignMat seq1 seq2 (row - 1, col - 1) <> (curState, charToUnMaybe $ seq1 `grabSubChar` (row - 1), charToUnMaybe $ seq2 `grabSubChar` (col - 1))
            | otherwise = error "Incorrect direction in matrix traversal for alignment"
                where
                    curDirect = getElem row col (traversal alignMat)
                    curState = charToUnMaybe $ ((seqs alignMat) ! row) `grabSubChar` col

                    charToUnMaybe :: SeqConstraint s b => Maybe b -> s
                    charToUnMaybe inBit = case inBit of
                        Nothing -> emptySeq
                        Just b -> charToSeq b


{--
-- It want's an `Eq` instance?
data MatrixEntry b
   = MatrixEntry
   { entryCost :: Float
   , entryChar :: b
   , entryDir  :: Direction
   }

optMat :: SeqConstraint s b => s -> s -> Costs -> AlignMatrix s
optMat seqB seqC (indelCost, subCost) = AlignMatrix costMatrix charVector dirMatrix
  where
    costMatrix = entryCost <$> opt
    charVector :: SeqConstraint s b => Vector s
    charVector = generate m (foldl1 (<>) . fmap (entryChar) . (`getRow` opt))
    dirMatrix  = entryDir  <$> opt
    -- Maybe use `matrix` with generating function
    opt :: SeqConstraint s b => Matrix (MatrixEntry b)
    opt = fromList m n [ score i j | i <- [0..m], j <- [0..n] ]
    optCost = entryCost . (opt !!!)
    m = numChars seqB
    n = numChars seqC
    (!!!) matrix (i,j) = getElem i j matrix
    sigma :: CharConstraint b => b -> b -> Float
    sigma x y
      |  gapChar == x
      || gapChar == y = indelCost
      |  otherwise    = subCost
    score :: SeqConstraint s b => Int -> Int -> MatrixEntry s
    score 0 0 = MatrixEntry 0 gapChar DiagDir
    score i 0 = let charVal = seqB `grabSubChar` i
                in  MatrixEntry (optCost (i-1,   0) + sigma charVal gapChar) charVal LeftDir
    score 0 j = let charVal = seqC `grabSubChar` j
                in  MatrixEntry (optCost (  0, j-1) + sigma charVal gapChar) charVal DownDir
    score i j = minimumBy (comparing entryCost)
              [ MatrixEntry costGapB  iuCharB   LeftDir
              , MatrixEntry costGapC  iuCharC   DownDir
              , MatrixEntry costAlign diagState DiagDir
              ]
      where
        charB     = seqB `grabSubChar` i
        charC     = seqC `grabSubChar` j
        costGapB  = optCost (i  , j-1) + sigma charB gapChar
        costGapC  = optCost (i-1, j  ) + sigma charB gapChar
        costAlign = optCost (i-1, j-1) + sigma charB charC
        iuCharB   = getOverlapState gapChar (Just charB)
        iuCharC   = getOverlapState gapChar (Just charC)
        intersect = charB .&. charC
        union     = charB .|. charC
        diagState = if intersect == zeroBits then union else intersect
--}
