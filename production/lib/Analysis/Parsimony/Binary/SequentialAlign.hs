{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Analysis.Parsimony.Binary.SequentialAlign (sequentialAlign) where

import           Analysis.Parsimony.Binary.Internal
--import           Bio.Phylogeny.Tree.Node.Preliminary
import           Bio.Sequence.Coded
import qualified Analysis.Parsimony.Binary.SequentialAlign.SeqAlignFFI as FF (sequentialAlign)
--import           Control.Applicative        (liftA2)
import           Data.Bits
import           Data.Foldable              (minimumBy)
--import           Data.Function              (on)
import           Data.List                  (elemIndex)
import           Data.List.Split            (chunksOf)
import           Data.Matrix.NotStupid      (fromList, Matrix, (<->), nrows, ncols, getElem, zero, matrix)
import           Data.Maybe
import           Data.Monoid                ((<>))
import           Data.Ord                   (comparing)
import           Data.Vector                (Vector, singleton, length, cons, empty, toList, (!), ifoldr)
import qualified Data.Vector as V           (fromList)
import           Prelude             hiding (length, zipWith, or)

-- import           Debug.Trace

data Direction = LeftDir | DiagDir | DownDir deriving (Eq, Show)

type AlignRow s = (Vector Float, s, Vector Direction)
data AlignMatrix s = AlignMatrix {costs :: Matrix Float, seqs :: Vector s, traversal :: Matrix Direction} deriving (Eq, Show)

type Costs = (Float, Float)

-- | sequentialAlign is similar to DO, but uses Yu's and Vahid's information theoretical sequential alignment algorithm to produce the alignment
sequentialAlign :: SeqConstraint s b => s -> s -> (s, Double, s, s, s)
sequentialAlign inpSeq1 inpSeq2 = (inferredParent', (fromIntegral cost :: Double), alignedParent', alignment1', alignment2')
    where
        (inferredParent, alignedParent) = foldr (\(x, y) acc -> createParentSeqs x y acc) ([],[]) (zip alignment1 alignment2)
        createParentSeqs x y (xs, ys)
            | x == '-' && y == '-' = (xs    , '-' : ys)
            | x == '-'             = (xs, x : ys) -- So I'm prioritizing gap over everything else
            | y == '-'             = (xs, y : ys) -- Note that '-' comes before letters alphabetically, 
                                                  -- but I still have to do this to deal with gap removal in inferred Parent
            | x < y                = (x : xs, x : ys)
            | y < x                = (y : xs, y : ys)
            | otherwise            = (x : xs, x : ys) -- they must be equal, so choose x
        inferredParent' = simpleEncode inferredParent
        alignedParent'  = simpleEncode alignedParent
        alignment1'     = simpleEncode alignment1
        alignment2'     = simpleEncode alignment2
        inpSeq1'        = simpleDecode inpSeq1
        inpSeq2'        = simpleDecode inpSeq2
        (cost, alignment1, alignment2) = case FF.sequentialAlign 1 1 inpSeq1' inpSeq2' of
            Left e -> error e -- Better error handling later
            Right r -> r

-- | Simple encoding over a string just for you
simpleEncode :: Bits s => String -> s
simpleEncode inStr = ifoldr simpleSetElem zeroBits vecStr
    where
        vecStr = V.fromList inStr

        -- | Simple functionality to set an element in a bitvector
        simpleSetElem :: Bits b => Int -> Char -> b -> b
        simpleSetElem i char curBit = case elemIndex char "ACGT-" of
                                            Nothing -> curBit
                                            Just pos -> setBit curBit (pos + (i * 5))


-- | Hardcoded decode FTW
simpleDecode :: Bits s => s -> String
simpleDecode inVec = foldr (\x acc -> acc ++ [makeAmbDNA x]) [] chunks
    where
        tehBitz = showTehBit <$> toBitStream inVec
        chunks  = chunksOf 5 tehBitz
        showTehBit True  = '1'
        showTehBit False = '0'

        toBitStream :: Bits b => b -> [Bool]
        toBitStream = f 0
            where
                f :: Bits b => Int -> b -> [Bool]
                f i xs
                    | zeroBits == xs = []
                    | otherwise      = xs `testBit` i : f (i+1) (xs `clearBit` i) 

        -- TODO: order these by popularity?
        makeAmbDNA :: String -> Char
        makeAmbDNA x = case x of
            "10000" -> 'A'
            "01000" -> 'C'
            "00100" -> 'G'
            "00010" -> 'T'
            "00001" -> '-'
            "01110" -> 'B'
            "10110" -> 'D'
            "11010" -> 'H'
            "00110" -> 'K'
            "11000" -> 'M'
            "11110" -> 'N'
            "10100" -> 'R'
            "01100" -> 'S'
            "11100" -> 'V'
            "10010" -> 'W'
            "01010" -> 'Y'
            _       -> '-'

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
    | newState /= gapChar = --trace ("new state on first row " ++ show newState) $ -- if there's no indel overlap
        (singleton $ prevCost + indelCost, charToSeq newState, singleton LeftDir) <> firstAlignRow indelCost inSeq rowLength (position + 1) (prevCost + indelCost)
    | otherwise = --trace ("new state on first row, otherwise " ++ show newState) $ -- matching indel so no cost
        (singleton prevCost, charToSeq newState, singleton LeftDir) <> firstAlignRow indelCost inSeq rowLength (position + 1) prevCost
        where
            newState = getOverlapState gapChar (inSeq `grabSubChar` (position - 1))

-- | Gets the overlap state: intersect if possible and union if that's empty
getOverlapState :: CharConstraint b => b -> Maybe b -> b
getOverlapState compChar  Nothing = zeroBits
getOverlapState compChar (Just x) = compChar `op` x
  where op = if compChar .&. x == zeroBits then (.|.) else (.&.)

-- | Main recursive function to get alignment rows
getAlignRows :: SeqConstraint s b => s -> s -> Costs -> Int -> AlignRow s -> AlignMatrix s
getAlignRows seq1 seq2 costs rowNum prevRow
    | rowNum == numChars seq2 + 1 = AlignMatrix (zero 0 0) empty (matrix 0 0 (const LeftDir))
    | otherwise = 
        let thisRow = generateRow seq1 seq2 costs rowNum prevRow (0, 0)
        in thisRow `joinMat` getAlignRows seq1 seq2 costs (rowNum + 1) thisRow

-- | Generates a single alignment row
generateRow :: SeqConstraint s b => s -> s -> Costs -> Int -> AlignRow s -> (Int, Float) -> AlignRow s
--generateRow seq1 seq2 costvals@(indelCost, subCost) rowNum prevRow@(costs, _, _) (position, prevCost)  | trace ("generateRow " ++ show seq1 ++ show seq2) False = undefined
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
            leftCost      = overlapCost char1 indelCost + prevCost
            downCost      = overlapCost char2 indelCost + upValue
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
            unwrapSub = fromMaybe (error "Cannot access sequence at given position for matrix generation")

-- | Performs the traceback of an alignment matrix
traceback :: SeqConstraint s b => AlignMatrix s -> s -> s -> (s, s, s)
--traceback alignMat seq1 seq2 | trace ("traceback with matrix " ++ show alignMat) False = undefined
traceback alignMat seq1 seq2 = tracebackInternal alignMat seq1 seq2 (numChars seq1, numChars seq2)
    where
        -- read it from the matrix instead of grabbing
        tracebackInternal :: SeqConstraint s b => AlignMatrix s -> s -> s -> (Int, Int) -> (s, s, s)
        --tracebackInternal alignMat seq1 seq2 (row, col)  | trace ("traceback " ++ show (traversal alignMat) ++ show (getElem row col (traversal alignMat))++ " with position " ++ show (row, col)) False = undefined
        tracebackInternal alignMat seq1 seq2 (row, col) 
            | length (seqs alignMat) < row - 1 || nrows (traversal alignMat) < row - 1 || ncols (traversal alignMat) < col - 1 = error "Traceback cannot function because matrix is incomplete"
            | row == 0 && col == 0 = (emptySeq, emptySeq, emptySeq)
            | curDirect == LeftDir = tracebackInternal alignMat seq1 seq2 (row, col - 1) <> (curState, charToSeq gapChar, charToUnMaybe $ seq2 `grabSubChar` (col - 1)) 
            | curDirect == DownDir = tracebackInternal alignMat seq1 seq2 (row - 1, col) <> (curState, charToUnMaybe $ seq1 `grabSubChar` (row - 1), charToSeq gapChar) 
            | curDirect == DiagDir = tracebackInternal alignMat seq1 seq2 (row - 1, col - 1) <> (curState, charToUnMaybe $ seq1 `grabSubChar` (row - 1), charToUnMaybe $ seq2 `grabSubChar` (col - 1))
            | otherwise = error "Incorrect direction in matrix traversal for alignment"
                where
                    curDirect = getElem row col (traversal alignMat)
                    curState = charToUnMaybe $ seqs alignMat ! row `grabSubChar` col

                    charToUnMaybe :: SeqConstraint s b => Maybe b -> s
                    charToUnMaybe inBit = case inBit of
                                            Nothing -> emptySeq
                                            Just b  -> charToSeq b
