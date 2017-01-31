-----------------------------------------------------------------------------
-- |
-- Module      :  Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.Internal
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Direct optimization functionality for binary trees
--
-----------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds, FlexibleContexts, TypeFamilies #-}

module Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.Internal where

import Bio.Character.Encodable
import Data.Bits
--import Data.BitVector hiding (foldr, reverse)
import Data.Foldable         (minimumBy)
--import Data.Function.Memoize
import Data.Key              ((!))
import Data.Matrix.NotStupid (Matrix, matrix, nrows, ncols)
import Data.MonoTraversable
import Data.Ord

--import Debug.Trace (trace)

-- | The direction to align the character at a given matrix point.
data Direction = LeftArrow | DiagArrow | UpArrow deriving (Eq, Show)

-- | A representation of an alignment matrix for DO.
-- The matrix itself stores tuples of the cost and direction at that position.
-- We also store a vector of characters that are generated.
type DOAlignMatrix s = Matrix (Int, Direction, s)

-- | Constraints on the input dynamic characters that direct optiomization operates on.
type DOCharConstraint c = (EncodableDynamicCharacter c {-, Show c, Show (Element c) -})

-- | Performs a naive direct optimization
-- Takes in two characters to run DO on and a metadata object
-- Returns an assignment character, the cost of that assignment, the assignment character with gaps included,
-- the aligned version of the first input character, and the aligned version of the second input character
-- The process for this algorithm is to generate a traversal matrix, then perform a traceback.
naiveDO :: DOCharConstraint s
        => s                    -- ^ First  dynamic character
        -> s                    -- ^ Second dynamic character
        -> (Int -> Int -> Int)  -- ^ Structure defining the transition costs between character states
        -> (s, Double, s, s, s) -- ^ The /ungapped/ character derived from the the input characters' N-W-esque matrix traceback
                                -- 
                                --   The cost of the alignment
                                -- 
                                --   The /gapped/ character derived from the the input characters' N-W-esque matrix traceback
                                -- 
                                --   The gapped alignment of the /first/ input character when aligned with the second character
                                -- 
                                --   The gapped alignment of the /second/ input character when aligned with the first character
naiveDO char1 char2 costStruct
    | onull char1 = (char1, 0, char1, char1, char1)
    | onull char2 = (char2, 0, char2, char2, char2)
    | otherwise   = (ungapped, fromIntegral cost, gapped', out1, out2)
    where
      char1Len = olength char1
      char2Len = olength char2
      (shorterChar, longerChar, _longLen) = if   char1Len > char2Len
                                            then (char2, char1, char1Len)
                                            else (char1, char2, char2Len)
      traversalMat = -- (\x -> trace (show $ (\(a,b,_) -> (a,b)) <$> x) x) $
                      createDOAlignMatrix longerChar shorterChar costStruct
      cost = getTotalAlignmentCost traversalMat
      (gapped , left , right ) = traceback traversalMat shorterChar longerChar
--      (gapped', left', right') = traceback traversalMat shorterChar longerChar
{--}
      (gapped', left', right') = (\(x,y,z) -> (constructDynamic x, constructDynamic y, constructDynamic z)) 
                               $ correctBiasing (getGapElement $ gapped `indexStream` 0) (otoList gapped, otoList left, otoList right)
{--}
      -- TODO: change to occur in traceback, to remove constant factor.
      ungapped = filterGaps gapped'
      (out1, out2) = if char1Len > char2Len
                     then (right', left')
                     else (left', right')
           
-- | Wrapper function to do an enhanced Needleman-Wunsch algorithm.
-- Calls naiveDO, but only returns the last two fields (gapped alignments of inputs)
doAlignment :: DOCharConstraint s => s -> s -> (Int -> Int -> Int) -> (s, s)
doAlignment char1 char2 costStruct = (char1Align, char2Align)
    where
        (_, _, _, char1Align, char2Align) = naiveDO char1 char2 costStruct
        

-- |
-- Strips the gaps from the supplied character.
filterGaps :: EncodableDynamicCharacter c => c -> c
filterGaps char = constructDynamic . filter (/= gap) $ otoList char
  where
    gap = getGapElement $ char `indexStream` 0


-- | Main function to generate an 'DOAlignMatrix'. Works as in Needleman-Wunsch,
-- but allows for multiple indel/replacement costs, depending on the 'CostStructure'.
-- Also, returns the aligned parent characters, with appropriate ambiguities, as the third of
-- each tuple in the matrix.
-- Takes in two 'EncodableDynamicCharacter's and a 'CostStructure'. The first character
-- must be the longer of the two and is the top labeling of the matrix.
-- Returns an 'DOAlignMatrix'.
-- TODO: See if we can move topDynChar logic inside here. It's also necessary in DO. 
-- Or maybe DO can just call doAlignment?
createDOAlignMatrix :: EncodableDynamicCharacter s => s -> s -> (Int -> Int -> Int) -> DOAlignMatrix (Element s)
createDOAlignMatrix topDynChar leftDynChar costStruct = result
    where
        result = matrix (olength leftDynChar + 1) (olength topDynChar + 1) generateMat

        -- TODO: attempt to make tail recursive? Maybe not possible, given multiple tuple values.
        -- | Internal generator function for the matrix
        -- Deals with both first row and other cases, a merge of two previous algorithms
        -- generateMat :: (EncodableStreamElement b) => (Int, Int) -> (Double, Direction, b)
        -- generateMat (row, col) | trace (mconcat ["(",show row,",",show col,")"]) False = undefined
        generateMat (row, col)
          | row == 0 && col == 0                    = (0                               , DiagArrow, gap      )
          | row == 0 && rightChar /= gap            = (leftwardValue + rightOverlapCost, LeftArrow, staticCharFromLeft)
          | row == 0                                = (leftwardValue                   , LeftArrow, staticCharFromLeft)
          | col == 0 && downChar  /= gap            = (upwardValue + downOverlapCost   , UpArrow  , staticCharFromTop )
          | col == 0                                = (upwardValue                     , UpArrow  , staticCharFromTop )
          | staticCharFromLeft == gap &&
            staticCharFromTop  == gap               = (diagCost                        , DiagArrow, gap)
          | staticCharFromLeft == staticCharFromTop = (diagCost                        , DiagArrow, staticCharFromTop)
          | otherwise                               = (minCost                         , minDir   , minState )
          where
            gap                           = gapOfStream leftDynChar -- Why would you give me an empty Dynamic Character?
            staticCharFromLeft            = topDynChar  `indexStream` (col - 1)
            staticCharFromTop             = leftDynChar `indexStream` (row - 1)
            (leftwardValue, _, _)         = result ! (row    , col - 1)
            (diagonalValue, _, _)         = result ! (row - 1, col - 1)
            (upwardValue  , _, _)         = result ! (row - 1, col)
            (rightChar, rightOverlapCost) = overlap costStruct staticCharFromLeft gap               
            (diagChar , diagOverlapCost)  = overlap costStruct staticCharFromLeft staticCharFromTop 
            (downChar , downOverlapCost)  = overlap costStruct gap                staticCharFromTop 
            rightCost                     = rightOverlapCost + leftwardValue
            diagCost                      = diagOverlapCost  + diagonalValue
            downCost                      = downOverlapCost  + upwardValue
            (minCost, minState, minDir)   = minimumBy (comparing (\(a,_,_) -> a))
                                          -- This order is important!
                                          -- In the event of equal cost we want to
                                          -- prioritize elements earlier in the list.
                                          -- TODO: POY prioritizes gaps to shorter char, make sure it prioritizes
                                          -- right on equal-length chars
                                          [ (rightCost, rightChar, LeftArrow)
                                          , (downCost , downChar , UpArrow  )
                                          , (diagCost , diagChar , DiagArrow)
                                          ]


-- | Performs the traceback of an 'DOAlignMatrix'.
-- Takes in an 'DOAlignMatrix', two 'EncodableDynamicCharacter's, and the 'Alphabet' length.
-- Returns an aligned 'EncodableDynamicCharacter', as well as the aligned versions of the two inputs.
-- Essentially does the second step of Needleman-Wunsch, following the arrows from the bottom right corner, 
-- accumulating the sequences as it goes, but returns three alignments: the left character, the right character,
-- and the parent. The child alignments *should* be biased toward the shorter of the two sequences.
-- TODO: Change order of input characters to match createDOAlignMatrix inputs.
traceback :: (DOCharConstraint s) => DOAlignMatrix (Element s) -> s -> s -> (s, s, s)
traceback alignMat' char1' char2' = ( constructDynamic $ reverse t1
                                    , constructDynamic $ reverse t2
                                    , constructDynamic $ reverse t3
                                    )
    where
        (t1, t2, t3) = tracebackInternal alignMat' char1' char2' (nrows alignMat' - 1, ncols alignMat' - 1)
        -- read it from the matrix instead of grabbing
        tracebackInternal :: (DOCharConstraint s) => DOAlignMatrix (Element s) -> s -> s -> (Int, Int) -> ([Element s], [Element s], [Element s]) -- TODO: make s's into Element s
        tracebackInternal alignMat char1 char2 (row, col)
            | nrows alignMat < row - 1 || ncols alignMat < col - 1 = error "Traceback cannot function because matrix is incomplete"
            | row == 0 && col == 0 = (mempty, mempty, mempty)
            | otherwise = -- trace (mconcat ["(",show row,",",show col,") ",show curState]) $ 
                let (trace1, trace2, trace3) = tracebackInternal alignMat char1 char2 (i, j)
                in (curState : trace1, leftCharacter : trace2, rightCharacter : trace3)
            where
              (_, curDirect, curState) = alignMat ! (row, col)
              leftCharacter            = if row == i 
                                         then getGapElement curState
                                         else char1 `indexStream` i
              rightCharacter           = if col == j 
                                         then getGapElement curState
                                         else char2 `indexStream` j
              (i, j) =
                case curDirect of
                  LeftArrow -> (row    , col - 1)
                  UpArrow   -> (row - 1, col    )
                  DiagArrow -> (row - 1, col - 1)


-- | Simple function to get the cost from an alignment matrix
getTotalAlignmentCost :: Matrix (a, b, c) -> a
getTotalAlignmentCost alignmentMatrix = c
  where
    (c, _, _) = alignmentMatrix ! (nrows alignmentMatrix - 1, ncols alignmentMatrix - 1) 


-- | Memoized wrapper of the overlap function
getOverlap :: (EncodableStreamElement c {- , Memoizable c, -}) => c -> c -> (Int -> Int -> Int) -> (c, Int)
getOverlap inChar1 inChar2 costStruct = result
    where
        result = {- memoize2 -} overlap costStruct inChar1 inChar2

        
-- | Takes two 'EncodableStreamElement' and a 'CostStructure' and returns a tuple of a new character, 
-- along with the cost of obtaining that character. The return character may be (or is even likely to be)
-- ambiguous. Will attempt to intersect the two characters, but will union them if that is not possible,
-- based on the 'CostStructure'. 
--
-- To clarify, the return character is an intersection of all possible least-cost combinations, so for instance,
-- if @ char1 == A,T @ and @ char2 == G,C @, and the two (non-overlapping) least cost pairs are A,C and T,G, then
-- the return value is A,C,G,T. 
-- Tests exist in the test suite.
overlap :: (EncodableStreamElement c {- , Show c -}) => (Int -> Int -> Int) -> c -> c -> (c, Int)
--overlap _ inChar1 inChar2 | trace (unwords [show inChar1, show inChar2]) False = undefined
overlap costStruct char1 char2
    | intersectionStates == zeroBits = -- (\x -> trace (unwords [show char1, show char2, show x]) x) $
                                       minimalChoice $ allPossibleBaseCombosCosts costStruct char1 char2
    | otherwise                      = (intersectionStates, 0)
    where
      intersectionStates = char1 .&. char2

-- |
-- Given a structure of character elements and costs, calculates the least
-- costly intersection of character elements and the cost of that intersection
-- of chaarcter elements.
minimalChoice :: (Bits c, Foldable t, Ord n) => t (c, n) -> (c, n)
minimalChoice = foldr1 f
  where
    f (val1, cost1) (val2, cost2)
      | cost1 == cost2 = (val1 .|. val2, cost1)
      | cost1 < cost2  = (val1         , cost1)
      | otherwise      = (val2         , cost2)


-- TODO: Can we eliminate all characters from below, and just pass around Ints?
-- |
-- Finds the cost of a pairing of two static characters.
-- Takes in a 'CostStructure' and two ambiguous 'EncodableStreamElement's. Returns a list of tuples of all possible unambiguous
-- pairings, along with their costs. 
allPossibleBaseCombosCosts :: EncodableStreamElement s => (Int -> Int -> Int) -> s -> s -> [(s, Int)]
allPossibleBaseCombosCosts costStruct char1 char2 = [ (x .|. y, costStruct i j)
                                                    | (i,x) <- getSubChars char1
                                                    , (j,y) <- getSubChars char2
                                                    ]

-- |
-- Given a 'CostStructure' and two tuples of an 'Int' and an unambiguous 'EncodableStreamElement', determines the cost 
-- of a pairing (intersection) of those characters into an ambiguous character. The 'Int's are the set bits in each character
-- and are used as lookup into the 'CostStructure'. 
-- Tests exist in the test suite.
getCost :: EncodableStreamElement s => (Int -> Int -> Int) -> (Int, s) -> (Int, s) -> (s, Int)
getCost costStruct seqTup1 seqTup2 = 
    case (seqTup1, seqTup2) of
        ((pos1, c1), (pos2, c2)) -> (c1 .|. c2, costStruct pos1 pos2)


-- |
-- Takes in a 'EncodableStreamElement', possibly with more than one bit set, and returns a list of tuples of 
-- 'Int's and 'EncodableStreamElement's, such that, for each set bit in the input, there is one element in the output list, 
-- a tuple with an 'Int', @ x @, giving the location of the set bit, as well as an 'EncodableStreamElement' of the same
-- length as the input, but with only the bit at location @ x @ set.
-- Tests exist in the test suite.
getSubChars :: EncodableStreamElement s => s -> [(Int, s)]
getSubChars fullChar = foldMap f [0 .. symbolCount fullChar - 1]
  where
    f i
      | fullChar `testBit` i = pure (i,  z `setBit` i)
      | otherwise            = mempty
    z = fullChar `xor` fullChar

-- |
-- Transformation should no longer be nescissary
-- Replaced definition with the identiy function over two values.
correctBiasing :: a -> ([a], [a], [a]) -> ([a], [a], [a])
correctBiasing = const id
{-
correctBiasing   _ ( [], [], []) = ( [],  [],  [])
correctBiasing   _ ([x],[y],[z]) = ([x], [y], [z])
correctBiasing gap (x1:x2:xs, y1:y2:ys, z1:z2:zs)
  | y1 == gap && z1 /= gap && z1 == y2 && z1 == z2 = (x2:xs'', y2:ys'', z2:zs'')
  | y1 /= gap && z1 == gap && y1 == y2 && y1 == z2 = (x2:xs'', y2:ys'', z2:zs'')
  | otherwise                                      = (x1:xs' , y1:ys' , z1:zs' )
  where
    (xs' , ys' , zs' ) = correctBiasing gap ( x2:xs, y2:ys, z2:zs )
    (xs'', ys'', zs'') = correctBiasing gap ( x1:xs, y1:ys, z1:zs )
-}




-- | setLeftRight returns sequence that is longer first,
--shorter second.  Handles equal length by returning max first.
setLeftRight :: BaseChar -> BaseChar -> (BaseChar, Int, BaseChar, Int)
setLeftRight inL inR
    | V.length inL < V.length inR = (inR, V.length inR, inL, V.length inL)
    | V.length inL > V.length inR = (inL, V.length inL, inR, V.length inR)
    | otherwise = (outL, V.length outL, outR, V.length outR)
        where
            outL = max inL inR
            outR = min inL inR 

-- | ukkonenCore core functions of Ukkonen to allow for recursing with maxGap
--doubled if not large enough (returns Nothing)  
ukkonenCore :: BaseChar -> Int -> BaseChar -> Int -> Int -> Int -> Int -> (BaseChar, Float, BaseChar, BaseChar, BaseChar)
ukkonenCore lSeq lLength rSeq rLength maxGap indelCost subCost | trace "ukkonenCore" False = undefined
ukkonenCore lSeq lLength rSeq rLength maxGap indelCost subCost
    | V.head median /= (0 :: Int64)  = --trace (show nwMatrix) 
        (median, fromIntegral cost, medianGap, alignLeft, alignRight)
    | otherwise = --trace ("Going back!! " ++ show cost) 
        ukkonenCore lSeq lLength rSeq rLength (2 * maxGap) indelCost subCost
        where
            firstRow = getFirstRowUkkonen indelCost lLength 0 0 lSeq maxGap
            nwMatrix = V.cons firstRow (getRowsUkkonen lSeq rSeq indelCost subCost 1 firstRow maxGap)
            (cost, _, _) = V.last (V.last nwMatrix) -- V.! rLength) --V.! (transformFullYShortY lLength rLength  maxGap) --fix for offset
            (medianGap, alignLeft, alignRight) = V.unzip3 $ V.reverse (tracebackUkkonen nwMatrix lSeq rSeq rLength lLength maxGap 0 0)
            median = V.filter (/= inDelBit) medianGap

--FOR both DO's  lseq is a row, acrosss so num columns = length of lseq
--There are rseq rows
-- | UkkonenDO takes two input sequences and returns median sequence and cost
--only 1:1 for now. Uses Ukkonen's space/time saving algorithm
--need to make sure Left/Right and diag/ins/del orders consistent and with
--POY4/5
--lseq > rseq appeard more efficient--could be wrong
--move to C via FFI
--Still occasional error in cost and median (disagreement) show in Chel.seq
ukkonenDO :: BaseChar -> BaseChar -> CharInfo -> (BaseChar, Float, BaseChar, BaseChar, BaseChar)
ukkonenDO inlSeq inrSeq charInfo | trace ("calling ukonnen DO with seqs " ++ show inlSeq ++ show inrSeq) False = undefined
ukkonenDO inlSeq inrSeq charInfo
    | V.null inlSeq = (inrSeq, 0, inrSeq, V.replicate (V.length inrSeq) (maxBound :: Int64), inrSeq)
    | V.null inrSeq = (inlSeq, 0, inlSeq, inlSeq, V.replicate (V.length inlSeq) (maxBound :: Int64))
    | otherwise = trace ("got stuff "++ show alignRight) 
                    (median, cost, medGap, alignLeft, alignRight)
        where
            indelCost = 1
            subCost = 1
            --this for left right constant--want longer in left for Ukkonnen
            (lSeq, lLength, rSeq, rLength) = setLeftRight inlSeq inrSeq
            maxGap = 1 + lLength - rLength  --10000 :: Int --holder lseq - rSeq + 1
            (median, cost, medGap, alignLeft, alignRight) = ukkonenCore lSeq lLength rSeq rLength maxGap indelCost subCost

-- | tracebackUkkonen creates REVERSE mediian from nwMatrix, reverse to make tail
--recusive, for Ukkonen space/time saving offsets
--need to count gaps in traceback for threshold/barrier stuff
--CHANGE TO MAYBE (V.Vector Int64) FOR BARRIER CHECK
tracebackUkkonen :: V.Vector (V.Vector (Int, Int64, Direction)) -> BaseChar -> BaseChar -> Int -> Int -> Int -> Int -> Int -> V.Vector (Int64, Int64, Int64)
tracebackUkkonen nwMatrix inlSeq inrSeq posR posL maxGap rInDel lInDel | trace ("tracebackUkkonen " ++ show posR ++ show posL ++ show inlSeq ++ show inrSeq) False = undefined
tracebackUkkonen nwMatrix inlSeq inrSeq posR posL maxGap rInDel lInDel
--trace ("psLR " ++ show posR ++ " " ++ show posL ++ " Left " ++ show lInDel ++ " Right " ++ show rInDel ++ " maxGap " ++ show maxGap) (
    | (rInDel  > (maxGap - 2)) || (lInDel > (maxGap - 2)) = V.singleton ((0 :: Int64), (0 :: Int64), (0 :: Int64))  
    | posL <= 0 && posR <= 0 = trace "not y" V.empty
    | otherwise = trace "y" $ let 
        y   | direction == LeftDir = V.cons (state, inDelBit, inrSeq V.! (posR - 1)) (tracebackUkkonen nwMatrix inlSeq inrSeq posR (posL - 1) maxGap rInDel (lInDel + 1))
            | direction == DownDir = V.cons (state, inlSeq V.! (posL - 1), inDelBit) (tracebackUkkonen nwMatrix inlSeq inrSeq (posR - 1) posL maxGap (rInDel + 1) lInDel)  
            | otherwise = V.cons (state, inlSeq V.! (posL - 1), inrSeq V.! (posR - 1)) (tracebackUkkonen nwMatrix inlSeq inrSeq (posR - 1) (posL - 1) maxGap rInDel lInDel)
        in trace (show y) y
        where (_, state, direction) = (nwMatrix V.! posR) V.! (transformFullYShortY posL posR  maxGap) --(transformFullYShortY posL posR maxGap)

-- | getFirstRowUkkonen initializes first row of NW-Ukkonen matrix
getFirstRowUkkonen :: Int -> Int -> Int -> Int -> BaseChar -> Int -> V.Vector (Int, Int64, Direction)
--getFirstRowUkkonen _ rowLen position _ lSeq _ | trace ("getFirstRowUkkonen " ++ show lSeq ++ show position ++ show rowLen) False = undefined
getFirstRowUkkonen indelCost rowLength position prevCost lSeq  maxGap
 --trace ("row 0 pos " ++ show position ++ "/" ++ show (maxShortY rowLength 0 maxGap) ++ " rowLength " ++ show rowLength ++ " maxGap " ++ show maxGap ++ " lseq " ++ show lSeq)
    | position == rowLength  + 1 = V.empty
    | position == (maxGap + 1) = V.singleton (barrierCost, barrierBit, LeftDir) 
    | position == 0 = V.cons (0, inDelBit, DiagDir) (getFirstRowUkkonen indelCost rowLength (position + 1) 0 lSeq maxGap) 
    | otherwise = --trace ("FRC " ++ show newCost)
        let 
        y   | (newState /= inDelBit) = --if there was no inDel overlap between states
                V.cons (newCost, newState, LeftDir) (getFirstRowUkkonen  indelCost rowLength (position + 1) newCost lSeq maxGap)
            | otherwise = --indel in both states so no cost
                V.cons (prevCost, newState, LeftDir) (getFirstRowUkkonen  indelCost rowLength (position + 1) prevCost lSeq maxGap)
        in y
        where 
            newCost = prevCost + indelCost
            newState = getUnionIntersectionState inDelBit (lSeq V.! (position - 1))


-- | getRowUkkonen starts at second row (=1) and creates each row in turn--Ukkonen
getRowsUkkonen :: BaseChar -> BaseChar -> Int -> Int -> Int -> V.Vector (Int, Int64, Direction) -> Int -> V.Vector (V.Vector (Int, Int64, Direction))
getRowsUkkonen lSeq rSeq indelCost subCost rowNum prevRow maxGap | trace "getRowsUkkonen" False = undefined
getRowsUkkonen lSeq rSeq indelCost subCost rowNum prevRow maxGap
    | rowNum == ((V.length rSeq) + 1) = V.empty
    | startPosition == 0 = --trace ("Row " ++ show rowNum ++ " of " ++ show (V.length rSeq) ++ " starts " ++ show startPosition ++ ":" ++ show thisRowZero) (
                V.cons thisRowZero (getRowsUkkonen lSeq rSeq indelCost subCost (rowNum + 1) thisRowZero maxGap) --)
    | otherwise = --trace ("Row " ++ show rowNum ++ " of " ++ show (V.length rSeq) ++" starts " ++ show startPosition ++ ":" ++ show thisRowNonZero) (
        V.cons thisRowNonZero (getRowsUkkonen lSeq rSeq indelCost subCost (rowNum + 1) thisRowNonZero maxGap) --)
        where 
            startPosition = max 0 (rowNum - maxGap) --check for left barriers 
            thisRowZero =  getThisRowUkkonen lSeq rSeq indelCost subCost rowNum prevRow startPosition (V.length lSeq) 0 maxGap
            thisRowNonZero = V.cons (barrierCost, barrierBit, DownDir) (getThisRowUkkonen lSeq rSeq indelCost subCost rowNum prevRow startPosition  (V.length lSeq) barrierCost maxGap )

-- | getThisRowUkkonen takes sequences and parameters with row number and make a non-first
--row--Ukkonen
getThisRowUkkonen :: BaseChar -> BaseChar -> Int -> Int -> Int ->  V.Vector (Int, Int64, Direction) -> Int -> Int -> Int -> Int -> V.Vector (Int, Int64, Direction)
getThisRowUkkonen lSeq rSeq indelCost subCost rowNum prevRow position rowLength prevCost maxGap
    | position ==  rowLength  + 1 = V.empty
    | position == (rowNum + maxGap + 1) = V.singleton (barrierCost, barrierBit, LeftDir)
    | position == 0 = let
        x   | (newState /= inDelBit) =
                V.cons (upValue + indelCost, newState, DownDir) (getThisRowUkkonen lSeq rSeq indelCost subCost rowNum prevRow (position + 1) rowLength (upValue + indelCost) maxGap)
            | otherwise = 
                V.cons (upValue, newState, DownDir) (getThisRowUkkonen lSeq rSeq indelCost subCost rowNum prevRow (position + 1) rowLength upValue maxGap)
                where
                    newState = getUnionIntersectionState inDelBit (rSeq V.! (rowNum - 1))
                    (upValue, _, _) = prevRow V.! position
        in x
    | otherwise = V.cons (minCost, minState, minDir) (getThisRowUkkonen lSeq rSeq indelCost subCost rowNum prevRow (position + 1) rowLength minCost maxGap)
        where
            lSeqPos = position - 1 --since first is '-' the index is row/pos - 1
            rSeqRow = rowNum - 1 --since first is '-' the index is row/pos - 1
            leftCost = getOverlapCost prevCost indelCost (lSeq V.! lSeqPos) --need to check for overlap
            (upValue, _, _) = prevRow V.! (transformFullYShortY  position (rowNum - 1) maxGap)
            downCost = getOverlapCost upValue indelCost (rSeq V.! rSeqRow) --need to check for overlap
            (diagValue, _, _) = prevRow V.! (transformFullYShortY  (position - 1) (rowNum - 1) maxGap)
            intersection = (lSeq V.! lSeqPos) .&. (rSeq V.! rSeqRow)
            union = (lSeq V.! lSeqPos) .|. (rSeq V.! rSeqRow)
            (diagCost, diagState) = getDiagDirCost diagValue intersection union subCost
            (minCost, minState, minDir) = getMinCostDir leftCost downCost diagCost diagState 
                (getUnionIntersectionState inDelBit (lSeq V.! lSeqPos)) (getUnionIntersectionState inDelBit (rSeq V.! rSeqRow)) 

-- | firstOfThree takes a triple and returns first member
firstOfThree :: (a, b, c) -> a
firstOfThree (in1, in2, in3) = in1

-- | secondOfThree takes a triple and returns second member
secondOfThree :: (a, b, c) -> b
secondOfThree (in1, in2, in3) = in2

-- | thirdOfThree takes a triple and returns third member
thirdOfThree :: (a, b, c) -> c
thirdOfThree (in1, in2, in3) = in3
