-----------------------------------------------------------------------------
-- |
-- Module      :  Analysis.Parsimony.Binary.DirectOptimization.Internal
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

module Analysis.Parsimony.Binary.DirectOptimization.Internal where

import Bio.Metadata
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
type DOAlignMatrix s = Matrix (Double, Direction, s)

-- | Constraints on the input dynamic characters that direct optiomization operates on.
type DOCharConstraint c = (EncodableDynamicCharacter c, Show c, {- Memoizable c, -} Show (Element c))

-- | Performs a naive direct optimization
-- Takes in two characters to run DO on and a metadata object
-- Returns an assignment character, the cost of that assignment, the assignment character with gaps included,
-- the aligned version of the first input character, and the aligned version of the second input character
-- The process for this algorithm is to generate a traversal matrix, then perform a traceback.
naiveDO :: DOCharConstraint s  
        => s                    -- ^ First  dynamic character
        -> s                    -- ^ Second dynamic character
        -> CostStructure        -- ^ Structure defining the transition costs between character states
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
    | otherwise   = (ungapped, cost, gapped', out1, out2)
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
doAlignment :: DOCharConstraint s => s -> s -> CostStructure -> (s, s)
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
createDOAlignMatrix :: EncodableDynamicCharacter s => s -> s -> CostStructure -> DOAlignMatrix (Element s)
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
getOverlap :: (EncodableStreamElement c {- , Memoizable c, -}) => c -> c -> CostStructure -> (c, Double)
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
overlap :: (EncodableStreamElement c {- , Show c -}) => CostStructure -> c -> c -> (c, Double)
--overlap _ inChar1 inChar2 | trace (unwords [show inChar1, show inChar2]) False = undefined
overlap costStruct char1 char2
    | intersectionStates == zeroBits = -- (\x -> trace (unwords [show char1, show char2, show x]) x) $
                                       minimalChoice $ allPossibleBaseCombosCosts costStruct char1 char2
    | otherwise                      = (intersectionStates, 0)
    {-
       | 0 == char1 || 0 == char2 = (zeroBitVec, 0) -- Commented out, because nonsense. Problem for testing?
       | char1 .&. char2 == 0 = foldr1 ambigChoice allPossibleBaseCombosCosts
       | otherwise            = (char1 .&. char2, 0)
    -}
    where
      intersectionStates = char1 .&. char2
--        alphLen    = width char1
--        z = char1 `xor` char1
        -- make possible combinations with a double fold
        
        -- now take an ambiguous minimum

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
allPossibleBaseCombosCosts :: EncodableStreamElement s => CostStructure -> s -> s -> [(s, Double)]
allPossibleBaseCombosCosts costStruct char1 char2 = [ getCost costStruct x y | x <- getSubChars char1
                                                                             , y <- getSubChars char2
                                                    ]

-- TODO: This won't work with an asymmetric TCM.
-- |
-- Given a 'CostStructure' and two tuples of an 'Int' and an unambiguous 'EncodableStreamElement', determines the cost 
-- of a pairing (intersection) of those characters into an ambiguous character. The 'Int's are the set bits in each character
-- and are used as lookup into the 'CostStructure'. 
-- Tests exist in the test suite.
getCost :: EncodableStreamElement s => CostStructure -> (Int, s) -> (Int, s) -> (s, Double)
getCost costStruct seqTup1 seqTup2 = 
    case (costStruct, seqTup1, seqTup2) of
       -- (AffineCost {}        , _         , _         ) -> error "Cannot apply DO algorithm on affine cost" -- When this is added, remember to write a test.
        (TCM costMatrix       , (pos1, c1), (pos2, c2)) -> (c1 .|. c2, costMatrix ! (pos1, pos2))
        (GeneralCost indel sub, (_   , c1), (_   , c2)) -> -- f indel sub c1 c2
                                                           if c1 == gap || c2 == gap
                                                           then (c1 .|. c2, indel)
                                                           else (c1 .|. c2, sub)
    where
      s   = snd seqTup1
--      z   = s `xor` s
      gap = getGapElement s -- z `setBit` (symbolCount s - 1)
{-      
      f indel sub c1 c2
        | indel == sub                             = (c1 .|. c2,   sub)
        | indel >  sub &&  c1 == gap && c2 == gap  = (      gap, indel)
        | indel >  sub &&  c1 == gap && c2 /= gap  = (       c2,   sub)
        | indel >  sub &&  c1 /= gap && c2 == gap  = (       c1,   sub)
        | indel >  sub                             = (c1 .|. c2,   sub)
        | indel <  sub &&  c1 == gap && c2 == gap  = (      gap, indel)
        | indel <  sub &&  c1 == gap && c2 /= gap  = (      gap, indel)
        | indel <  sub &&  c1 /= gap && c2 == gap  = (      gap, indel)
        | otherwise                                = (c1 .|. c2,   sub)
-}

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
