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
import Data.DList            (snoc)
import Data.Foldable
import Data.Key              ((!))
import Data.Matrix.NotStupid (Matrix, getRow, matrix, nrows, ncols, toLists)
import Data.MonoTraversable
import Data.Ord
import Data.Semigroup

-- import Debug.Trace



-- It should be noted that the ordering of the three arrow types are important
-- as it guarantees that the derived Ord instance will have the following property:
--
-- DiagArrow < LeftArrow < UpArrow
--
-- This means DiagArrow is biased towards most   when one or more costs are equal
--            LeftArrow is biased towards second when one or more costs are equal
--              UpArrow is biased towards least  when one or more costs are equal
--
-- Using this Ord instance, we can resolve ambiguous transformations in a
-- deterministic way. Without loss of generality in determining the ordering,
-- we choose the same biasing as in POY 5.
-- | The direction to align the character at a given matrix point.
data Direction = DiagArrow | LeftArrow | UpArrow
  deriving (Eq, Ord)


instance Show Direction where

  show DiagArrow = "↖"
  show LeftArrow = "←"
  show UpArrow   = "↑"


-- | A representation of an alignment matrix for DO.
-- The matrix itself stores tuples of the cost and direction at that position.
-- We also store a vector of characters that are generated.
type DOAlignMatrix s = Matrix (Word, Direction, s)


-- | Constraints on the input dynamic characters that direct optiomization operates on.
type DOCharConstraint c = (EncodableDynamicCharacter c, {- Show c, -} Show (Element c), Integral (Element c))


type OverlapFunction c = c -> c -> (c, Word)


handleMissingCharacter
  :: PossiblyMissingCharacter s
  => s
  -> s
  -> (Word, s, s, s, s)
  -> (Word, s, s, s, s) 
handleMissingCharacter lhs rhs v =
    -- Appropriately handle missing data:
    case (isMissing lhs, isMissing rhs) of
      (True , True ) -> (0, lhs, lhs, lhs, rhs) --WLOG
      (True , False) -> (0, rhs, rhs, rhs, rhs)
      (False, True ) -> (0, lhs, lhs, lhs, lhs)
      (False, False) -> v


-- |
-- Performs a naive direct optimization.
-- Takes in two characters to run DO on and a metadata object
-- Returns an assignment character, the cost of that assignment, the assignment character with gaps included,
-- the aligned version of the first input character, and the aligned version of the second input character
-- The process for this algorithm is to generate a traversal matrix, then perform a traceback.
naiveDO :: DOCharConstraint s
        => s                       -- ^ First  dynamic character
        -> s                       -- ^ Second dynamic character
        -> (Word -> Word -> Word)  -- ^ Structure defining the transition costs between character states
        -> (Word, s, s, s, s)      -- ^ The cost of the alignment
                                   -- 
                                   --   The /ungapped/ character derived from the the input characters' N-W-esque matrix traceback
                                   -- 
                                   --   The /gapped/ character derived from the the input characters' N-W-esque matrix traceback
                                   -- 
                                   --   The gapped alignment of the /first/ input character when aligned with the second character
                                   -- 
                                   --   The gapped alignment of the /second/ input character when aligned with the first character
naiveDO char1 char2 costStruct = handleMissingCharacter char1 char2 $ naiveDOInternal char1 char2 (overlap costStruct)


-- |
-- The same as 'naiveDO' except that the "cost structure" parameter is ignored.
-- Instead a constant cost is used.
naiveDOConst :: DOCharConstraint s => s -> s -> (Word -> Word -> Word) -> (Word, s, s, s, s)
naiveDOConst char1 char2 _ = handleMissingCharacter char1 char2 $ naiveDOInternal char1 char2 overlapConst


-- |
-- The same as 'naiveDO' except that the "cost structure" parameter is assumed to be a memoized overlap function.
naiveDOMemo :: DOCharConstraint s
            => s
            -> s
            -> OverlapFunction (Element s)
            -> (Word, s, s, s, s)
naiveDOMemo char1 char2 tcm = handleMissingCharacter char1 char2 $ naiveDOInternal char1 char2 tcm


-- | Wrapper function to do an enhanced Needleman-Wunsch algorithm.
-- Calls naiveDO, but only returns the last two fields (gapped alignments of inputs)
doAlignment :: DOCharConstraint s => s -> s -> (Word -> Word -> Word) -> (s, s)
doAlignment char1 char2 costStruct = (char1Align, char2Align)
    where
        (_, _, _, char1Align, char2Align) = naiveDO char1 char2 costStruct
        

-- |
-- Strips the gaps from the supplied character.
filterGaps :: EncodableDynamicCharacter c => c -> c
filterGaps char = constructDynamic . filter (/= gap) $ otoList char
  where
    gap = getGapElement $ char `indexStream` 0


-- |
-- Main function to generate an 'DOAlignMatrix'. Works as in Needleman-Wunsch,
-- but allows for multiple indel/replacement costs, depending on the 'CostStructure'.
-- Also, returns the aligned parent characters, with appropriate ambiguities, as the third of
-- each tuple in the matrix.
-- Takes in two 'EncodableDynamicCharacter's and a 'CostStructure'. The first character
-- must be the longer of the two and is the top labeling of the matrix.
-- Returns an 'DOAlignMatrix'.
createDOAlignMatrix :: DOCharConstraint s => s -> s -> OverlapFunction (Element s) -> DOAlignMatrix (Element s)
createDOAlignMatrix topChar leftChar overlapFunction = {- trace renderedMatrix $ -} result
  where
    -- :)
    -- renderedMatrix = renderCostMatrix topChar leftChar result
    
    result = matrix (olength leftChar + 1) (olength topChar + 1) generateMat
--    gap    = gapOfStream leftChar -- The constructors of DynamicChar prevent an empty character construction.
    gap = gapOfStream topChar

    -- Internal generator function for the matrix
    -- Deals with both first row and other cases.
    generateMat (row, col)
      -- :)
      | row == 0 && col == 0         = (0                               , DiagArrow,         gap)
      | row == 0 && rightChar /= gap = (leftwardValue + rightOverlapCost, LeftArrow,   rightChar)
      | row == 0                     = (leftwardValue                   , LeftArrow,   rightChar)
      | col == 0 &&  downChar /= gap = (  upwardValue +  downOverlapCost,   UpArrow,    downChar)
      | col == 0                     = (  upwardValue                   ,   UpArrow,    downChar)
      | leftElement == gap &&
         topElement == gap           = (diagCost                        , DiagArrow,         gap)
      | otherwise                    = (minCost                         , minDir   ,    minState)
      where
        -- | 
        topElement                    =  topChar `indexStream` (col - 1)
        leftElement                   = leftChar `indexStream` (row - 1)
        (leftwardValue, _, _)         = result ! (row    , col - 1)
        (diagonalValue, _, _)         = result ! (row - 1, col - 1)
        (  upwardValue, _, _)         = result ! (row - 1, col    )
        (rightChar, rightOverlapCost) = overlapFunction topElement  gap
        ( diagChar,  diagOverlapCost) = overlapFunction topElement  leftElement 
        ( downChar,  downOverlapCost) = overlapFunction gap         leftElement
        rightCost                     = rightOverlapCost + leftwardValue
        diagCost                      =  diagOverlapCost + diagonalValue
        downCost                      =  downOverlapCost +   upwardValue
        (minCost, minState, minDir)   = minimumBy (comparing (\(c,_,d) -> (c,d)))
                                      [ (diagCost ,  diagChar        , DiagArrow)
                                      , (rightCost, rightChar .|. gap, LeftArrow)
                                      , (downCost ,  downChar .|. gap, UpArrow  )
                                      ]
{-                                        
        err = unlines
          [ show (row, col)
          , "  right: " <> show (fromIntegral rightChar, rightOverlapCost, leftwardValue, rightCost)
          , "   down: " <> show (fromIntegral  downChar,  downOverlapCost,   upwardValue,  downCost)
          , "   diag: " <> show (fromIntegral  diagChar,  diagOverlapCost, diagonalValue,  diagCost)
          , "Chosen:"
          , "  " <> show (minCost, fromIntegral minState, minDir) 
          ]            
-}


renderCostMatrix :: DOCharConstraint s => s -> s -> DOAlignMatrix a -> String
renderCostMatrix lhs rhs mtx = unlines
    [ dimensionPrefix
    , headerRow
    , barRow
    , renderedRows
    ]
  where
    (longer, lesser)
      | olength lhs >= olength rhs = (lhs, rhs)
      | otherwise                  = (rhs, lhs)
    longerTokens     = toShownIntegers longer
    lesserTokens     = toShownIntegers lesser
    toShownIntegers  = fmap (show . (fromIntegral :: Integral a => a -> Integer)) . otoList
    matrixTokens     = showCell <$> mtx
    showCell (c,d,_) = show c <> show d
    maxPrefixWidth   = maxLengthOf lesserTokens
    maxColumnWidth   = max (maxLengthOf longerTokens) . maxLengthOf $ toList matrixTokens
    maxLengthOf      = maximum . fmap length

    dimensionPrefix  = " " <> unwords
        [ "Dimensions:"
        , show $ olength longer + 1
        , "X"
        , show $ olength lesser + 1
        ]
    
    headerRow = mconcat
        [ " "
        , pad maxPrefixWidth "\\"
        , "| "
        , pad maxColumnWidth "*"
        , concatMap (pad maxColumnWidth) longerTokens
        ]

    barRow    = mconcat
        [ " "
        , bar maxPrefixWidth
        , "+"
        , concatMap (const (bar maxColumnWidth)) $ undefined : longerTokens
        ]
      where
        bar n = replicate (n+1) '-'

    renderedRows = unlines . zipWith renderRow ("*":lesserTokens) $ getRows matrixTokens
      where
        renderRow e vs = " " <> pad maxPrefixWidth e <> "| " <> concatMap (pad maxColumnWidth) vs
        getRows m = (`getRow` m) <$> [0 .. nrows m - 1]

    pad :: Int -> String -> String
    pad n e = replicate (n - len) ' ' <> e <> " "
      where
        len = length e


renderMatrix :: DOAlignMatrix a -> String
renderMatrix mat = unlines . fmap unwords . toLists $ showCell <$> mat
  where
    showCell (c,d,_) = show (c, d)
--    pokedVal = showCell $ getElem (nrows mat -1) (ncols mat - 1) mat


-- |
-- Performs the traceback of an 'DOAlignMatrix'.
-- Takes in an 'DOAlignMatrix', two 'EncodableDynamicCharacter's
-- Returns an aligned 'EncodableDynamicCharacter', as well as the aligned versions of the two inputs.
-- Essentially does the second step of Needleman-Wunsch, following the arrows from the bottom right corner, 
-- accumulating the sequences as it goes, but returns three alignments: the left character, the right character,
-- and the parent. The child alignments *should* be biased toward the shorter of the two sequences.
traceback :: (DOCharConstraint s) => DOAlignMatrix (Element s) -> s -> s -> (s, s, s)
traceback alignMatrix longerChar lesserChar = ( constructDynamic medianStates
                                              , constructDynamic alignedLongerChar
                                              , constructDynamic alignedLesserChar
                                              )
  where
      (medianStates, alignedLongerChar, alignedLesserChar) = tracebackInternal (nrows alignMatrix - 1, ncols alignMatrix - 1)
      gap = gapOfStream longerChar

      tracebackInternal p@(row, col)
        | p == (0,0) = (mempty, mempty, mempty)
        | otherwise  = ( previousMedianCharElements `snoc` medianElement
                       , previousLongerCharElements `snoc` longerElement
                       , previousLesserCharElements `snoc` lesserElement
                       )
        where
          (previousMedianCharElements, previousLongerCharElements, previousLesserCharElements) = tracebackInternal (row', col')
              
          (_, directionArrow, medianElement) = alignMatrix ! p

          (row', col', longerElement, lesserElement) =
              case directionArrow of
                LeftArrow -> (row    , col - 1, longerChar `indexStream` (col - 1),                               gap )
                UpArrow   -> (row - 1, col    ,                               gap , lesserChar `indexStream` (row - 1))
                DiagArrow -> (row - 1, col - 1, longerChar `indexStream` (col - 1), lesserChar `indexStream` (row - 1))


-- |
-- Simple function to get the cost from an alignment matrix
getTotalAlignmentCost :: Matrix (a, b, c) -> a
getTotalAlignmentCost alignmentMatrix = cost
  where
    (cost, _, _) = alignmentMatrix ! (nrows alignmentMatrix - 1, ncols alignmentMatrix - 1) 


-- |
-- Memoized wrapper of the overlap function
getOverlap :: (EncodableStreamElement c {- , Memoizable c, -}) => c -> c -> (Word -> Word -> Word) -> (c, Word)
getOverlap inChar1 inChar2 costStruct = result
    where
        result = {- memoize2 -} overlap costStruct inChar1 inChar2

        
-- |
-- Takes two 'EncodableStreamElement' and a 'CostStructure' and returns a tuple of a new character, 
-- along with the cost of obtaining that character. The return character may be (or is even likely to be)
-- ambiguous. Will attempt to intersect the two characters, but will union them if that is not possible,
-- based on the 'CostStructure'. 
--
-- To clarify, the return character is an intersection of all possible least-cost combinations, so for instance,
-- if @ char1 == A,T @ and @ char2 == G,C @, and the two (non-overlapping) least cost pairs are A,C and T,G, then
-- the return value is A,C,G,T. 
-- Tests exist in the test suite.
overlap :: (EncodableStreamElement c {- , Show c -}) => (Word -> Word -> Word) -> c -> c -> (c, Word)
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


-- |
-- Finds the cost of a pairing of two static characters.
-- Takes in a 'CostStructure' and two ambiguous 'EncodableStreamElement's. Returns a list of tuples of all possible unambiguous
-- pairings, along with their costs. 
allPossibleBaseCombosCosts :: EncodableStreamElement s => (Word -> Word -> Word) -> s -> s -> [(s, Word)]
allPossibleBaseCombosCosts costStruct char1 char2 = [ (x .|. y, costStruct i j)
                                                    | (i,x) <- getSubChars char1
                                                    , (j,y) <- getSubChars char2
                                                    ]

-- |
-- Given a 'CostStructure' and two tuples of an 'Int' and an unambiguous 'EncodableStreamElement', determines the cost 
-- of a pairing (intersection) of those characters into an ambiguous character. The 'Int's are the set bits in each character
-- and are used as lookup into the 'CostStructure'. 
-- Tests exist in the test suite.
getCost :: EncodableStreamElement s => (Word -> Word -> Word) -> (Word, s) -> (Word, s) -> (s, Word)
getCost costStruct seqTup1 seqTup2 = 
    case (seqTup1, seqTup2) of
        ((pos1, c1), (pos2, c2)) -> (c1 .|. c2, costStruct pos1 pos2)


-- |
-- Takes in a 'EncodableStreamElement', possibly with more than one bit set, and returns a list of tuples of 
-- 'Int's and 'EncodableStreamElement's, such that, for each set bit in the input, there is one element in the output list, 
-- a tuple with an 'Int', @ x @, giving the location of the set bit, as well as an 'EncodableStreamElement' of the same
-- length as the input, but with only the bit at location @ x @ set.
-- Tests exist in the test suite.
getSubChars :: EncodableStreamElement s => s -> [(Word, s)]
getSubChars fullChar = foldMap f [0 .. symbolCount fullChar - 1]
  where
    f i
      | fullChar `testBit` i = pure (toEnum i,  z `setBit` i)
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


overlapConst :: (EncodableStreamElement c {- , Show c -}) => c -> c -> (c, Word)
overlapConst lhs rhs
  | intersect == zeroBits = (lhs .|. rhs, 1)
  | otherwise             = (intersect  , 0)
  where
    intersect = lhs .&. rhs


naiveDOInternal
  :: DOCharConstraint s
  => s
  -> s
  -> OverlapFunction (Element s)
  -> (Word, s, s, s, s)
naiveDOInternal char1 char2 overlapFunction = (alignmentCost, ungapped, gapped', alignedChar1, alignedChar2)
    where
      char1Len = olength char1
      char2Len = olength char2
      swapped  = char1Len < char2Len
      (longerChar, shorterChar)
        | swapped   = (char2, char1)
        | otherwise = (char1, char2)
      traversalMat  = createDOAlignMatrix longerChar shorterChar overlapFunction
      alignmentCost = getTotalAlignmentCost traversalMat
      (gapped , left , right ) = traceback traversalMat longerChar shorterChar
      (gapped', left', right') = (\(x,y,z) -> (constructDynamic x, constructDynamic y, constructDynamic z)) 
                               $ correctBiasing (getGapElement $ gapped `indexStream` 0) (otoList gapped, otoList left, otoList right)
      ungapped = filterGaps gapped'
      (alignedChar1, alignedChar2)
        | swapped   = (right', left' )
        | otherwise = (left' , right')


