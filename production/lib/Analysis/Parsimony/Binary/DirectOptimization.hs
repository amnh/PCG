-----------------------------------------------------------------------------
-- |
-- Module      :  Analysis.Parsimony.Binary.DirectOptimization
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

module Analysis.Parsimony.Binary.DirectOptimization where

import Analysis.General.NeedlemanWunsch
import Analysis.Parsimony.Binary.Constraints
import Bio.Metadata
import Bio.Character.Dynamic.Coded



-- | Performs a naive direct optimization
-- Takes in two characters to run DO on and a metadata object
-- Returns an assignment character, the cost of that assignment, the assignment character with gaps included,
-- the aligned version of the first input character, and the aligned version of the second input character
-- The process for this algorithm is to generate a traversal matrix, then perform a traceback.
naiveDO :: (Metadata m s, SeqConstraint' s) => s -> s -> m -> (s, Double, s, s, s)
naiveDO char1 char2 meta
    | isEmpty char1 = (char1, 0, char1, char1, char1)
    | isEmpty char2 = (char2, 0, char2, char2, char2)
    | otherwise =
        let
            char1Len = numChars char1
            char2Len = numChars char2
            (shorterChar, longerChar, _longLen) = if char1Len > char2Len
                                         then (char2, char1, char1Len)
                                         else (char1, char2, char2Len)
            traversalMat = createAlignMtx longerChar shorterChar meta
            cost = getMatrixCost traversalMat
            (gapped, left, right) = traceback traversalMat shorterChar longerChar
            -- TODO: change to occur in traceback, to remove constant factor.
            ungapped = filterGaps gapped
            (out1, out2) = if char1Len > char2Len
                                then (right, left)
                                else (left, right)
        in (ungapped, cost, gapped, out1, out2)
            
