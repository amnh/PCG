-----------------------------------------------------------------------------
-- |
-- Module      :  Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.NeedlemanWunsch
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Direct optimization pairwise alignment using the Needleman-Wunsch algorithm.
-- These funtions will allocate an M * N matrix.
--
-----------------------------------------------------------------------------

{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.NeedlemanWunsch
  ( naiveDO
  , naiveDOConst
  , naiveDOMemo
  ) where

import Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.Internal
import Data.Matrix.NotStupid                                           (matrix)
import Data.MonoTraversable


-- |
-- Performs a naive direct optimization.
-- Takes in two characters to run DO on and a metadata object
-- Returns an assignment character, the cost of that assignment, the assignment
-- character with gaps included, the aligned version of the first input character,
-- and the aligned version of the second input character. The process for this
-- algorithm is to generate a traversal matrix, then perform a traceback.
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

naiveDO char1 char2 costStruct = directOptimization char1 char2 (overlap costStruct) createNeedlemanWunchMatrix


-- |
-- The same as 'naiveDO' except that the "cost structure" parameter is ignored.
-- Instead a constant cost is used.
naiveDOConst :: DOCharConstraint s => s -> s -> (Word -> Word -> Word) -> (Word, s, s, s, s)
naiveDOConst char1 char2 _ = directOptimization char1 char2 overlapConst createNeedlemanWunchMatrix


-- |
-- The same as 'naiveDO' except that the "cost structure" parameter is assumed to
-- be a memoized overlap function.
naiveDOMemo :: DOCharConstraint s
            => s
            -> s
            -> OverlapFunction (Element s)
            -> (Word, s, s, s, s)
naiveDOMemo char1 char2 tcm = directOptimization char1 char2 tcm createNeedlemanWunchMatrix


-- |
-- Main function to generate a 'NeedlemanWunchMatrix'. Works as in Needleman-Wunsch,
-- but allows for multiple indel/replacement costs, depending on the symbol change
-- cost function. Also, returns the aligned parent characters, with appropriate
-- ambiguities, as the third of each tuple in the matrix.
--
-- Takes in two 'EncodableDynamicCharacter's and a 'CostStructure'. The first
-- character must be the longer of the two and is the top labeling of the matrix.
-- Returns a 'NeedlemanWunchMatrix'.
createNeedlemanWunchMatrix :: DOCharConstraint s => s -> s -> OverlapFunction (Element s) -> NeedlemanWunchMatrix (Element s)
--createNeedlemanWunchMatrix topChar leftChar overlapFunction = trace renderedMatrix result
createNeedlemanWunchMatrix topChar leftChar overlapFunction = result
  where
    result             = matrix rows cols generatingFunction
    rows               = olength leftChar + 1
    cols               = olength topChar  + 1
    generatingFunction = needlemanWunschDefinition topChar leftChar overlapFunction result
--    renderedMatrix     = renderCostMatrix topChar leftChar result
