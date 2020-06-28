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
-- These functions will allocate an M * N matrix.
--
-----------------------------------------------------------------------------

{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.NeedlemanWunsch
  ( naiveDO
  , naiveDOMemo
  ) where

import Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.Internal
import Bio.Character.Encodable
import Bio.Metadata.Dynamic                                            (overlap2)
import Data.Bits
import Data.Matrix.NotStupid                                           (matrix)
import Data.MonoTraversable


-- |
-- Performs a naive direct optimization.
-- Takes in two characters to run DO on and a metadata object
-- Returns an assignment character, the cost of that assignment, the assignment
-- character with gaps included, the aligned version of the first input character,
-- and the aligned version of the second input character. The process for this
-- algorithm is to generate a traversal matrix, then perform a traceback.
{-# INLINE naiveDO #-}
{-# SPECIALISE naiveDO :: (Word -> Word -> Word) -> DynamicCharacter -> DynamicCharacter -> (Word, DynamicCharacter) #-}
naiveDO
  :: ( DOCharConstraint s
     , FiniteBits (Subcomponent (Element s))
     )
  => (Word -> Word -> Word)  -- ^ Structure defining the transition costs between character states
  -> s                       -- ^ First  dynamic character
  -> s                       -- ^ Second dynamic character
  -> (Word, s)               -- ^ The cost and resulting the alignment
naiveDO costStruct char1 char2 = directOptimization (overlap2 costStruct) char1 char2 createNeedlemanWunchMatrix


-- |
-- The same as 'naiveDO' except that the "cost structure" parameter is assumed to
-- be a memoized overlap function.
{-# INLINE naiveDOMemo #-}
{-# SPECIALISE naiveDOMemo :: OverlapFunction AmbiguityGroup -> DynamicCharacter -> DynamicCharacter -> (Word, DynamicCharacter) #-}
naiveDOMemo :: DOCharConstraint s
            => OverlapFunction (Subcomponent (Element s))
            -> s
            -> s
            -> (Word, s)
naiveDOMemo tcm char1 char2 = directOptimization tcm char1 char2 createNeedlemanWunchMatrix


-- |
-- Main function to generate a 'NeedlemanWunchMatrix'. Works as in Needleman-Wunsch,
-- but allows for multiple indel/replacement costs, depending on the symbol change
-- cost function. Also, returns the aligned parent characters, with appropriate
-- ambiguities, as the third of each tuple in the matrix.
--
-- Takes in two 'EncodableDynamicCharacter's and a 'CostStructure'. The first
-- character must be the longer of the two and is the top labeling of the matrix.
-- Returns a 'NeedlemanWunchMatrix'.
{-# INLINE createNeedlemanWunchMatrix #-}
{-# SPECIALISE createNeedlemanWunchMatrix ::OverlapFunction AmbiguityGroup ->  DynamicCharacter -> DynamicCharacter -> NeedlemanWunchMatrix #-}
createNeedlemanWunchMatrix
  :: DOCharConstraint s
  => OverlapFunction (Subcomponent (Element s))
  -> s
  -> s
  -> NeedlemanWunchMatrix
createNeedlemanWunchMatrix overlapFunction topChar leftChar = result
  where
    result             = matrix rows cols generatingFunction
    rows               = olength leftChar + 1
    cols               = olength topChar  + 1
    generatingFunction = needlemanWunschDefinition overlapFunction topChar leftChar result
