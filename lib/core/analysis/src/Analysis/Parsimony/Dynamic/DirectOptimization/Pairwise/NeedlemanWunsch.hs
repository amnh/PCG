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
{-# SPECIALISE naiveDO :: DynamicCharacter -> DynamicCharacter -> (Word -> Word -> Word) -> (Word, DynamicCharacter) #-}
naiveDO
  :: ( DOCharConstraint s
     , FiniteBits (Subcomponent (Element s))
     )
  => s                       -- ^ First  dynamic character
  -> s                       -- ^ Second dynamic character
  -> (Word -> Word -> Word)  -- ^ Structure defining the transition costs between character states
  -> (Word, s)      -- ^ The cost of the alignment
                                   --
                                   --   The /ungapped/ character derived from the the input characters' N-W-esque matrix traceback
                                   --
                                   --   The /gapped/ character derived from the the input characters' N-W-esque matrix traceback
                                   --
                                   --   The gapped alignment of the /first/ input character when aligned with the second character
                                   --
                                   --   The gapped alignment of the /second/ input character when aligned with the first character

naiveDO char1 char2 costStruct = directOptimization char1 char2 (overlap2 costStruct) createNeedlemanWunchMatrix


-- |
-- The same as 'naiveDO' except that the "cost structure" parameter is assumed to
-- be a memoized overlap function.
{-# INLINE naiveDOMemo #-}
{-# SPECIALISE naiveDOMemo :: DynamicCharacter -> DynamicCharacter -> OverlapFunction AmbiguityGroup -> (Word, DynamicCharacter) #-}
naiveDOMemo :: DOCharConstraint s
            => s
            -> s
            -> OverlapFunction (Subcomponent (Element s))
            -> (Word, s)
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
{-# INLINE createNeedlemanWunchMatrix #-}
{-# SPECIALISE createNeedlemanWunchMatrix :: DynamicCharacter -> DynamicCharacter -> OverlapFunction AmbiguityGroup -> NeedlemanWunchMatrix #-}
createNeedlemanWunchMatrix :: DOCharConstraint s => s -> s -> OverlapFunction (Subcomponent (Element s)) -> NeedlemanWunchMatrix
createNeedlemanWunchMatrix topChar leftChar overlapFunction = result
  where
    result             = matrix rows cols generatingFunction
    rows               = olength leftChar + 1
    cols               = olength topChar  + 1
    generatingFunction = needlemanWunschDefinition topChar leftChar overlapFunction result
