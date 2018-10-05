-----------------------------------------------------------------------------
-- |
-- Module      :  Analysis.Parsimony.Fitch
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Standard Fitch (non-additive) character analysis (cost and medians).
--
-- This only works on static characters, and due to the traversal, only one
-- character will be received at a time.
--
-- Assumes binary trees.
--
-----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

module Analysis.Parsimony.Fitch.Internal where

import Bio.Character.Decoration.Discrete
import Bio.Character.Decoration.Fitch
import Bio.Character.Encodable
import Control.Lens
import Data.Bits
import Data.List.NonEmpty                (NonEmpty (..))
import Analysis.Parsimony.Internal


-- | Used on the post-order (i.e. first) traversal.
fitchPostorder
  :: DiscreteCharacterDecoration d c
  => (PostorderBinaryContext d (FitchOptimizationDecoration c))
  -> (FitchOptimizationDecoration c)
fitchPostorder = postorderBinaryContext initializeLeaf updatePostorder



-- | Used on the pre-order (i.e. second) traversal.
fitchPreOrder
  :: EncodableStaticCharacter c
  => FitchOptimizationDecoration c
  -> [(Word, FitchOptimizationDecoration c)]
  -> FitchOptimizationDecoration c
fitchPreOrder childDecoration (_:_:_) = childDecoration   -- two parents; shouldn't be possible, but here for completion
fitchPreOrder childDecoration []      = let !prelim = childDecoration ^. preliminaryMedian
                                        in  childDecoration
                                              & finalMedian       .~ prelim
                                              & discreteCharacter .~ prelim
fitchPreOrder childDecoration [(_, parentDecoration)]
  | childDecoration ^. isLeaf = childDecoration & finalMedian .~ (childDecoration ^. preliminaryMedian) -- leaf
  | otherwise                 = determineFinalState parentDecoration childDecoration                    -- internal node


-- |
-- Used in first, post-order, pass. Take in parent and two child nodes. Using the child preliminary decorations,
-- calculate the preliminary character state for the parent node. In addition, calculate the cost of assigning
-- that character state to the parent.
updatePostorder
  :: DiscreteCharacterDecoration d c
  => d
  -> (FitchOptimizationDecoration c , FitchOptimizationDecoration c)
  -> FitchOptimizationDecoration c
updatePostorder _parentDecoration (leftChildDec , rightChildDec) =
    extendDiscreteToFitch
      leftChildDec
      totalCost
      median
      emptyChar
      (leftChildDec ^. preliminaryMedian, rightChildDec ^. preliminaryMedian)
      False
  where
    -- fold over states of character. This is Fitch so final cost is either 0 or 1.
    (median, parentCost)
      | popCount intersection > 0 = (intersection, 0)
      | otherwise                 = (       union, 1)

    union        = (leftChildDec ^. preliminaryMedian) .|. (rightChildDec ^. preliminaryMedian)
    intersection = (leftChildDec ^. preliminaryMedian) .&. (rightChildDec ^. preliminaryMedian)
    totalCost    = parentCost + (leftChildDec ^. characterCost) + (rightChildDec ^. characterCost)
    emptyChar    = emptyStatic $ leftChildDec ^. discreteCharacter


-- |
-- A leaf has cost 0 and its preliminary character state is also its final character state.
-- Its "child preliminary medians" are empty lists.
initializeLeaf :: DiscreteCharacterDecoration d c => d -> FitchOptimizationDecoration c
initializeLeaf leafDecoration =
    extendDiscreteToFitch leafDecoration 0 leafChar emptyChar (emptyChar, emptyChar) True
  where
    emptyChar = emptyStatic leafChar
    leafChar  = leafDecoration ^. discreteCharacter


-- |
-- Using the preliminary state of the current node, as well as the preliminary states of its parent and sibling,
-- compute the final state of the character using Fitch's ordered rules.
determineFinalState
  :: EncodableStaticCharacter c
  => FitchOptimizationDecoration c
  -> FitchOptimizationDecoration c
  -> FitchOptimizationDecoration c
determineFinalState parentDecoration childDecoration = interimDecoration & discreteCharacter .~ median
  where
    -- Following two should both short-circuit.
    curIsSuperset = (ancestor .&. preliminary) == ancestor

    curIsUnion    = (left .|. right) == preliminary

    -- Using parentDecoration here because I need a DiscreteCharacterDecoration.
    -- Safe because new char is created.
    interimDecoration = extendDiscreteToFitch parentDecoration cost preliminary median (left, right) leafVal
    leafVal           = childDecoration  ^. isLeaf
    cost              = childDecoration  ^. characterCost
    preliminary       = childDecoration  ^. preliminaryMedian
    ancestor          = parentDecoration ^. preliminaryMedian
    (left, right)     = childDecoration  ^. childMedians
    median
      | curIsSuperset = ancestor                                                  -- Fitch rule 1
      | curIsUnion    = ancestor .|. preliminary                                  -- Fitch rule 2
      | otherwise     = ancestor .|. (left .&. ancestor) .|. (right .&. ancestor) -- Fitch rule 3
