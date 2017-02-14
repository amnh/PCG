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

{-# LANGUAGE FlexibleContexts #-}

module Analysis.Parsimony.Fitch.Internal where

import Bio.Character.Decoration.Discrete
import Bio.Character.Decoration.Fitch
import Bio.Character.Encodable
import Control.Lens
import Data.Bits
-- import Data.Key
import Data.List.NonEmpty (NonEmpty( (:|) ))
-- import Debug.Trace


-- | Used on the post-order (i.e. first) traversal.
fitchPostOrder ::  DiscreteCharacterDecoration d c
               => d
               -> [FitchOptimizationDecoration c]
               -> FitchOptimizationDecoration c
fitchPostOrder parentDecoration xs =
    case xs of
        []   -> initializeLeaf  parentDecoration         -- a leaf
        y:ys -> updatePostOrder parentDecoration (y:|ys)


-- | Used on the pre-order (i.e. second) traversal.
fitchPreOrder :: EncodableStaticCharacter c
              => FitchOptimizationDecoration c
              -> [(Word, FitchOptimizationDecoration c)]
              -> FitchOptimizationDecoration c
fitchPreOrder childDecoration (_:_:_) = childDecoration   -- two parents; shouldn't be possible, but here for completion
fitchPreOrder childDecoration []      = childDecoration & finalMedian .~ (childDecoration ^. preliminaryMedian)
fitchPreOrder childDecoration [(_, parentDecoration)] =
    if childDecoration ^. isLeaf
        then childDecoration & finalMedian .~ (childDecoration ^. preliminaryMedian)
        else determineFinalState parentDecoration childDecoration


-- |
-- Used in first, postorder, pass. Take in parent and two child nodes. Using the child preliminary decorations,
-- calculate the preliminary character state for the parent node. In addition, calculate the cost of assigning
-- that character state to the parent.
updatePostOrder :: DiscreteCharacterDecoration d c
                => d
                -> NonEmpty (FitchOptimizationDecoration c)
                -> FitchOptimizationDecoration c
updatePostOrder _parentDecoration (x:|[])                         = x                    -- Shouldn't be possible, but here for completion.
updatePostOrder _parentDecoration (leftChildDec:|rightChildDec:_) = {- trace (show returnNodeDecoration) $ -} returnNodeDecoration -- Not a leaf
    where
        returnNodeDecoration =
            extendDiscreteToFitch leftChildDec totalCost median emptyChar (leftChildDec ^. preliminaryMedian,
                                                                           rightChildDec ^. preliminaryMedian) False
        -- fold over states of character. This is Fitch so final cost is either 0 or 1.
        -- (median, parentCost) = foldlWithKey f initializedAcc [0..length (leftChildDec ^. characterAlphabet) - 1]
        (median, parentCost)
            | popCount union > 0 = (union, 0)
            | otherwise          = (intersection,  1)
        union        = (leftChildDec ^. preliminaryMedian) .&. (rightChildDec ^. preliminaryMedian)
        intersection = (leftChildDec ^. preliminaryMedian) .|. (rightChildDec ^. preliminaryMedian)
        totalCost    = parentCost + (leftChildDec ^. characterCost) + (rightChildDec ^. characterCost)
        emptyChar    = emptyStatic $ leftChildDec ^. discreteCharacter


-- |
-- A leaf has cost 0 and its preliminary character state is also its final character state.
-- Its "child preliminary medians" are empty lists.
initializeLeaf :: DiscreteCharacterDecoration d c => d -> FitchOptimizationDecoration c
initializeLeaf leafDecoration =
    extendDiscreteToFitch leafDecoration 0 leafChar emptyChar (emptyChar, emptyChar) True
    where
        --label     = leafDecoration ^. discreteCharacter -- can skip this now, because it's set in post order
        emptyChar = emptyStatic leafChar
        leafChar  = leafDecoration ^. discreteCharacter


-- |
-- Using the preliminary state of the current node, as well as the preliminary states of its parent and sibling,
-- compute the final state of the character using Fitch's ordered rules.
determineFinalState :: EncodableStaticCharacter c
                    => FitchOptimizationDecoration c
                    -> FitchOptimizationDecoration c
                    -> FitchOptimizationDecoration c
determineFinalState parentDecoration childDecoration = finalDecoration
    where
        -- following two should both short-circuit
        curIsSuperset = (ancestor `intersect` preliminary) == ancestor

        curIsUnion    = (left `union` right) == preliminary

            -- using parentDecoration here because I need a DiscreteCharacterDecoration.
            -- Safe because new char is created.
        finalDecoration = extendDiscreteToFitch parentDecoration cost preliminary median (left, right) leafVal
        leafVal         = childDecoration  ^. isLeaf
        cost            = childDecoration  ^. characterCost
        preliminary     = childDecoration  ^. preliminaryMedian
        ancestor        = parentDecoration ^. preliminaryMedian
        (left, right)   = childDecoration  ^. childMedians
        union     l r   = l .|. r
        intersect l r   = l .&. r
        median
            | curIsSuperset = ancestor                                                                          -- Fitch rule 1
            | curIsUnion    = ancestor `union` preliminary                                                      -- Fitch rule 2
            | otherwise     = ancestor `union` (left `intersect` ancestor) `union` (right `intersect` ancestor) -- Fitch rule 3

