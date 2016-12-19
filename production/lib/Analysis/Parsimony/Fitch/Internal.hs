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
import Data.Key
import Data.List.NonEmpty (NonEmpty( (:|) ))

-- data FitchCharacterDecoration c = FitchCharacterDecoration
--     { minCost           :: Word32                                               -- cost of the subtree
--     , preliminaryMedian :: (EncodableStaticCharacter)                           -- held here until final state is determined
--                                                                                 -- and we can assign that into discreteCharacter
--     , childMedians      :: (EncodableStaticCharacter, EncodableStaticCharacter) -- (left, right) so that we can do post
--                                                                                 -- order pass with all of Fitch's rules
--     , isLeaf            :: Bool                                                 -- need this in preorder
--     }

-- | Used on the post-order (i.e. first) traversal.
fitchPostOrder ::  DiscreteCharacterDecoration d c
               => d
               -> [FitchOptimizationDecoration c]
               -> FitchOptimizationDecoration c
fitchPostOrder parentDecoration xs =
    case xs of
        []   -> initializeLeaf  parentDecoration                  -- a leaf
        y:ys -> updatePostOrder parentDecoration (y:|ys)

-- | Used on the pre-order (i.e. second) traversal.
fitchPreOrder :: EncodableStaticCharacter c
              => FitchOptimizationDecoration c
              -> [(Word, FitchOptimizationDecoration c)]
              -> FitchOptimizationDecoration c
fitchPreOrder childDecoration (_x:_y:_)                    = childDecoration   -- two parents; shouldn't be possible, but here for completion
fitchPreOrder childDecoration []                         = childDecoration   -- is a root TODO: need to change preliminary to final
fitchPreOrder childDecoration ((_, parentDecoration):[]) =
    if childDecoration ^. isLeaf
        then childDecoration
        else determineFinalState parentDecoration childDecoration

-- |
-- Used in second, preorder, pass. Take in parent and two child nodes. Using the child preliminary decorations,
-- calculate the preliminary character state for the parent node. In addition, calculate the cost of assigning
-- that character state to the parent.
updatePostOrder :: DiscreteCharacterDecoration d c
                => d
                -> NonEmpty (FitchOptimizationDecoration c)
                -> FitchOptimizationDecoration c
updatePostOrder _parentDecoration (x:|[])                        = x                    -- Shouldn't be possible, but here for completion.
updatePostOrder parentDecoration (leftChildDec:|rightChildDec:_) = returnNodeDecoration -- Not a leaf
    where
        returnNodeDecoration =
            extendDiscreteToFitch parentDecoration totalCost median emptyChar (leftChildDec ^. preliminaryMedian, rightChildDec ^. preliminaryMedian) False
        (median, parentCost) = foldlWithKey' f initializedAcc [0..length (parentDecoration ^. characterAlphabet) - 1]

        initializedAcc       = (emptyStatic $ parentDecoration ^. discreteCharacter , 1) -- Cost is set to 1 so that branches in guards below work correctly.
        isSet decoration key = (decoration   ^. preliminaryMedian) `testBit` key
        indel l r k          = (isSet l k) `xor` (isSet r k)
        noSub l r k          = (isSet l k)  &&   (isSet r k)    -- Same bit is on in both characters.
        totalCost            = parentCost + (leftChildDec ^. minCost) + (rightChildDec ^. minCost)
        emptyChar = emptyStatic $ parentDecoration ^. discreteCharacter
        f (inChar, cost) key _                                  -- In following, note that a 1 has been set to the character by
                                                                -- default, above. So we never have
                                                                -- to add value to the cost (it can never be > 1 under Fitch).
            | noSub leftChildDec rightChildDec key =            -- Characters share a state.
                if cost > 0
                    then (emptyChar `setBit` key, 0)            -- If there's a cost, then a previous indel has registered; reset cost and char value.
                    else (inChar    `setBit` key, 0)            -- Otherwise, add this state to character.
            | indel leftChildDec rightChildDec key =            -- There's an indel.
                if cost > 0
                    then (inChar `setBit` key, cost)            -- If there's a cost, then a previous indel has registered; add this state.
                    else (inChar,              cost)            -- Otherwise, make no changes.
            | otherwise = (inChar, cost)

-- |
-- A leaf has cost 0 and its preliminary character state is also its final character state.
-- Its "child preliminary medians" are empty lists.
initializeLeaf :: DiscreteCharacterDecoration d c => d -> FitchOptimizationDecoration c
initializeLeaf leafDecoration =
    extendDiscreteToFitch leafDecoration 0 emptyChar emptyChar (emptyChar, emptyChar) True
    where
        --label     = leafDecoration ^. discreteCharacter -- can skip this now, because it's set in post order
        emptyChar = emptyStatic $ leafDecoration ^. discreteCharacter


-- |
-- Using the preliminary state of the current node, as well as the preliminary states of its parent and sibling,
-- compute the final state of the character using Fitch's ordered rules.
determineFinalState :: DiscreteCharacterDecoration d c
                    => d
                    -> FitchOptimizationDecoration c
                    -> FitchOptimizationDecoration c
determineFinalState parentDecoration childDecoration = finalDecoration
    where
        curIsSuperset = foldl (\acc k -> if (ancestor `testBit` k) && (preliminary `testBit` k)
                                                       then acc && False
                                                       else acc && True
                              ) True [0..alphLen]
        -- TODO: see if this short-circuits; otherwise rewrite doing testbit three times and then logical operations
        curIsUnion    = foldl (\acc _index -> acc && (popCount (left .|. right `xor` preliminary) > 0)
                              ) True [0..alphLen]                         -- preliminary is 0 if both are 0, 1 otherwise
        finalDecoration = extendDiscreteToFitch parentDecoration cost preliminary median (left, right) leafVal
        leafVal         = childDecoration    ^. isLeaf
        cost            = childDecoration    ^. minCost
        preliminary     = childDecoration    ^. preliminaryMedian
        ancestor        = parentDecoration ^. discreteCharacter
        (left, right)   = childDecoration    ^. childMedians
        alphLen         = symbolCount $ childDecoration ^. discreteCharacter - 1
        union     l r   = l .|. r
        intersect l r   = l .&. r
        median
            | curIsSuperset = ancestor                                                                          -- Fitch rule 1
            | curIsUnion    = ancestor `union` preliminary                                                      -- Fitch rule 2
            | otherwise     = ancestor `union` (left `intersect` ancestor) `union` (right `intersect` ancestor) -- Fitch rule 3

