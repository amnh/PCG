-----------------------------------------------------------------------------
-- |
-- Module      :  Analysis.Parsimony.Dynamic.DirectOptimization.Internal
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Sankoff character analysis (cost and median)
--
-- This only works on static characters, and due to the traversal, only one
-- character will be received at a time.
--
-- Assumes binary trees.
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}

module Analysis.Parsimony.Dynamic.DirectOptimization.Internal where

import           Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise
import           Bio.Character.Decoration.Dynamic
import           Bio.Character.Encodable
import           Control.Lens
import           Data.IntMap        (IntMap)
import qualified Data.IntMap as IM
import           Data.Key    hiding ((!))
import           Data.List.NonEmpty (NonEmpty( (:|) ))
import           Data.Monoid
import           Data.MonoTraversable 
import           Data.Word
import           Prelude     hiding (lookup, zip, zipWith)

--import Debug.Trace


type PairwiseAlignment s = s -> s -> (s, Double, s, s, s)


directOptimizationPostOrder
  :: SimpleDynamicDecoration d c
  => PairwiseAlignment c
  -> d
  -> [DynamicDecorationDirectOptimizationPostOrderResult c]
  ->  DynamicDecorationDirectOptimizationPostOrderResult c
directOptimizationPostOrder pairwiseAlignment charDecoration xs =
    case xs of
        []   -> initializeLeaf charDecoration
        y:ys -> updateFromLeaves pairwiseAlignment $ y:|ys


initializeLeaf
  :: SimpleDynamicDecoration d c
  => d
  -> DynamicDecorationDirectOptimizationPostOrderResult c
initializeLeaf =
    extendDynamicToPostOrder
      <$> id
      <*> const 0
      <*> const 0
      <*> (^. encoded)
      <*> (^. encoded)
      <*> (^. encoded)
      <*> (^. encoded)


updateFromLeaves
  :: EncodableDynamicCharacter c
  => PairwiseAlignment c
  -> NonEmpty (DynamicDecorationDirectOptimizationPostOrderResult c)
  -> DynamicDecorationDirectOptimizationPostOrderResult c
updateFromLeaves _ (x:|[]) = x -- This shouldn't happen
updateFromLeaves pairwiseAlignment (leftChild:|rightChild:_) =
    extendDynamicToPostOrder leftChild localCost totalCost ungapped gapped lhsAlignment rhsAlignment
  where
    -- TODO:
    -- Change type of 'cost' to an integral value like 'Word'
    (ungapped, cost, gapped, lhsAlignment, rhsAlignment) = pairwiseAlignment (leftChild ^. preliminaryUngapped) (rightChild ^. preliminaryUngapped)
    localCost = truncate cost
    totalCost = localCost + leftChild ^. characterCost + rightChild ^. characterCost


directOptimizationPreOrder
  :: (DirectOptimizationPostOrderDecoration d c, Show c, Show (Element c))
  => PairwiseAlignment c
  -> d
  -> [(Word, DynamicDecorationDirectOptimization c)]
  ->  DynamicDecorationDirectOptimization c
directOptimizationPreOrder pairwiseAlignment charDecoration xs =
    case xs of
        []            -> initializeRoot charDecoration
        (_, parent):_ -> updateFromParent pairwiseAlignment charDecoration parent


initializeRoot
  :: DirectOptimizationPostOrderDecoration d c
  => d
  -> DynamicDecorationDirectOptimization c
initializeRoot =
    extendPostOrderToDirectOptimization
      <$> id
      <*> (^. preliminaryUngapped)
      <*> (^. preliminaryGapped)


updateFromParent
  :: (EncodableDynamicCharacter c, DirectOptimizationPostOrderDecoration d c, Show c, Show (Element c))
  => PairwiseAlignment c
  -> d
  -> DynamicDecorationDirectOptimization c
  -> DynamicDecorationDirectOptimization c
updateFromParent pairwiseAlignment currentDecoration parentDecoration =
    extendPostOrderToDirectOptimization currentDecoration ungapped gapped
  where
    -- If the current node has a missing character value representing it's
    -- preliminary median assignment, then we take the parent's final assingment
    -- values and assign them to the current node as it's own final assignments.
    --
    -- Otherwise we perform a local alignmnet between the parent's *UNGAPPED*
    -- final assignments and the current node's *GAPPED* preliminary assignment.
    -- Afterwards we calculate the indicies of the new gaps in the alignment,
    -- insert them into the current node's left and right child alignments.
    -- Lastly a three-way mean between the locally aligned parent assignment and
    -- the expanded left and right child alignments is used to calculate the
    -- final assignment of the current node.
    (ungapped, gapped)
      | isMissing $ currentDecoration ^. preliminaryGapped = (pUngapped, pGapped)
      | otherwise =  tripleComparison pairwiseAlignment currentDecoration pUngapped
    pUngapped = parentDecoration ^. finalUngapped
    pGapped   = parentDecoration ^. finalGapped

  
-- |
-- A three way comparison of characters used in the DO preorder traversal.
tripleComparison
  :: ( EncodableDynamicCharacter c, DirectOptimizationPostOrderDecoration d c, Show c, Show (Element c))
  => PairwiseAlignment c
  -> d
  -> c
  -> (c, c)
tripleComparison pairwiseAlignment childDecoration parentCharacter = (ungapped, gapped)
  where
    costStructure     = childDecoration ^. symbolChangeMatrix
    childCharacter    = childDecoration ^. preliminaryGapped
    childLeftAligned  = childDecoration ^. leftAlignment
    childRightAligned = childDecoration ^. rightAlignment
    
    (_, _, derivedAlignment, _, childAlignment) = pairwiseAlignment parentCharacter childCharacter
    newGapIndicies         = newGapLocations childCharacter childAlignment
    extendedLeftCharacter  = insertNewGaps newGapIndicies childLeftAligned
    extendedRightCharacter = insertNewGaps newGapIndicies childRightAligned
    (_, ungapped, gapped)  = {- trace context $ -} threeWayMean costStructure derivedAlignment extendedLeftCharacter extendedRightCharacter
    {--
    context = unlines
        [ "New Gap indices: |" <> show (sum newGapIndicies) <> "| " <> show newGapIndicies
        , "Parent:"
        , show (olength parentCharacter)
        , show (olength derivedAlignment)
        , "Center char:"
        , show (olength childCharacter)
        , show (olength childAlignment)
        , "Left  chars:"
        , show (olength childLeftAligned)
        , show (olength extendedLeftCharacter)
        , "Right chars:"
        , show (olength childRightAligned)
        , show (olength extendedRightCharacter)
        ]
    --}
    

-- |
-- Returns the indicies of the gaps that were added in the second character when
-- compared to the first character.
newGapLocations :: (EncodableDynamicCharacter c, Show (Element c)) => c -> c -> IntMap Int
newGapLocations originalChar newChar
  | olength originalChar == olength newChar = mempty
  | otherwise                               = newGaps
  where
    (_,_,newGaps) = ofoldl' f (otoList originalChar, 0, mempty) newChar
    gap = getGapElement $ newChar `indexStream` 0
    incrementAt i is = IM.insertWith (+) i 1 is

    
    f acc@([], i, is) e
      | e == gap             = ([], i, incrementAt i is)
      | otherwise            = acc
    f (x:xs, i, is) e
      | e == gap && x /= gap = (x:xs, i  , incrementAt i is)
      | otherwise            = (  xs, i+1, is)



-- |
-- Given a list of gap location and a character returns a longer character with
-- the supplied gaps inserted at the corersponding locations.
insertNewGaps :: EncodableDynamicCharacter c => IntMap Int -> c -> c
insertNewGaps insertionIndicies character = constructDynamic . (<> trailingGaps) . foldMapWithKey f $ otoList character
  where
    len = olength character
    gap = getGapElement $ character `indexStream` 0
    trailingGaps = maybe [] (`replicate` gap) $ len `lookup` insertionIndicies
    f i e =
      case i `lookup` insertionIndicies of
        Nothing -> [e]
        Just n  -> replicate n gap <> [e]


-- |
-- Calculates the mean character and cost between three supplied characters.
threeWayMean
  :: (EncodableDynamicCharacter c, Show c)
  => (Word -> Word -> Word)
  -> c
  -> c
  -> c
  -> (Word, c, c)
threeWayMean costStructure char1 char2 char3
  | not uniformLength = error $ unwords [ "Three sequences supplied to 'threeWayMean' function did not have uniform length.", show (olength char1), show (olength char2), show (olength char3) ]
  | otherwise         = (sum costValues, constructDynamic $ filter (/= gap) meanStates, constructDynamic meanStates)
  where
    gap                 = getGapElement $ char1 `indexStream` 0
    uniformLength       = olength char1 == olength char2 && olength char2 == olength char3
    (meanStates, costValues) = unzip $ zipWith3 f (otoList char1) (otoList char2) (otoList char3)
    f a b c = minimalChoice -- minimumBy (comparing snd)
            [ getOverlap a b costStructure
            , getOverlap a c costStructure
            , getOverlap b c costStructure
            ]



