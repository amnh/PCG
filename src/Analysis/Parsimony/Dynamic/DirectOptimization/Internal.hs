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

{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Analysis.Parsimony.Dynamic.DirectOptimization.Internal
  ( directOptimizationPostorder
  , directOptimizationPreorder
  , selectDynamicMetric
  ) where

import           Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise
import           Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.Internal (overlap)
import           Analysis.Parsimony.Internal
import           Bio.Character.Decoration.Dynamic
import           Bio.Character.Encodable
import           Bio.Character.Exportable
import           Bio.Metadata
import           Control.DeepSeq
import           Control.Lens
import           Data.Bits
import           Data.Foldable
import           Data.Foldable.Custom                                            (sum')
import           Data.IntMap                                                     (IntMap)
import qualified Data.IntMap                                                     as IM
import           Data.Key
import           Data.List.NonEmpty                                              (NonEmpty ((:|)))
import qualified Data.List.NonEmpty                                              as NE
import           Data.List.Utility                                               (invariantTransformation)
import           Data.MonoTraversable
import           Data.Range
import           Data.Semigroup
import           Data.TCM.Memoized
import           Data.Word
import           Numeric.Extended.Natural
import           Prelude                                                         hiding (lookup)


-- |
-- A function representing an alignment of two dynamic characters.
--
-- The first result in the tuple is the cost of the alignment.
--
-- The second result in the tuple is the /ungapped/ median alignment.
--
-- The third result in the tuple is the /gapped/ median alignment.
--
-- The fourth result in the tuple is the first input aligned with respect to the second.
--
-- The fifth result in the tuple is the second input aligned with respect to the first.
type PairwiseAlignment s = s -> s -> (Word, s, s, s, s)


-- |
-- sequentialAlignOverride, iff True forces seqAlign to run; otherwise, DO runs.
sequentialAlignOverride :: Bool
sequentialAlignOverride = False


-- |
-- Select the most appropriate direct optimization metric implementation.
selectDynamicMetric
  :: ( EncodableDynamicCharacter c
     , Exportable c
     , GetDenseTransitionCostMatrix  dec (Maybe DenseTransitionCostMatrix)
     , GetPairwiseTransitionCostMatrix       dec (OverlapFunction (Element c))
     , Ord (Element c)
     )
  => dec
  -> c
  -> c
  -> (Word, c, c, c, c)
selectDynamicMetric candidate
  | sequentialAlignOverride = undefined -- sequentialAlign sTCM
  | otherwise =
      case candidate ^. denseTransitionCostMatrix of
        Just dm -> \x y -> foreignPairwiseDO x y dm
        Nothing -> let !sTCM' = sTCM
                   in  \x y -> ukkonenDO x y sTCM'
  where
    !sTCM = candidate ^. pairwiseTransitionCostMatrix


-- |
-- The post-order scoring logic for dynamic characters.
--
-- Parameterized over a 'PairwiseAlignment' function to allow for different
-- atomic alignments depending on the character's metadata.
directOptimizationPostorder
  :: SimpleDynamicDecoration d c
  => PairwiseAlignment c
  -> PostorderContext d (DynamicDecorationDirectOptimizationPostorderResult c)
  ->  DynamicDecorationDirectOptimizationPostorderResult c
directOptimizationPostorder pairwiseAlignment
  = postorderContext
      initializeLeaf
      (\_ (lChild, rChild) -> updateFromLeaves pairwiseAlignment (lChild, rChild))


-- |
-- Given a simple dynamic character as input, initializes the leaf node
-- decoration as the base case of the post-order traversal.
initializeLeaf
  :: SimpleDynamicDecoration d c
  => d
  -> DynamicDecorationDirectOptimizationPostorderResult c
initializeLeaf =
    extendDynamicToPostorder
      <$> id
      <*> const 0
      <*> const 0
      <*> toAverageLength . toEnum . olength . (^. encoded)
      <*> (^. encoded)
      <*> (^. encoded)
      <*> (^. encoded)
      <*> (^. encoded)


-- |
-- Use the decoration(s) of the descendant nodes to calculate the current node
-- decoration. The recursive logic of the post-order traversal.
updateFromLeaves
  :: ( EncodableDynamicCharacter c
     , Exportable (Element c)
     )
  => PairwiseAlignment c
  -> (DynamicDecorationDirectOptimizationPostorderResult c, DynamicDecorationDirectOptimizationPostorderResult c)
  -> DynamicDecorationDirectOptimizationPostorderResult c
updateFromLeaves pairwiseAlignment (lChild , rChild) = resultDecoration
  where
    resultDecoration = extendDynamicToPostorder lChild localCost totalCost combinedAverageLength ungapped gapped lhsAlignment rhsAlignment
    (localCost, ungapped, gapped, lhsAlignment, rhsAlignment) = pairwiseAlignment (lChild ^. preliminaryUngapped) (rChild ^. preliminaryUngapped)
    totalCost = localCost + lChild ^. characterCost +  rChild ^. characterCost
    combinedAverageLength = lChild ^. averageLength <> rChild ^. averageLength


-- |
-- The pre-order scoring logic for dynamic characters.
--
-- Parameterized over a 'PairwiseAlignment' function to allow for different
-- atomic alignments depending on the character's metadata.
directOptimizationPreorder
  :: ( DirectOptimizationPostorderDecoration d c
     , Bound (Element c) ~ Word
     , Ranged (Element c)
--     , GetSparseTransitionCostMatrix (DynamicCharacterMetadataDec (Element c)) MemoizedCostMatrix
     )
  => PairwiseAlignment c
  -> DynamicCharacterMetadataDec (Element c)
  -> PreorderContext d (DynamicDecorationDirectOptimization c)
  -> DynamicDecorationDirectOptimization c
directOptimizationPreorder pairwiseAlignment meta =
    preorderContextSym rootFn internalFn
  where
    rootFn     = initializeRoot
    internalFn = updateFromParent pairwiseAlignment meta


-- |
-- Given a post-order traversal result of a dynamic character as input,
-- initializes the root node decoration as the base case of the pre-order
-- traversal.
initializeRoot
  :: DirectOptimizationPostorderDecoration d c
  => d
  -> DynamicDecorationDirectOptimization c
initializeRoot =
    extendPostorderToDirectOptimization
      <$> id
      <*> (^. preliminaryUngapped)
      <*> (^. preliminaryGapped)
      <*> lexicallyDisambiguate . (^. preliminaryUngapped)


-- |
-- Disambiguate the elements of a dynamic character using only lexical ordering
-- of the alphabet.
lexicallyDisambiguate :: (MonoFunctor f, FiniteBits (Element f)) => f -> f
lexicallyDisambiguate = omap disambiguateElement


-- |
-- Disambiguate a single element of a Dynamic Character.
disambiguateElement :: FiniteBits b => b -> b
disambiguateElement x = zed `setBit` idx
  where
    idx = min (finiteBitSize x - 1) $ countLeadingZeros x
    zed = x `xor` x


-- |
-- Use the decoration(s) of the ancestral nodes to calculate the corrent node
-- decoration. The recursive logic of the pre-order traversal.
updateFromParent
  :: ( DirectOptimizationPostorderDecoration d c
     , Bound (Element c) ~ Word
     , Ranged (Element c)
--     , GetSparseTransitionCostMatrix (DynamicCharacterMetadataDec (Element c)) MemoizedCostMatrix
     )
  => PairwiseAlignment c
  -> DynamicCharacterMetadataDec (Element c)
  -> d
  -> DynamicDecorationDirectOptimization c
  -> DynamicDecorationDirectOptimization c
updateFromParent pairwiseAlignment meta currentDecoration parentDecoration = resultDecoration
  where
    -- If the current node has a missing character value representing its
    -- preliminary median assignment then we take the parent's final assignment
    -- values and assign them to the current node as its own final assignments.
    --
    -- Otherwise we perform a local alignment between the parent's *UNGAPPED*
    -- final assignment and the current node's *GAPPED* preliminary assignment.
    -- Afterward we calculate the indices of the new gaps in the alignment and
    -- insert these gaps into the current node's left and right child alignments.
    -- Lastly, a three-way mean between the locally-aligned parent assignment and
    -- the expanded left and right child alignments is used to calculate the
    -- final assignment of the current node.
    --
    -- We do these convoluted operations to account for deletion events in the
    -- parent assignment when comparing to child assignments.
    resultDecoration = extendPostorderToDirectOptimization currentDecoration ungapped gapped single
    (ungapped, gapped, single)
      | isMissing $ currentDecoration ^. preliminaryGapped = (pUngapped, pGapped, pSingle)
      | otherwise = tripleComparison pairwiseAlignment meta currentDecoration pUngapped pSingle
    pUngapped     = parentDecoration ^. finalUngapped
    pGapped       = parentDecoration ^. finalGapped
    pSingle       = parentDecoration ^. singleDisambiguation


-- |
-- A three way comparison of characters used in the DO preorder traversal.
tripleComparison
  :: ( DirectOptimizationPostorderDecoration d c
     , Ranged (Element c)
     , Bound (Element c) ~ Word
--     , GetSparseTransitionCostMatrix (DynamicCharacterMetadataDec (Element c)) MemoizedCostMatrix
     )
  => PairwiseAlignment c
  -> DynamicCharacterMetadataDec (Element c)
  -> d
  -> c
  -> c
  -> (c, c, c)
tripleComparison pairwiseAlignment meta childDecoration parentCharacter parentSingle =
   {-  trace context () `seq` -} (ungapped, gapped, single)
  where
    childCharacter    = childDecoration ^. preliminaryGapped
    childLeftAligned  = childDecoration ^. leftAlignment
    childRightAligned = childDecoration ^. rightAlignment

    -- We conditionally decide how to derive the metric.
    -- If we are working with large alphabets we use the memoized TCM.
    -- Otherwise we use the naive calculations.
    --
    -- We do this so that we don't allocate and begin using a memoized TCM
    -- for all characters regardless of alphabet size on the pre-order.
    -- If we have a small alphabet, there will not have been a call to
    -- initialize a memoized TCM. We certainly don't want to force that here!
    costStructure =
        case meta ^. sparseTransitionCostMatrix of
          Just memo -> getMedianAndCost3D memo
          Nothing   -> naiveMedianAndCost3D
      where
        !tcm = meta ^. pairwiseTransitionCostMatrix
        !gap = gapOfStream parentCharacter
        !zed = gap `xor` gap

        singletonStates = (zed `setBit`) <$> [0 .. fromEnum (symbolCount zed) - 1]

        naiveMedianAndCost3D a b c = foldl' g (zed, maxBound :: Word) singletonStates
          where
            g acc@(combinedState, curentMinCost) singleState =
                case combinedCost `compare` curentMinCost of
                  EQ -> (combinedState .|. singleState, curentMinCost)
                  LT -> (                  singleState, combinedCost)
                  GT -> acc
              where
                combinedCost = sum' $ snd . tcm singleState <$> [a, b, c] 


    single = lexicallyDisambiguate $ filterGaps almostSingle
    (_, ungapped, gapped)  = threeWayMean costStructure extendedParentFinal  extendedLeftCharacter1 extendedRightCharacter1
    (_, almostSingle, _)   = threeWayMean costStructure extendedParentSingle extendedLeftCharacter2 extendedRightCharacter2

    (extendedParentFinal , extendedLeftCharacter1, extendedRightCharacter1) = alignAroundCurrentNode pairwiseAlignment childCharacter parentCharacter childLeftAligned childRightAligned
    (extendedParentSingle, extendedLeftCharacter2, extendedRightCharacter2) = alignAroundCurrentNode pairwiseAlignment childCharacter parentSingle    childLeftAligned childRightAligned

    {-
    context = unlines
        [ ""
        , "Center char (prelim/final/single):"
        , showStream alph childCharacter
        , showStream alph ungapped
        , showStream alph single
--        , showStream alph childAlignment
        , ""
        , "Parent Final Char:"
        , showStream alph parentCharacter
--        , showStream alph parentAlignment
        , mconcat [showStream alph extendedParentFinal, " (", show (olength extendedParentFinal), ")"]
        , "Left  chars:"
        , mconcat [showStream alph childLeftAligned, " (", show (olength childLeftAligned), ")"]
        , mconcat [showStream alph extendedLeftCharacter1, " (", show (olength extendedLeftCharacter1), ")"]
        , "Right chars:"
        , mconcat [showStream alph childRightAligned, " (", show (olength childRightAligned), ")"]
        , mconcat [showStream alph extendedRightCharacter1, " (", show (olength extendedRightCharacter1), ")"]
        , ""
        , "Parent Single char:"
        , showStream alph parentSingle
--        , showStream alph singleAlignment
        , mconcat [showStream alph extendedParentSingle, " (", show (olength extendedParentSingle), ")"]
        , "Left  chars:"
        , mconcat [showStream alph childLeftAligned, " (", show (olength childLeftAligned), ")"]
        , mconcat [showStream alph extendedLeftCharacter2, " (", show (olength extendedLeftCharacter2), ")"]
        , "Right chars:"
        , mconcat [showStream alph childRightAligned, " (", show (olength childRightAligned), ")"]
        , mconcat [showStream alph extendedRightCharacter2, " (", show (olength extendedRightCharacter2), ")"]
        ]
      where
        alph = childDecoration ^. characterAlphabet
    -}


-- |
-- Given a node, its parent, and its children; this function aligns the dynamic
-- characters around the current node.
alignAroundCurrentNode
  :: EncodableDynamicCharacter c
  => PairwiseAlignment c
  -> c -- ^ local character
  -> c -- ^ parent character
  -> c -- ^ one child character
  -> c -- ^ other child character
  -> (c, c, c) -- ^ parent & child characters aligned with respect to the current node
alignAroundCurrentNode pairwiseAlignment current parent child1 child2 =
    (extendedParent, extendedChild1, extendedChild2)
  where
    (_, _, _, parentAlignment, currentAlignment) = pairwiseAlignment parent current

    newGapIndiciesInParent  = newGapLocations parent  parentAlignment
    newGapIndiciesInCurrent = newGapLocations current currentAlignment

    extendedParent = insertNewGaps newGapIndiciesInParent  parent
    extendedChild1 = insertNewGaps newGapIndiciesInCurrent child1
    extendedChild2 = insertNewGaps newGapIndiciesInCurrent child2


-- |
-- Returns the indices of the gaps that were added in the second character when
-- compared to the first character.
newGapLocations :: EncodableDynamicCharacter c => c -> c -> IntMap Int
newGapLocations unaligned aligned
  | olength unaligned == olength aligned = mempty
  | otherwise                            = newGaps
  where
    (_, _, newGaps)  = ofoldl' f accumulator aligned
    accumulator      = (otoList unaligned, 0, mempty)
    gap              = gapOfStream unaligned
    incrementAt is i = IM.insertWith (+) i 1 is

    f (remainingUnalignedElements, unalignedIndex, newGapIndices) alignedElement =
        case remainingUnalignedElements of

          -- In the case that the unaligned input character has had all of its
          -- elements accounted for, we can determine if a deletion event happened
          -- by simply checking whether the remaining element from the aligned
          -- character is a gap character.
          []   -> ( remainingUnalignedElements
                  , unalignedIndex
                  , if   alignedElement == gap
                    then incrementedGapIndices
                    else newGapIndices
                  )

          -- In the case that the unaligned character has one or more elements
          -- that have not been accounted for in the alignment, we use standard
          -- logic for determining if a deletion event occurred.
          --
          -- If a deletion event *DID* occur, we note the index in the unaligned
          -- character where deletion events occurred and *DO NOT* advance the
          -- "cursor" in our accumulator.
          --
          -- If a deletion event *DID NOT* occur, we just advance the "cursor"
          -- in our accumulator.
          unalignedElement:tailUnalignedElements ->
              if   unalignedElement /= gap && alignedElement == gap -- Deletion Event Occured!
              then (remainingUnalignedElements, unalignedIndex    , incrementedGapIndices)
              else (     tailUnalignedElements, unalignedIndex + 1,         newGapIndices)
      where
        incrementedGapIndices = newGapIndices `incrementAt` unalignedIndex


-- |
-- Given a list of gap locations and a character, returns a longer character with
-- the supplied gaps inserted at the corresponding locations.
insertNewGaps :: EncodableDynamicCharacter c => IntMap Int -> c -> c
insertNewGaps insertionIndicies character = constructDynamic . appendGaps . foldMapWithKey1 f . NE.fromList $ otoList character
  where
    len = olength character
    gap = getGapElement $ character `indexStream` 0
    appendGaps (x:|xs) = x :| (xs <> trailingGaps)
    trailingGaps = maybe [] (`replicate` gap) $ len `lookup` insertionIndicies
--    f :: Int -> a -> NonEmpty a
    f i e =
      case i `lookup` insertionIndicies of
        Nothing -> pure e
        Just n  -> NE.fromList (replicate n gap) <> pure e


-- |
-- Calculates the mean character and cost between three supplied characters.
threeWayMean
  :: ( EncodableDynamicCharacter c
     )
  => (Element c -> Element c -> Element c -> (Element c, Word))
  -> c
  -> c
  -> c
  -> (Word, c, c)
threeWayMean sigma char1 char2 char3 =
  case invariantTransformation olength [char1, char2, char3] of
    Nothing -> error $ unwords [ "Three sequences supplied to 'threeWayMean' function did not have uniform length.", show (olength char1), show (olength char2), show (olength char3) ]
    Just 0  -> (0, char1, char1)
    Just _  -> ( unsafeToFinite   $ sum' costValues
               , constructDynamic . NE.fromList $ filter (/= gap) meanStates
               , constructDynamic $ NE.fromList meanStates
               )
  where
    gap = gapOfStream char1
    (meanStates, costValues) = unzip $ zipWith3 sigma (otoList char1) (otoList char2) (otoList char3)
