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

{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module Analysis.Parsimony.Dynamic.DirectOptimization.Internal where

import           Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise
import           Analysis.Parsimony.Dynamic.SequentialAlign
import           Bio.Character.Decoration.Dynamic
import           Bio.Character.Encodable
import           Bio.Character.Exportable
import           Control.Lens
import           Data.Bits
import           Data.Foldable
import           Data.IntMap        (IntMap)
import qualified Data.IntMap as IM
import           Data.Key
import           Data.List.NonEmpty (NonEmpty( (:|) ))
import           Data.List.Utility  (invariantTransformation)
import           Data.Ord           (comparing)
import           Data.Semigroup
import           Data.TCM.Memoized
import           Data.MonoTraversable
import           Data.Word
import           Numeric.Extended.Natural
import           Prelude     hiding (lookup, zipWith)

-- import Debug.Trace


-- |
-- A function representing an alignment of two dynamic characters.
--
-- The first  result in the tuple is the cost of the alignment.
--
-- The second result in the tuple is the /ungapped/ median alignment.
--
-- The third  result in the tuple is the   /gapped/ median alignment.
--
-- The fourth result in the tuple is the first  input aligned with respect to the second.
--
-- The fifth  result in the tuple is the second input aligned with respect to the first.
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
     , Exportable (Element c)
     , HasDenseTransitionCostMatrix  dec (Maybe DenseTransitionCostMatrix)
     , HasSparseTransitionCostMatrix dec MemoizedCostMatrix
     , Ord (Element c)
     )
  => dec
  -> c
  -> c
  -> (Word, c, c, c, c)
selectDynamicMetric candidate
  | sequentialAlignOverride = sequentialAlign sTCM
  | otherwise =
      case candidate ^. denseTransitionCostMatrix of
        Just dm -> \x y -> foreignPairwiseDO x y dm
        Nothing -> let !sTCM' = getMedianAndCost sTCM
                   in  \x y -> ukkonenDO x y sTCM'
  where
    !sTCM = candidate ^. sparseTransitionCostMatrix


-- |
-- The post-order scoring logic for dynamic characters.
--
-- Parameterized over a 'PairwiseAlignment' function to allow for different
-- atomic alignments depending on the character's metadata.
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


-- |
-- Given a simple dynamic character as input, initializes the leaf node
-- decoration as the base case of the post-order traversal.
initializeLeaf
  :: SimpleDynamicDecoration d c
  => d
  -> DynamicDecorationDirectOptimizationPostOrderResult c
initializeLeaf =
    extendDynamicToPostOrder
      <$> id
      <*> const 0
      <*> const 0
      <*> toAverageLength . toEnum . olength . (^. encoded)
      <*> (^. encoded)
      <*> (^. encoded)
      <*> (^. encoded)
      <*> (^. encoded)


-- |
-- Use the decoration(s) of the descendant nodes to calculate the currect node
-- decoration. The recursive logic of the post-order traversal.
updateFromLeaves
  :: ( EncodableDynamicCharacter c
     )
  => PairwiseAlignment c
  -> NonEmpty (DynamicDecorationDirectOptimizationPostOrderResult c)
  -> DynamicDecorationDirectOptimizationPostOrderResult c
updateFromLeaves _ (x:|[]) = x -- This shouldn't happen
updateFromLeaves pairwiseAlignment (leftChild:|rightChild:_) = resultDecoration
  where
    resultDecoration = extendDynamicToPostOrder leftChild localCost totalCost combinedAverageLength ungapped gapped lhsAlignment rhsAlignment
    (localCost, ungapped, gapped, lhsAlignment, rhsAlignment) = pairwiseAlignment (leftChild ^. preliminaryUngapped) (rightChild ^. preliminaryUngapped)
    totalCost = localCost + leftChild ^. characterCost + rightChild ^. characterCost
    combinedAverageLength = leftChild ^. averageLength <> rightChild ^. averageLength


-- |
-- The pre-order scoring logic for dynamic characters.
--
-- Parameterized over a 'PairwiseAlignment' function to allow for different
-- atomic alignments depending on the character's metadata.
directOptimizationPreOrder
  :: ( DirectOptimizationPostOrderDecoration d c
     , EncodedAmbiguityGroupContainer c
     , Exportable (Element c)
     , Show c
     , Show (Element c)
     )
  => PairwiseAlignment c
  -> d
  -> [(Word, DynamicDecorationDirectOptimization c)]
  ->  DynamicDecorationDirectOptimization c
directOptimizationPreOrder pairwiseAlignment charDecoration parents =
    case parents of
        []            -> initializeRoot charDecoration
        (_, parent):_ -> updateFromParent pairwiseAlignment charDecoration parent


-- |
-- Given a post-order traversal result of a dynamic character as input,
-- initializes the root node decoration as the base case of the pre-order
-- traversal.
initializeRoot
  :: DirectOptimizationPostOrderDecoration d c
  => d
  -> DynamicDecorationDirectOptimization c
initializeRoot =
    extendPostOrderToDirectOptimization
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
    idx = countLeadingZeros x
    zed = x `xor` x
    

-- |
-- Disambiguate the elements of a dynamic Character so that they are consistent
-- with the ancestoral disambiguation.
disambiguateFromParent
  :: (EncodableDynamicCharacter c, Show (Element c))
  => {-IntMap Int -- ^ parent gap locations
  -> IntMap Int -- ^ child  gap locations 
  -> -} c          -- ^ parent single disambiguation field
  -> c          -- ^ child  final gapped
  -> c          -- ^ child  single disambiguation field
disambiguateFromParent {- pGaps cGaps -} pSingle cFinal = result
  where
    result = constructDynamic $ zipWith f (otoList pSingle) (otoList cFinal)
    {-
--    gap = gapOfStream pSingle

    -- |
    -- We zip shittily.
    shittyZip :: (FiniteBits b, Show b) => Int -> [b] -> [b] -> [b]
    shittyZip i xs ys = go xs' ys'
      where
        go    []     []  = []
        go    []     bs  = error $ unwords [ "Didn't end cleanly, too many child  states: ", show i, show pGaps, show cGaps, show bs ]
        go    as     []  = error $ unwords [ "Didn't end cleanly, too many parent states: ", show i, show pGaps, show cGaps, show as ]
        go (a:as) (b:bs) = f a b : shittyZip (i+1) as bs
        
        xs' = case i `lookup` pGaps of
                Nothing -> xs
                Just v  -> drop v xs
        ys' = case i `lookup` cGaps of
                Nothing -> ys
                Just v  -> drop v ys
-}
    f pS cF
      | popCount val /= 0 = val
      | otherwise         = disambiguateElement cF
      where
        -- Since pS will have only one bit set,
        -- there can only ever be an symbol intersection of size 1
        val = pS .&. cF


-- |
-- Use the decoration(s) of the ancestoral nodes to calculate the currect node
-- decoration. The recursive logic of the pre-order traversal.
updateFromParent
  :: ( DirectOptimizationPostOrderDecoration d c
     , EncodableDynamicCharacter c
     , EncodedAmbiguityGroupContainer c
     , Exportable (Element c)
   --  , Show c
     , Show (Element c)
     )
  => PairwiseAlignment c
  -> d
  -> DynamicDecorationDirectOptimization c
  -> DynamicDecorationDirectOptimization c
updateFromParent pairwiseAlignment currentDecoration parentDecoration = resultDecoration
  where
    -- If the current node has a missing character value representing its
    -- preliminary median assignment, then we take the parent's final assignment
    -- values and assign them to the current node as its own final assignments.
    --
    -- Otherwise we perform a local alignment between the parent's *UNGAPPED*
    -- final assignment and the current node's *GAPPED* preliminary assignment.
    -- Afterwards we calculate the indices of the new gaps in the alignment and
    -- insert these gaps into the current node's left and right child alignments.
    -- Lastly, a three-way mean between the locally-aligned parent assignment and
    -- the expanded left and right child alignments is used to calculate the
    -- final assignment of the current node.
    --
    -- We do these convoluted operations to account for deletion events in the
    -- parent assignment when comparing to child assignments.
    resultDecoration = extendPostOrderToDirectOptimization currentDecoration ungapped gapped single
    (ungapped, gapped, single)
      | isMissing $ currentDecoration ^. preliminaryGapped = (pUngapped, pGapped, pSingle)
      | otherwise = tripleComparison pairwiseAlignment currentDecoration pUngapped pSingle
    pUngapped     = parentDecoration ^. finalUngapped
    pGapped       = parentDecoration ^. finalGapped
    pSingle       = parentDecoration ^. singleDisambiguation


-- |
-- A three way comparison of characters used in the DO preorder traversal.
tripleComparison
  :: ( {- EncodableDynamicCharacter c, -}Exportable (Element c), DirectOptimizationPostOrderDecoration d c, EncodedAmbiguityGroupContainer c, {- Show c, -} Show (Element c))
  => PairwiseAlignment c
  -> d
  -> c
  -> c
  -> (c, c, c)
tripleComparison pairwiseAlignment childDecoration parentCharacter parentSingle =
   {-  trace context () `seq` -} (ungapped, gapped, single)
  where
    childCharacter    = childDecoration ^. preliminaryGapped
    childLeftAligned  = childDecoration ^. leftAlignment
    childRightAligned = childDecoration ^. rightAlignment

    -- We conditionally decide how to get derive the metric
    -- If we are working with large alphabets we use the memoized TCM.
    -- Otherwise with a small alphabet, we use the naive calcualtions.
    --
    -- We do this so that we don't allocate and begin using a memoized TCM
    -- for all characters regardless of alphabet size on the pre-order.
    -- If we have a small alphabet, there will not have been a call to
    -- initialize a memoized TCM. We certainly don't want to force that here!
    costStructure =
      case childDecoration ^. denseTransitionCostMatrix of
        Nothing -> getMedianAndCost (childDecoration ^. sparseTransitionCostMatrix)
        Just _  -> let !scm = childDecoration ^. symbolChangeMatrix
                   in \x y -> getOverlap x y scm

    single = lexicallyDisambiguate $ filterGaps almostSingle
    (_, ungapped, gapped)  = threeWayMean costStructure extendedParentFinal  extendedLeftCharacter1 extendedRightCharacter1
    (_, almostSingle, _)   = threeWayMean costStructure extendedParentSingle extendedLeftCharacter2 extendedRightCharacter2

    (extendedParentFinal , extendedLeftCharacter1, extendedRightCharacter1) = alignAroundCurrentNode pairwiseAlignment childCharacter parentCharacter childLeftAligned childRightAligned
    (extendedParentSingle, extendedLeftCharacter2, extendedRightCharacter2) = alignAroundCurrentNode pairwiseAlignment childCharacter parentSingle    childLeftAligned childRightAligned

    {--}
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
    {--}


-- |
-- Given a node, it's parent, and it's children; this function aligns the dynamic
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
          -- logic for determining if a deletion event occured.
          --
          -- If a deletion event *DID* occur, we note the index in the unaligned
          -- character where deletion event occurred and *DO NOT* advance the
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
  :: ( EncodableDynamicCharacter c
     , EncodedAmbiguityGroupContainer c
     )
  => (Element c -> Element c -> (Element c, Word))
  -> c
  -> c
  -> c
  -> (Word, c, c)
threeWayMean sigma char1 char2 char3 =
  case invariantTransformation olength [char1, char2, char3] of
    Nothing -> error $ unwords [ "Three sequences supplied to 'threeWayMean' function did not have uniform length.", show (olength char1), show (olength char2), show (olength char3) ]
    Just 0  -> (0, char1, char1)
    Just _  -> (unsafeToFinite $ sum costValues, constructDynamic $ filter (/= gap) meanStates, constructDynamic meanStates)
  where
    gap = gapOfStream char1
    zed = gap `xor` gap
    singletonStates = (zed `setBit`) <$> [0 .. fromEnum (symbolCount char1) - 1]
    (meanStates, costValues) = unzip $ zipWith3 f (otoList char1) (otoList char2) (otoList char3)
    f a b c = foldl' g (zed, infinity :: ExtendedNatural) singletonStates
      where
        g acc@(combinedState, curentMinCost) singleState =
            case combinedCost `compare` curentMinCost of
              EQ -> (combinedState .|. singleState, curentMinCost)
              LT -> (                  singleState, combinedCost)
              GT -> acc
          where
            combinedCost = fromFinite . sum $ (snd . sigma singleState) <$> [a, b, c]
      
{-
f a b c = minimalChoice $
              sigma a b  :|
            [ sigma a c
            , sigma b c
            ]
-}
