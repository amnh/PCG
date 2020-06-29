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

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Analysis.Parsimony.Dynamic.DirectOptimization.Internal
  ( directOptimizationPostorder
  , directOptimizationPostorderPairwise
  , directOptimizationPreorder
  , selectDynamicMetric
  ) where

import           Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise
import           Bio.Character.Decoration.Dynamic
import           Bio.Character.Encodable
import           Bio.Character.Exportable
import           Bio.Graph.Node.Context
import           Bio.Metadata                                           hiding (DenseTransitionCostMatrix)
import           Control.Lens                                           hiding ((<|), (|>))
import           Data.Bits
import           Data.Either (isLeft)
import           Data.Foldable
import qualified Data.List.NonEmpty                                     as NE
import           Data.MonoTraversable
import           Data.Semigroup
import           Data.TCM.Dense
import           Data.Word
import           Prelude                                                hiding (zipWith)


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
type PairwiseAlignment s = s -> s -> (Word, s)


-- |
-- Select the most appropriate direct optimization metric implementation.
selectDynamicMetric
  :: ( EncodableDynamicCharacter c
     , ExportableElements c
     , GetDenseTransitionCostMatrix dec (Maybe DenseTransitionCostMatrix)
     , GetPairwiseTransitionCostMatrix dec (Subcomponent (Element c)) Word
     , Ord (Subcomponent (Element c))
     , Show c
     )
  => dec
  -> c
  -> c
  -> (Word, c)
selectDynamicMetric meta =
    case {-# SCC getDense #-} meta ^. denseTransitionCostMatrix of
      Just dm -> {-# SCC foreignPairwiseDO #-} foreignPairwiseDO dm
      Nothing -> let !pTCM = meta ^. pairwiseTransitionCostMatrix
                 in  {-# SCC unboxedUkkonen #-} unboxedUkkonenFullSpaceDO pTCM


-- |
-- The post-order scoring logic for dynamic characters.
--
-- Parameterized over a 'PairwiseAlignment' function to allow for different
-- atomic alignments depending on the character's metadata.
directOptimizationPostorder
  :: ( SimpleDynamicDecoration d c
     , SimpleDynamicExtensionPostorderDecoration s c
     )
  => PairwiseAlignment c
  -> PostorderContext d s
  -> s
directOptimizationPostorder pairwiseAlignment
  = postorderContext
      initializeLeaf
      (directOptimizationPostorderPairwise pairwiseAlignment)


-- |
-- Given a simple dynamic character as input, initializes the leaf node
-- decoration as the base case of the post-order traversal.
initializeLeaf
  :: ( SimpleDynamicDecoration d c
     , SimpleDynamicExtensionPostorderDecoration r c
     )
  => d
  -> r
initializeLeaf =
    extendDynamicToPostorder
      <$> id
      <*> const 0
      <*> const 0
      <*> toAverageLength . toEnum . olength . (^. encoded)
      <*> (^. encoded)


-- |
-- Use the decoration(s) of the descendant nodes to calculate the current node
-- decoration. The recursive logic of the post-order traversal.
directOptimizationPostorderPairwise
  :: ( DirectOptimizationPostorderDecoration a c
     , DirectOptimizationPostorderDecoration b c
     , SimpleDynamicExtensionPostorderDecoration d c
     )
  => PairwiseAlignment c
  -> (a, b)
  -> d
directOptimizationPostorderPairwise pairwiseAlignment (lChild , rChild) = resultDecoration
  where
    resultDecoration = extendDynamicToPostorder lChild localCost totalCost combinedAverageLength subtreeAlignment
    (localCost, subtreeAlignment) = pairwiseAlignment (lChild ^. alignmentContext) (rChild ^. alignmentContext)
    totalCost = localCost + lChild ^. characterCost +  rChild ^. characterCost
    combinedAverageLength = lChild ^. averageLength <> rChild ^. averageLength


-- |
-- The pre-order scoring logic for dynamic characters.
--
-- Parameterized over a 'PairwiseAlignment' function to allow for different
-- atomic alignments depending on the character's metadata.
directOptimizationPreorder
  :: ( DirectOptimizationPostorderDecoration d c
     , EncodableStreamElement (Subcomponent (Element c))
     )
  => PairwiseAlignment c
  -> DynamicCharacterMetadataDec (Subcomponent (Element c))
  -> PreorderContext d (DynamicDecorationDirectOptimization c)
  -> DynamicDecorationDirectOptimization c
directOptimizationPreorder pairwiseAlignment meta =
    preorderContext rootFn internalFn
  where
    rootFn     = initializeRoot meta
    internalFn = updateFromParent pairwiseAlignment meta


-- |
-- Given a post-order traversal result of a dynamic character as input,
-- initializes the root node decoration as the base case of the pre-order
-- traversal.
initializeRoot
  :: ( DirectOptimizationPostorderDecoration d c
     , EncodableStreamElement (Subcomponent (Element c))
     )
  => DynamicCharacterMetadataDec (Subcomponent (Element c))
  -> d
  -> DynamicDecorationDirectOptimization c
initializeRoot _meta =
    extendPostorderToDirectOptimization
      <$> id
      <*> lexicallyDisambiguate . (^. alignmentContext)
      <*> (^. alignmentContext)


-- |
-- Disambiguate the elements of a dynamic character using only lexical ordering
-- of the alphabet.
lexicallyDisambiguate
  :: ( EncodableDynamicCharacterElement (Element c)
     , FiniteBits (Subcomponent (Element c))
     , MonoFunctor c
     )
  => c
  -> c
lexicallyDisambiguate = omap disambiguateElement


-- |
-- Disambiguate a single element of a Dynamic Character.
disambiguateElement
  :: ( EncodableDynamicCharacterElement e
     , FiniteBits (Subcomponent e)
     )
  => e
  -> e
disambiguateElement x = alignElement val val val
  where
    med = getMedian x
    idx = min (finiteBitSize med - 1) $ countLeadingZeros med
    zed = med `xor` med
    val = zed `setBit` idx


-- |
-- Use the decoration(s) of the ancestral nodes to calculate the corrent node
-- decoration. The recursive logic of the pre-order traversal.
updateFromParent
  :: ( DirectOptimizationPostorderDecoration d c
     , EncodableStreamElement (Subcomponent (Element c))
     )
  => PairwiseAlignment c
  -> DynamicCharacterMetadataDec (Subcomponent (Element c))
  -> Either d d
  -> DynamicDecorationDirectOptimization c
  -> DynamicDecorationDirectOptimization c
updateFromParent _pairwiseAlignment _meta decorationDirection parentDecoration = resultDecoration
  where
    resultDecoration  = extendPostorderToDirectOptimization currentDecoration single cia
    currentDecoration = either id id decorationDirection

    pia = parentDecoration ^. impliedAlignment
    pac = parentDecoration ^. alignmentContext
    cac = f $ currentDecoration ^. alignmentContext

    f   = case decorationDirection of
            Left {} -> id -- omap swapContext
            Right{} -> id

    (cia, single)
      | isMissing cac = (pia, parentDecoration ^. singleDisambiguation)
      | otherwise     = let x = deriveImpliedAlignment (isLeft decorationDirection) pia pac cac
                        in  (x, lexicallyDisambiguate x)


{-# INLINEABLE deriveImpliedAlignment #-}
{-# SPECIALISE deriveImpliedAlignment :: Bool -> DynamicCharacter -> DynamicCharacter -> DynamicCharacter -> DynamicCharacter #-}
deriveImpliedAlignment
  :: EncodableDynamicCharacter c
  => Bool
  -> c -- ^ Parent Final       Alignment
  -> c -- ^ Parent Preliminary Context
  -> c -- ^ Child  Preliminary Context
  -> c -- ^ Child  Final       Alignment
--deriveImpliedAlignment isLeftChild _ _ _ | trace ("isLeftChild " <> show isLeftChild) False = undefined
deriveImpliedAlignment isLeftChild pAlignment pContext cContext = cAlignment
  where
    gap          = gapOfStream pAlignment
    initialState = ([], otoList cContext, otoList pContext)
    cAlignment   = extractVector . foldl' go initialState $ otoList pAlignment
    extractVector (x,_,_) = constructDynamic . NE.fromList $ reverse x

{--
    showChar = foldMap (showElem . getContext)
      where
        showElem Gapping   = "G"
        showElem Insertion = "I"
        showElem Deletion  = "D"
        showElem Alignment = "A"

    go (acc,   xs,   ys) _ | trace ( unlines [ "Parent alignment: " <> showChar (otoList pAlignment)
                                             , "Parent context:   " <> showChar ys
                                             , "Child  context:   " <> showChar xs
                                             , "Child  alignment: " <> showChar (reverse acc)
                                             ]
                                   ) False = undefined
--}
    go (acc,   [],    _) _ = (gap : acc, [], [])
    go (  _,    _,   []) _ = error "Impossible happened in 'deriveImpliedAlignment'"
    go (acc, x:xs, y:ys) e =
        case getContext e of
          Gapping   -> (e : acc, x:xs, y:ys)
          Alignment -> (x : acc,   xs,   ys)
          Deletion  ->
              case getContext y of
                Deletion  | not isLeftChild -> (  x : acc,   xs,   ys)
                _                           -> (gap : acc, x:xs,   ys)
          Insertion ->
              case getContext y of
                Insertion | isLeftChild -> (  x : acc,   xs,   ys)
                _                       -> (gap : acc, x:xs,   ys)
