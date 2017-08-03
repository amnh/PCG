----------------------------------------------------------------------------
-- |
-- Module      :  PCG.Command.Read.DecorationInitialization
-- Copyright   :  () 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Containing the master command for unifying all input types: tree, metadata, and sequence
--
-----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns, FlexibleContexts #-}

module PCG.Command.Read.DecorationInitialization where


import Analysis.Parsimony.Additive.Internal
import Analysis.Parsimony.Fitch.Internal
import Analysis.Parsimony.Sankoff.Internal
import Analysis.Parsimony.Dynamic.DirectOptimization
import Analysis.Parsimony.Dynamic.SequentialAlign
import Bio.Character
import Bio.Character.Decoration.Additive
import Bio.Character.Decoration.Dynamic
import Bio.Graph
import Bio.Graph.PhylogeneticDAG
import Control.Lens
import Data.MonoTraversable (Element)
import Prelude       hiding (lookup, zip, zipWith)

-- import Debug.Trace


{-
traceOpt :: [Char] -> a -> a
traceOpt identifier x = (trace ("Before " <> identifier) ())
                  `seq` (let !v = x
                         in v `seq` (trace ("After " <> identifier) v)
                        )
-}

-- | sequentialAlignOverride, iff True forces seqAlign to run; otherwise, DO runs.
sequentialAlignOverride :: Bool
sequentialAlignOverride = False


chooseDirectOptimizationComparison :: ( SimpleDynamicDecoration d  c
                                      , SimpleDynamicDecoration d' c
                                      , Exportable c
                                      , Show c
                                      , Show (Element c)
                                      , Integral (Element c)
                                      )
                                   => d
                                   -> [d']
                                   -> c
                                   -> c
                                   -> (Word, c, c, c, c)
chooseDirectOptimizationComparison dec decs =
    case decs of
      []  -> selectBranch dec
      x:_ -> selectBranch x
  where
--    selectBranch x | trace (show . length $ x ^. characterAlphabet) False = undefined
    selectBranch candidate
      | sequentialAlignOverride = sequentialAlign (candidate ^. sparseTransitionCostMatrix)
      | otherwise =
          case candidate ^. denseTransitionCostMatrix of
            Just  d -> \x y -> foreignPairwiseDO x y d
            Nothing ->
              let !scm = (candidate ^. symbolChangeMatrix)
              in \x y -> naiveDO x y scm


chooseDirectOptimizationComparison2 :: ( SimpleDynamicDecoration d  c
                                      , SimpleDynamicDecoration d' c
                                      , Exportable c
                                      , Show c
                                      , Show (Element c)
                                      , Integral (Element c)
                                      )
                                   => d
                                   -> [(a,d')]
                                   -> c
                                   -> c
                                   -> (Word, c, c, c, c)
chooseDirectOptimizationComparison2 dec decs =
    case decs of
      []  -> selectBranch dec
      (_,x):_ -> selectBranch x
  where
--    selectBranch x | trace (show . length $ x ^. characterAlphabet) False = undefined
    selectBranch candidate
      | sequentialAlignOverride = sequentialAlign (candidate ^. sparseTransitionCostMatrix)
      | otherwise =
          case candidate ^. denseTransitionCostMatrix of
            Just  d -> \x y -> foreignPairwiseDO x y d
            Nothing ->
              let !scm = (candidate ^. symbolChangeMatrix)
              in \x y -> naiveDO x y scm


id2 :: a -> b -> a
id2 x _ = x


{--}
initializeDecorations2 :: CharacterResult -> PhylogeneticSolution FinalDecorationDAG
initializeDecorations2 (PhylogeneticSolution forests) = PhylogeneticSolution $ fmap performDecoration <$> forests
  where
    performDecoration :: CharacterDAG -> FinalDecorationDAG
    performDecoration x = performPreOrderDecoration performPostOrderDecoration
      where
    
        performPreOrderDecoration :: PostOrderDecorationDAG -> FinalDecorationDAG
        performPreOrderDecoration =
            preorderFromRooting
              adaptiveDirectOptimizationPreOrder
              edgeCostMapping
              contextualNodeDatum
              
            . preorderSequence'
              additivePreOrder
              fitchPreOrder
              additivePreOrder
              sankoffPreOrder
              sankoffPreOrder
              id2
          where
            adaptiveDirectOptimizationPreOrder dec kidDecs = directOptimizationPreOrder pairwiseAlignmentFunction dec kidDecs
              where
                pairwiseAlignmentFunction = chooseDirectOptimizationComparison2 dec kidDecs
    
        performPostOrderDecoration :: PostOrderDecorationDAG
        performPostOrderDecoration = assignPunitiveNetworkEdgeCost post
        
        (post, edgeCostMapping, contextualNodeDatum) =
             assignOptimalDynamicCharacterRootEdges adaptiveDirectOptimizationPostOrder
             . postorderSequence'
                 (g additivePostOrder)
                 (g    fitchPostOrder)
                 (g additivePostOrder)
                 (g  sankoffPostOrder)
                 (g  sankoffPostOrder)
                 (g adaptiveDirectOptimizationPostOrder)
             $ x
          where
            g _  Nothing  [] = error "Uninitialized leaf node. This is bad!"
            g h (Just  v) [] = h v []
            g h        e  xs = h (error $ "We shouldn't be using this value." ++ show e ++ show (length xs)) xs


{--}
        adaptiveDirectOptimizationPostOrder dec kidDecs = directOptimizationPostOrder pairwiseAlignmentFunction dec kidDecs
          where
            pairwiseAlignmentFunction = chooseDirectOptimizationComparison dec kidDecs

{--}


{-
        postOrderTransformation parentalNode childNodes =
          PNode
          { nodeDecorationDatum = (nodeDecorationDatum parentalNode)
          , sequenceDecoration  = postOrderLogic (sequenceDecoration parentalNode) (sequenceDecoration <$> childNodes)
          }

        preOrderTransformation parentalNode childNodes =
          PNode
          { nodeDecorationDatum = (nodeDecorationDatum parentalNode)
          , sequenceDecoration  = preOrderLogic (sequenceDecoration parentalNode) (second sequenceDecoration <$> childNodes)
          }
-}
{--}
{--
initializeDecorations :: CharacterResult -> PhylogeneticSolution InitialDecorationDAG
initializeDecorations (PhylogeneticSolution forests) = PhylogeneticSolution $ fmap performDecoration <$> forests
  where
--    performDecoration :: CharacterDAG -> InitialDecorationDAG
    performDecoration (PDAG dag) = PDAG . nodePreOrder preOrderTransformation $ nodePostOrder postOrderTransformation dag
      where
        postOrderTransformation parentalNode childNodes =
          PNode
          { nodeDecorationDatum = (nodeDecorationDatum parentalNode)
          , sequenceDecoration  = postOrderLogic (sequenceDecoration parentalNode) (sequenceDecoration <$> childNodes)
          }

        preOrderTransformation childNode parentNodes =
          PNode
          { nodeDecorationDatum = (nodeDecorationDatum childNode)
          , sequenceDecoration  = preOrderLogic (sequenceDecoration childNode) (second sequenceDecoration <$> parentNodes)
          }

    postOrderLogic :: CharacterSequence
           UnifiedDiscreteCharacter
           UnifiedDiscreteCharacter
           UnifiedContinuousCharacter
           UnifiedDiscreteCharacter
           UnifiedDiscreteCharacter
           UnifiedDynamicCharacter
      -> [ CharacterSequence
             (SankoffOptimizationDecoration StaticCharacter)
             (SankoffOptimizationDecoration StaticCharacter)
             (ContinuousPostorderDecoration ContinuousChar ) --(ContinuousOptimizationDecoration ContinuousChar)
             (FitchOptimizationDecoration   StaticCharacter)
             (AdditivePostorderDecoration   StaticCharacter)
             (DynamicDecorationDirectOptimizationPostOrderResult DynamicChar) -- UnifiedDynamicCharacter
         ]
      -> CharacterSequence
           (SankoffOptimizationDecoration StaticCharacter)
           (SankoffOptimizationDecoration StaticCharacter)
           (ContinuousPostorderDecoration ContinuousChar ) --(ContinuousOptimizationDecoration ContinuousChar)
           (FitchOptimizationDecoration   StaticCharacter)
           (AdditivePostorderDecoration   StaticCharacter)
           (DynamicDecorationDirectOptimizationPostOrderResult DynamicChar) -- UnifiedDynamicCharacter

    postOrderLogic currentCharSeq childCharSeqs =
        hexZipWith
          (g  sankoffPostOrder)
          (g  sankoffPostOrder)
          (g additivePostOrder)
          (g    fitchPostOrder)
          (g additivePostOrder)
          (g adaptiveDirectOptimizationPostOrder)
          currentCharSeq
          childCharSeqs'
      where
        id2 x _ = x
        childCharSeqs' =
            case childCharSeqs of
              x:xs -> hexTranspose $ x:|xs
              []   -> let c = const []
                      in hexmap c c c c c c currentCharSeq
        g _  Nothing  [] = error $ "Uninitialized leaf node. This is bad!"
        g h (Just  v) [] = h v []
        g h        e  xs = h (error $ "We shouldn't be using this value." ++ show e ++ show (length xs)) xs

        adaptiveDirectOptimizationPostOrder dec kidDecs = directOptimizationPostOrder pairwiseAlignmentFunction dec kidDecs
          where
            pairwiseAlignmentFunction = chooseDirectOptimizationComparison dec kidDecs

{-
    preOrderLogic ::
        CharacterSequence
          (SankoffOptimizationDecoration  StaticCharacter)
          (SankoffOptimizationDecoration  StaticCharacter)
          UnifiedContinuousCharacter --(ContinuousOptimizationDecoration ContinuousChar)
          (FitchOptimizationDecoration    StaticCharacter)
          (AdditiveOptimizationDecoration StaticCharacter)
          UnifiedDynamicCharacter
      -> [ (Word
           , CharacterSequence
               (SankoffOptimizationDecoration  StaticCharacter)
               (SankoffOptimizationDecoration  StaticCharacter)
               UnifiedContinuousCharacter --(ContinuousOptimizationDecoration ContinuousChar)
               (FitchOptimizationDecoration    StaticCharacter)
               (AdditiveOptimizationDecoration StaticCharacter)
               UnifiedDynamicCharacter
           )
         ]
      -> CharacterSequence
           (SankoffOptimizationDecoration  StaticCharacter)
           (SankoffOptimizationDecoration  StaticCharacter)
           UnifiedContinuousCharacter --(ContinuousOptimizationDecoration ContinuousChar)
           (FitchOptimizationDecoration    StaticCharacter)
           (AdditiveOptimizationDecoration StaticCharacter)
           UnifiedDynamicCharacter
-}
    preOrderLogic currentCharSeq parentCharSeqs =
        hexZipWith
          sankoffPreOrder
          sankoffPreOrder
          additivePreOrder
          fitchPreOrder
          additivePreOrder
          adaptiveDirectOptimizationPreOrder
          currentCharSeq
          parentCharSeqs'
      where
        id2 x _ = x
{-
        parentCharSeqs' :: CharacterSequence
                               [(Word, SankoffOptimizationDecoration   StaticCharacter)]
                               [(Word, SankoffOptimizationDecoration   StaticCharacter)]
                               [(Word, UnifiedContinuousCharacter)]
                               [(Word, FitchOptimizationDecoration     StaticCharacter)]
                               [(Word, AdditiveOptimizationDecoration  StaticCharacter)]
                               [(Word, DynamicDecorationDirectOptimization DynamicChar)]
-}
        parentCharSeqs' =
            case parentCharSeqs of
              []   -> let c = const []
                      in hexmap c c c c c c currentCharSeq
              x:xs -> let f = zip (fst <$> (x:xs))
                      in hexmap f f f f f f . hexTranspose $ snd <$> x:|xs

        adaptiveDirectOptimizationPreOrder dec kidDecs = directOptimizationPreOrder pairwiseAlignmentFunction dec kidDecs
          where
            pairwiseAlignmentFunction = chooseDirectOptimizationComparison dec $ snd <$> kidDecs

--}


