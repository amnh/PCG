----------------------------------------------------------------------------
-- |
-- Module      :  Analysis.Scoring
-- Copyright   :  () 2015-2018 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns, FlexibleContexts #-}

module Analysis.Scoring
  (
  -- * Decoration
    performDecoration
  , scoreSolution
  -- * Decoration Removal
  , wipeNode
  , wipeScoring
  ) where


import           Analysis.Parsimony.Additive.Internal
import           Analysis.Parsimony.Fitch.Internal
import           Analysis.Parsimony.Sankoff.Internal
import           Analysis.Parsimony.Dynamic.DirectOptimization
import           Analysis.Parsimony.Dynamic.SequentialAlign
import           Bio.Character
import           Bio.Character.Decoration.Additive
import           Bio.Character.Decoration.Dynamic
import           Bio.Graph
import           Bio.Graph.Node
import           Bio.Graph.ReferenceDAG.Internal
import           Bio.Sequence
import           Control.Lens
import           Data.EdgeLength
import qualified Data.List.NonEmpty as NE
import           Data.MonoTraversable      (Element)
import           Data.TCM.Memoized


-- |
-- sequentialAlignOverride, iff True forces seqAlign to run; otherwise, DO runs.
sequentialAlignOverride :: Bool
sequentialAlignOverride = False


-- |
-- Remove all scoring data from nodes.
wipeScoring
  :: Monoid n
  => PhylogeneticDAG2 e n u v w x y z
  -> PhylogeneticDAG2 e n (Maybe u) (Maybe v) (Maybe w) (Maybe x) (Maybe y) (Maybe z)
wipeScoring (PDAG2 dag) = PDAG2 wipedDAG
  where
    wipedDAG =
        RefDAG
          <$> fmap wipeDecorations . references
          <*> rootRefs
          <*> ((mempty, mempty, Nothing) <$) . graphData
          $ dag
    
    wipeDecorations
      :: Monoid n
      => IndexData e (PhylogeneticNode2 (CharacterSequence u v w x y z) n)
      -> IndexData e (PhylogeneticNode2 (CharacterSequence (Maybe u) (Maybe v) (Maybe w) (Maybe x) (Maybe y) (Maybe z)) n)
    wipeDecorations x =
          IndexData
            <$> wipeNode shouldWipe . nodeDecoration
            <*> parentRefs
            <*> childRefs
            $ x
      where
        shouldWipe = not . null $ childRefs x


-- |
-- Conditionally wipe the scoring of a single node.
wipeNode
  :: Monoid n
  => Bool -- ^ Do I wipe?
  -> PhylogeneticNode2 (CharacterSequence        u         v         w         x         y         z ) n
  -> PhylogeneticNode2 (CharacterSequence (Maybe u) (Maybe v) (Maybe w) (Maybe x) (Maybe y) (Maybe z)) n 
wipeNode wipe = PNode2 <$> pure . g . NE.head . resolutions <*> f . nodeDecorationDatum2
      where
        f :: Monoid a => a -> a
        f | wipe      = const mempty
          | otherwise = id
        
        g = ResInfo
              <$> totalSubtreeCost
              <*> localSequenceCost
              <*> leafSetRepresentation
              <*> subtreeRepresentation
              <*> subtreeEdgeSet
              <*> topologyRepresentation
              <*> hexmap h h h h h h . characterSequence
        h :: a -> Maybe a
        h | wipe      = const Nothing
          | otherwise = Just
        


-- |
-- Take a solution of one or more undecorated trees and assign peliminary and
-- final states to all nodes.
scoreSolution :: CharacterResult -> PhylogeneticSolution FinalDecorationDAG
scoreSolution (PhylogeneticSolution forests) = PhylogeneticSolution $ fmap performDecoration <$> forests


-- |
-- Take an undecorated tree and assign peliminary and final states to all nodes.
performDecoration 
  :: ( DiscreteCharacterMetadata u
     , DiscreteCharacterMetadata w
     , DiscreteCharacterDecoration v StaticCharacter
     , DiscreteCharacterDecoration x StaticCharacter
     , DiscreteCharacterDecoration y StaticCharacter
     , RangedCharacterDecoration u ContinuousChar
     , RangedCharacterDecoration w StaticCharacter
     , SimpleDynamicDecoration z DynamicChar
     , Show u
     , Show v
     , Show w
     , Show x
     , Show y
     , Show z
     )
  => PhylogeneticDAG2 EdgeLength (Maybe String) (Maybe u) (Maybe v) (Maybe w) (Maybe x) (Maybe y) (Maybe z)
  -> FinalDecorationDAG
performDecoration x = performPreOrderDecoration performPostOrderDecoration
  where
    performPreOrderDecoration :: PostOrderDecorationDAG -> FinalDecorationDAG
    performPreOrderDecoration =
        preorderFromRooting''
          adaptiveDirectOptimizationPreOrder
          edgeCostMapping
          contextualNodeDatum
          minBlockConext
              
        . preorderSequence''
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
    performPostOrderDecoration = postOrderResult

    (minBlockConext, postOrderResult) = assignPunitiveNetworkEdgeCost post
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

    g _  Nothing  [] = error "Uninitialized leaf node. This is bad!"
    g h (Just  v) [] = h v []
    g h        e  xs = h (error $ mconcat [ "We shouldn't be using this value.", show e, show $ length xs ]) xs

    adaptiveDirectOptimizationPostOrder dec kidDecs = directOptimizationPostOrder pairwiseAlignmentFunction dec kidDecs
      where
        pairwiseAlignmentFunction = chooseDirectOptimizationComparison dec kidDecs


-- |
-- Contextually select the direct optimization method to perform on dynamic
-- characters.
chooseDirectOptimizationComparison
  :: ( SimpleDynamicDecoration d  c
     , SimpleDynamicDecoration d' c
     , Exportable c
     , Exportable (Element c)
--     , Show c
     , Ord (Element c)
     )
  => d
  -> [d']
  -> c
  -> c
  -> (Word, c, c, c, c)
chooseDirectOptimizationComparison dec decs =
    case decs of
      []  -> selectDynamicMetric dec
      x:_ -> selectDynamicMetric x


chooseDirectOptimizationComparison2
  :: ( SimpleDynamicDecoration d  c
     , SimpleDynamicDecoration d' c
     , Exportable c
     , Exportable (Element c)
--     , Show c
     , Ord (Element c)
     )
  => d
  -> [(a,d')]
  -> c
  -> c
  -> (Word, c, c, c, c)
chooseDirectOptimizationComparison2 dec decs =
    case decs of
      []      -> selectDynamicMetric dec
      (_,x):_ -> selectDynamicMetric x


-- |
-- An identety function which ignores the second parameter.
id2 :: a -> b -> a
id2 = const


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


