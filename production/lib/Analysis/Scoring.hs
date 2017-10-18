----------------------------------------------------------------------------
-- |
-- Module      :  Analysis.Scoring
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

module Analysis.Scoring where


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
import           Data.Semigroup
import           Prelude            hiding (lookup, zip, zipWith)


-- |
-- sequentialAlignOverride, iff True forces seqAlign to run; otherwise, DO runs.
sequentialAlignOverride :: Bool
sequentialAlignOverride = False


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
          <*> defaultGraphMetadata . graphData
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
        


scoreSolution :: CharacterResult -> PhylogeneticSolution FinalDecorationDAG
scoreSolution (PhylogeneticSolution forests) = PhylogeneticSolution $ fmap performDecoration <$> forests

    
--performDecoration :: CharacterDAG -> FinalDecorationDAG
performDecoration 
  :: ( DiscreteCharacterMetadata u
     , DiscreteCharacterMetadata w
     , DiscreteCharacterDecoration v StaticCharacter
     , DiscreteCharacterDecoration x StaticCharacter
     , DiscreteCharacterDecoration y StaticCharacter
     , HasRootCost  u v w x y z Double
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
        preorderFromRooting
          adaptiveDirectOptimizationPreOrder
          edgeCostMapping
          contextualNodeDatum
              
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

    g _  Nothing  [] = error "Uninitialized leaf node. This is bad!"
    g h (Just  v) [] = h v []
    g h        e  xs = h (error $ mconcat [ "We shouldn't be using this value.", show e, show $ length xs ]) xs

    adaptiveDirectOptimizationPostOrder dec kidDecs = directOptimizationPostOrder pairwiseAlignmentFunction dec kidDecs
      where
        pairwiseAlignmentFunction = chooseDirectOptimizationComparison dec kidDecs


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


