------------------------------------------------------------------------------
-- |
-- Module      :  Bio.PhyloGraphPrime.PhylogeneticDAG.Internal
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Containing the master command for unifying all input types: tree, metadata, and sequence
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}

module Bio.PhyloGraphPrime.PhylogeneticDAG.Internal where

import           Bio.Character
import           Bio.Character.Decoration.Additive
import           Bio.Character.Decoration.Continuous
import           Bio.Character.Decoration.Discrete
import           Bio.Character.Decoration.Dynamic
import           Bio.Character.Decoration.Fitch
import           Bio.Character.Decoration.Metric 
import           Bio.Sequence
import           Bio.Sequence.Block        (CharacterBlock)
import           Bio.PhyloGraphPrime
import           Bio.PhyloGraphPrime.Node
import           Bio.PhyloGraphPrime.ReferenceDAG.Internal
import           Control.Applicative       (liftA2)
import           Control.Evaluation
import           Data.Bits
import           Data.EdgeLength
import           Data.Foldable
import           Data.IntSet               (IntSet)
import qualified Data.IntSet        as IS
import           Data.Key
import           Data.List.NonEmpty        (NonEmpty( (:|) ))
import qualified Data.List.NonEmpty as NE
import           Data.List.Utility
import           Data.Maybe
import           Data.MonoTraversable
import           Data.Semigroup
import           Data.Semigroup.Foldable
import           Prelude            hiding (zipWith)


type SearchState = EvaluationT IO (Either TopologicalResult (PhylogeneticSolution InitialDecorationDAG))


type TopologicalResult = PhylogeneticSolution (ReferenceDAG EdgeLength (Maybe String))


type CharacterResult = PhylogeneticSolution CharacterDAG


type DecoratedCharacterResult = PhylogeneticSolution InitialDecorationDAG


type UnRiefiedCharacterDAG =
       PhylogeneticDAG
         EdgeLength
         (Maybe String)
         UnifiedDiscreteCharacter
         UnifiedDiscreteCharacter
         UnifiedContinuousCharacter
         UnifiedDiscreteCharacter
         UnifiedDiscreteCharacter
         UnifiedDynamicCharacter


type CharacterDAG =
       PhylogeneticDAG2
         EdgeLength
         (Maybe String)
         UnifiedDiscreteCharacter
         UnifiedDiscreteCharacter
         UnifiedContinuousCharacter
         UnifiedDiscreteCharacter
         UnifiedDiscreteCharacter
         UnifiedDynamicCharacter


type InitialDecorationDAG =
       PhylogeneticDAG2
         EdgeLength
         (Maybe String)
         (SankoffOptimizationDecoration  StaticCharacter)
         (SankoffOptimizationDecoration  StaticCharacter)
         -- UnifiedContinuousCharacter
         -- (ContinuousOptimizationDecoration ContinuousChar)
         (ContinuousPostorderDecoration  ContinuousChar)
         (FitchOptimizationDecoration    StaticCharacter)
         (AdditiveOptimizationDecoration StaticCharacter)
         -- UnifiedDynamicCharacter
         -- (DynamicDecorationDirectOptimization DynamicChar)
         (DynamicDecorationDirectOptimizationPostOrderResult DynamicChar)


type UnifiedCharacterSequence
     = CharacterSequence
         UnifiedDiscreteCharacter
         UnifiedDiscreteCharacter
         UnifiedContinuousCharacter
         UnifiedDiscreteCharacter
         UnifiedDiscreteCharacter
         UnifiedDynamicCharacter


type UnifiedCharacterBlock
     = CharacterBlock
         UnifiedDiscreteCharacter
         UnifiedDiscreteCharacter
         UnifiedContinuousCharacter
         UnifiedDiscreteCharacter
         UnifiedDiscreteCharacter
         UnifiedDynamicCharacter


type UnifiedContinuousCharacter = Maybe (ContinuousDecorationInitial ContinuousChar)


type UnifiedDiscreteCharacter   = Maybe (DiscreteDecoration StaticCharacter)


type UnifiedDynamicCharacter    = Maybe (DynamicDecorationInitial DynamicChar)


data PhylogeneticDAG e n m i c f a d
     = PDAG (ReferenceDAG e (PhylogeneticNode n (CharacterSequence m i c f a d)))


data PhylogeneticDAG2 e n m i c f a d
     = PDAG2 (ReferenceDAG e (PhylogeneticNode2 (CharacterSequence m i c f a d) n))


instance ( Show e
         , Show n
         , Show m
         , Show i
         , Show c
         , Show f
         , Show a
         , Show d
         , HasCharacterCost   m Word
         , HasCharacterCost   i Word
         , HasCharacterCost   c Double
         , HasCharacterCost   f Word
         , HasCharacterCost   a Word
         , HasCharacterCost   d Word
         , HasCharacterWeight m Double
         , HasCharacterWeight i Double
         , HasCharacterWeight c Double
         , HasCharacterWeight f Double
         , HasCharacterWeight a Double
         , HasCharacterWeight d Double
         ) => Show (PhylogeneticDAG e n m i c f a d) where

    show (PDAG dag) = show dag <> "\n" <> foldMapWithKey f dag
      where
        f i (PNode n sek) = mconcat [ "Node {", show i, "}:\n\n", unlines [show n, show sek] ] 


instance ( Show e
         , Show n
         , Show m
         , Show i
         , Show c
         , Show f
         , Show a
         , Show d
         , HasCharacterCost   m Word
         , HasCharacterCost   i Word
         , HasCharacterCost   c Double
         , HasCharacterCost   f Word
         , HasCharacterCost   a Word
         , HasCharacterCost   d Word
         , HasCharacterWeight m Double
         , HasCharacterWeight i Double
         , HasCharacterWeight c Double
         , HasCharacterWeight f Double
         , HasCharacterWeight a Double
         , HasCharacterWeight d Double
         ) => Show (PhylogeneticDAG2 e n m i c f a d) where

    show (PDAG2 dag) = show dag <> "\n" <> foldMapWithKey f dag
      where
--        f i (PNode2 n sek) = mconcat [ "Node {", show i, "}:\n\n", unlines [show n, show sek], "\n\n" ] 
        f i n = mconcat [ "Node {", show i, "}:\n\n", show n ]


type EdgeReference = (Int, Int)


type IncidentEdges = [EdgeReference]

    
type Cost = Double


type ReRootedEdgeContext u v w x y z =
   ( ResolutionCache (CharacterSequence u v w x y z)
   , ResolutionCache (CharacterSequence u v w x y z)
   , ResolutionCache (CharacterSequence u v w x y z)
   )


applySoftwireResolutions :: [(ResolutionCache s, IntSet)] -> NonEmpty [ResolutionInformation s]
applySoftwireResolutions inputContexts =
    case inputContexts of
      []   -> pure []
      [x]  ->
          let y = pure <$> fst x
          -- TODO: review this logic thouroughly
          in  if   multipleParents x
              then y -- <> pure []
              else y
      x:xs ->
        case pairs $ x:xs of
          y:ys -> foldMap1 pairingLogic $ y :| ys
          -- This will never happen, covered by previous case statement
          []   -> error "Fatal logic error in 'applySoftwireResolutions' definition when matching pattern in 'pairs' application."

  where
    multipleParents = not . isSingleton . otoList . snd
{-
    pairingLogic :: ( (ResolutionCache s), IntSet)
                    , (ResolutionCache s), IntSet)
                    )
                 -> NonEmpty [ResolutionInformation s]
-}
    pairingLogic (lhs, rhs) =
        case (multipleParents lhs, multipleParents rhs) of
          (False, False) -> pairedSet
          (False, True ) -> pairedSet <> rhsSet
          (True , False) -> pairedSet <> lhsSet
          (True , True ) -> pairedSet <> lhsSet <> rhsSet
       where
         lhsSet = pure <$> lhs'
         rhsSet = pure <$> rhs'
         lhs'   = fst lhs
         rhs'   = fst rhs
         pairedSet =
             case cartesianProduct lhs' rhs' of
               x:xs -> x:|xs
               []   -> pure [] -- This shouldn't ever happen
--         cartesianProduct :: (Foldable t, Foldable t') => t a -> t a' -> [[a]]

         cartesianProduct xs ys =
             [ [x,y]
             | x <- toList xs
             , y <- toList ys
             , resolutionsDoNotOverlap x y
             ]


resolutionsDoNotOverlap :: ResolutionInformation a -> ResolutionInformation b -> Bool
resolutionsDoNotOverlap x y = leafSetRepresentation x .&. leafSetRepresentation y == zeroBits


localResolutionApplication :: (d -> [d] -> d')
                           -> NonEmpty (ResolutionInformation (CharacterSequence u v w x y d))
                           -> ResolutionCache (CharacterSequence u v w x y d)
                           -> NonEmpty (ResolutionInformation (CharacterSequence u v w x y d'))
localResolutionApplication f x y =
    liftA2 (generateLocalResolutions id2 id2 id2 id2 id2 f) mutalatedChild relativeChildResolutions
  where
    relativeChildResolutions = applySoftwireResolutions
      [ (x, IS.singleton 0)
      , (y, IS.singleton 0)
      ]
    id2 z _ = z
    mutalatedChild = pure
        ResInfo
        { totalSubtreeCost      = 0
        , localSequenceCost     = 0
        , subtreeEdgeSet        = mempty
        , leafSetRepresentation = zeroBits
        , subtreeRepresentation = singletonNewickSerialization 0
        , characterSequence     = characterSequence $ NE.head x
        }


generateLocalResolutions :: (u -> [u'] -> u'')
                         -> (v -> [v'] -> v'')
                         -> (w -> [w'] -> w'')
                         -> (x -> [x'] -> x'')
                         -> (y -> [y'] -> y'')
                         -> (z -> [z'] -> z'')
                         ->  ResolutionInformation (CharacterSequence u   v   w   x   y   z  )
                         -> [ResolutionInformation (CharacterSequence u'  v'  w'  x'  y'  z' )]
                         ->  ResolutionInformation (CharacterSequence u'' v'' w'' x'' y'' z'')
generateLocalResolutions f1 f2 f3 f4 f5 f6 parentalResolutionContext childResolutionContext =
                ResInfo
                { totalSubtreeCost      = sum $ totalSubtreeCost  <$> childResolutionContext
                , localSequenceCost     = sum $ localSequenceCost <$> childResolutionContext
                , subtreeEdgeSet        = newSubtreeEdgeSet
                , leafSetRepresentation = newLeafSetRep
                , subtreeRepresentation = newSubtreeRep
                , characterSequence     = transformation (characterSequence parentalResolutionContext) (characterSequence <$> childResolutionContext)
                }
              where
                newSubtreeEdgeSet = foldMap subtreeEdgeSet childResolutionContext

                (newLeafSetRep, newSubtreeRep) =
                    case childResolutionContext of
                      []   -> (,) <$>          leafSetRepresentation <*>          subtreeRepresentation $ parentalResolutionContext
                      x:xs -> (,) <$> foldMap1 leafSetRepresentation <*> foldMap1 subtreeRepresentation $ x:|xs

                transformation pSeq cSeqs = hexZipWith f1 f2 f3 f4 f5 f6 pSeq transposition
                  where
                    transposition = 
                        case cSeqs of
                          x:xs -> hexTranspose $ x:|xs
                          []   -> let c = const []
                                  in hexmap c c c c c c pSeq


pairs :: Foldable f => f a -> [(a, a)]
pairs = f . toList
  where
    f    []  = []
    f   [_]  = []
    f (x:xs) = ((\y -> (x, y)) <$> xs) <> f xs


