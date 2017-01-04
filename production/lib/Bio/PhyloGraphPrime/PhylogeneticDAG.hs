------------------------------------------------------------------------------
-- |
-- Module      :  Bio.PhyloGraphPrime.PhylogeneticDAG
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

module Bio.PhyloGraphPrime.PhylogeneticDAG where

import           Bio.Character
import           Bio.Character.Decoration.Continuous
import           Bio.Character.Decoration.Discrete
import           Bio.Character.Decoration.Dynamic
import           Bio.Sequence
import           Bio.Sequence.Block
import           Bio.PhyloGraphPrime
import           Bio.PhyloGraphPrime.Node
import           Bio.PhyloGraphPrime.ReferenceDAG.Internal
import           Control.Arrow            ((&&&))
import           Control.Evaluation
import           Data.Bits
import           Data.DuplicateSet
import qualified Data.DuplicateSet  as DS
import           Data.Foldable
import qualified Data.IntMap        as IM
import           Data.IntSet              (IntSet)
import           Data.List.NonEmpty       (NonEmpty( (:|) ))
import           Data.List.Utility
-- import qualified Data.List.NonEmpty as NE
--import           Data.Monoid
import           Data.MonoTraversable
import           Data.Semigroup
import           Data.Semigroup.Foldable
import           Data.Vector              (Vector)
import qualified Data.Vector        as V

-- import Debug.Trace


type SearchState = EvaluationT IO (Either TopologicalResult CharacterResult)


type TopologicalResult = PhylogeneticSolution (ReferenceDAG (Maybe Double) (Maybe String))


type CharacterResult   = PhylogeneticSolution CharacterDAG


type CharacterDAG      = PhylogeneticDAG
                             (Maybe Double)
                             (Maybe String)
                             UnifiedDiscreteCharacter
                             UnifiedDiscreteCharacter
                             UnifiedContinuousCharacter
                             UnifiedDiscreteCharacter
                             UnifiedDiscreteCharacter
                             UnifiedDynamicCharacter


type  UnifiedCharacterSequence
    = CharacterSequence
        UnifiedDiscreteCharacter
        UnifiedDiscreteCharacter
        UnifiedContinuousCharacter
        UnifiedDiscreteCharacter
        UnifiedDiscreteCharacter
        UnifiedDynamicCharacter


type  UnifiedCharacterBlock
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


-- PhylogeneticDAG (Maybe Double) (Maybe String) (Maybe StaticCharacterBlock) (Maybe DynamicChar)


data  PhylogeneticDAG e n m i c f a d
    = PDAG (ReferenceDAG e (PhylogeneticNode n (CharacterSequence m i c f a d)))


data  PhylogeneticDAG2 e n m i c f a d
    = PDAG2 (ReferenceDAG e (PhylogeneticNode2 n (CharacterSequence m i c f a d)))


instance ( Show e
         , Show n
         , Show m
         , Show i
         , Show c
         , Show f
         , Show a
         , Show d
         ) => Show (PhylogeneticDAG e n m i c f a d) where

    show (PDAG dag) =
        show dag <> "\n" <> foldMap f dag
      where
        f (PNode n sek) = unlines [show n, show sek]


metric :: (m -> [m'] -> m')
       -> (PhylogeneticNode2 n (CharacterSequence m i c f a d) -> [PhylogeneticNode2 n (CharacterSequence m' i c f a d)] ->  PhylogeneticNode2 n (CharacterSequence m' i c f a d))
metric _f = undefined


pairs :: Foldable f => f a -> [(a, a)]
pairs = f . toList
  where
    f    []  = []
    f   [_]  = []
    f (x:xs) = ((\y -> (x, y)) <$> xs) <> f xs


{--}
-- One or more 
-- Do I need the whole DAG in scope to resolve resolutions?
resolutionTransform :: Vector (IndexData e (PhylogeneticNode2 (CharacterSequence m' i' c' f' a' d') n))
                    -> PhylogeneticNode2 (CharacterSequence m i c f a d) n
                    -> (CharacterSequence m i c f a d -> [CharacterSequence m' i' c' f' a' d'] -> CharacterSequence m' i' c' f' a' d')
                    -> Int
                    -> PhylogeneticNode2 (CharacterSequence m' i' c' f' a' d') n
resolutionTransform dataVector currentNode transformation index =
    PNode2
    { resolutions          = newResolutions
    , nodeDecorationDatum2 = nodeDecorationDatum2 currentNode
    }
  where

--    newResolutions = selfResolutions >>= (\x -> g x <$> childListings)
    newResolutions = cartesianProductWith g selfResolutions childListings
      where
--        g :: ResolutionInformation s -> [ResolutionInformation s] -> ResolutionInformation s
        g parentalResolution childResolutions =
            ResInfo
            { leafSetRepresentation = newLeafSetRep
            , subtreeRepresentation = newSubtreeRep
            , characterSequence     = transformation (characterSequence parentalResolution) (characterSequence <$> childResolutions)
            , localSequenceCost     = sum $ localSequenceCost <$> childResolutions
            , totalSubtreeCost      = sum $ totalSubtreeCost  <$> childResolutions
            }
          where
            (newLeafSetRep, newSubtreeRep) =
                case childResolutions of
                  []   -> (,) <$>          leafSetRepresentation <*>          subtreeRepresentation $ parentalResolution
                  x:xs -> (,) <$> foldMap1 leafSetRepresentation <*> foldMap1 subtreeRepresentation $ x:|xs

--    childIndices :: [Int]
    childIndices        = IM.keys . childRefs $ dataVector V.! index

    selfResolutions     = resolutions currentNode

--    childResolutionData :: [(DuplicateSet (ResolutionInformation s), IntSet)]
    childResolutionData = ((resolutions . nodeDecoration &&& parentRefs) . (dataVector V.!)) <$> childIndices

--    childListings :: DuplicateSet [ResolutionInformation s]
    childListings =
      -- Here we check the number of "real" child edges.
      case childResolutionData of
        []   -> singleton [] 
        [x]  ->
            let y = DS.map (:[]) $ fst x
            in  if   multipleParents x
                then y <> singleton []
                else y
        x:xs ->
          case pairs $ x:xs of
            y:ys -> foldMap1 pairingLogic $ y :| ys
            []   -> singleton [] -- This will never happen, covered by previous case statement
      where
        multipleParents = not . isSingleton . otoList . snd
        pairingLogic :: ( (DuplicateSet (ResolutionInformation s), IntSet)
                        , (DuplicateSet (ResolutionInformation s), IntSet)
                        )
                     -> DuplicateSet [ResolutionInformation s]
        pairingLogic (lhs, rhs) =
            case (pairedSet, multipleParents lhs, multipleParents rhs) of
              -- The Nothing cases *should* never happen, but best to handle them anyways
              (Nothing, True , True ) -> singleton []
              (Nothing, True , False) -> lhsSet
              (Nothing, False, True ) -> rhsSet
              (Nothing, False, False) -> lhsSet <> rhsSet
              (Just ds, True , True ) -> ds
              (Just ds, True , False) -> ds <> lhsSet
              (Just ds, False, True ) -> ds <> rhsSet
              (Just ds, False, False) -> ds <> lhsSet <> rhsSet
           where
             pairedSet = cartesianProductWithFilter f lhs' rhs'
               where
                 f x y
                   | leafSetRepresentation x .&. leafSetRepresentation y == zeroBits = Just [x,y]
                   | otherwise = Nothing
             lhsSet = DS.map (:[]) lhs'
             rhsSet = DS.map (:[]) rhs'
             lhs'   = fst lhs
             rhs'   = fst rhs

{--}  

nodeInternalPostorderMap :: (PhylogeneticNode2 n (CharacterSequence m i c f a d) -> [PhylogeneticNode2 n' (CharacterSequence m' i' c' f' a' d')] -> PhylogeneticNode2 n' (CharacterSequence m' i' c' f' a' d'))
                         -> PhylogeneticDAG2 e n m i c f a d
                         -> PhylogeneticDAG2 e n' m' i' c' f' a' d'
nodeInternalPostorderMap f (PDAG2 dag) = PDAG2 $ nodePostOrder f dag


-- TODO:

-- Option 1: Assume    NonEmpty resolutions are of correct size
-- Option 2: Disregard NonEmpty resolutions and recalculate sizes
--
-- Ignore above! Just apply transformation.
{-
nodeInternalPostorderMap :: (PhylogeneticNode2 n  s -> [PhylogeneticNode2 n' s'] ->  PhylogeneticNode2 n' s')
                -> ReferenceDAG e (PhylogeneticNode2 n  s )
                -> ReferenceDAG e (PhylogeneticNode2 n' s')
nodeInternalPostorderMap f = nodePostOrder f
-}
{-
  RefDAG <$> const newReferences <*> rootRefs <*> graphData $ dag
  where
    dagSize       = length $ references dag
    newReferences = V.generate dagSize h
      where
        h i = IndexData <$> const (memo ! i) <*> parentRefs <*> childRefs $ references dag V.! i
    memo = V.generate dagSize h
      where
        h i = f datum $ (memo V.!) <$> childIndices
          where
            datum        = nodeDecoration node
            node         = references dag V.! i
            childIndices = IM.keys $ childRefs node
-}
  


nodePreorderMap :: (n -> [n'] -> n')
nodePreorderMap = undefined

edgePreorderMap :: (e -> [e'] -> e')
edgePreorderMap = undefined

nodePostorderMap :: (n -> [n'] -> n')
nodePostorderMap = undefined

edgePostorderMap :: (e -> [e'] -> e')
edgePostorderMap = undefined

nodePreorderFold :: (n -> [a] -> a)
nodePreorderFold = undefined

edgePreorderFold :: (e -> [a] -> a)
edgePreorderFold = undefined

nodePostorderFold :: (n -> [a] -> a)
nodePostorderFold = undefined

edgePostorderFold :: (e -> [a] -> a)
edgePostorderFold = undefined

