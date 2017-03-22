------------------------------------------------------------------------------
-- |
-- Module      :  Bio.PhyloGraphPrime.PhylogeneticDAG
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

module Bio.PhyloGraphPrime.PhylogeneticDAG where

import           Bio.Character
import           Bio.Character.Encodable.Continuous
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
import           Control.Arrow             ((&&&))
import           Control.Applicative       (liftA2)
import           Control.Evaluation
import           Control.Lens
import           Control.Monad.State.Lazy
import           Data.Bits
--import           Data.DuplicateSet
--import qualified Data.DuplicateSet  as DS
import           Data.Foldable
import           Data.Hashable
import           Data.Hashable.Memoize
--import           Data.HashMap.Lazy         (HashMap)
--import qualified Data.HashMap.Lazy  as HM
import qualified Data.IntMap        as IM
import           Data.IntSet               (IntSet)
import qualified Data.IntSet        as IS
import           Data.Key
import           Data.List.NonEmpty        (NonEmpty( (:|) ))
import qualified Data.List.NonEmpty as NE
import           Data.List.Utility
--import           Data.Map                  (Map)
import qualified Data.Map           as M
import           Data.Maybe
--import           Data.Monoid
import           Data.MonoTraversable
import           Data.Semigroup
import           Data.Semigroup.Foldable
--import           Data.Vector               (Vector)
import qualified Data.Vector        as V

import           Prelude            hiding (zipWith)

--import Debug.Trace


type SearchState = EvaluationT IO (Either TopologicalResult (PhylogeneticSolution InitialDecorationDAG))


type TopologicalResult = PhylogeneticSolution (ReferenceDAG (Maybe Double) (Maybe String))


type CharacterResult   = PhylogeneticSolution CharacterDAG


type UnRiefiedCharacterDAG = PhylogeneticDAG
                               (Maybe Double)
                               (Maybe String)
                               UnifiedDiscreteCharacter
                               UnifiedDiscreteCharacter
                               UnifiedContinuousCharacter
                               UnifiedDiscreteCharacter
                               UnifiedDiscreteCharacter
                               UnifiedDynamicCharacter

type CharacterDAG = PhylogeneticDAG
                        (Maybe Double)
                        (Maybe String)
                        UnifiedDiscreteCharacter
                        UnifiedDiscreteCharacter
                        UnifiedContinuousCharacter
                        UnifiedDiscreteCharacter
                        UnifiedDiscreteCharacter
                        UnifiedDynamicCharacter


type DecoratedCharacterResult = PhylogeneticSolution InitialDecorationDAG


type InitialDecorationDAG = PhylogeneticDAG
                                (Maybe Double)
                                (Maybe String)
                                (SankoffOptimizationDecoration  StaticCharacter)
                                (SankoffOptimizationDecoration  StaticCharacter)
                                --UnifiedContinuousCharacter
                                (ContinuousOptimizationDecoration ContinuousChar)
                                (FitchOptimizationDecoration    StaticCharacter)
                                (AdditiveOptimizationDecoration StaticCharacter)
                                -- UnifiedDynamicCharacter
                                (DynamicDecorationDirectOptimization DynamicChar)
                                --(DynamicDecorationDirectOptimizationPostOrderResult DynamicChar)


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
    = PDAG2 (ReferenceDAG e (PhylogeneticNode2 (CharacterSequence m i c f a d) n))


--rootCosts :: PhylogeneticDAG2 -> NonEmpty Double

rootCosts :: ( Integral e
             , HasCharacterWeight u Double
             , HasCharacterWeight v Double
             , HasCharacterWeight x Double
             , HasCharacterWeight y Double
             , HasCharacterWeight z Double
             , HasCharacterCost u e
             , HasCharacterCost v e
             , HasCharacterCost x e
             , HasCharacterCost y e
             , HasCharacterCost z e
             )
{--
rootCosts :: ( HasCharacterWeight u Double
             , HasCharacterWeight v Double
             , HasCharacterWeight x Double
             , HasCharacterWeight y Double
             , HasCharacterWeight z Double
             , HasCharacterCost u Word
             , HasCharacterCost v Word
             , HasCharacterCost x Word
             , HasCharacterCost y Word
             , HasCharacterCost z Word
             )
-}
          => PhylogeneticDAG2 s t u v w x y z -> NonEmpty Double
rootCosts (PDAG2 dag) = sequenceCost <$> rootDecs
  where
    roots     = rootRefs dag
    rootDecs  = (characterSequence . NE.head . resolutions . nodeDecoration . (references dag !)) <$> roots


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
--         , HasCharacterCost   c Double
         , HasCharacterCost   f Word
         , HasCharacterCost   a Word
         , HasCharacterCost   d Word
         , HasCharacterWeight m Double
         , HasCharacterWeight i Double
--         , HasCharacterWeight c Double
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
--         , HasCharacterCost   c Double
         , HasCharacterCost   f Word
         , HasCharacterCost   a Word
         , HasCharacterCost   d Word
         , HasCharacterWeight m Double
         , HasCharacterWeight i Double
--         , HasCharacterWeight c Double
         , HasCharacterWeight f Double
         , HasCharacterWeight a Double
         , HasCharacterWeight d Double
         ) => Show (PhylogeneticDAG2 e n m i c f a d) where

    show (PDAG2 dag) = show dag <> "\n" <> foldMapWithKey f dag
      where
--        f i (PNode2 n sek) = mconcat [ "Node {", show i, "}:\n\n", unlines [show n, show sek], "\n\n" ] 
        f i n = mconcat [ "Node {", show i, "}:\n\n", show n ]

{-
riefiedSolution :: PhylogeneticSolution UnRiefiedCharacterDAG -> CharacterResult
riefiedSolution  = PhylogeneticSolution . fmap (fmap riefiedToCharacterDAG) . phylogeneticForests


riefiedToCharacterDAG :: UnRiefiedCharacterDAG -> CharacterDAG
riefiedToCharacterDAG (PDAG dag) = PDAG2
    RefDAG
    { references = newRefs
    , rootRefs   = rootRefs  dag
    , graphData  = graphData dag
    }
  where
    dagSize   = length $ references dag

    newRefs = V.generate dagSize g
      where
        g i = IndexData <$> const (memo ! i) <*> parentRefs <*> childRefs $ references dag ! i

    memo = V.generate dagSize g
      where
        g i = newNode -- IndexData <$> const newNode <*> parentRefs <*> childRefs $ indexData
          where
            indexData = references dag ! i
            newNode =
                PNode2
                { resolutions          = res
                , nodeDecorationDatum2 = nodeDecorationDatum $ nodeDecoration indexData
                }
            res = pure
                ResInfo
                { leafSetRepresentation = bv
                , subtreeRepresentation = ns
                , characterSequence     = sequenceDecoration $ nodeDecoration indexData
                , localSequenceCost     = 0
                , totalSubtreeCost      = 0
                }
            
            (bv, ns) =
              case buildLeafNodeAssignments ! i of
                Just n  -> ( leafCount `singletonSubtreeLeafSet` n
                           , singletonNewickSerialization n
                           )
                Nothing ->
                  case IM.keys $ childRefs indexData of
                    x:xs -> ( foldr1 (.|.) $ leafSetRepresentation . NE.head . resolutions . (memo !) <$> (x:|xs)
                            , foldMap1 (subtreeRepresentation . NE.head . resolutions . (memo !)) (x:|xs)
                            )
                    []   -> error "Never occurs."
        
        (buildLeafNodeAssignments, leafCount) = (`runState` 0) . traverse f $ references dag
          where
            f e
              | (not . null) (childRefs e) = pure Nothing
              | otherwise = do
                  c <- get
                  modify (+1)
                  pure $ Just c
-}

-- |
-- Applies a traversal logic function over a 'ReferenceDAG' in a /pre-order/ manner.
--
-- The logic function takes a current node decoration,
-- a list of parent node decorations with the logic function already applied,
-- and returns the new decoration for the current node.
postorderSequence' :: (Eq z, Eq z', Hashable z, Hashable z')
                   => (u -> [u'] -> u')
                   -> (v -> [v'] -> v')
                   -> (w -> [w'] -> w')
                   -> (x -> [x'] -> x')
                   -> (y -> [y'] -> y')
                   -> (z -> [z'] -> z')
                   -> PhylogeneticDAG2 e n u  v  w  x  y  z
                   -> PhylogeneticDAG2 e n u' v' w' x' y' z'
postorderSequence' f1 f2 f3 f4 f5 f6 (PDAG2 dag) = PDAG2 $ newDAG dag
  where
    f6' = memoize2 f6
    newDAG        = RefDAG <$> const newReferences <*> rootRefs <*> graphData
    dagSize       = length $ references dag
    newReferences = V.generate dagSize h
      where
        h i = IndexData <$> const (memo ! i) <*> parentRefs <*> childRefs $ references dag ! i

--    memo :: Vector (PhylogeneticNode2 n (CharacterSequence u' v' w' x' y' z'))
    memo = V.generate dagSize h
      where
        h i =
          PNode2
              { resolutions          = liftA2 (generateLocalResolutions f1 f2 f3 f4 f5 f6) datumResolutions childResolutions
              , nodeDecorationDatum2 = nodeDecorationDatum2 $ nodeDecoration node
              }
          where
            node             = references dag ! i
            childIndices     = IM.keys $ childRefs node
            datumResolutions = resolutions $ nodeDecoration node
            
--            childResolutions :: NonEmpty [a]
            childResolutions = applySoftwireResolutions $ extractResolutionContext <$> childIndices
            extractResolutionContext = resolutions . (memo !) &&& parentRefs . (references dag !)

            --        g :: ResolutionInformation s -> [ResolutionInformation s] -> ResolutionInformation s
generateLocalResolutions f1 f2 f3 f4 f5 f6 parentalResolutionContext childResolutionContext =
                ResInfo
                { leafSetRepresentation = newLeafSetRep
                , subtreeRepresentation = newSubtreeRep
                , characterSequence     = transformation (characterSequence parentalResolutionContext) (characterSequence <$> childResolutionContext)
                , localSequenceCost     = sum $ localSequenceCost <$> childResolutionContext
                , totalSubtreeCost      = sum $ totalSubtreeCost  <$> childResolutionContext
                }
              where
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





applySoftwireResolutions :: [(ResolutionCache s, IntSet)] -> NonEmpty [ResolutionInformation s]
applySoftwireResolutions inputContexts =
    case inputContexts of
      []   -> pure []
      [x]  ->
          let y = pure <$> fst x
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
          -- The Nothing cases *should* never happen, but best to handle them anyways
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
             , leafSetRepresentation x .&. leafSetRepresentation y == zeroBits
             ]

{-
generatePairs :: [NonEmpty a] -> NonEmpty [a]
generatePairs    [] =  []:|[]
generatePairs   [x] = [x]:|[]
generatePairs [x,y] = x `cartesianProduct` y
generatePairs  x:xs = ((cartesianProduct x) <$> xs) <> generatePairs xs
  where
    cartesianProduct :: NonEmpty a -> NonEmpty a -> NonEmpty [a]
    cartesianProduct xs ys = do
        x <- xs
        y <- ys
        pure [x,y]
-}




{-
metric :: (m -> [m'] -> m')
       -> (PhylogeneticNode2 n (CharacterSequence m i c f a d) -> [PhylogeneticNode2 n (CharacterSequence m' i c f a d)] ->  PhylogeneticNode2 n (CharacterSequence m' i c f a d))
metric _f = undefined

-}

pairs :: Foldable f => f a -> [(a, a)]
pairs = f . toList
  where
    f    []  = []
    f   [_]  = []
    f (x:xs) = ((\y -> (x, y)) <$> xs) <> f xs


{-
-- One or more 
-- Do I need the whole DAG in scope to resolve resolutions?
resolutionTransform :: Vector (IndexData e (PhylogeneticNode2 (CharacterSequence m' i' c' f' a' d') n))
                    -> Int
                    -> (CharacterSequence m i c f a d -> [CharacterSequence m' i' c' f' a' d'] -> CharacterSequence m' i' c' f' a' d')
                    -> PhylogeneticNode2 (CharacterSequence m i c f a d) n
                    -> PhylogeneticNode2 (CharacterSequence m' i' c' f' a' d') n
resolutionTransform dataVector index transformation currentNode =
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


-}

{-
-- |
-- Applies a traversal logic function over a 'ReferenceDAG' in a /post-order/ manner.
--
-- The logic function takes a current node decoration,
-- a list of child node decorations with the logic function already applied,
-- and returns the new decoration for the current node.
decorationPostOrder :: (m -> [m'] -> m') -- ^ post-order transformation for /metric/       characters
                    -> (i -> [i'] -> i') -- ^ post-order transformation for /non-metric/   characters
                    -> (c -> [c'] -> c') -- ^ post-order transformation for /continuous/   characters
                    -> (f -> [f'] -> f') -- ^ post-order transformation for /non-additive/ characters
                    -> (a -> [a'] -> a') -- ^ post-order transformation for /additive/     characters
                    -> (d -> [d'] -> d') -- ^ post-order transformation for /dynamic/      characters
                    ->  PhylogeneticDAG2 e n m  i  c  f  a  d 
                    ->  PhylogeneticDAG2 e n m' i' c' f' a' d'
decorationPostOrder f1 f2 f3 f4 f5 f6 (PDAG2 pSolution) =
    PDAG2 $ decorationPostOrderDAG f1 f2 f3 f4 f5 f6 pSolution


decorationPostOrderDAG :: (m -> [m'] -> m')
                       -> (i -> [i'] -> i')
                       -> (c -> [c'] -> c')
                       -> (f -> [f'] -> f')
                       -> (a -> [a'] -> a')
                       -> (d -> [d'] -> d')
                       -> ReferenceDAG e (PhylogeneticNode2 n (CharacterSequence m  i  c  f  a  d ))
                       -> ReferenceDAG e (PhylogeneticNode2 n (CharacterSequence m' i' c' f' a' d'))
decorationPostOrderDAG f1 f2 f3 f4 f5 f6 = RefDAG <$> generateNewReferences <*> rootRefs <*> graphData
  where
    newReferences dag = V.generate dagSize h
      where
        dagSize = length $ references dag
        h i     = IndexData <$> const (memo ! i) <*> parentRefs <*> childRefs $ references dag ! i
        memo    = V.generate dagSize h
          where
            h i = f datum $ (memo !) <$> childIndices
              where
                datum        = nodeDecoration node
                node         = references dag ! i
                childIndices = IM.keys $ childRefs node 
-}


{--}  

{-
nodeInternalPostorderMap :: (PhylogeneticNode2 n (CharacterSequence m i c f a d) -> [PhylogeneticNode2 n' (CharacterSequence m' i' c' f' a' d')] -> PhylogeneticNode2 n' (CharacterSequence m' i' c' f' a' d'))
                         -> PhylogeneticDAG2 e n m i c f a d
                         -> PhylogeneticDAG2 e n' m' i' c' f' a' d'
nodeInternalPostorderMap f (PDAG2 dag) = PDAG2 $ nodePostOrder f dag
-}

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


type EdgeReference = (Int, Int)


type IncidentEdges = [EdgeReference]

    
type Cost = Double


type ReRootedEdgeContext u v w x y z =
   ( ResolutionCache (CharacterSequence u v w x y z)
   , ResolutionCache (CharacterSequence u v w x y z)
   , ResolutionCache (CharacterSequence u v w x y z)
   )
   

{--}

-- |
-- For every edge in the component:
--
-- * If the edge is *not* a network edge:
--
-- The re-rooting candidate cost for that edge (for a character) is the minimum
-- cost of the cartesian product of the resolutions of the adjacent nodes.
--
-- * If the edge *is* a network edge:
--
-- The re-rooting candidate cost for that edge (for a character) is the minimum
-- cost of the cartesian product of the resolutions of the adjacent nodes minus any
-- resolutions that contain the "incident" network edge contained on the current
-- network edge.

assignOptimalDynamicCharacterRootEdges
  :: ( HasCharacterCost m Word
     , HasCharacterCost i Word
     , HasCharacterCost f Word
     , HasCharacterCost a Word
     , HasCharacterCost d Word
     , HasCharacterWeight m Double
     , HasCharacterWeight i Double
     , HasCharacterWeight f Double
     , HasCharacterWeight a Double
     , HasCharacterWeight d Double
     , Show m
     , Show i
     , Show c
     , Show f
     , Show a
     , Show d
     ) --x, Ord x, Show x)
  => (d -> [d] -> d)
  -> PhylogeneticDAG2 e n m i c f a d
  -> PhylogeneticDAG2 e n m i c f a d
assignOptimalDynamicCharacterRootEdges extensionTransformation (PDAG2 inputDag) = PDAG2 updatedDag
  where

    -- Step 1: Construct a hashmap of all the edges.
    unrootedEdges = rootEdgeReferences <> otherUnrootedEdges
    
    -- Step 2: Create a lazy memoized hashmap of the edge costs for each dynmaic character.

    edgeCostMapping = referenceEdgeMapping

    -- Step 3: For each dynamic character, find the minimal cost edge(s).
    minimalCostSequence = sequenceOfEdgesWithMinimalCost
    
    -- Step 4: Update the dynamic character decoration's cost & add an edge reference.
    updatedDag = inputDag { references = refVec V.// toList modifiedRootRefs }


    -- These are the edges of the DAG which might, not including the current root edge,
    -- which maybe be the optimal root for a given dynamic character.
    otherUnrootedEdges :: [EdgeReference]
    otherUnrootedEdges = foldMapWithKey f refVec
      where
        f i n
          -- Don't consider edges from a root node, as the edges are "artificial" in an unrooted context.
          | i `elem` rootRefs inputDag = []
          | otherwise                  = fmap (\e -> (i,e)) . IM.keys $ childRefs n

    rootEdgeReferences = foldMap f $ rootRefs inputDag
      where
        f i =
          case IM.keys . childRefs $ refVec ! i of
            []    -> []
            [x]   -> []
            x:y:_ -> [(x,y)]

    refVec = references inputDag

{-
    referenceEdgeMapping :: HashMap EdgeReference IncidentEdges
    referenceEdgeMapping = HM.fromList $ foldMap f otherUnrootedEdges <> foldMap g rootEdgeReferences
      where
        f e@(i,j) = [(e, parRefs <> cldRefs)]
          where
            parRefs = ofoldMap (\k -> [(k,i)])           . parentRefs $ refVec ! i
            cldRefs =  foldMap (\k -> [(j,k)]) . IM.keys .  childRefs $ refVec ! j
        g e@(i,j) = [(e, lhsRefs <> rhsRefs)]
          where
            lhsRefs =  foldMap (\k -> [(i,k)]) . IM.keys .  childRefs $ refVec ! i
            rhsRefs = ofoldMap (\k -> [(j,k)]) . IM.keys .  childRefs $ refVec ! j
-}

--    referenceEdgeMapping :: HashMap EdgeReference (ResolutionCache (CharacterSequence u v w x y z))
    referenceEdgeMapping = foldMap f unrootedEdges
      where
        f e@(i,j) = M.singleton e $ localResolutionApplication extensionTransformation lhsContext rhsContext
          where
            lhsContext = (contextualNodeDatum ! i) ! (i,j)
            rhsContext = (contextualNodeDatum ! j) ! (j,i)
    


--    rerootedEdgeContexts :: HashMap EdgeReference (ReRootedEdgeContext u v w x y z)
{-
    rerootedEdgeContexts = foldMap f unrootedEdges
      where
        f e@(i,j)
          | e `elem` rootEdgeReferences = HM.singleton e (getCache i, getRootByChildren e, getCache j) -- identity case
          | otherwise                   = undefined -- memoized reference case
          where
-}            
   
    getCache i = resolutions . nodeDecoration $ refVec ! i
{-
    getRootByChildren (i,j) = resolutions . nodeDecoration . fst . head $ NE.filter findMatchingChildren rootChildren
      where
        rootChildren = (id &&& IM.keys . childRefs) . (refVec !) <$> rootRefs inputDag
        findMatchingChildren (_,is) = i `elem` is && j `elem` is
-}
--    contextualNodeDatum :: Vector (Map EdgeReference (ResolutionCache (CharacterSequence u v w x y z)))
    contextualNodeDatum = V.generate (length refVec) f
      where
        f i
          | i `elem` rootRefs inputDag = undefined
          | otherwise                  = parentEdgeValue <> childEdgeValues
          where
            parentRef
              | candidate `notElem` rootRefs inputDag = candidate
              | otherwise = sibling
              where
                candidate = head . otoList . parentRefs $ refVec ! i
                sibling   = head . filter (/=i) . IM.keys .  childRefs $ refVec ! candidate
                
            kidRefs          = IM.keys .  childRefs $ refVec ! i 
            parentEdgeValue  = M.singleton (i, parentRef) $ getCache i
            childEdgeValues  = foldMap deriveDirectionalDatum [ (x,y) | x <- kidRefs, y <- kidRefs, x /= y ]
            deriveDirectionalDatum (j, k) = M.singleton (i,j) relativeSubtreeDatumValue

              where
                childMemoizedSubstructure    = (contextualNodeDatum ! k        ) ! (        k, i)
                parentalMemoizedSubstructure = (contextualNodeDatum ! parentRef) ! (parentRef, i)
                relativeSubtreeDatumValue    = localResolutionApplication extensionTransformation parentalMemoizedSubstructure childMemoizedSubstructure

    rootRefWLOG  = NE.head $ rootRefs inputDag
    sequenceWLOG = fmap dynamicCharacters . toBlocks . characterSequence . NE.head $ getCache rootRefWLOG

    sequenceOfEdgesWithMinimalCost = foldMapWithKey1 f sequenceWLOG
      where
        f k v = (V.generate (length v) g) :| []
          where
            g i = result
              where
                result@(minimumEdge, minimumCost) = fromJust $ foldlWithKey h Nothing edgeIndexCostMapping
                edgeIndexCostMapping = fmap (fmap ((^. characterCost) . (! i) . dynamicCharacters . (! k) . toBlocks . characterSequence)) edgeCostMapping
                h acc e cs =
                    case acc of
                      Nothing      -> Just (e, c)
                      Just (e',c') ->
                        if   c < c'
                        then Just (e, c)
                        else acc
                  where
                    c = minimum cs


    -- Step 4: Update the dynamic character decoration's cost & add an edge reference.
    modifiedRootRefs = (id &&& modifyRootCosts . (refVec !)) <$> rootRefs inputDag
      where
        modifyRootCosts idxData = idxData { nodeDecoration = nodeDatum }
          where
            node = nodeDecoration idxData
            nodeDatum =
                PNode2
                { resolutions          = fmap f $ resolutions node
                , nodeDecorationDatum2 = nodeDecorationDatum2 node
                }
        f resInfo = resInfo { characterSequence = modifiedSequence  }
          where
            modifiedSequence = fromBlocks . foldMapWithKey1 g . toBlocks $ characterSequence resInfo
        g k charBlock = pure $ charBlock { dynamicCharacters = modifiedDynamicChars }
          where
            modifiedDynamicChars = zipWith h (minimalCostSequence ! k) $ dynamicCharacters charBlock
            h (_edgeVal, costVal) originalDec = originalDec & characterCost .~ costVal
        

localResolutionApplication f x y =
    liftA2 (generateLocalResolutions id2 id2 id2 id2 id2 f) mutalatedChild relativeChildResolutions
  where
    relativeChildResolutions = applySoftwireResolutions
      [ (x, IS.singleton 0)
      , (y, IS.singleton 0)
      ]
    id2 x _ = x
    mutalatedChild = pure
        ResInfo
        { leafSetRepresentation = zeroBits
        , subtreeRepresentation = singletonNewickSerialization 0
        , characterSequence     = characterSequence $ NE.head x
        , localSequenceCost     = 0
        , totalSubtreeCost      = 0
        }
  

{--}


