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
import           Bio.Character.Decoration.Additive
import           Bio.Character.Decoration.Continuous
import           Bio.Character.Decoration.Discrete
import           Bio.Character.Decoration.Dynamic
import           Bio.Character.Decoration.Fitch
import           Bio.Character.Decoration.Metric 
import           Bio.Sequence
import           Bio.Sequence.Block (CharacterBlock)
import           Bio.PhyloGraphPrime
import           Bio.PhyloGraphPrime.Node
import           Bio.PhyloGraphPrime.ReferenceDAG.Internal
import           Control.Arrow            ((&&&))
import           Control.Applicative      (liftA2)
import           Control.Evaluation
import           Control.Monad.State.Lazy
import           Data.Bits
--import           Data.DuplicateSet
--import qualified Data.DuplicateSet  as DS
import           Data.Foldable
import           Data.Hashable
import           Data.Hashable.Memoize
import qualified Data.IntMap        as IM
import           Data.IntSet              (IntSet)
import           Data.Key
import           Data.List.NonEmpty       (NonEmpty( (:|) ))
import qualified Data.List.NonEmpty as NE
import           Data.List.Utility
--import           Data.Monoid
import           Data.MonoTraversable
import           Data.Semigroup
import           Data.Semigroup.Foldable
--import           Data.Vector              (Vector)
import qualified Data.Vector        as V

-- import Debug.Trace


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

type CharacterDAG = PhylogeneticDAG2
                        (Maybe Double)
                        (Maybe String)
                        UnifiedDiscreteCharacter
                        UnifiedDiscreteCharacter
                        UnifiedContinuousCharacter
                        UnifiedDiscreteCharacter
                        UnifiedDiscreteCharacter
                        UnifiedDynamicCharacter


type DecoratedCharacterResult = PhylogeneticSolution InitialDecorationDAG


type InitialDecorationDAG = PhylogeneticDAG2
                                (Maybe Double)
                                (Maybe String)
                                (SankoffOptimizationDecoration  StaticCharacter)
                                (SankoffOptimizationDecoration  StaticCharacter)
                                UnifiedContinuousCharacter --(ContinuousOptimizationDecoration ContinuousChar)
                                (FitchOptimizationDecoration    StaticCharacter)
                                (AdditiveOptimizationDecoration StaticCharacter)
                                -- (DynamicDecorationDirectOptimization DynamicChar) -- UnifiedDynamicCharacter
                                (DynamicDecorationDirectOptimizationPostOrderResult DynamicChar)


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


instance ( Show e
         , Show n
         , Show m
         , Show i
         , Show c
         , Show f
         , Show a
         , Show d
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
         ) => Show (PhylogeneticDAG2 e n m i c f a d) where

    show (PDAG2 dag) = show dag <> "\n" <> foldMapWithKey f dag
      where
--        f i (PNode2 n sek) = mconcat [ "Node {", show i, "}:\n\n", unlines [show n, show sek], "\n\n" ] 
        f i n = mconcat [ "Node {", show i, "}:\n\n", show n ]


riefiedSolution :: PhylogeneticSolution UnRiefiedCharacterDAG -> CharacterResult
riefiedSolution  = PhylogeneticSolution . fmap (fmap riefiedToCharacterDAG) . phylogeneticForests


riefiedToCharacterDAG :: UnRiefiedCharacterDAG -> CharacterDAG
riefiedToCharacterDAG (PDAG dag) = PDAG2 $
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
            res = pure $
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
              { resolutions          = liftA2 generateLocalResolutions datumResolutions childResolutions
              , nodeDecorationDatum2 = nodeDecorationDatum2 $ nodeDecoration node
              }
          where
            node             = references dag ! i
            childIndices     = IM.keys $ childRefs node
            datumResolutions = resolutions $ nodeDecoration node
            
--            childResolutions :: NonEmpty [a]
            childResolutions = applySoftwireResolutions $ extractResolutionContext <$> childIndices
            extractResolutionContext = (resolutions . (memo !) &&& parentRefs . (references dag !))

            --        g :: ResolutionInformation s -> [ResolutionInformation s] -> ResolutionInformation s
            generateLocalResolutions parentalResolutionContext childResolutionContext =
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

                transformation pSeq cSeqs = hexZipWith f1 f2 f3 f4 f5 f6' pSeq transposition
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

