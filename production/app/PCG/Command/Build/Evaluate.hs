{-# LANGUAGE BangPatterns, FlexibleContexts, TypeFamilies #-}

module PCG.Command.Build.Evaluate
  ( evaluate
  ) where

import           Analysis.Scoring
import           Bio.Character
import           Bio.Character.Decoration.Additive
import           Bio.Character.Decoration.Continuous
--import           Bio.Character.Decoration.Discrete
import           Bio.Character.Decoration.Dynamic
import           Bio.Character.Decoration.Fitch
import           Bio.Character.Decoration.Metric 
import           Bio.Graph
import           Bio.Graph.LeafSet
import           Bio.Graph.Node
--import           Bio.Graph.PhylogeneticDAG
import           Bio.Graph.ReferenceDAG
import           Bio.Sequence
--import           Control.Evaluation
import           Control.Lens
import           Control.Monad                (replicateM)
import           Control.Monad.IO.Class
--import           Control.Monad.Trans.Either
--import           Control.Parallel.Strategies
--import           Control.Parallel.Custom
--import           Data.Alphabet   --    hiding (AmbiguityGroup)
--import           Data.Alphabet.IUPAC
--import           Data.Bifunctor               (bimap,first)
--import           Data.Char                    (isLower,toLower,isUpper,toUpper)
--import           Data.EdgeLength
--import           Data.Either.Custom
import           Data.Foldable
--import           Data.Functor
--import           Data.Hashable
--import           Data.Key
--import           Data.List                    (intercalate)
import           Data.List.NonEmpty           (NonEmpty(..))
import qualified Data.List.NonEmpty    as NE
--import           Data.List.Utility            (subsetOf)
--import           Data.Map                     (Map,assocs,insert,union)
--import qualified Data.Map              as M
--import           Data.Maybe                   (fromMaybe)
import           Data.Ord                     (comparing)
import           Data.Semigroup.Foldable
--import           Data.TCM                     (TCMDiagnosis(..), TCMStructure(..), diagnoseTcm)
--import qualified Data.TCM              as TCM
--import           Data.Text.IO                 (readFile)
--import           Data.Vector                  (Vector)
--import qualified Data.Vector           as V   (zipWith)
--import           Data.Void
import           PCG.Command.Build
import           PCG.Syntax                   (Command(..))
import           Prelude             hiding   (lookup, readFile)
import           System.Random.Shuffle
--import Debug.Trace (trace)


type DatNode =
  PhylogeneticNode2
    (CharacterSequence
      (ContinuousOptimizationDecoration ContinuousChar)
      (FitchOptimizationDecoration   StaticCharacter)
      (AdditiveOptimizationDecoration StaticCharacter)
      (SankoffOptimizationDecoration StaticCharacter)
      (SankoffOptimizationDecoration StaticCharacter)
      (DynamicDecorationDirectOptimization DynamicChar)
    )
    (Maybe String)


--evaluate :: Command -> EvaluationT IO a -> EvaluationT IO (Either TopologicalResult DecoratedCharacterResult)
--evaluate :: Command -> EvaluationT IO a -> EvaluationT IO (Either TopologicalResult CharacterResult)
evaluate
{-
  :: ( DiscreteCharacterMetadata u
     , DiscreteCharacterMetadata w
     , DiscreteCharacterDecoration v StaticCharacter
     , DiscreteCharacterDecoration x StaticCharacter
     , DiscreteCharacterDecoration y StaticCharacter
     , Eq z
     , Hashable z
--     , HasLeafSet a (LeafSet (PhylogeneticNode2 (CharacterSequence u v w x y z) (Maybe String)))
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
  => -}
  :: Command
--  -> EvaluationT IO (Either TopologicalResult (PhylogeneticSolution (PhylogeneticDAG2 EdgeLength (Maybe String) u v w x y z)))
  -> SearchState
  -> SearchState
-- EvaluationT IO (Either TopologicalResult CharacterResult)
-- evaluate (READ fileSpecs) _old | trace ("Evaluated called: " <> show fileSpecs) False = undefined
-- evaluate (READ fileSpecs) _old | trace "STARTING READ COMMAND" False = undefined
evaluate (BUILD (BuildCommand trajectoryCount buildType)) oldState = do
    x <- oldState
    
    case x of
      Left  e -> pure $ Left e
      Right v ->
        case toList $ v ^. leafSet of
          []   -> fail "There are no nodes with which to build a tree."
          y:ys ->
            if trajectoryCount < 1
            then fail "A non-positive number was supplied to the number of BUILD trajectories."
            else do
                trajectories <- case trajectoryCount of
                                1 -> pure $ (y:|ys):|[]
                                n -> liftIO . fmap (NE.fromList . fmap NE.fromList) $ replicateM n (shuffleM (y:ys))
                let !bestTrees = naiveWagnerParallelBuild trajectories
                bestNetwork  <- case buildType of
                                   WagnerTree     -> pure $ bestTrees
                                   WheelerNetwork -> do liftIO $ putStrLn "Beginning network construction."
--                                                        pure $ parmap rpar iterativeNetworkBuild bestTrees
                                                        pure $ fmap iterativeNetworkBuild bestTrees
                                   WheelerForest  -> fail "The BUILD command type 'Forest' is not yet implemented!"
                let bestSolution = Right $ toSolution bestNetwork
                pure bestSolution
  where
    toSolution = PhylogeneticSolution . pure . PhylogeneticForest

evaluate _ _ = fail "Invalid BUILD command binding"


naiveWagnerParallelBuild
  :: ( Foldable1 f
     , Foldable1 t
     , Traversable t
     )
  => t (f DatNode) -- (PhylogeneticNode2 (CharacterSequence u v w x y z) (Maybe String))
  -> t FinalDecorationDAG
--naiveWagnerParallelBuild = parmap rpar naiveWagnerBuild
naiveWagnerParallelBuild = fmap naiveWagnerBuild


naiveWagnerBuild
{-  :: ( DiscreteCharacterMetadata u
     , DiscreteCharacterMetadata w
     , DiscreteCharacterDecoration v StaticCharacter
     , DiscreteCharacterDecoration x StaticCharacter
     , DiscreteCharacterDecoration y StaticCharacter
     , Eq z
     , Foldable f
     , Hashable z
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
  => -}
  :: Foldable1 f
  => (f DatNode) -- (PhylogeneticNode2 (CharacterSequence u v w x y z) (Maybe String))
  -> FinalDecorationDAG
naiveWagnerBuild ns =
   case toNonEmpty ns of
      x:|[]   -> fromRefDAG $ unfoldDAG (\_ -> ([], wipeNode False x, [])) ()
      x:|[y]  ->
          let f e = case e of
                      0 -> ([]           , wipeNode True  x, [(mempty, 1), (mempty, 2)])
                      1 -> ([(mempty, 0)], wipeNode False x, [])
                      _ -> ([(mempty, 0)], wipeNode False y, [])
          in  fromRefDAG $ unfoldDAG f (0 :: Int)
      x:|(y:z:xs) ->
          let initTree = fromRefDAG $ unfoldDAG f (0 :: Int)
              f e = case e of
                      0 -> ([]           , wipeNode True  x, [(mempty, 1), (mempty, 4)])
                      1 -> ([(mempty, 0)], wipeNode True  x, [(mempty, 2), (mempty, 3)])
                      2 -> ([(mempty, 1)], wipeNode False x, [])
                      3 -> ([(mempty, 1)], wipeNode False y, [])
                      _ -> ([(mempty, 0)], wipeNode False z, [])
          in  iterativeBuild initTree xs

  where
    fromRefDAG = performDecoration . PDAG2 . defaultMetadata


iterativeBuild
  ::
{-    ( DiscreteCharacterMetadata u
     , DiscreteCharacterMetadata w
     , DiscreteCharacterDecoration v StaticCharacter
     , DiscreteCharacterDecoration x StaticCharacter
     , DiscreteCharacterDecoration y StaticCharacter
     , Eq z
     , Hashable z
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
  => -}FinalDecorationDAG
--  -> [PhylogeneticNode2 (CharacterSequence (Maybe u) (Maybe v) (Maybe w) (Maybe x) (Maybe y) (Maybe z)) (Maybe String)]
--  -> [PhylogeneticNode2 (CharacterSequence u v w x y z) (Maybe String)]
  -> [DatNode]
  -> FinalDecorationDAG
iterativeBuild currentTree [] = currentTree
--iterativeBuild currentTree (nextLeaf:_) | trace (show $ nodeDecorationDatum2 nextLeaf) False = undefined
iterativeBuild currentTree (nextLeaf:remainingLeaves) = iterativeBuild nextTree remainingLeaves
  where
    (PDAG2 dag) = wipeScoring currentTree
    edgeSet     = NE.fromList . toList $ referenceEdgeSet dag

    tryEdge :: (Int, Int) -> FinalDecorationDAG
    tryEdge     = performDecoration . PDAG2 . invadeEdge (defaultMetadata dag) deriveInternalNode (wipeNode False nextLeaf)
    nextTree    = minimumBy (comparing getCost) $ fmap tryEdge edgeSet

    getCost (PDAG2 v) = dagCost $ graphData v

    deriveInternalNode parentDatum oldChildDatum _newChildDatum =
        PNode2 (resolutions oldChildDatum) (nodeDecorationDatum2 parentDatum)
        

iterativeNetworkBuild
  ::
{-    ( DiscreteCharacterMetadata u
     , DiscreteCharacterMetadata w
     , DiscreteCharacterDecoration v StaticCharacter
     , DiscreteCharacterDecoration x StaticCharacter
     , DiscreteCharacterDecoration y StaticCharacter
     , Eq z
     , Hashable z
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
  => -}FinalDecorationDAG
--  -> [PhylogeneticNode2 (CharacterSequence (Maybe u) (Maybe v) (Maybe w) (Maybe x) (Maybe y) (Maybe z)) (Maybe String)]
--  -> [PhylogeneticNode2 (CharacterSequence u v w x y z) (Maybe String)]
  -> FinalDecorationDAG
iterativeNetworkBuild currentNetwork@(PDAG2 inputDag) = 
    case toList $ candidateNetworkEdges inputDag of
      []   -> currentNetwork
      x:xs ->
        let edgesToTry     = x:|xs
            bestNewNetwork = minimumBy (comparing getCost) $ fmap tryNetworkEdge edgesToTry
        in  if getCost currentNetwork <= getCost bestNewNetwork
            then currentNetwork
            else iterativeNetworkBuild bestNewNetwork
  where
    (PDAG2 dag) = wipeScoring currentNetwork

    tryNetworkEdge :: ((Int, Int), (Int, Int)) -> FinalDecorationDAG
    tryNetworkEdge = performDecoration . PDAG2 . connectEdge'

    getCost (PDAG2 v) = dagCost $ graphData v

    connectEdge' = uncurry (connectEdge (defaultMetadata dag) deriveOriginEdgeNode deriveTargetEdgeNode)

    deriveOriginEdgeNode parentDatum oldChildDatum _newChildDatum =
        PNode2 (resolutions oldChildDatum) (nodeDecorationDatum2 parentDatum)

    deriveTargetEdgeNode parentDatum oldChildDatum =
        PNode2 (resolutions oldChildDatum) (nodeDecorationDatum2 parentDatum)
        


