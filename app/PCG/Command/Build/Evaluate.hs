{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module PCG.Command.Build.Evaluate
  ( evaluate
  ) where

import           Analysis.Scoring
import           Bio.Character
import           Bio.Character.Decoration.Additive
import           Bio.Character.Decoration.Continuous
import           Bio.Character.Decoration.Dynamic
import           Bio.Character.Decoration.Fitch
import           Bio.Character.Decoration.Metric
import           Bio.Graph
import           Bio.Graph.LeafSet
import           Bio.Graph.Node
import qualified Bio.Graph.ReferenceDAG              as DAG
import           Bio.Graph.ReferenceDAG.Internal
import           Bio.Sequence
import           Bio.Sequence.Metadata
import           Control.Arrow                       ((&&&))
import           Control.DeepSeq
import           Control.Lens
import           Control.Monad                       (replicateM)
import           Control.Monad.IO.Class
import           Control.Parallel.Custom
import           Data.Compact (compact, getCompact)
import           Control.Parallel.Strategies
import           Data.Foldable
import qualified Data.IntMap                         as IM
import qualified Data.IntSet                         as IS
import           Data.List.NonEmpty                  (NonEmpty (..))
import qualified Data.List.NonEmpty                  as NE
import           Data.NodeLabel
import           Data.Ord                            (comparing)
import           Data.Semigroup.Foldable
import           PCG.Command.Build
import           PCG.Syntax                          (Command (..))
import           System.Random.Shuffle


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
    NodeLabel


evaluate
  :: Command
  -> GraphState
  -> SearchState
-- evaluate (READ fileSpecs) _old | trace ("Evaluated called: " <> show fileSpecs) False = undefined
-- evaluate (READ fileSpecs) _old | trace "STARTING READ COMMAND" False = undefined
evaluate (BUILD (BuildCommand trajectoryCount buildType)) cpctInState = do
    let inState = getCompact cpctInState
    case inState of
      Left  _ -> pure $ cpctInState
      Right v ->
        case toList $ v ^. leafSet of
          []   -> fail "There are no nodes with which to build a tree."
          y:ys ->
            if trajectoryCount < 1
            then fail "A non-positive number was supplied to the number of BUILD trajectories."
            else let (PDAG2 _ m) = NE.head . toNonEmpty . NE.head $ phylogeneticForests v
                 in  do
                     trajectories <- case trajectoryCount of
                                       1 -> pure $ (y:|ys):|[]
                                       n -> liftIO . fmap (NE.fromList . fmap NE.fromList) $ replicateM n (shuffleM (y:ys))
                     let !bestTrees = naiveWagnerParallelBuild m trajectories
                     bestNetwork  <- case buildType of
                                       WagnerTree     -> pure bestTrees
                                       WheelerNetwork -> do liftIO $ putStrLn "Beginning network construction."
                                                            pure $ parmap rpar iterativeNetworkBuild bestTrees
--                                                            pure $ fmap iterativeNetworkBuild bestTrees
                                       WheelerForest  -> fail "The BUILD command type 'Forest' is not yet implemented!"
                     bestSolution <- liftIO $  compact . Right $ toSolution bestNetwork
                     pure bestSolution
  where
    toSolution :: NonEmpty a -> PhylogeneticSolution a
    toSolution = PhylogeneticSolution . pure . PhylogeneticForest

evaluate _ _ = fail "Invalid BUILD command binding"


naiveWagnerParallelBuild
  :: ( Foldable1 f
     , Traversable t
     )
  => MetadataSequence m --(TraversalTopology, Double, Double, Double, Data.Vector.Vector (NonEmpty TraversalFocusEdge))
  -> t (f DatNode)
  -> t FinalDecorationDAG
naiveWagnerParallelBuild m = parmap rpar (naiveWagnerBuild m)


naiveWagnerBuild
  :: Foldable1 f
  => MetadataSequence m
  -> f DatNode
  -> FinalDecorationDAG
naiveWagnerBuild metaSeq ns =
   case toNonEmpty ns of
      x:|[]   -> fromRefDAG $ DAG.fromList
                   [ ( mempty        , wipeNode False x, mempty )
                   ]
      x:|[y]  -> fromRefDAG $ DAG.fromList
                   [ ( mempty        , wipeNode True  x, IM.fromList [(1,mempty), (2,mempty)] )
                   , ( IS.singleton 0, wipeNode False x, mempty )
                   , ( IS.singleton 0, wipeNode False y, mempty )
                   ]
      x:|(y:z:xs) ->
          let initTree = fromRefDAG $ DAG.fromList
                   [ ( mempty        , wipeNode True  x, IM.fromList [(1,mempty), (4,mempty)] )
                   , ( IS.singleton 0, wipeNode True  x, IM.fromList [(2,mempty), (3,mempty)] )
                   , ( IS.singleton 1, wipeNode False x, mempty )
                   , ( IS.singleton 1, wipeNode False y, mempty )
                   , ( IS.singleton 0, wipeNode False z, mempty )
                   ]
--          in  iterativeBuild (trace ("Leaves remaining: " <> show (length xs) <> "\n"<> show initTree) initTree) xs
          in  foldl' iterativeBuild initTree xs

  where
    fromRefDAG = performDecoration . (`PDAG2`  metaSeq) . resetMetadata

iterativeBuild
  :: FinalDecorationDAG
  -> DatNode
  -> FinalDecorationDAG
iterativeBuild currentTree@(PDAG2 _ metaSeq) nextLeaf = nextTree
  where
    (PDAG2 dag _) = wipeScoring currentTree
    edgeSet     = NE.fromList . toList $ referenceEdgeSet dag

    tryEdge :: (Int, Int) -> FinalDecorationDAG
    tryEdge     = performDecoration . (`PDAG2` metaSeq) . invadeEdge (resetMetadata dag) deriveInternalNode (wipeNode False nextLeaf)
    nextTree    = minimumBy (comparing getCost) $ parmap rpar tryEdge edgeSet

    getCost (PDAG2 v _) = dagCost $ graphData v

    deriveInternalNode parentDatum oldChildDatum _newChildDatum =
        PNode2 (resolutions oldChildDatum) (nodeDecorationDatum2 parentDatum)


iterativeNetworkBuild
  :: FinalDecorationDAG
  -> FinalDecorationDAG

iterativeNetworkBuild currentNetwork@(PDAG2 inputDag metaSeq) =
    case toList $ candidateNetworkEdges inputDag of
      []   -> currentNetwork
      x:xs ->
        let !edgesToTry = x:|xs
            (minNewCost, !bestNewNetwork) = minimumBy (comparing fst)
                                          . parmap (rparWith rdeepseq) (getCost &&& id)
                                          $ tryNetworkEdge <$> edgesToTry
        in  if   getCost currentNetwork <= minNewCost
            then currentNetwork
            else iterativeNetworkBuild bestNewNetwork
  where
    (PDAG2 dag _) = force $ wipeScoring currentNetwork

    tryNetworkEdge :: ((Int, Int), (Int, Int)) -> FinalDecorationDAG
    tryNetworkEdge = performDecoration . (`PDAG2` metaSeq) . connectEdge'

    getCost (PDAG2 v _) = dagCost $ graphData v

    connectEdge' = uncurry (connectEdge (resetMetadata dag) deriveOriginEdgeNode deriveTargetEdgeNode)

    deriveOriginEdgeNode parentDatum oldChildDatum _newChildDatum =
        PNode2 (resolutions oldChildDatum) (nodeDecorationDatum2 parentDatum)

    deriveTargetEdgeNode parentDatum oldChildDatum =
        PNode2 (resolutions oldChildDatum) (nodeDecorationDatum2 parentDatum)


resetMetadata :: (Monoid a, Monoid b) => ReferenceDAG d e n -> ReferenceDAG (a, b, Maybe c) e n
resetMetadata =
    RefDAG
      <$> references
      <*> rootRefs
      <*> ((mempty, mempty, Nothing) <$) . graphData
