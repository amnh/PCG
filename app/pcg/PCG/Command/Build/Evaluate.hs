{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module PCG.Command.Build.Evaluate
  ( evaluate
  ) where

import qualified Analysis.Clustering                           as AC
import           Analysis.Parsimony.Additive
import           Analysis.Parsimony.Dynamic.DirectOptimization
import           Analysis.Parsimony.Fitch
import           Analysis.Parsimony.Sankoff
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
import           Bio.Graph.PhylogeneticDAG                     (PostorderContextualData, setDefaultMetadata)
import qualified Bio.Graph.ReferenceDAG                        as DAG
import           Bio.Graph.ReferenceDAG.Internal
import           Bio.Sequence
import           Control.Arrow                                 ((&&&))
import           Control.DeepSeq
import           Control.Evaluation
import           Control.Lens
import           Control.Monad                                 (replicateM)
import           Control.Monad.IO.Class
import           Control.Monad.Reader                          (ReaderT)
import           Control.Parallel.Custom
import           Control.Parallel.Strategies
import           Data.Coerce                                   (coerce)
import           Data.Compact                                  (compact, getCompact)
import           Data.Foldable
import qualified Data.IntMap                                   as IM
import qualified Data.IntSet                                   as IS
import           Data.Key
import           Data.List.NonEmpty                            (NonEmpty (..))
import qualified Data.List.NonEmpty                            as NE
import qualified Data.Map                                      as M
import           Data.NodeLabel
import           Data.Ord                                      (comparing)
import           Data.Semigroup.Foldable
import           Data.Vector                                   (Vector)
import           Data.Vector.NonEmpty                          (unsafeFromVector)
import qualified Data.Vector.NonEmpty                          as NE
import           Immutable.Shuffle                             (shuffleM)
import           PCG.Command.Build



evaluate
  :: BuildCommand
  -> GraphState
  -> SearchState
evaluate (BuildCommand trajectoryCount buildType clusterType) cpctInState =
    case getCompact cpctInState of
      Left  _ -> pure cpctInState
      Right v -> do
        let buildLogic = case buildType of
                           WagnerTree     -> wagnerBuildLogic
                           WheelerNetwork -> networkBuildLogic
                           WheelerForest  -> forestBuildLogic
        let buildMethod = case buildType of
                           WagnerTree     -> naiveWagnerBuild @NE.Vector
                           WheelerNetwork -> naiveNetworkBuild
                           WheelerForest  -> naiveForestBuild
        let cluster = clusterBuildLogic buildMethod
        let clusterLogic = case clusterType of
                             ClusterOption _ 1               -> buildLogic
                             ClusterOption NoCluster       n -> cluster AC.NoCluster       n
                             ClusterOption SingleLinkage   n -> cluster AC.SingleLinkage   n
                             ClusterOption CompleteLinkage n -> cluster AC.CompleteLinkage n
                             ClusterOption UPGMALinkage    n -> cluster AC.UPGMALinkage    n
                             ClusterOption WeightedLinkage n -> cluster AC.WeightedLinkage n
                             ClusterOption WardLinkage     n -> cluster AC.WardLinkage     n
                             ClusterOption KMedians        n -> cluster AC.KMedians        n

        bestNetwork <- clusterLogic v trajectoryCount
        liftIO . compact . Right $ toSolution bestNetwork

  where
    toSolution :: NonEmpty a -> PhylogeneticSolution a
    toSolution = PhylogeneticSolution . pure . PhylogeneticForest


wagnerBuildLogic
  :: PhylogeneticSolution FinalDecorationDAG
  -> Int
  -> EvaluationT (ReaderT GlobalSettings IO) (NonEmpty FinalDecorationDAG)
wagnerBuildLogic = buildLogicMethod naiveWagnerParallelBuild


networkBuildLogic
  :: PhylogeneticSolution FinalDecorationDAG
  -> a
  -> EvaluationT (ReaderT GlobalSettings IO) (NonEmpty FinalDecorationDAG)
networkBuildLogic v _ = do
    let bestTrees = toNonEmpty . NE.head $ phylogeneticForests v
    liftIO $ putStrLn "Beginning network construction."
    pure $ parmap rpar iterativeNetworkBuild bestTrees
--  pure $ fmap iterativeNetworkBuild bestTrees

forestBuildLogic
  :: PhylogeneticSolution FinalDecorationDAG
  -> Int
  -> EvaluationT (ReaderT GlobalSettings IO) (NonEmpty FinalDecorationDAG)
forestBuildLogic _ _ = fail "The BUILD command type 'Forest' is not yet implemented!"


clusterBuildLogic
   :: (MetadataSequence FinalMetadata -> NE.Vector FinalCharacterNode -> FinalDecorationDAG)
  -> AC.ClusterOptions
  -> Int
  -> PhylogeneticSolution FinalDecorationDAG
  -> Int
  -> EvaluationT (ReaderT GlobalSettings IO) (NonEmpty FinalDecorationDAG)
clusterBuildLogic buildMethod clusterOption numberOfClusters
  = buildLogicMethod (clusterParallelBuild buildMethod clusterOption numberOfClusters)


buildLogicMethod
  :: (  MetadataSequence FinalMetadata
     -> NonEmpty (NE.Vector FinalCharacterNode)
     -> NonEmpty FinalDecorationDAG
     )
  -> PhylogeneticSolution FinalDecorationDAG
  -> Int
  -> EvaluationT (ReaderT GlobalSettings IO) (NonEmpty FinalDecorationDAG)
buildLogicMethod parallelBuildLogic v count =
    let
      leaves = fromLeafSet $ v ^. leafSet
    in
      if null leaves
        then fail "There are no nodes with which to build a tree."
        else
          if count < 1
          then fail "A non-positive number was supplied to the number of BUILD trajectories."
          else
            let
              (PDAG2 _ metaSeq) = NE.head . toNonEmpty . NE.head $ phylogeneticForests v
              leavesNE :: NE.Vector FinalCharacterNode
              leavesNE = unsafeFromVector leaves

            in
              do
                    trajectories :: NonEmpty (NE.Vector FinalCharacterNode)
                        <- case count of
                             1 -> pure $ leavesNE :| []
                             n -> liftIO . convert  $ replicateM n (shuffleM leaves)
                    pure $ parallelBuildLogic metaSeq trajectories
  where
    convert :: IO [Vector a] -> IO (NonEmpty (NE.Vector a))
    convert = fmap (NE.fromList . fmap unsafeFromVector)


naiveWagnerParallelBuild
  :: ( Foldable1 f
     , Traversable t
     )
  => MetadataSequence m
  -> t (f FinalCharacterNode)
  -> t FinalDecorationDAG
naiveWagnerParallelBuild m = parmap rpar (naiveWagnerBuild m)


clusterParallelBuild
  :: (Traversable t)
  => (MetadataSequence FinalMetadata -> NE.Vector FinalCharacterNode -> FinalDecorationDAG)
  -> AC.ClusterOptions
  -> Int
  -> MetadataSequence FinalMetadata
  -> t (NE.Vector FinalCharacterNode)
  -> t FinalDecorationDAG
clusterParallelBuild buildMethod clusterOptions numberOfClusters m
  = parmap rpar (clusterBuildMethod buildMethod clusterOptions numberOfClusters m)


naiveWagnerBuild
  :: Foldable1 f
  => MetadataSequence m
  -> f FinalCharacterNode
  -> FinalDecorationDAG
naiveWagnerBuild metaSeq ns =
   case toNonEmpty ns of
      x:|[]  -> fromRefDAG $ DAG.fromList
                  [ ( mempty        , wipeNode False x, mempty )
                  ]
      x:|[y] -> fromRefDAG $ DAG.fromList
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

naiveNetworkBuild
  :: Foldable1 f
  => MetadataSequence m
  -> f FinalCharacterNode
  -> FinalDecorationDAG
naiveNetworkBuild = error "Naive network build not yet implemented!"


naiveForestBuild
  :: Foldable1 f
  => MetadataSequence m
  -> f FinalCharacterNode
  -> FinalDecorationDAG
naiveForestBuild = error "Naive forest build not yet implemented!"


clusterBuildMethod
  :: (MetadataSequence m -> NE.Vector FinalCharacterNode -> FinalDecorationDAG)
  -> AC.ClusterOptions
  -> Int
  -> MetadataSequence m
  -> NE.Vector FinalCharacterNode
  -> FinalDecorationDAG
clusterBuildMethod buildMethod option numberOfClusters meta leafSetV
    = parallelClusterMethod buildMethod meta clusters
  where
    leafSetId :: LeafSet (DecoratedCharacterNode Identity)
    leafSetId = coerce leafSetV

    clusters :: NE.Vector (NE.Vector (DecoratedCharacterNode Identity))
    clusters = AC.clusterIntoGroups meta leafSetId option numberOfClusters


parallelClusterMethod
  :: (MetadataSequence m -> NE.Vector FinalCharacterNode -> FinalDecorationDAG)
  -> MetadataSequence m
  -> NE.Vector (NE.Vector (DecoratedCharacterNode Identity))
  -> FinalDecorationDAG
parallelClusterMethod buildMethod meta clusters =
  let
    finalDecClusters :: NE.Vector (NE.Vector FinalCharacterNode)
    finalDecClusters = coerce clusters

    clusterTrees :: NE.Vector FinalDecorationDAG
    clusterTrees = parmap rpar (buildMethod meta) finalDecClusters
  in
    subTreeMethod buildMethod meta clusterTrees


subTreeMethod
  :: (MetadataSequence m -> NE.Vector FinalCharacterNode -> FinalDecorationDAG)
  -> MetadataSequence m
  -> NE.Vector FinalDecorationDAG
  -> FinalDecorationDAG
subTreeMethod buildMethod meta subTrees =
  let
    p :: (M.Map NodeLabel FinalDecorationDAG, [NodeLabel])
    p =
      foldMap (\t -> (M.singleton (getRootName t) t, pure (getRootName t))) subTrees

    (subTreeDict, rootNodeLabels) = p
    rootCharNodes :: NE.Vector FinalCharacterNode
    rootCharNodes = fmap getRootDecoration subTrees

    rootNodeTree :: FinalDecorationDAG
    rootNodeTree = buildMethod meta rootCharNodes

    namedContext :: M.Map NodeLabel Int
    namedContext = rootNodeTree `getNamedContext` rootNodeLabels
  in
    substituteDAGs namedContext subTreeDict rootNodeTree
  where
    getRootDecoration :: FinalDecorationDAG -> FinalCharacterNode
    {-# INLINE getRootDecoration #-}
    getRootDecoration dag =
      let
        refDAG  = dag ^. _phylogeneticForest
        rootInd = NE.head (refDAG ^. _rootRefs)
      in
        (refDAG ^. _references) ! rootInd ^. _nodeDecoration

    getRootName :: FinalDecorationDAG -> NodeLabel
    {-# INLINE getRootName #-}
    getRootName dag = getRootDecoration dag ^. _nodeDecorationDatum


iterativeBuild :: FinalDecorationDAG -> FinalCharacterNode -> FinalDecorationDAG
iterativeBuild currentTree@(PDAG2 _ metaSeq) nextLeaf = nextTree
  where
    (PDAG2 dag _) = wipeScoring currentTree
    edgeSet       = NE.fromList . toList $ referenceEdgeSet dag
    resetDAG      = resetEdgeData $ resetMetadata dag

    tryEdge :: (Int, Int) -> Double
    tryEdge (i,j) = delta
      where
        delta   = sequenceCost metaSeq compSeq - sequenceCost metaSeq edgeSeq - sequenceCost metaSeq thisSeq
        edgeSeq = snd $ childRefs (references dag ! i) ! j
        thisSeq = characterSequence . NE.head $ resolutions nextLeaf
        compSeq ::
          CharacterSequence
            (ContinuousPostorderDecoration ContinuousCharacter)
            (FitchOptimizationDecoration       StaticCharacter)
            (AdditivePostorderDecoration       StaticCharacter)
            (SankoffOptimizationDecoration     StaticCharacter)
            (SankoffOptimizationDecoration     StaticCharacter)
            (DynamicDecorationDirectOptimizationPostorderResult DynamicCharacter)
        compSeq = hexZipMeta
                    (const additivePostorderPairwise)
                    (const    fitchPostorderPairwise)
                    (const additivePostorderPairwise)
                    sankoffPostorderPairwise
                    sankoffPostorderPairwise
                    adaptiveDirectOptimizationPostorderPairwise
                    metaSeq $ hexZip edgeSeq thisSeq

    nextEdge :: (Int, Int)
    nextEdge    = fst . minimumBy (comparing snd) $ parmap rpar (id &&& tryEdge) edgeSet

    nextTree    = performDecoration . (`PDAG2` metaSeq) . invadeEdge resetDAG deriveInternalNode (wipeNode False nextLeaf) $ nextEdge

--    getCost (PDAG2 v _) = dagCost $ graphData v

    deriveInternalNode parentDatum oldChildDatum _newChildDatum =
        PNode2 (resolutions oldChildDatum) (nodeDecorationDatum2 parentDatum)

    adaptiveDirectOptimizationPostorderPairwise meta = directOptimizationPostorderPairwise pairwiseAlignmentFunction
      where
        pairwiseAlignmentFunction = selectDynamicMetric meta


iterativeNetworkBuild :: FinalDecorationDAG -> FinalDecorationDAG
iterativeNetworkBuild currentNetwork@(PDAG2 inputDag metaSeq) =
    case toList $ DAG.candidateNetworkEdges inputDag of
      []   -> currentNetwork
      x:xs ->
        -- DO NOT use rdeepseq! Prefer rseq!
        -- When trying candidate edges, we can construct graphs for which a
        -- pre-order traversal is not logically possible. These graphs will
        -- necissarily result in an infinite cost. So long as we lazily compute
        -- the cost, the minimization routine will discard the incoherent,
        -- infinite-cost candidates and we won't run into interesting runtime problems.
        let !edgesToTry = x:|xs
            (minNewCost, !bestNewNetwork) = minimumBy (comparing fst)
                                          . parmap (rparWith rseq) (getCost &&& id)
                                          $ tryNetworkEdge <$> edgesToTry
        in  if   getCost currentNetwork <= minNewCost
            then currentNetwork
            else iterativeNetworkBuild bestNewNetwork
  where
    (PDAG2 dag _) = force $ wipeScoring currentNetwork

    tryNetworkEdge :: ((Int, Int), (Int, Int)) -> FinalDecorationDAG
    tryNetworkEdge = performDecoration . (`PDAG2` metaSeq) . connectEdge'

    getCost (PDAG2 v _) = dagCost $ graphData v

    resetDAG = resetEdgeData $ resetMetadata dag

    connectEdge'
      = uncurry (connectEdge resetDAG deriveOriginEdgeNode deriveTargetEdgeNode)

    deriveOriginEdgeNode parentDatum oldChildDatum _newChildDatum =
        PNode2 (resolutions oldChildDatum) (nodeDecorationDatum2 parentDatum)

    deriveTargetEdgeNode parentDatum oldChildDatum =
        PNode2 (resolutions oldChildDatum) (nodeDecorationDatum2 parentDatum)


resetMetadata :: ReferenceDAG d e n -> ReferenceDAG (PostorderContextualData t) e n
resetMetadata ref = ref & _graphData %~ setDefaultMetadata

resetEdgeData :: ReferenceDAG d (e,a) n -> ReferenceDAG d e n
resetEdgeData ref = ref & _references . (mapped . _childRefs . mapped) %~ fst
