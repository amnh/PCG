{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
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
import           Control.Arrow                                 (first, second, (&&&))
import           Control.DeepSeq
import           Control.Evaluation
import           Control.Lens                                  hiding (snoc, _head)
import           Control.Monad.IO.Class
import           Control.Monad.State.Strict
import           Control.Parallel.Custom
import           Control.Parallel.Strategies
import           Data.Coerce                                   (coerce)
import           Data.Compact                                  (compact, getCompact)
import           Data.Foldable
import           Data.GraphViz.Printing
import qualified Data.IntMap                                   as IM
import qualified Data.IntSet                                   as IS
import           Data.Key
import           Data.List.NonEmpty                            (NonEmpty (..))
import qualified Data.List.NonEmpty                            as NE
import           Data.List.Utility                             (HasHead (_head))
import qualified Data.Map                                      as M
import           Data.Monoid
import           Data.NodeLabel
import           Data.Ord                                      (comparing)
import           Data.Semigroup.Foldable
import           Data.ShortText.Custom
import           Data.Text.Lazy                                (unpack)
import           Data.Vector                                   (Vector, snoc)
import           Data.Vector.NonEmpty                          (unsafeFromVector)
import qualified Data.Vector.NonEmpty                          as NE
import           Data.Word
import           Immutable.Shuffle                             (shuffleM)
import           Numeric.Extended.Real                         (ExtendedReal)
import           PCG.Command.Build

-- For adhoc logging. Obviously unsafe, TODO: remove later
import           Data.IORef
import           System.IO.Unsafe


type BuildType m =
     MetadataSequence m
  -> NE.Vector FinalCharacterNode
  -> FinalDecorationDAG


type ParallelBuildType m
  =  MetadataSequence m
  -> NE.NonEmpty (NE.Vector FinalCharacterNode)
  -> NE.NonEmpty FinalDecorationDAG


evaluate
  :: BuildCommand
  -> GraphState
  -> SearchState
evaluate (BuildCommand trajectoryCount buildType clusterType) cpctInState =
    case getCompact cpctInState of
      Left  _ -> pure cpctInState
      Right v -> do
        let isInitialBuild =
              let
                leafNumber = length . fromLeafSet $ v ^. leafSet
                rootNumber = extractNumberOfRoots v
              in
                (leafNumber == rootNumber)
        let buildLogic =
              case buildType of
                WagnerTree     -> wagnerBuildLogic
                WheelerNetwork -> networkBuildLogic isInitialBuild
                WheelerForest  -> forestBuildLogic
        let buildMethod =
              case buildType of
                WagnerTree     -> naiveWagnerBuild @NE.Vector
                WheelerNetwork -> naiveNetworkBuild
                WheelerForest  -> naiveForestBuild
        let
          cluster
            :: AC.ClusterOptions
            -> PhylogeneticSolution FinalDecorationDAG
            -> Int
            -> EvaluationT GlobalSettings IO (NonEmpty FinalDecorationDAG)
          cluster = clusterBuildLogic buildMethod

        let clusterLogic =
              case clusterType of
                ClusterOption _ (ClusterGroup 1) -> buildLogic
                ClusterOption _ _                -> cluster clusterOptions
        if numberOfClusterCheck clusterType then
          fail "A non-positive number was supplied to the number of clusters."
        else
          do
            bestNetwork <- clusterLogic v trajectoryCount
            liftIO . compact . Right $ toSolution bestNetwork

  where

    extractNumberOfRoots :: PhylogeneticSolution FinalDecorationDAG -> Int
    extractNumberOfRoots =
        getSum
      . foldMap1 (Sum . length . view (_phylogeneticForest . _rootRefs))
      . extractPhylogeneticForest
    numberOfClusterCheck :: ClusterOption -> Bool
    numberOfClusterCheck (ClusterOption _ (ClusterGroup n)) = n < 1
    numberOfClusterCheck _                                  = False

    toSolution :: NonEmpty a -> PhylogeneticSolution a
    toSolution = PhylogeneticSolution . pure . PhylogeneticForest

    toClusterSplit :: ClusterSplit -> AC.ClusterCut
    toClusterSplit = \case
      ClusterGroup n -> AC.ClusterGroup n
      ClusterCut   d -> AC.ClusterSplit d

    clusterOptions :: AC.ClusterOptions
    clusterOptions = case clusterType of
      ClusterOption NoCluster       _     -> AC.NoCluster
      ClusterOption KMedians        _     -> AC.KMedians
      ClusterOption SingleLinkage   split -> AC.SingleLinkage   (toClusterSplit split)
      ClusterOption CompleteLinkage split -> AC.CompleteLinkage (toClusterSplit split)
      ClusterOption UPGMALinkage    split -> AC.UPGMALinkage    (toClusterSplit split)
      ClusterOption WeightedLinkage split -> AC.WeightedLinkage (toClusterSplit split)
      ClusterOption WardLinkage     split -> AC.WardLinkage     (toClusterSplit split)


wagnerBuildLogic
  :: PhylogeneticSolution FinalDecorationDAG
  -> Int
  -> EvaluationT GlobalSettings IO (NonEmpty FinalDecorationDAG)
wagnerBuildLogic = buildLogicMethod naiveWagnerParallelBuild


networkBuildLogic
  :: Bool
  -> PhylogeneticSolution FinalDecorationDAG
  -> Int
  -> EvaluationT GlobalSettings IO (NonEmpty FinalDecorationDAG)
networkBuildLogic isInitialBuild sol n = do
--    let
--      bestTrees :: NonEmpty FinalDecorationDAG
--      bestTrees = toNonEmpty . NE.head $ phylogeneticForests sol
    liftIO . putStrLn $ unlines
      [ "Beginning network construction..."
      , "Input Graph:"
      , replicate 16 '<'
      , unpack . renderDot $ toDot sol
      , replicate 16 '>'
      ]
    if isInitialBuild then
   -- If we have only the trivial forest solution then first perform a
   -- Wagner build before trying to add network edges.
      buildLogicMethod naiveNetworkParallelBuild sol n
    else
      do
        let
          bestTrees :: NonEmpty FinalDecorationDAG
          bestTrees = toNonEmpty . NE.head $ phylogeneticForests sol
        pure $ parmap rpar iterativeGreedyNetworkBuild bestTrees


forestBuildLogic
  :: PhylogeneticSolution FinalDecorationDAG
  -> Int
  -> EvaluationT GlobalSettings IO (NonEmpty FinalDecorationDAG)
forestBuildLogic _ _ = fail "The BUILD command type 'Forest' is not yet implemented!"


clusterBuildLogic
  :: BuildType FinalMetadata
  -> AC.ClusterOptions
  -> PhylogeneticSolution FinalDecorationDAG
  -> Int
  -> EvaluationT GlobalSettings IO (NonEmpty FinalDecorationDAG)
clusterBuildLogic buildMethod clusterOption
  = buildLogicMethod (clusterParallelBuild buildMethod clusterOption)


buildLogicMethod
  :: ParallelBuildType FinalMetadata
  -> PhylogeneticSolution FinalDecorationDAG
  -> Int
  -> EvaluationT GlobalSettings IO (NonEmpty FinalDecorationDAG)
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
naiveWagnerParallelBuild meta = parmap rpar (naiveWagnerBuild meta)


naiveNetworkParallelBuild
  :: (Foldable1 f
     , Traversable t
     )
  => MetadataSequence m
  -> t (f FinalCharacterNode)
  -> t FinalDecorationDAG
naiveNetworkParallelBuild meta = parmap rpar (naiveNetworkBuild meta)


clusterParallelBuild
  :: (Traversable t)
  => BuildType FinalMetadata
  -> AC.ClusterOptions
  -> MetadataSequence FinalMetadata
  -> t (NE.Vector FinalCharacterNode)
  -> t FinalDecorationDAG
clusterParallelBuild buildMethod clusterOptions m
  = parmap rpar (clusterBuildMethod buildMethod clusterOptions m)


naiveWagnerBuild
  :: (Foldable1 f)
  => MetadataSequence m
  -> f FinalCharacterNode
  -> FinalDecorationDAG
naiveWagnerBuild metaSeq ns =
   case toNonEmpty ns of
      x:|[]  -> fromRefDAG $
                  DAG.fromList
                  [ ( mempty        , wipeNode False x, mempty )
                  ]
      x:|[y] -> fromRefDAG $ DAG.fromList
                  [ ( mempty        , wipeNode True  x, IM.fromList [(1,mempty), (2,mempty)] )
                  , ( IS.singleton 0, wipeNode False x, mempty )
                  , ( IS.singleton 0, wipeNode False y, mempty )
                  ]
      x:|(y:z:xs) ->
          let len      = length ns
              initTree = initTaxaCounter len . fromRefDAG $ DAG.fromList
                  [ ( mempty        , wipeNode True  x, IM.fromList [(1,mempty), (4,mempty)] )
                  , ( IS.singleton 0, wipeNode True  x, IM.fromList [(2,mempty), (3,mempty)] )
                  , ( IS.singleton 1, wipeNode False x, mempty )
                  , ( IS.singleton 1, wipeNode False y, mempty )
                  , ( IS.singleton 0, wipeNode False z, mempty )
                  ]
          in
            foldl' iterativeBuild initTree xs

  where
    fromRefDAG = performDecoration . (`PDAG2`  metaSeq) . resetMetadata


initCounter :: (Enum x, NFData a) => IORef (x, x) -> String -> String -> Int -> Int -> a -> a
initCounter ref prefix name startNum totalNum x = unsafePerformIO $ do
    writeIORef ref (toEnum startNum, toEnum totalNum)
    putStrLn $ unwords [prefix, show totalNum, name]
    pure $ force x


printCounter :: (Enum x, NFData a, Real x, Real y, Show x, Show y) => IORef (x, y) -> a -> a
printCounter ref x = unsafePerformIO $ do
    let res = force x
    modifyIORef ref (first succ)
    (count, total) <- readIORef ref
    let shownTotal = show total
    let shownCount = show count
    let shownInfo  = replicate (length shownTotal - length shownCount) ' ' <> shownCount
    let ratioDone  = 100 * (realToFrac count / realToFrac total) :: Double
    let (num,dec)  = second (take 4) . span (/='.') $ show ratioDone
    let percentStr = fold [replicate (3 - length num) ' ', num, dec, replicate (4 - length dec) ' ']
    putStrLn $ fold [ "  - ", percentStr, "% ", shownInfo, "/", shownTotal, " taxa"]
    pure res


taxaCounter :: IORef (Word, Word)
{-# NOINLINE taxaCounter #-}
taxaCounter =
  unsafePerformIO $ newIORef (0,1)


initTaxaCounter :: NFData a => Int -> a -> a
initTaxaCounter = initCounter taxaCounter "Beginning Wagner build of" "taxa" 3


printTaxaCounter :: NFData a => a -> a
printTaxaCounter = printCounter taxaCounter


netEdgeCounter :: IORef (Word, Word)
{-# NOINLINE netEdgeCounter #-}
netEdgeCounter =
  unsafePerformIO $ newIORef (0,1)


initNetworkEdgeCounter :: NFData a => Int -> a -> a
initNetworkEdgeCounter = initCounter netEdgeCounter "Beginning network search with" "edges" 0


printNetworkEdgeCounter :: NFData a => a -> a
printNetworkEdgeCounter = printCounter netEdgeCounter


{-
costCounter :: IORef Int
{-# NOINLINE costCounter #-}
costCounter =
  unsafePerformIO (newIORef 0)
-}


naiveNetworkBuild
  :: (Foldable1 f)
  => MetadataSequence m
  -> f FinalCharacterNode
  -> FinalDecorationDAG
naiveNetworkBuild meta = iterativeGreedyNetworkBuild . naiveWagnerBuild meta


naiveForestBuild
  :: MetadataSequence m
  -> f FinalCharacterNode
  -> FinalDecorationDAG
naiveForestBuild = error "Naive forest build not yet implemented!"


clusterBuildMethod
  :: BuildType m
  -> AC.ClusterOptions
  -> MetadataSequence m
  -> NE.Vector FinalCharacterNode
  -> FinalDecorationDAG
clusterBuildMethod buildMethod option meta leafSetV
    = parallelClusterMethod buildMethod meta clusters
  where
    leafSetId :: LeafSet (DecoratedCharacterNode Identity)
    leafSetId = coerce leafSetV

    clusters :: NE.Vector (NE.Vector (DecoratedCharacterNode Identity))
    clusters = AC.clusterIntoGroups meta leafSetId option


parallelClusterMethod
  :: BuildType m
  -> MetadataSequence m
  -> NE.Vector (NE.Vector (DecoratedCharacterNode Identity))
  -> FinalDecorationDAG
parallelClusterMethod buildMethod meta clusters =
  let
    finalDecClusters :: NE.Vector (NE.Vector FinalCharacterNode)
    finalDecClusters  = coerce clusters

    clusterTrees :: NE.Vector FinalDecorationDAG
    clusterTrees =
      parmap rpar (buildMethod meta) finalDecClusters
  in
    subTreeMethod buildMethod meta clusterTrees


subTreeMethod
  :: BuildType m
  -> MetadataSequence m
  -> NE.Vector FinalDecorationDAG
  -> FinalDecorationDAG
subTreeMethod buildMethod meta subTrees =
  let
    subTreeDict    :: M.Map NodeLabel FinalDecorationDAG
    rootCharNodesV :: Vector FinalCharacterNode
    (subTreeDict, rootCharNodesV) = makeTemporaryNameDict subTrees

 -- This is safe as we are passing in a non-empty vector of trees.
    rootCharNodes :: NE.Vector FinalCharacterNode
    rootCharNodes = unsafeFromVector rootCharNodesV

    rootNodeLabels :: [NodeLabel]
    rootNodeLabels =
      M.keys subTreeDict

    rootNodeTree :: FinalDecorationDAG
    rootNodeTree =
      buildMethod meta rootCharNodes

    namedContext :: M.Map NodeLabel Int
    namedContext =
      rootNodeTree `getNamedContext` rootNodeLabels
  in
    substituteDAGs subTreeDict rootNodeTree `evalState` namedContext
  where
    getRootNode :: FinalDecorationDAG -> FinalCharacterNode
    {-# INLINE getRootNode #-}
    getRootNode dag =
      let
        refDAG  = dag ^. _phylogeneticForest

        refs :: FinalReferenceVector
        refs    = refDAG ^. _references

        rootInd :: Int
        rootInd = refDAG ^. _rootRefs . _head
      in
        (refs ! rootInd) ^. _nodeDecoration

    makeTemporaryNameDict
      :: NE.Vector FinalDecorationDAG
      -> (M.Map NodeLabel FinalDecorationDAG, Vector FinalCharacterNode)
    {-# INLINE makeTemporaryNameDict #-}
    makeTemporaryNameDict subT
        = foldM labelSubTree (mempty, mempty) subT `evalState` 0
      where
        labelSubTree
          :: ( M.Map NodeLabel FinalDecorationDAG
             , Vector FinalCharacterNode
             )
          -> FinalDecorationDAG
          -> State Word64 (M.Map NodeLabel FinalDecorationDAG, Vector FinalCharacterNode)
        {-# INLINE labelSubTree #-}
        labelSubTree (currDict, currNodes) dag = do
          currLabel <- get
          let tempLabel    = nodeLabel . makeIllegalShortText $ currLabel
          let rootNode     = getRootNode dag
          let tempRootNode = giveTemporaryName rootNode tempLabel
          let newDict      = M.insert tempLabel dag currDict
          let newNodes     = currNodes `snoc` tempRootNode
          put (currLabel + 1)
          pure (newDict, newNodes)

        giveTemporaryName
          :: FinalCharacterNode
          -> NodeLabel
          -> FinalCharacterNode
        {-# INLINE giveTemporaryName #-}
        giveTemporaryName node label = node & _nodeDecorationDatum .~ label



iterativeBuild
  :: FinalDecorationDAG
  -> FinalCharacterNode
  -> FinalDecorationDAG
iterativeBuild currentTree@(PDAG2 _ metaSeq) nextLeaf = printTaxaCounter nextTree
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


{-
iterativeNetworkBuild
  :: FinalDecorationDAG
  -> FinalDecorationDAG
iterativeNetworkBuild currentNetwork@(PDAG2 inputDag metaSeq) =
    case toList $ DAG.candidateNetworkEdges inputDag of
      []   -> currentNetwork
      x:xs ->
        -- DO NOT use rdeepseq! Prefer rseq!
        -- When trying candidate edges, we can construct graphs for which a
        -- pre-order traversal is not logically possible. These graphs will
        -- necessarily result in an infinite cost. So long as we lazily compute
        -- the cost, the minimization routine will discard the incoherent,
        -- infinite-cost candidates and we won't run into interesting runtime problems.
        let !edgesToTry = x:|xs
            len = length xs
            (minNewCost, !bestNewNetwork) =
              unsafePerformIO $ do
                putStrLn $ unlines
                         [ ""
                         , "Starting network edge search..."
                         , "Number of candidate network edges: " <> show len
                         , "Progress   "
                         ]
                let !v = minimumBy (comparing fst)
                       . parmap (rparWith rseq) f
                       . unsafePerformIO $ traverse tryNetworkEdge edgesToTry
                pure v
        in
          if getCost currentNetwork <= minNewCost
            then currentNetwork
            else iterativeNetworkBuild bestNewNetwork
  where
    f p =
      unsafePerformIO $
        do
          counter <- readIORef costCounter
          writeIORef costCounter (counter + 1)
          putStrLn $ "  - " <> show counter <> " cost computed."
          pure $ (getCost &&& id) p

    (PDAG2 dag _) = force $ wipeScoring currentNetwork

    tryNetworkEdge :: ((Int, Int), (Int, Int)) -> IO FinalDecorationDAG
    tryNetworkEdge e =
        do
          networkEdges <- readIORef costCounter
          modifyIORef costCounter succ
          putStrLn $ "  - " <> show networkEdges <> " network edges tried."
          pure . performDecoration . (`PDAG2` metaSeq) . connectEdge' $ e

    getCost (PDAG2 v _) = dagCost $ graphData v

    resetDAG = resetEdgeData $ resetMetadata dag

    connectEdge'
      = uncurry (connectEdge resetDAG deriveOriginEdgeNode deriveTargetEdgeNode)

    deriveOriginEdgeNode
      :: DecoratedCharacterNode Maybe
      -> DecoratedCharacterNode Maybe
      -> DecoratedCharacterNode Maybe
      -> DecoratedCharacterNode Maybe
    deriveOriginEdgeNode parentDatum oldChildDatum _newChildDatum =
        PNode2 (resolutions oldChildDatum) (nodeDecorationDatum2 parentDatum)

    deriveTargetEdgeNode parentDatum oldChildDatum =
        PNode2 (resolutions oldChildDatum) (nodeDecorationDatum2 parentDatum)
-}


iterativeGreedyNetworkBuild
  :: FinalDecorationDAG
  -> FinalDecorationDAG
iterativeGreedyNetworkBuild currentNetwork@(PDAG2 inputDag metaSeq) =
    case toList $ DAG.candidateNetworkEdges inputDag of
      [] -> currentNetwork
      xs ->
        -- DO NOT use rdeepseq! Prefer rseq!
        -- When trying candidate edges, we can construct graphs for which a
        -- pre-order traversal is not logically possible. These graphs will
        -- necessarily result in an infinite cost. So long as we lazily compute
        -- the cost, the minimization routine will discard the incoherent,
        -- infinite-cost candidates and we won't run into interesting runtime problems.
        let edgesToTry :: [((Int, Int), (Int, Int))]
            !edgesToTry = xs

            currCost :: ExtendedReal
            !currCost = initNetworkEdgeCounter (length xs) $ getCost currentNetwork

            smallerThanCurr :: (ExtendedReal, FinalDecorationDAG) -> Bool
            smallerThanCurr (c, _) = c <= currCost

            -- See note above about rseq
            newNetworkOpt :: Maybe (ExtendedReal, FinalDecorationDAG)
            newNetworkOpt = find smallerThanCurr $
              parMapBuffer 8 (rparWith rseq) (f . tryNetworkEdge) edgesToTry
        in
          case newNetworkOpt of
            Nothing              -> currentNetwork
            Just (_, newNetwork) ->
              unsafePerformIO $
                do
                  putStrLn "Added network edge"
                  putStrLn "New Graph:"
                  putStrLn $ replicate 16 '<'
                  putStrLn . unpack . renderDot $ toDot newNetwork
                  putStrLn $ replicate 16 '>'
                  putStrLn "Continuing search:"
                  pure $ iterativeGreedyNetworkBuild newNetwork
  where
    f :: FinalDecorationDAG -> (ExtendedReal, FinalDecorationDAG)
    f = printNetworkEdgeCounter . getCost &&& id

    (PDAG2 dag _) = force $ wipeScoring currentNetwork

    tryNetworkEdge :: ((Int, Int), (Int, Int)) -> FinalDecorationDAG
    tryNetworkEdge = performDecoration . (`PDAG2` metaSeq) . connectEdge'

--    tryNetworkEdge e = do
--      networkEdges <- readIORef netEdgeCounter
--      writeIORef netEdgeCounter (networkEdges + 1)
--      putStrLn $ "  - " <> show networkEdges <> " network edges tried."
--      pure . performDecoration . (`PDAG2` metaSeq) . connectEdge' $ e


    getCost (PDAG2 v _) = dagCost $ graphData v

    resetDAG = resetEdgeData $ resetMetadata dag

    connectEdge'
      = uncurry (connectEdge resetDAG deriveOriginEdgeNode deriveTargetEdgeNode)

    deriveOriginEdgeNode
      :: DecoratedCharacterNode Maybe
      -> DecoratedCharacterNode Maybe
      -> DecoratedCharacterNode Maybe
      -> DecoratedCharacterNode Maybe
    deriveOriginEdgeNode parentDatum oldChildDatum _newChildDatum =
        PNode2 (resolutions oldChildDatum) (nodeDecorationDatum2 parentDatum)

    deriveTargetEdgeNode parentDatum oldChildDatum =
        PNode2 (resolutions oldChildDatum) (nodeDecorationDatum2 parentDatum)



resetMetadata :: ReferenceDAG d e n -> ReferenceDAG (PostorderContextualData t) e n
resetMetadata ref = ref & _graphData %~ setDefaultMetadata

resetEdgeData :: ReferenceDAG d (e,a) n -> ReferenceDAG d e n
resetEdgeData ref = ref & _references . (mapped . _childRefs . mapped) %~ fst


parMapBuffer :: Int -> Strategy b -> (a -> b) -> [a] -> [b]
parMapBuffer buffer strat f =
  withStrategy (parBuffer buffer strat) . fmap f
