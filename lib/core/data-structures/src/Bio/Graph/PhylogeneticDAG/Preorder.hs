------------------------------------------------------------------------------
-- |
-- Module      :  Bio.Graph.PhylogeneticDAG.Preorder
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

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
-- This gap is necessary for stylish Haskell not to re-arrange
-- NoMonoLocalBinds before TypeFamilies
{-# LANGUAGE NoMonoLocalBinds    #-}

module Bio.Graph.PhylogeneticDAG.Preorder
  ( preorderSequence
  , preorderFromRooting
  , setEdgeSequences
  ) where

-- TODO: Change to more sensible qualified name
import           Bio.Character.Decoration.Dynamic
import           Bio.Character.Encodable
import           Bio.Graph.Node
import           Bio.Graph.Node.Context             as AP
import           Bio.Graph.PhylogeneticDAG.Internal
import           Bio.Graph.ReferenceDAG.Internal
import           Bio.Metadata
import           Bio.Sequence
import qualified Bio.Sequence.Block                 as BLK
import           Control.Lens
import           Control.Monad.State.Lazy
import           Data.Bifunctor
import           Data.Either.Custom                 (fromTaggedRep, toTaggedRep)
import           Data.Foldable
import           Data.GraphViz.Printing
import           Data.HashMap.Lazy                  (HashMap)
import qualified Data.IntMap                        as IM
import qualified Data.IntSet                        as IS
import           Data.Key
import           Data.List.NonEmpty                 (NonEmpty ((:|)))
import qualified Data.List.NonEmpty                 as NE
import qualified Data.Matrix.NotStupid              as MAT
import           Data.Maybe
import           Data.MonoTraversable
import           Data.Ord                           (comparing)
import           Data.Semigroup
import           Data.Semigroup.Foldable
import qualified Data.Text.Lazy                     as L
import           Data.TopologyRepresentation
import           Data.Vector                        (Vector)
import qualified Data.Vector                        as V
import           Data.Vector.Instances              ()
import qualified Data.Vector.NonEmpty               as NEV
import           Prelude                            hiding (lookup, zip, zipWith)
import           TextShow


type BlockTopologies = NEV.Vector TraversalTopology


-- |
-- Applies a traversal logic function over a 'ReferenceDAG' in a /pre-order/ manner.
--
-- The logic function takes a current node decoration,
-- a list of parent node decorations with the logic function already applied,
-- and returns the new decoration for the current node.
preorderSequence ::
  forall m e n u v w x y z u' v' w' x' y' z' . HasBlockCost u  v  w  x  y  z
  => (ContinuousCharacterMetadataDec                         -> AP.PreorderContext u u' -> u')
  -> (DiscreteCharacterMetadataDec                           -> AP.PreorderContext v v' -> v')
  -> (DiscreteCharacterMetadataDec                           -> AP.PreorderContext w w' -> w')
  -> (DiscreteWithTCMCharacterMetadataDec StaticCharacter    -> AP.PreorderContext x x' -> x')
  -> (DiscreteWithTCMCharacterMetadataDec StaticCharacter    -> AP.PreorderContext y y' -> y')
  -> (DynamicCharacterMetadataDec (Element DynamicCharacter) -> AP.PreorderContext z z' -> z')
  -> PhylogeneticDAG m e n u  v  w  x  y  z
  -> PhylogeneticDAG m e n u' v' w' x' y' z'
preorderSequence f1 f2 f3 f4 f5 f6 pdag2@(PDAG2 dag meta) = pdag2 & _phylogeneticForest .~ newRDAG
  where
    refs          = references dag
    dagSize       = length $ references dag
    newRDAG       = dag & _references .~ newReferences
                        & _graphData  %~ setDefaultMetadata
    newReferences = V.generate dagSize g
      where
        g i =
          (refs ! i) & _nodeDecoration .~ (memo ! i)

    -- A "sequence" of the minimum topologies that correspond to each block.
    sequenceOfBlockMinimumTopologies :: BlockTopologies
    sequenceOfBlockMinimumTopologies = getSequenceOfBlockMinimumTopologies pdag2

    -- Here we generate a memoized vector of the updated node decorations from
    -- the pre-order traversal. This memoization technique relies on lazy
    -- evaluation to compute the data for each vector index in the correct order
    -- of dependency with the root node(s) as the base case(es).
    memo :: Vector (PhylogeneticNode (CharacterSequence u' v' w' x' y' z') n)
    memo = V.generate dagSize g
      where

        -- This is the generating function.
        -- It computes the updated node decoration for a given index of the vector.
        g currInd =
          (node ^. _nodeDecoration) & _resolutions .~ newResolution
          where

            node            = refs ! currInd

            -- This is a singleton resolution cache to conform the the
            -- PhylogeneticNode type requirements. It is the part of that gets
            -- updated and requires a bunch of work to be performed.
            newResolution    = mockResInfo datumResolutions newSequence

            -- The character sequence for the current index with the node decorations
            -- updated to their pre-order values with their final states assigned.

            newSequence      =
              computeOnApplicableResolution f1 f2 f3 f4 f5 f6 meta parentalContext

            -- This is *really* important.
            -- Here is where we collect the parental context for the current node.
            --
            -- In the root node context where there are no parents, this is easy.
            -- We simply create the "sequence" with no information derived.
            --
            -- In the tree node case where there is only one parent, we grab the
            -- parent context via memoization and match each parent block with
            -- it's corresponding topology reprsentation.
            --
            -- In the network node case where there are two parents, we grab both
            -- of the parent contexts via memoization and then select the block
            -- from the parent that was connected to the current node on the
            -- minimal display tree for that block.
            parentalContext  = mapWithKey parentalAccessor sequenceOfBlockMinimumTopologies

            parentalAccessor =
                case parentIndices of
                  NoParent
                    -> \_ x -> (x, RootContext datumResolutions)
                  OneParent parInd
                    -> selectTopologyFromParentOptions $ (parInd, memo ! parInd):|[]
                  TwoParents parInd1 parInd2
                    -> selectTopologyFromParentOptions
                         $ (parInd1, memo ! parInd1):|[(parInd2, memo ! parInd2)]

            datumResolutions :: ResolutionCache (CharacterSequence u v w x y z)
            datumResolutions = node ^. _nodeDecoration . _resolutions

            parentIndices = otoParentContext $ parentRefs node
            -- In sparsely connected graphs (like ours) this will be effectively constant.
            childPosition j
              = toEnum . length . takeWhile (/= currInd) . IM.keys . childRefs $ refs ! j

            selectTopologyFromParentOptions
              :: NonEmpty (Int, PhylogeneticNode (CharacterSequence u1 v1 w1 x1 y1 z1) n)
              -> Int
              -> TraversalTopology
              -> ( TraversalTopology
                 , AP.PreorderContext
                     (ResolutionCache (CharacterSequence u v w x y z))
                     (BLK.CharacterBlock u1 v1 w1 x1 y1 z1)
                 )
            selectTopologyFromParentOptions nodeOptions key topology =

                case NE.filter matchesTopology
                       $ second (NE.head . resolutions) <$> nodeOptions of
                  (x,y):_ ->
                    let
                      parBlock        = (characterSequence y ^. blockSequence) ! key
                      childCacheContext = leftRightChild (childPosition x) datumResolutions
                    in
                      ( topology
                      , PreInternalContext parBlock childCacheContext
                      )

                  []      -> error $ unlines
                                 [ unwords
                                   ["No Matching topology for Block"
                                   , show key
                                   , "on Node"
                                   , show currInd
                                   ]
                                 , "The minimal topologies for each block: "
                                   <> show sequenceOfBlockMinimumTopologies
                                 , "And this was the problem topology: " <> show topology
                                 , "And these were our options: "
                                   <> show
                                       ((^. _topologyRepresentation)
                                        . NE.head
                                        . resolutions
                                        . snd
                                        <$> nodeOptions
                                       )
                                 ]
              where
                leftRightChild :: Int -> (a -> Either a a)
                leftRightChild = \case
                  0 -> Left
                  _ -> Right

                matchesTopology :: (Int, b) -> Bool
                matchesTopology (pInd, _)
                    = (pInd, currInd) `notElem` excludedNetworkEdges topology



    -- A "sequence" of the minimum topologies that correspond to each block.
getSequenceOfBlockMinimumTopologies
  :: HasBlockCost u v w x y z
  => PhylogeneticDAG m e n u v w x y z
  -> BlockTopologies
getSequenceOfBlockMinimumTopologies (PDAG2 dag meta) = getTopologies blockMinimalResolutions
      where
        getTopologies  = fmap (^. _topologyRepresentation)

        blockMinimalResolutions = mapWithKey f $ sequenceWLOG ^. blockSequence

        sequenceWLOG   = characterSequence $ NE.head rootResolutions

        getMetaBlock i = (meta ^. blockSequence) ! i

        f key _block   = minimumBy (comparing extractedBlockCost)
                         rootResolutions
          where
            extractedBlockCost
              = blockCost (getMetaBlock key) . (! key) . (^. blockSequence) . characterSequence
        rootResolutions = resolutions . nodeDecoration $ references dag ! rootWLOG

        rootWLOG = NE.head $ rootRefs dag

mockResInfo :: ResolutionCache s -> s' -> ResolutionCache s'
mockResInfo currentResolutions newSequence =
    -- Default the ResolutionInformation valus, insert the preorder sequence result
    pure $
      NE.head currentResolutions & _characterSequence .~ newSequence



computeOnApplicableResolution
  :: forall m u v w x y z u' v' w' x' y' z'
   . (ContinuousCharacterMetadataDec                         -> AP.PreorderContext u u' -> u')
  -> (DiscreteCharacterMetadataDec                           -> AP.PreorderContext v v' -> v')
  -> (DiscreteCharacterMetadataDec                           -> AP.PreorderContext w w' -> w')
  -> (DiscreteWithTCMCharacterMetadataDec StaticCharacter    -> AP.PreorderContext x x' -> x')
  -> (DiscreteWithTCMCharacterMetadataDec StaticCharacter    -> AP.PreorderContext y y' -> y')
  -> (DynamicCharacterMetadataDec (Element DynamicCharacter) -> AP.PreorderContext z z' -> z')
  -> MetadataSequence m
  -> NEV.Vector
       ( TraversalTopology
       , AP.PreorderContext
           (ResolutionCache (CharacterSequence u v w x y z))
           (BLK.CharacterBlock u' v' w' x' y' z')
       )
  -> CharacterSequence u' v' w' x' y' z'
computeOnApplicableResolution f1 f2 f3 f4 f5 f6 meta preContextOptions =
    (^. from blockSequence) $ zipWithKey f (meta ^. blockSequence) preContextOptions

  where
    (f1Root, f1Bin) = (rootFunction <$> f1 , preBinaryFunction <$> f1)
    (f2Root, f2Bin) = (rootFunction <$> f2 , preBinaryFunction <$> f2)
    (f3Root, f3Bin) = (rootFunction <$> f3 , preBinaryFunction <$> f3)
    (f4Root, f4Bin) = (rootFunction <$> f4 , preBinaryFunction <$> f4)
    (f5Root, f5Bin) = (rootFunction <$> f5 , preBinaryFunction <$> f5)
    (f6Root, f6Bin) = (rootFunction <$> f6 , preBinaryFunction <$> f6)

    f key metaBlock (topology, preorderOpt) =
      preorderContext rootFn internalFn preorderOpt
      where
        rootFn rootResCache =
          let
            rootCharBlock = selectChildBlockByTopology key topology rootResCache
          in
            BLK.hexZipMeta f1Root f2Root f3Root f4Root f5Root f6Root metaBlock rootCharBlock


        internalFn childResCacheContext parentBlock =
          let
{-             ┌───┤ Left/Right direction
               │
               │       ┌───┤ Value
               │       │  -}
             (tag, childRes) = toTaggedRep childResCacheContext
             childBlock      = selectChildBlockByTopology key topology childRes
           -- re-tagging function
             dir = fromTaggedRep tag
           -- Tag each character block with the correct context
             childBlockContext =
               BLK.hexmap dir dir dir dir dir dir childBlock
          in
            BLK.hexZipWithMeta
              f1Bin f2Bin f3Bin f4Bin f5Bin f6Bin metaBlock childBlockContext parentBlock

    selectChildBlockByTopology
      :: Int
      -> TraversalTopology
      -> ResolutionCache (CharacterSequence u v w x y z)
      -> BLK.CharacterBlock u v w x y z
    selectChildBlockByTopology key topology childCache =
        case NE.filter matchesTopology childCache of
          x:_ -> (characterSequence x ^. blockSequence) ! key
          []  -> (characterSequence (NE.head childCache) ^. blockSequence) ! key
      where
        matchesTopology = (`isCompatableWithTopology` topology) . (^. _topologyRepresentation)


selectApplicableResolutions :: TraversalTopology -> ResolutionCache s -> ResolutionInformation s
selectApplicableResolutions topology cache =
    case filter (\x -> (^. _topologyRepresentation) x `isCompatableWithTopology` topology) $ toList cache of
      []  -> error $ unlines
                 [ "No applicable resolution found on pre-order traversal"
                 , "Input set:   " <> show topology
                 , "Local sets:  " <> show ((^. _subtreeEdgeSet) <$> cache)
                 , "Local Topos: " <> show ((^. _topologyRepresentation) <$> cache)
                 ]
      [x] -> x
      xs  -> maximumBy (comparing (length . (^. _subtreeEdgeSet))) xs


-- |
-- Different contexts used to mediate an effcient multidimensional traversal.
data  PreorderContext c
    = NormalNode   Int
    | SetRootNode  c
    | FociEdgeNode Int c
    | NoBlockData


generateDotFile :: TextShow n => PhylogeneticDAG m e n u v w x y z -> String
generateDotFile = (<> "\n") . L.unpack . renderDot . toDot


adjustResolution
  :: (ResolutionInformation (CharacterSequence u v w x y z) -> ResolutionInformation (CharacterSequence u v w x y z'))
  -> IndexData e (PhylogeneticNode (CharacterSequence u v w x y z) n)
  -> ResolutionCache (CharacterSequence u v w x y z')
adjustResolution f = pure . f . NE.head . resolutions . nodeDecoration


-- |
-- Applies a traversal logic function over a 'ReferenceDAG' in a /pre-order/ manner.
--
-- The logic function takes a current node decoration and
-- a list of parent node decorations with the logic function already applied,
-- and returns the new decoration for the current node.
preorderFromRooting
  :: forall m u v w x y z e' n' u' v' w' x' y' z'
  .  (TextShow n')
  => (DynamicCharacterMetadataDec (Element DynamicCharacter) ->  AP.PreorderContext z z' -> z')
  ->         HashMap EdgeReference (ResolutionCache (CharacterSequence u v w x y z))
  -> Vector (HashMap EdgeReference (ResolutionCache (CharacterSequence u v w x y z)))
  -> NEV.Vector (TraversalTopology, Double, Double, Double, Vector (NonEmpty TraversalFocusEdge))
  -> PhylogeneticDAG m e' n' u' v' w' x' y' z
  -> PhylogeneticDAG m e' n' u' v' w' x' y' z'
preorderFromRooting transformation edgeCostMapping nodeDatumContext minTopologyContextPerBlock pdag2@(PDAG2 dag meta)
  =  pdag2 & _phylogeneticForest .~ newRDAG
  where
    newRDAG
      :: ReferenceDAG
      (PostorderContextualData (CharacterSequence u' v' w' x' y' z')) e' (PhylogeneticNode (CharacterSequence u' v' w' x' y' z') n')
    newRDAG =
         dag & _references .~ newReferences
             & _graphData  %~ buildMetaData

    newReferences = case nodeCount of
      1 -> singleRef <$> (dag ^. _references)
      n -> V.generate n g
      where
        g i = (refs ! i) & _nodeDecoration .~ (memo ! i)

    singleRef node = node & _nodeDecoration .~ updatedNode
      where
        updatedNode = (node ^. _nodeDecoration) & _resolutions .~ newResolution

        newResolution :: ResolutionCache (CharacterSequence u' v' w' x' y' z')
        newResolution = adjustResolution updateDynamicCharactersInSequence node

        dynCharGen m x = transformation m (RootContext x)

        updateDynamicCharactersInSequence
           :: ResolutionInformation (CharacterSequence u1 v1 w1 x1 y1 z)
           -> ResolutionInformation (CharacterSequence u1 v1 w1 x1 y1 z')
        updateDynamicCharactersInSequence resInfo
           = resInfo { characterSequence = updatedCharacterSequence }
           where
             updatedCharacterSequence =
               over blockSequence
               (zipWith blockGen (meta ^. blockSequence))
               $ characterSequence resInfo

             blockGen mBlock cBlock = cBlock & dynamicBin .~ updatedDynamicCharacters
               where
                 updatedDynamicCharacters :: Vector z'
                 updatedDynamicCharacters = zipWith dynCharGen (mBlock ^. dynamicBin) (cBlock ^. dynamicBin)

    buildMetaData
      :: GraphData (PostorderContextualData (CharacterSequence u' v' w' x' y' z))
      -> GraphData (PostorderContextualData (CharacterSequence u' v' w' x' y' z'))
    buildMetaData gD =
      gD & setDefaultMetadata
         & _graphMetadata . _minimalNetworkContext ?~ toNonEmpty minTopologyContextPerBlock

    rootSet    = IS.fromList . toList $ rootRefs dag
    refs       = references dag
    nodeCount  = length refs
    blockCount = length . (^. blockSequence) . characterSequence . NE.head . resolutions . nodeDecoration $ refs ! NE.head (rootRefs dag)

    getDynCharSeq = fmap (^. dynamicBin) . (^. blockSequence) . characterSequence
    getAdjacentNodes i = foldMap f $ otoList ns
      where
        f j
          | j `oelem` rootSet = [j]
          | otherwise         = [j]
        v  = refs ! i
        ns = ps <> cs
        ps = parentRefs v
        cs = IM.keysSet $ childRefs v


    -- |
    -- For each Node,
    --   for each block,
    --     for each dynamic character,
    --        if   the block exists in the network context,
    --        then either the parent ref index or the root datum.
    --
    -- matrix rows   index: node index
    -- matrix column index: block index
    -- nested vector index: dynamic character index of block
    -- block context value: the parental context for the given block
    --   NormalNode   -> parent  is another node in the graph
    --   SetRootNode  -> parent is a root node of the graph structure
    --   FociEdgeNode -> parent is a "virtual root node" from rerooting
    --   NoBlockData  -> parent does not exist because for this block because
    --                     no block data exists in the network context
    -- parentVectors :: Matrix (Vector (PreorderContext c))
    parentVectors = MAT.matrix nodeCount blockCount g
      where
        g e@(nodeIndex, blockIndex)
          | nodeIndex == 0 = head . toList   <$> dynCharVec
          | otherwise      = (!!! nodeIndex) <$> dynCharVec
          where
             -- :: Vector (IM.IntMap (PreorderContext z))
            dynCharVec = parentMapping ! blockIndex

            (!!!) v i = fromMaybe err $ i `lookup` v
              where
                err = error $ fold
                    [ "Can't index at "
                    , show i
                    , " when given "
                    , show e
                    , "\nIn the IntMap: "
                    , show $ IM.keysSet <$> dynCharVec
                    , "\n\n"
                    , generateDotFile pdag2
                    ]

        parentMapping = delta minTopologyContextPerBlock

        -- Takes a list of blocks, each containing a vector of dynamic characters
        -- and returns the mapping of nodes to their parents under the rerooting
        -- assignment.
        --
        -- Gives either a virtual node, that is, the root for the lowest cost for this dynamic character, or
        -- when the current node is a child of the root, ??
        --
        -- Left is the virtual root node for a dynamic character given this topology, because the _node's_ root isn't another
        -- node in the DAG: it is directly connected to the rooting
        -- edge.
        --
        -- Right is a reference to the parent index in the DAG.

--      delta :: NEV.Vector (TopologyRepresentation (IS.Key, Int), b, c, d, Vector (NonEmpty (IS.Key, IS.Key)))
--            -> NEV.Vector (Vector (IM.IntMap (PreorderContext z)))
        delta = mapWithKey (\k (topo, _, _, _, v) -> mapWithKey (f topo k) v)
          where
            f topo blockIndex charIndex = foldMap epsilon
              where
                epsilon rootingEdge@(r1,r2) =
                    case virtualRoot of
                      Nothing    -> IM.singleton r1 NoBlockData <> IM.singleton r2 NoBlockData
                      Just vRoot ->
                          let lhs = IM.singleton r1 $ FociEdgeNode r2 virtualRootDatum
                              rhs = IM.singleton r2 $ FociEdgeNode r1 virtualRootDatum
                              virtualRootDatum = (! charIndex) . (! blockIndex) $ getDynCharSeq vRoot
                          in  lhs <> rhs <> gen virtualRootDatum mempty (r1,r2) <> gen virtualRootDatum mempty (r2,r1)
                  where
                    virtualRoot =
                        case NE.filter (\x -> (^. _topologyRepresentation) x == topo) $ edgeCostMapping ! rootingEdge of
                          []  -> Nothing
                          x:_ -> Just x

                    excludedEdges = excludedNetworkEdges topo
                    gen virtualRootDatum seenSet (n1,n2)
                      | isExcludedEdge = mempty
                      | otherwise      = (currentVal <>) . foldMap toMap . filter (`onotElem` seenSet') $ getAdjacentNodes n2
                      where
                        isExcludedEdge = (n1,n2) `elem` excludedEdges || (n2,n1) `elem` excludedEdges
                        currentVal
                          | n2 `oelem` rootSet = IM.singleton n2 $ SetRootNode virtualRootDatum
                          | n1 `oelem` rootSet =
                                         case toList . headMay . filter (/=n2) . IM.keys . childRefs $ refs ! n1 of
                                           x:_ -> IM.singleton n2 $ NormalNode x
                                           []  -> IM.singleton n2 $ NormalNode n1 -- I guess it's the root and a single leaf...?
                          | otherwise          =  IM.singleton n2 $ NormalNode n1
                        toMap v     = gen virtualRootDatum seenSet' (n2, v)
                        seenSet'    = IS.insert n1 seenSet


    -- Here we generate a memoized vector of the updated node decorations from
    -- the pre-order traversal. This memoization technique relies on lazy
    -- evaluation to compute the data for each vector index in the correct order
    -- of dependency with the root node(s) as the base case(es).
    --
    -- Unlike 'preorderSequence' this memoized vector only updates the dynamic
    -- characters.

    memo :: Vector (PhylogeneticNode (CharacterSequence u' v' w' x' y' z') n')
    memo = V.generate nodeCount gen
      where

        -- This is the generating function.
        -- It computes the updated node decoration for a given index of the vector.
        gen :: Int -> PhylogeneticNode (CharacterSequence u' v' w' x' y' z') n'
        gen i = (node ^. _nodeDecoration) & _resolutions .~ newResolution
          where

            -- This is a singleton resolution cache to conform to the
            -- PhylogeneticNode type requirements. It is the part that gets
            -- updated, and requires a bunch of work to be performed.
            -- Remember, this only updates the dynamic characters.
            newResolution = adjustResolution updateDynamicCharactersInSequence node

            node          = refs ! i

            kids          = IM.keys $ childRefs node

            updateDynamicCharactersInSequence
              :: ResolutionInformation (CharacterSequence u1 v1 w1 x1 y1 z)
              -> ResolutionInformation (CharacterSequence u1 v1 w1 x1 y1 z')
            updateDynamicCharactersInSequence resInfo
              = resInfo { characterSequence = updatedCharacterSequence }
              where
                updatedCharacterSequence =
                  over blockSequence
                  (zipWithKey blockGen (meta ^. blockSequence))
                  $ characterSequence resInfo

                blockGen j mBlock cBlock = cBlock & dynamicBin .~ updatedDynamicCharacters
                  where
                    (topology,_,_,_,_) = minTopologyContextPerBlock ! j
                    excludedEdges = excludedNetworkEdges topology
                    updatedDynamicCharacters = mapWithKey dynCharGen $ mBlock ^. dynamicBin

                    dynCharGen :: Int -> DynamicCharacterMetadataDec DynamicCharacterElement -> z'
                    dynCharGen k m =
                        case parentRefContext of
                          NoBlockData      -> error "This is bad and sad plus I'm mad."
                          SetRootNode  x   -> transformation m (RootContext x)
                          FociEdgeNode p x ->
                            let currentContext
                                  = selectApplicableResolutions topology
                                  $ (nodeDatumContext .!>. i) .!>. (p,i)

                                currentDecoration =
                                    (!k)
                                  . (^. dynamicBin)
                                  . (!j)
                                  . (^. blockSequence)
                                  . characterSequence
                                  $ currentContext

                                parentalDecoration = transformation m (RootContext x)
                            in  transformation m
                                  PreInternalContext
                                    { preParent       = parentalDecoration
                                    , preChildContext = Left currentDecoration
                                    }

                          NormalNode   p   ->
                            let isDeadEndNode = -- This only checks one edge away, probably should be transitive.
                                  case kids of
                                    [c] -> (c,i) `elem` excludedEdges || (i,c) `elem` excludedEdges
                                    _   -> False
                                currentContext     = selectApplicableResolutions topology $ (nodeDatumContext .!>. i) .!>. (p,i)
                                currentDecoration  = (!k) . (^. dynamicBin) . (!j) . (^. blockSequence) . characterSequence $ currentContext
                                parentalDecoration = getDynCharDecoration . NE.head . resolutions $ memo ! p
                            in  if   isDeadEndNode
                                then parentalDecoration
                                else transformation m
                                       PreInternalContext
                                         { preParent       = parentalDecoration
                                         , preChildContext = Left currentDecoration
                                         }
                      where
                        parentRefContext     = (parentVectors ! (i,j)) ! k
                        -- Stupid monomorphisms prevent elegant code reuse

                        getDynCharDecoration =
                          (!k) . (^. dynamicBin) . (!j) . (^. blockSequence) . characterSequence

(.!>.) :: (Lookup f, Show (Key f)) => f a -> Key f -> a
(.!>.) s k = fromMaybe (error $ "Could not index: " <> show k) $ k `lookup` s


{-
constructDefaultMetadata :: (Monoid a, Monoid b) => ReferenceDAG d e n -> GraphData (a, b, Maybe c)
constructDefaultMetadata = ((mempty, mempty, Nothing) <$) . graphData
-}


-- |
-- Computes and sets the virtual node sequence on each edge.
setEdgeSequences
  :: forall m e n u v w x y z u' v' w' x' y' z' u'' v'' w'' x'' y''
  .  (ContinuousCharacterMetadataDec                         -> (u, u) -> u')
  -> (DiscreteCharacterMetadataDec                           -> (v, v) -> v')
  -> (DiscreteCharacterMetadataDec                           -> (w, w) -> w')
  -> (DiscreteWithTCMCharacterMetadataDec StaticCharacter    -> (x, x) -> x')
  -> (DiscreteWithTCMCharacterMetadataDec StaticCharacter    -> (y, y) -> y')
  -> (DynamicCharacterMetadataDec (Element DynamicCharacter) -> (z, z) -> z')
  -> Vector (HashMap EdgeReference (ResolutionCache (CharacterSequence u'' v'' w'' x'' y'' z')))
  -> PhylogeneticDAG m e n u v w x y z
  -> PhylogeneticDAG m (e, CharacterSequence u' v' w' x' y' z') n u v w x y z
setEdgeSequences f1 f2 f3 f4 f5 f6 edgeMappingVec (PDAG2 dag meta) = PDAG2 updatedDAG meta
  where
    -- Looks like this value is bad and can't be used.
--    edgeMapping  = p ^. _virtualNodeMapping
    refVec       = references dag
    updatedDAG   = dag { references = updatedEdges }
    updatedEdges = updateEdgeData <#$> zip refVec edgeMappingVec

    updateEdgeData
      :: Int
      -> ( IndexData
             e
             (PhylogeneticNode (CharacterSequence u v w x y z) n)
         , HashMap
             EdgeReference
             (ResolutionCache (CharacterSequence u'' v'' w'' x'' y'' z'))
         )
      -> IndexData
           (e, CharacterSequence u' v' w' x' y' z')
           (PhylogeneticNode (CharacterSequence u v w x y z) n)
    updateEdgeData _i (idx, _edgeMapping) = idx { childRefs = addEdgeSeq <#$> childRefs idx }
      where
        thisSeq  = getDatum idx :: CharacterSequence u v w x y z
        getDatum = characterSequence . NE.head . resolutions . nodeDecoration
        addEdgeSeq j v = (v, edgeSeq)
          where
            kidSeq :: CharacterSequence u v w x y z
            kidSeq = getDatum $ refVec ! j

{-
            rerootSeq = characterSequence . NE.head .
                fromMaybe (edgeMapping ! (j, i)) $ (i, j) `lookup` edgeMapping
-}

{-
            f1' m (a,b,_) = f1 m (a,b)
            f2' m (a,b,_) = f2 m (a,b)
            f3' m (a,b,_) = f3 m (a,b)
            f4' m (a,b,_) = f4 m (a,b)
            f5' m (a,b,_) = f5 m (a,b)
            f6' m (a,b,_) = f6 m (a,b)
--            f6' _ (_,_,c) = c
-}
            edgeSeq :: CharacterSequence u' v' w' x' y' z'
            edgeSeq = hexZipMeta f1 f2 f3 f4 f5 f6 meta $ hexZip thisSeq kidSeq

