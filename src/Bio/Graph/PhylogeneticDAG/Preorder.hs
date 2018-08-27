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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Bio.Graph.PhylogeneticDAG.Preorder
  ( preorderFromRooting
  , preorderSequence
  ) where

import           Bio.Character.Decoration.Dynamic
import           Bio.Character.Encodable
import           Bio.Graph.Node
import           Bio.Graph.PhylogeneticDAG.Internal
import           Bio.Graph.ReferenceDAG.Internal
import           Bio.Metadata
import           Bio.Sequence
import qualified Bio.Sequence.Block                 as BLK
import qualified Bio.Sequence.Character             as CS
import           Bio.Sequence.Metadata              (MetadataSequence)
import qualified Bio.Sequence.Metadata              as M
import           Control.Arrow                      ((&&&))
import           Control.Lens
import           Control.Monad.State.Lazy
import           Data.Bifunctor
import           Data.Foldable
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
import           Data.TopologyRepresentation
import           Data.Vector                        (Vector)
import qualified Data.Vector                        as V
import           Data.Vector.Instances              ()
import qualified Data.Vector.NonEmpty               as NEV
import           Prelude                            hiding (lookup)


type BlockTopologies = NEV.Vector TraversalTopology


type ParentalContext u v w x y z = NEV.Vector (TraversalTopology, Word, Maybe (BLK.CharacterBlock u v w x y z))


-- |
-- Applies a traversal logic function over a 'ReferenceDAG' in a /pre-order/ manner.
--
-- The logic function takes a current node decoration,
-- a list of parent node decorations with the logic function already applied,
-- and returns the new decoration for the current node.
preorderSequence
  :: HasBlockCost u  v  w  x  y  z
  => (ContinuousCharacterMetadataDec                      -> u -> [(Word, u')] -> u')
  -> (DiscreteCharacterMetadataDec                        -> v -> [(Word, v')] -> v')
  -> (DiscreteCharacterMetadataDec                        -> w -> [(Word, w')] -> w')
  -> (DiscreteWithTCMCharacterMetadataDec StaticCharacter -> x -> [(Word, x')] -> x')
  -> (DiscreteWithTCMCharacterMetadataDec StaticCharacter -> y -> [(Word, y')] -> y')
  -> (DynamicCharacterMetadataDec (Element DynamicChar)   -> z -> [(Word, z')] -> z')
  -> PhylogeneticDAG2 m e n u  v  w  x  y  z
  -> PhylogeneticDAG2 m e n u' v' w' x' y' z'
preorderSequence f1 f2 f3 f4 f5 f6 pdag@(PDAG2 dag meta) = PDAG2 (newDAG dag) meta
  where
    refs          = references dag
    dagSize       = length $ references dag
    newDAG        = RefDAG <$> const newReferences <*> rootRefs <*> constructDefaultMetadata
    newReferences = V.generate dagSize g
      where
        g i = IndexData <$> const (memo ! i) <*> parentRefs <*> childRefs $ refs ! i

    -- A "sequence" of the minimum topologies that correspond to each block.
    sequenceOfBlockMinimumTopologies :: BlockTopologies
    sequenceOfBlockMinimumTopologies = getSequenceOfBlockMinimumTopologies pdag

    -- Here we generate a memoized vector of the updated node decorations from
    -- the pre-order traversal. This memoization technique relies on lazy
    -- evaluation to compute the data for each vector index in the correct order
    -- of dependancy with the root node(s) as the base case(es).
--  memo :: Vector (PhylogeneticNode2 (CharacterSequence u' v' w' x' y' z') n)
    memo = V.generate dagSize g
      where

        -- This is the generating function.
        -- It computes the updated node decoration for a given index of the vector.
        g i = PNode2 newResolution nodeDatum
          where

            -- This is a singleton resolution cache to conform the the
            -- PhylogeneticNode2 type requirements. It is the part of that gets
            -- updated and requires a bunch of work to be performed.
            newResolution    = mockResInfo datumResolutions newSequence

            -- We just copy this value over from the previous decoration.
            nodeDatum        = nodeDecorationDatum2 $ nodeDecoration node

            -- The character sequence for the current index with the node decorations
            -- updated to thier pre-order values with their final states assigned.
            newSequence      = computeOnApplicableResolution f1 f2 f3 f4 f5 f6 meta parentalContext datumResolutions

            -- This is *really* important.
            -- Here is where we collect the parental context for the current node.
            --
            -- In the root node context where there are no parents, this is easy.
            -- We simply create the "sequence" with no information derived.
            --
            -- In the tree node case where there is only one parent, we grab the
            -- parent context via memoization and match each parent block with
            -- it's coresponding topology reprsentation.
            --
            -- In the network node case where there are two parents, we grab both
            -- of the parent contexts via memoization and then select the block
            -- from the parent that was connected to the current node on the
            -- minimal display tree for that block.
            parentalContext  = mapWithKey parentalAccessor sequenceOfBlockMinimumTopologies

            parentalAccessor =
                case parentIndices of
                  []    -> \_ x -> (x, 0, Nothing)
                  [p]   -> selectTopologyFromParentOptions $ (p, memo ! p):|[]
                  x:y:_ -> selectTopologyFromParentOptions $ (x, memo ! x):|[(y, memo ! y)]

            datumResolutions = resolutions $ nodeDecoration node

            node            = refs ! i
            parentIndices   = otoList $ parentRefs node
            -- In sparsely connected graphs (like ours) this will be effectively constant.
            childPosition j = toEnum . length . takeWhile (/=i) . IM.keys . childRefs $ refs ! j

            selectTopologyFromParentOptions
              :: NonEmpty (Int, PhylogeneticNode2 (CharacterSequence u v w x y z) n)
              -> Int
              -> TraversalTopology
              -> (TraversalTopology, Word, Maybe (BLK.CharacterBlock u v w x y z))
            selectTopologyFromParentOptions nodeOptions key topology =
                case NE.filter matchesTopology $ second (NE.head . resolutions) <$> nodeOptions of
                  (x,y):_ -> (topology, childPosition x, Just $ toNonEmpty (characterSequence y ^. blockSequence) ! key)
                  []      -> error $ unlines
                                 [ unwords ["No Matching topology for Block", show key, "on Node", show i]
                                 , "The minimal topologies for each block: " <> show sequenceOfBlockMinimumTopologies
                                 , "And this was the problem topology: " <> show topology
                                 , "And these were our options: " <> show (topologyRepresentation . NE.head . resolutions . snd <$> nodeOptions)
                                 ]
              where
--                matchesTopology = (`isCompatableWithTopology` topology) . topologyRepresentation . snd
                matchesTopology = (`notElem` excludedNetworkEdges topology) . (id &&& const i) . fst



    -- A "sequence" of the minimum topologies that correspond to each block.
getSequenceOfBlockMinimumTopologies
  :: HasBlockCost u v w x y z
  => PhylogeneticDAG2 m e n u v w x y z
  -> BlockTopologies
getSequenceOfBlockMinimumTopologies (PDAG2 dag meta) = getTopologies blockMinimalResolutions
      where
        getTopologies  = fmap topologyRepresentation

        blockMinimalResolutions = mapWithKey f $ sequenceWLOG ^. blockSequence

        sequenceWLOG   = characterSequence $ NE.head rootResolutions

        getMetaBlock i = M.toBlocks meta ! i

        f key _block   = minimumBy (comparing extractedBlockCost)
                         rootResolutions
          where
            extractedBlockCost = blockCost (getMetaBlock key) . (! key) . (^. blockSequence) . characterSequence

        rootResolutions = resolutions . nodeDecoration $ references dag ! rootWLOG

        rootWLOG = NE.head $ rootRefs dag


mockResInfo :: ResolutionCache s -> s' -> ResolutionCache s'
mockResInfo currentResolutions newSequence =
    -- Default the ResolutionInformation valus, insert the preorder sequence result
    pure .
      (ResInfo
        <$> totalSubtreeCost
        <*> localSequenceCost
        <*> leafSetRepresentation
        <*> subtreeRepresentation
        <*> subtreeEdgeSet
        <*> topologyRepresentation
        <*> const newSequence
      ) $ NE.head currentResolutions


computeOnApplicableResolution
  :: (ContinuousCharacterMetadataDec        -> u -> [(Word, u')] -> u')
  -> (DiscreteCharacterMetadataDec          -> v -> [(Word, v')] -> v')
  -> (DiscreteCharacterMetadataDec          -> w -> [(Word, w')] -> w')
  -> (DiscreteWithTCMCharacterMetadataDec StaticCharacter
       -> x -> [(Word, x')] -> x')
  -> (DiscreteWithTCMCharacterMetadataDec StaticCharacter
       -> y -> [(Word, y')] -> y')
  -> (DynamicCharacterMetadataDec (Element DynamicChar)
       -> z -> [(Word, z')] -> z')
  -> MetadataSequence m
  -> ParentalContext u' v' w' x' y' z'
  -> ResolutionCache (CharacterSequence u v w x y z)
  -> CharacterSequence u' v' w' x' y' z'
computeOnApplicableResolution f1 f2 f3 f4 f5 f6 meta parentalContexts currentResolutions =
    CS.fromNonEmpty $ zipWithKey f (meta ^. blockSequence) parentalContexts
--    over blockSequence (zipWithKey f parentalContexts) meta
  where
    f key metaBlock (topology, childRef, maybeParentBlock) = BLK.hexZipWithMeta f1 f2 f3 f4 f5 f6 metaBlock childBlock parentBlock
      where
        childBlock  = selectChildBlockByTopology currentResolutions key topology
        parentBlock =
            case maybeParentBlock of
              Just v  -> BLK.hexmap g g g g g g v
              Nothing -> BLK.hexmap h h h h h h childBlock

        g :: a -> [(Word, a)]
        g x = [(childRef, x)]

        h :: a -> [(Word, b)]
        h = const []

    selectChildBlockByTopology
      :: ResolutionCache (CharacterSequence u v w x y z)
      -> Int
      -> TraversalTopology
      -> BLK.CharacterBlock u v w x y z
    selectChildBlockByTopology childOptions key topology =
        case NE.filter matchesTopology childOptions of
          x:_ -> (characterSequence x ^. blockSequence) ! key
          []  -> (characterSequence (NE.head childOptions) ^. blockSequence) ! key
      where
        matchesTopology = (`isCompatableWithTopology` topology) . topologyRepresentation


selectApplicableResolutions :: TraversalTopology -> ResolutionCache s -> ResolutionInformation s
selectApplicableResolutions topology cache =
    case filter (\x -> topologyRepresentation x `isCompatableWithTopology` topology) $ toList cache of
      []  -> error $ unlines
                 [ "No applicable resolution found on pre-order traversal"
                 , "Input set:   " <> show topology
                 , "Local sets:  " <> show (subtreeEdgeSet <$> cache)
                 , "Local Topos: " <> show (topologyRepresentation <$> cache)
                 ]
      [x] -> x
      xs  -> maximumBy (comparing (length . subtreeEdgeSet)) xs


-- |
-- Different contexts used to mediate an effcient multidimensional traversal.
data  PreorderContext c
    = NormalNode   Int
    | SetRootNode  c
    | FociEdgeNode Int c


-- |
-- Applies a traversal logic function over a 'ReferenceDAG' in a /pre-order/ manner.
--
-- The logic function takes a current node decoration and
-- a list of parent node decorations with the logic function already applied,
-- and returns the new decoration for the current node.
preorderFromRooting
  :: (DynamicCharacterMetadataDec (Element DynamicChar) -> z -> [(Word, z')] -> z')
  ->         HashMap EdgeReference (ResolutionCache (CharacterSequence u v w x y z))
  -> Vector (HashMap EdgeReference (ResolutionCache (CharacterSequence u v w x y z)))
  -> NEV.Vector (TraversalTopology, Double, Double, Double, Vector (NonEmpty TraversalFocusEdge))
  -> PhylogeneticDAG2 m e n u' v' w' x' y' z
  -> PhylogeneticDAG2 m e n u' v' w' x' y' z'
preorderFromRooting transformation edgeCostMapping contextualNodeDatum minTopologyContextPerBlock (PDAG2 dag meta) = PDAG2 (newDAG dag) meta
  where
    newDAG        = RefDAG <$> const newReferences <*> rootRefs <*> reconstructMetadata
    newReferences = V.generate nodeCount g
      where
        g i = IndexData <$> const (memo ! i) <*> parentRefs <*> childRefs $ refs ! i

    reconstructMetadata = buildMetaData . graphData
      where
        buildMetaData =
          GraphData
            <$> dagCost
            <*> networkEdgeCost
            <*> rootingCost
            <*> totalBlockCost
            <*> const (mempty, mempty, Just $ toNonEmpty minTopologyContextPerBlock)

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
    -- For each Node, for each block, for each dynamic character, parent ref index or root datum.
    -- parentVectors :: Matrix (Vector (Either (c, Int) Int))
    parentVectors = MAT.matrix nodeCount blockCount g
      where
        g (nodeIndex, blockIndex) = (! nodeIndex) <$> dynCharVec
          where
            dynCharVec = parentMapping ! blockIndex

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
--        delta :: (Keyed f, Keyed v, Foldable r) => f (TraversalTopology, v (r TraversalFocusEdge)) -> f (v (IntMap (Either (c, Int) Int)))
        delta = mapWithKey (\k (topo, _, _, _, v) -> mapWithKey (f topo k) v)
          where
            f topo blockIndex charIndex = foldMap epsilon
              where
                epsilon rootingEdge@(r1,r2) = lhs <> rhs <> gen mempty (r1,r2) <> gen mempty (r2,r1)
                  where
                    lhs = IM.singleton r1 $ FociEdgeNode r2 virtualRootDatum
                    rhs = IM.singleton r2 $ FociEdgeNode r1 virtualRootDatum
                    virtualRootDatum = (! charIndex) . (! blockIndex) $ getDynCharSeq virtualRoot
                    -- TODO: What if there are no applicable resolutions?
                    virtualRoot = head . NE.filter (\x -> topologyRepresentation x == topo) $ edgeCostMapping ! rootingEdge
                    excludedEdges = excludedNetworkEdges topo
                    gen seenSet (n1,n2)
                      | isExcludedEdge = mempty
                      | otherwise      = (currentVal <>) . foldMap toMap . filter (`onotElem` seenSet') $ getAdjacentNodes n2
                      where
                        isExcludedEdge = (n1,n2) `elem` excludedEdges || (n2,n1) `elem` excludedEdges
                        currentVal
                          | n2 `oelem` rootSet = IM.singleton n2 $ SetRootNode virtualRootDatum
                          | n1 `oelem` rootSet =
                                         case toList . headMay . filter (/=n2) . IM.keys . childRefs $ refs ! n1 of
                                           x:_ -> IM.singleton n2 $ NormalNode x
                                           []  -> IM.singleton n2 $ NormalNode n1 -- I guess it's just the root and a single leaf...?
                          | otherwise          =  IM.singleton n2 $ NormalNode n1
                        toMap v     = gen seenSet' (n2, v)
                        seenSet'    = IS.insert n1 seenSet


    -- Here we generate a memoized vector of the updated node decorations from
    -- the pre-order traversal. This memoization technique relies on lazy
    -- evaluation to compute the data for each vector index in the correct order
    -- of dependancy with the root node(s) as the base case(es).
    --
    -- Unlike 'preorderSequence' this memoized vector only updates the dynamic
    -- characters.
    memo = V.generate nodeCount gen
      where

        -- This is the generating function.
        -- It computes the updated node decoration for a given index of the vector.
        -- gen :: Int -> PhylogeneticNode2 (CharacterSequence u' v' w' x' y' z') n'
        gen i = PNode2 newResolution nodeDatum
          where

            -- We just copy this value over from the previous decoration.
            nodeDatum        = nodeDecorationDatum2 $ nodeDecoration node

            -- This is a singleton resolution cache to conform to the
            -- PhylogeneticNode2 type requirements. It is the part that gets
            -- updated, and requires a bunch of work to be performed.
            -- Remember, this only updates the dynamic characters.
            newResolution    = pure . updateDynamicCharactersInSequence $ NE.head datumResolutions

            datumResolutions = resolutions $ nodeDecoration node

            node             = refs ! i

            kids             = IM.keys $ childRefs node

            updateDynamicCharactersInSequence resInfo = resInfo { characterSequence = updatedCharacterSequence }
              where
                updatedCharacterSequence = over blockSequence (zipWithKey blockGen (meta ^. blockSequence)) $ characterSequence resInfo

                blockGen j mBlock cBlock = cBlock & dynamicBin .~ updatedDynamicCharacters
                  where
                    (topology,_,_,_,_) = minTopologyContextPerBlock ! j
                    excludedEdges = excludedNetworkEdges topology
                    updatedDynamicCharacters = mapWithKey dynCharGen $ mBlock ^. dynamicBin

                    dynCharGen k m =
                        case parentRefContext of
                          SetRootNode  x -> transformation m x []
                          FociEdgeNode p x ->
                            let currentContext     = selectApplicableResolutions topology $ (contextualNodeDatum .!>. i) .!>. (p,i)
                                currentDecoration  = (!k) . (^. dynamicBin) . (!j) . (^. blockSequence) . characterSequence $ currentContext
                                parentalDecoration = transformation m x []
                            in  transformation m currentDecoration [(0, parentalDecoration)]
                          NormalNode   p   ->
                            let isDeadEndNode = -- This only checks one edge away, probably should be transitive.
                                  case kids of
                                    [c] -> (c,i) `elem` excludedEdges || (i,c) `elem` excludedEdges
                                    _   -> False
                                currentContext     = selectApplicableResolutions topology $ (contextualNodeDatum .!>. i) .!>. (p,i)
                                currentDecoration  = (!k) . (^. dynamicBin) . (!j) . (^. blockSequence) . characterSequence $ currentContext
                                parentalDecoration = getDynCharDecoration . NE.head . resolutions $ memo ! p
                            in  if   isDeadEndNode
                                then parentalDecoration
                                else transformation m currentDecoration [(0, parentalDecoration)]
                      where
                        parentRefContext     = (parentVectors ! (i,j)) ! k
                        -- Stupid monomorphisms prevent elegant code reuse
                        getDynCharDecoration = (!k) . (^. dynamicBin) . (!j) . (^. blockSequence) . characterSequence



(.!>.) :: (Lookup f, Show (Key f)) => f a -> Key f -> a
(.!>.) s k = fromMaybe (error $ "Could not index: " <> show k) $ k `lookup` s


constructDefaultMetadata :: (Monoid a, Monoid b) => ReferenceDAG d e n -> GraphData (a, b, Maybe c)
constructDefaultMetadata = ((mempty, mempty, Nothing) <$) . graphData
