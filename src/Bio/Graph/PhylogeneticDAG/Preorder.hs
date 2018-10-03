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
  ( preorderSequence''
  , preorderFromRooting
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
import           Control.Arrow                      ((&&&))
import           Control.Lens
import           Control.Lens.At                    (ix)
import           Control.Lens.Combinators           (singular)
import           Control.Monad.State.Lazy
import           Data.Bifunctor
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
import           Prelude                            hiding (lookup)


type BlockTopologies = NEV.Vector TraversalTopology


type ParentalContext u v w x y z = NEV.Vector (TraversalTopology, Word, Maybe (BLK.CharacterBlock u v w x y z))


-- |
-- Applies a traversal logic function over a 'ReferenceDAG' in a /pre-order/ manner.
--
-- The logic function takes a current node decoration,
-- a list of parent node decorations with the logic function already applied,
-- and returns the new decoration for the current node.
--
-- *The better version.*
preorderSequence'' ::
  forall m e n u v w x y z u' v' w' x' y' z' . HasBlockCost u  v  w  x  y  z
  => (ContinuousCharacterMetadataDec        -> u -> [(Word, u')] -> u')
  -> (DiscreteCharacterMetadataDec          -> v -> [(Word, v')] -> v')
  -> (DiscreteCharacterMetadataDec          -> w -> [(Word, w')] -> w')
  -> (DiscreteWithTCMCharacterMetadataDec StaticCharacter
       -> x -> [(Word, x')] -> x')
  -> (DiscreteWithTCMCharacterMetadataDec StaticCharacter
       -> y -> [(Word, y')] -> y')
  -> (DynamicCharacterMetadataDec (Element DynamicChar)
       -> z -> [(Word, z')] -> z')
  -> PhylogeneticDAG2 m e n u  v  w  x  y  z
  -> PhylogeneticDAG2 m e n u' v' w' x' y' z'
preorderSequence'' f1 f2 f3 f4 f5 f6 pdag2@(PDAG2 dag meta) = pdag2 & _phylogeneticForest .~ newRDAG
  where
    refs          = references dag
    dagSize       = length $ references dag
    newRDAG       = dag & _references .~ newReferences
                        & _graphData  %~ setDefaultMetadata
    newReferences = VE.generate dagSize g
      where
        g i =
          (refs ! i) & _nodeDecoration .~ (memo ! i)

    -- A "sequence" of the minimum topologies that correspond to each block.
    sequenceOfBlockMinimumTopologies :: BlockTopologies
    sequenceOfBlockMinimumTopologies = getSequenceOfBlockMinimumTopologies pdag2

    -- Here we generate a memoized vector of the updated node decorations from
    -- the pre-order traversal. This memoization technique relies on lazy
    -- evaluation to compute the data for each vector index in the correct order
    -- of dependancy with the root node(s) as the base case(es).
    memo :: Vector (PhylogeneticNode2 (CharacterSequence u' v' w' x' y' z') n)
    memo = VE.generate dagSize g
      where

        -- This is the generating function.
        -- It computes the updated node decoration for a given index of the vector.
        g currInd =
          (node ^. _nodeDecoration) & _resolutions .~ newResolution
          where

            node            = refs ! currInd

            -- This is a singleton resolution cache to conform the the
            -- PhylogeneticNode2 type requirements. It is the part of that gets
            -- updated and requires a bunch of work to be performed.
            newResolution    = mockResInfo datumResolutions newSequence

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

            datumResolutions = node ^. _nodeDecoration . _resolutions

            parentIndices   = otoList $ parentRefs node
            -- In sparsely connected graphs (like ours) this will be effectively constant.
            childPosition j = toEnum . length . takeWhile (/=currInd) . IM.keys . childRefs $ refs ! j

            selectTopologyFromParentOptions
              :: NonEmpty (Int, PhylogeneticNode2 (CharacterSequence u1 v1 w1 x1 y1 z1) n)
              -> Int
              -> TraversalTopology
              -> (TraversalTopology, Word, Maybe (BLK.CharacterBlock u1 v1 w1 x1 y1 z1))
            selectTopologyFromParentOptions nodeOptions key topology =
                case NE.filter matchesTopology $ second (NE.head . resolutions) <$> nodeOptions of
                  (x,y):_ -> (topology, childPosition x, Just $ toNonEmpty (characterSequence y ^. blockSequence) ! key)
                  []      -> error $ unlines
                                 [ unwords ["No Matching topology for Block", show key, "on Node", show currInd]
                                 , "The minimal topologies for each block: " <> show sequenceOfBlockMinimumTopologies
                                 , "And this was the problem topology: " <> show topology
                                 , "And these were our options: " <> show (topologyRepresentation . NE.head . resolutions . snd <$> nodeOptions)
                                 ]
              where
                matchesTopology :: (Int, b) -> Bool
                matchesTopology (parInd, _) = (parInd, currInd) `notElem` excludedNetworkEdges topology



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

        getMetaBlock i = (meta ^. blockSequence) ! i

        f key _block   = minimumBy (comparing extractedBlockCost)
                         rootResolutions
          where
            extractedBlockCost = blockCost (getMetaBlock key) . (! key) . (^. blockSequence) . characterSequence

        rootResolutions = resolutions . nodeDecoration $ references dag ! rootWLOG

        rootWLOG = NE.head $ rootRefs dag



{--
-- |
-- Applies a traversal logic function over a 'ReferenceDAG' in a /pre-order/ manner.
--
-- The logic function takes a current node decoration,
-- a list of parent node decorations with the logic function already applied,
-- and returns the new decoration for the current node.
preorderSequence'
  :: HasBlockCost u  v  w  x  y  z
  => (u -> [(Word, u')] -> u')
  -> (v -> [(Word, v')] -> v')
  -> (w -> [(Word, w')] -> w')
  -> (x -> [(Word, x')] -> x')
  -> (y -> [(Word, y')] -> y')
  -> (z -> [(Word, z')] -> z')
  -> PhylogeneticDAG2 m e n u  v  w  x  y  z
  -> PhylogeneticDAG2 m e n u' v' w' x' y' z'
preorderSequence' f1 f2 f3 f4 f5 f6 pdag@(PDAG2 dag m) = PDAG2 (newDAG dag) m
  where
    newDAG        = RefDAG <$> const newReferences <*> rootRefs <*> constructDefaultMetadata
    dagSize       = length $ references dag
    newReferences = VE.generate dagSize g
      where
        g i = IndexData <$> const (snd $ memo ! i) <*> parentRefs <*> childRefs $ references dag ! i

    sequenceOfBlockMinimumTopologies :: BlockTopologies
    sequenceOfBlockMinimumTopologies = getSequenceOfBlockMinimumTopologies pdag

--  memo :: Vector (BlockTopologies, PhylogeneticNode2 n (CharacterSequence u' v' w' x' y' z'))
    memo = VE.generate dagSize g
      where
        g i = (inheritedToplogies,
            PNode2
            { resolutions          = newResolution
            , nodeDecorationDatum2 = nodeDecorationDatum2 $ nodeDecoration node
            }
            )
          where
            (inheritedToplogies, newResolution)
              | i `elem` rootRefs dag =
                let newSequence = computeOnApplicableResolution f1 f2 f3 f4 f5 f6 sequenceOfBlockMinimumTopologies datumResolutions []
                in (sequenceOfBlockMinimumTopologies, mockResInfo datumResolutions newSequence)
              | otherwise             =
                let newSequence = computeOnApplicableResolution f1 f2 f3 f4 f5 f6 parentalToplogies datumResolutions parentalResolutions
                in  (parentalToplogies, mockResInfo datumResolutions newSequence)

            datumResolutions = resolutions $ nodeDecoration node

            node            = references dag ! i
            parentIndices   = otoList $ parentRefs node
            -- In sparsely connected graphs (like ours) this will be effectively constant.
            childPosition j = toEnum . length . takeWhile (/=i) . IM.keys . childRefs $ references dag ! j
            parentContexts  = (\x -> second (const (childPosition x) &&& NE.head . resolutions) $ memo ! x) <$> parentIndices
            parentalResolutions = snd <$> parentContexts
            parentalToplogies   = fst $ head parentContexts
--}

>>>>>>> master
mockResInfo :: ResolutionCache s -> s' -> ResolutionCache s'
mockResInfo currentResolutions newSequence =
    -- Default the ResolutionInformation valus, insert the preorder sequence result
    pure $
      NE.head currentResolutions & _characterSequence .~ newSequence



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
    | NoBlockData


generateDotFile :: Show n => PhylogeneticDAG2 m e n u v w x y z -> String
generateDotFile = (<> "\n") . L.unpack . renderDot . toDot


-- |
-- Applies a traversal logic function over a 'ReferenceDAG' in a /pre-order/ manner.
--
-- The logic function takes a current node decoration and
-- a list of parent node decorations with the logic function already applied,
-- and returns the new decoration for the current node.
preorderFromRooting''
  :: forall m u v w x y z e' n' u' v' w' x' y' z'.
    (DynamicCharacterMetadataDec (Element DynamicChar) -> z -> [(Word, z')] -> z')
  ->         HashMap EdgeReference (ResolutionCache (CharacterSequence u v w x y z))
  -> Vector (HashMap EdgeReference (ResolutionCache (CharacterSequence u v w x y z)))
  -> NonEmpty (TraversalTopology, Double, Double, Double, Vector (NonEmpty TraversalFocusEdge))
  -> PhylogeneticDAG2 m e' n' u' v' w' x' y' z
  -> PhylogeneticDAG2 m e' n' u' v' w' x' y' z'
preorderFromRooting'' transformation edgeCostMapping contextualNodeDatum minTopologyContextPerBlock pdag2@(PDAG2 dag meta)
  =  pdag2 & _phylogeneticForest .~ newRDAG
  where
    newRDAG
      :: ReferenceDAG
      (PostorderContextualData (CharacterSequence u' v' w' x' y' z')) e' (PhylogeneticNode2 (CharacterSequence u' v' w' x' y' z') n')
    newRDAG =
         dag & _references .~ newReferences
             & _graphData  %~ buildMetaData

    newReferences = VE.generate nodeCount g

      where
        g i = (refs ! i) & _nodeDecoration .~ (memo ! i)


    buildMetaData
      :: GraphData (PostorderContextualData (CharacterSequence u' v' w' x' y' z))
      -> GraphData (PostorderContextualData (CharacterSequence u' v' w' x' y' z'))
    buildMetaData gD =
      gD & setDefaultMetadata
         & _graphMetadata . _minimalNetworkContext ?~ minTopologyContextPerBlock

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

            (!!!) v i = fromMaybe
                (error $ mconcat
                    [ "Can't index at "
                    , show i
                    , " when given "
                    , show e
                    , "\nIn the IntMap: "
                    , show $ IM.keysSet <$> dynCharVec
                    , "\n\n"
                    , generateDotFile pdag
                    ]
                ) $ i `lookup` v

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
                        case NE.filter (\x -> topologyRepresentation x == topo) $ edgeCostMapping ! rootingEdge of
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
    -- of dependancy with the root node(s) as the base case(es).
    --
    -- Unlike 'preorderSequence' this memoized vector only updates the dynamic
    -- characters.

    memo :: Vector (PhylogeneticNode2 (CharacterSequence u' v' w' x' y' z') n')
    memo = VE.generate nodeCount gen
      where

        -- This is the generating function.
        -- It computes the updated node decoration for a given index of the vector.
        gen :: Int -> PhylogeneticNode2 (CharacterSequence u' v' w' x' y' z') n'
        gen i = (node ^. _nodeDecoration) & _resolutions .~ newResolution
          where

            -- This is a singleton resolution cache to conform to the
            -- PhylogeneticNode2 type requirements. It is the part that gets
            -- updated, and requires a bunch of work to be performed.
            -- Remember, this only updates the dynamic characters.
            newResolution :: ResolutionCache (CharacterSequence u' v' w' x' y' z')
            newResolution    = pure . updateDynamicCharactersInSequence $ NE.head datumResolutions

            datumResolutions = resolutions $ nodeDecoration node

            node             = refs ! i

            kids             = IM.keys $ childRefs node


            updateDynamicCharactersInSequence
              :: ResolutionInformation (CharacterSequence u1 v1 w1 x1 y1 z)
              -> ResolutionInformation (CharacterSequence u1 v1 w1 x1 y1 z')
            updateDynamicCharactersInSequence resInfo = resInfo { characterSequence = updatedCharacterSequence }
              where
                updatedCharacterSequence
                  = fromBlockVector
                  . zipWithKey blockGen (M.toBlockVector meta)
                  . toBlockVector
                  $ characterSequence resInfo

                blockGen j mBlock cBlock = setDynamicCharacters updatedDynamicCharacters cBlock
                  where
                    (topology,_,_,_,_) = minTopologyContextPerBlock ! j
                    excludedEdges = excludedNetworkEdges topology
                    updatedDynamicCharacters
                      = zipWithKey dynCharGen (getDynamicMetadata mBlock)
                      $ dynamicCharacters cBlock

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
                          NoBlockData      -> error "This is bad and sad plus I'm mad."
                          SetRootNode  x   -> transformation m x []
                          FociEdgeNode p x ->
                            let currentContext
                                  = selectApplicableResolutions topology
                                  $ (contextualNodeDatum .!>. i) .!>. (p,i)

                                currentDecoration
                                  = (!k)
                                  . dynamicCharacters
                                  . (!j)
                                  . toBlockVector
                                  . characterSequence
                                  $ currentContext

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

                        getDynCharDecoration = (!k) . dynamicCharacters . (!j) . toBlockVector . characterSequence

{--
-- |
-- Applies a traversal logic function over a 'ReferenceDAG' in a /pre-order/ manner.
--
-- The logic function takes a current node decoration and
-- a list of parent node decorations with the logic function already applied,
-- and returns the new decoration for the current node.
preorderFromRooting
  :: ( HasBlockCost u  v  w  x  y  z
     , HasBlockCost u' v' w' x' y' z'
     , HasTraversalFoci z  (Maybe TraversalFoci)
     , HasTraversalFoci z' (Maybe TraversalFoci)
--     , Show z
     )
  => (z -> [(Word, z')] -> z')
  ->         HashMap EdgeReference (ResolutionCache (CharacterSequence u v w x y z))
  -> Vector (HashMap EdgeReference (ResolutionCache (CharacterSequence u v w x y z)))
  -> PhylogeneticDAG2 m e' n' u' v' w' x' y' z
  -> PhylogeneticDAG2 m e' n' u' v' w' x' y' z'
preorderFromRooting f edgeCostMapping contextualNodeDatum (PDAG2 dag meta) = PDAG2 (newDAG dag) meta
  where
    newDAG        = RefDAG <$> const newReferences <*> rootRefs <*> constructDefaultMetadata
    dagSize       = length $ references dag
    roots         = rootRefs dag
    newReferences = VE.generate dagSize g
      where
        g i =
            IndexData
              <$> (applyNewDynamicCharacters (memo ! i) . nodeDecoration)
              <*> parentRefs
              <*> childRefs
              $ references dag ! i

    applyNewDynamicCharacters dynCharSeq oldNode = oldNode { resolutions = pure newResolution }
      where
        oldResolution = NE.head $ resolutions oldNode
        oldSequence   = characterSequence oldResolution
        newSequence   = fromBlocks . zipWith setDynamicCharacters dynCharSeq $ toBlocks oldSequence
        newResolution = oldResolution { characterSequence = newSequence }


    -- |
    -- For each block, for each dynamic character, a vector of parent ref indicies.
--    parentVectors :: NonEmpty (Vector (Vector (Either Int (Int, ResolutionCache (CharacterSequence u v w x y z)))))
    parentVectors = {-
                  trace "after force !!"
                  . (\x -> trace ("before force !!" <> show (fmap (fmap (fmap (fmap (fmap (const ()))))) x)) x
                    )
                  -}
                    mapWithKey deriveParentVectors sequenceOfBlockMinimumTopologies
      where
        rootEdges    = toList $ undirectedRootEdgeSet   dag
        treeEdges    = toList $ referenceTreeEdgeSet    dag
        deriveParentVectors k (topo, dynchars) = mapWithKey h dynchars
          where
            h charIndex rootEdge@(lhsRootRef, rhsRootRef) = VE.generate dagSize g
              where
--                g i | trace (unwords [show i, "/", show $ length dag, show rootEdge, show $ IM.keys parentalMapping]) False = undefined
                g i = parentalMapping ! i

                parentalMapping = lhs <> rhs
                  where
                    -- TODO: Get the appropriate resolution here!
                    lhs = IM.singleton lhsRootRef (Right (rhsRootRef, val)) <> genMap (IS.singleton rhsRootRef) lhsRootRef
                    rhs = IM.singleton rhsRootRef (Right (lhsRootRef, val)) <> genMap (IS.singleton lhsRootRef) rhsRootRef
--                    genMap _  j | trace (show j) False = undefined
                    genMap is j = foldMap (\x -> IM.singleton x $ Left j) kids <> foldMap (genMap (IS.insert j is)) kids
                      where
                        kids  = catMaybes $ nextEdges j is <$> topoEdges
                        topoEdges = toList (includedNetworkEdges topo) <> treeEdges <> rootEdges

                    val = (! charIndex) . dynamicCharacters
                          -- Get the appropriate block from the resolution that contains this character
                        . (! k) . toBlocks . characterSequence
                          -- Get the appropriate resolution based on this character's display tree toplogy
                        . selectApplicableResolutions topo $ edgeCostMapping ! rootEdge

                    nextEdges i is (x,y)
                      | x == i && isValid x = Just y
                      | y == i && isValid y = Just x
                      | otherwise           = Nothing
                      where
                        isValid j = j `onotElem` is && j `notElem` roots


    -- A "sequence" of the minimum topologies that correspond to each block.
    sequenceOfBlockMinimumTopologies :: NonEmpty (TraversalTopology, Vector (Int, Int))
    sequenceOfBlockMinimumTopologies = --trace "after force" $ force (trace "before force" blockMinimalResolutions)
        blockMinimalResolutions
      where
        blockMinimalResolutions = mapWithKey g $ toBlocks sequenceWLOG

        sequenceWLOG = characterSequence $ NE.head datumResolutions

        datumResolutions = resolutions . nodeDecoration $ references dag ! rootWLOG

        g key _block = (topologyRepresentation &&& grabTraversalFoci)
                     $ minimumBy (comparing extractedBlockCost) datumResolutions
          where
            getBlock           = (! key) . toBlocks . characterSequence
            extractedBlockCost = blockCost (M.toBlocks meta ! key) . getBlock
            grabTraversalFoci  = fmap (fst . NE.head . fromJust . (^. traversalFoci)) . dynamicCharacters . getBlock


    rootWLOG = NE.head $ rootRefs dag

--  applyMetadata :: NonEmpty (Vector z') -> NonEmpty (Vector z')
    applyMetadata = zipWith g sequenceOfBlockMinimumTopologies
      where
        g (topo, foci) = zipWith h foci
          where
            h focus dec = dec & traversalFoci ?~ pure (focus, topo)
--  memo :: Vector (NonEmpty (Vector z'))
    memo = VE.generate dagSize generateDatum
      where
        generateDatum i
          | i `notElem` rootRefs dag = applyMetadata $ zipWith (zipWith f) childCharSeqOnlyDynChars parentCharSeqOnlyDynChars
          | otherwise                = applyMetadata $ memo ! adjacentIndex
          where
            adjacentIndex = head . IM.keys . childRefs $ references dag ! i

--          parentCharSeqOnlyDynChars :: NonEmpty (Vector [a])
            parentCharSeqOnlyDynChars = mapWithKey g parentVectors
              where
                g k = mapWithKey h
                  where
                    h j x = [(0,dec)] -- Aways labeled as the first child (0) of the parent is technically incorrect. Probably won't matter, probably.
                      where
                        dec =
                            case x ! i of
                              Right (_, y) -> f y []
                              Left  p      -> if i == p
                                              then error $ "Recursive memoizeation for " <> show i
                                              else (! j) . (! k) $ memo ! p


--          childCharSeqOnlyDynChars   :: NonEmpty (Vector a)
            childCharSeqOnlyDynChars = zipWithKey g parentVectors $ fst <$> sequenceOfBlockMinimumTopologies
              where
                -- FoldMap is a bit inefficient with Vectors here, worry about it later.
                g k v topology = mapWithKey h v
                  where
                          -- Get this character from the block
                    h j x = (ere there are two parents, we grab both
            -- of the parent contexts via memoization and then select the block
            -- from the parent that was connected to the current node on the
            -- minimal display tree for that block.
            parentalContext  = mapWithKey parentalAccessor sequenceOfBlockMinimumTopologies

            parentalAccessor =
                case parentIndices of
                  []    -> \_ x -> (x, 0, Nothing)
                  [p]   -> selectTopologyFromParentOptions $ (p, memo ! p):|[]
                  x:y:_ -> selectTopologyFromParentOptions $ (x, memo ! x):|[(y, memo ! y)]

            datumResolutions = node ^. _nodeDecoration . _resolutions

            parentIndices   = otoList $ parentRefs node
            -- In sparsely connected graphs (like ours) this will be effectively constant.
            childPosition j = toEnum . length . takeWhile (/=i) . IM.keys . childRefs $ refs ! j

            selectTopologyFromParentOptions
              :: NonEmpty (Int, PhylogeneticNode2 (CharacterSequence u1 v1 w1 x1 y1 z1) n)
              -> Int
              -> TraversalTopology
              -> (TraversalTopology, Word, Maybe (BLK.CharacterBlock u1 v1 w1 x1 y1 z1))
            selectTopologyFromParentOptions nodeOptions key topology =
                case NE.filter matchesTopology $ second (NE.head . resolutions) <$> nodeOptions of
                  (x,y):_ -> (topology, childPosition x, Just $ toBlocks (characterSequence y) ! key)
                  []      -> error $ unlines
                                 [ unwords ["No Matching topology for Block", show key, "on Node", show i]
                                 , "The minimal topologies for each block: " <> show sequenceOfBlockMinimumTopologies
                                 , "And this was the problem topology: " <> show topology
                                 , "And these were our options: " <> show (topologyRepresentation . NE.head . resolutions . snd <$> nodeOptions)
                                 ]
              where
                matchesTopology :: (Int, b) -> Bool
                matchesTopology = (`notElem` exclu! j) . dynamicCharacters
                          -- Get the appropriate block from the resolution that contains this character
                          . (! k) . toBlocks . characterSequence
                          -- Get the appropriate resolution based on this character's display tree toplogy
                          $ selectApplicableResolutions topology directedResolutions
                      where
                        directedResolutions = --(contextualNodeDatum ! i) ! (trace (unwords ["\nkey:",show i,"sub-key:",show (p,i),"\nmapping sub-keys:",show (M.keys $ contextualNodeDatum ! i)])) (p,i)
                                               (contextualNodeDatum .!>. i) .!>. (p,i)
                        p = case x ! i of
                              Right (n,_) -> n
                              Left  n     -> n
--}

(.!>.) :: (Lookup f, Show (Key f)) => f a -> Key f -> a
(.!>.) s k = fromMaybe (error $ "Could not index: " <> show k) $ k `lookup` s
