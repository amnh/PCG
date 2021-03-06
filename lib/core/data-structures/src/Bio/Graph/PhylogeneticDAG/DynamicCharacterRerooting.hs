------------------------------------------------------------------------------
-- |
-- Module      :  Bio.Graph.PhylogeneticDAG.DynamicCharacterRerooting
-- Copyright   :  (c) 2015-2021 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds   #-}

module Bio.Graph.PhylogeneticDAG.DynamicCharacterRerooting
  ( assignOptimalDynamicCharacterRootEdges
  ) where

import           Bio.Character.Decoration.Additive
import           Bio.Character.Decoration.Dynamic
import           Bio.Character.Encodable
import           Bio.Graph.Node
import           Bio.Graph.Node.Context
import           Bio.Graph.PhylogeneticDAG.Internal
import           Bio.Graph.ReferenceDAG.Internal
import           Bio.Metadata.Dynamic
import           Bio.Sequence
import qualified Bio.Sequence.Metadata              as M
import           Control.Applicative
import           Control.Arrow                      ((&&&))
import           Control.Lens
import           Control.Monad.State.Lazy
import           Data.Foldable
import           Data.Foldable.Custom               (sum')
import           Data.GraphViz.Printing
import           Data.HashMap.Lazy                  (HashMap)
import qualified Data.HashMap.Lazy                  as HM
import qualified Data.IntMap                        as IM
import qualified Data.IntSet                        as IS
import           Data.Key
import           Data.List.NonEmpty                 (NonEmpty(..))
import           Data.Maybe
import           Data.MonoTraversable
import           Data.Semigroup
import           Data.Semigroup.Foldable
import           Data.Text.Lazy                     (unpack)
import           Data.Tuple                         (swap)
import           Data.Vector                        (Vector)
import qualified Data.Vector                        as V
import qualified Data.Vector.NonEmpty               as NEV
import           Prelude                            hiding (lookup, zipWith)
import           TextShow


-- |
-- For every edge in the component:
--
-- * If the edge is *not* a network edge:
--
-- The re-rooting candidate cost for that edge (for a character) is the minimum
-- cost of the Cartesian product of the resolutions of the adjacent nodes.
--
-- * If the edge *is* a network edge:
--
-- The re-rooting candidate cost for that edge (for a character) is the minimum
-- cost of the Cartesian product of the resolutions of the adjacent nodes minus any
-- resolutions that contain the incident network edge contained on the current
-- network edge.
assignOptimalDynamicCharacterRootEdges
  :: ( HasBlockCost u v w x y z
     , TextShow n
     , TextShow u
     , TextShow v
     , TextShow w
     , TextShow x
     , TextShow y
     , TextShow z
     )
  => (DynamicCharacterMetadataDec (Subcomponent (Element DynamicCharacter))
  -> PostorderContext z z -> z)  -- ^ Post-order traversal function for Dynamic Characters.
  -> PhylogeneticDAG m e n u v w x y z
  -> ( PhylogeneticDAG m e n u v w x y z
     ,         HashMap EdgeReference (ResolutionCache (CharacterSequence u v w x y z))
     , Vector (HashMap EdgeReference (ResolutionCache (CharacterSequence u v w x y z)))
     )
assignOptimalDynamicCharacterRootEdges extensionTransformation pdag@(PDAG2 inputDag meta) =
      -- degenerate cases
    case toList inputDag of
      []      ->     (pdag, mempty, mempty)
      [_]     ->     (pdag, mempty, mempty)
      -- Trivial case
      [_,_]   -> let r = ((0,1), getCache 1)
                     c = ((1,0), getCache 0)
                     m = HM.fromList [r, c]
                     f = pure ((1,0), mempty) :: TraversalFoci
                     meta' = omap (M.setAllFoci f) meta
                 in  (PDAG2 inputDag meta', m, V.generate 2 (const m))
      -- Complex case, see four steps below.
      _:_:_:_ ->     (PDAG2 updatedDag updatedMetadata, edgeCostMapping, contextNodeDatum)
  where

    -- Step 1: Construct a hashmap of all the *unrooted* edges.
    unrootedEdges = rootEdgeReferences <> otherUnrootedEdges

    -- Step 2: Create a lazy, memoized hashmap of the edge costs for each dynamic character.
    edgeCostMapping = referenceEdgeMapping

    -- Step 3: For each display tree, for each dynamic character, find the
    -- minimal cost edge(s).
    minimalDisplayTreeRerootings :: HashMap TraversalTopology (NonEmpty (Double, Vector (Word, NonEmpty TraversalFocusEdge)))
    minimalDisplayTreeRerootings = displayTreeRerooting

    -- Step 4: Update the dynamic character decoration's cost & add an edge reference.
    updatedDag = inputDag
        { references = refVec V.// toList modifiedRootRefs
        , graphData  =
          graphData inputDag & _graphMetadata .~
                                 PostorderContextualData
                                 { virtualNodeMapping    = edgeCostMapping
                                 , contextualNodeDatum   = contextNodeDatum
                                 , minimalNetworkContext = Nothing
                                 }
        }

    --Step 5: Update the metadata sequence to contain all applicable TraversalFoci for each dynamic character.
    updatedMetadata = modifiedMetadataSequence

    -- These are the edges of the DAG, not including the current root edge,
    -- which may be be the optimal root for a given dynamic character.
    otherUnrootedEdges :: [EdgeReference]
    otherUnrootedEdges = foldMapWithKey f refVec
      where
        f i n
          -- Don't consider edges from a root node, as the edges are "artificial" in an unrooted context.
          | i `elem` rootRefs inputDag = []
          | otherwise                  = fmap (const i &&& id) . IM.keys $ childRefs n

    rootEdgeReferences = foldMap f $ rootRefs inputDag
      where
        f = toList . rootIndexToUnrootedIndex

    rootIndexToUnrootedIndex i =
        case IM.keys . childRefs $ refVec ! i of
          []    -> Nothing
          [x]   -> Just (i,x)
          x:y:_ -> Just (x,y)

    refVec = references inputDag

    roots  = rootRefs   inputDag

    -- We want to get the directionality of the edge in the original DAG to
    -- determine if the target node had multiple parents, and hence is a network
    -- edge.
    isNetworkEdge (a,b)
      | isRootEdgeOfDAG = parentCount b > 1
      | otherwise       = parentCount y > 1
      where
        isRootEdgeOfDAG = not . onull $ IS.intersection sharedParents rootRefSet
        rootRefSet      = foldMap1 IS.singleton $ rootRefs inputDag
        sharedParents   = IS.intersection (getParents a) (getParents b)
        getParents      = parentRefs . (refVec !)
        parentCount     = olength . getParents
        (_,y) =
          if   (b `elem`) . IM.keys . childRefs $ refVec ! a
          then (a,b)
          else (b,a)

    -- Given the unrooted edge, return a Just value containing the root reference
    -- if the edge had a root node assigned to it in the Phylogenetic DAG.
    -- Otherwise return a Nothing value if there was no root node on the unrooted
    -- edge.
    getRootingNode (i,j) = headMay . otoList $ iRefs `IS.intersection` jRefs
      where
        iRefs = parentRefs $ refVec ! i
        jRefs = parentRefs $ refVec ! j

--    referenceEdgeMapping :: HashMap TraversalFocusEdge (ResolutionCache (CharacterSequence u v w x y z))
    referenceEdgeMapping = HM.fromList $ f <$> unrootedEdges
      where
        f e@(i,j) =
            case getRootingNode e of
              Just r  -> (e, getCache r)
              Nothing ->
                  case liftA2 (,) lhsContext rhsContext of
                    Just (lhs, rhs) -> (e, localResolutionApplication extensionTransformation meta lhs rhs)
                    Nothing         -> error errorContext
          where
            lhsContext = (i `lookup` contextNodeDatum) >>= ((j,i) `lookup`)
            rhsContext = (j `lookup` contextNodeDatum) >>= ((i,j) `lookup`)
            errorContext = unlines
                [ "Could not find one or more of the contexts:"
                , unpack . renderDot $ toDot inputDag
                , ""
                , show inputDag
                , "Rooting Edge " <> show e
                , show $ HM.keys <$> contextNodeDatum
                ]

    getCache i = resolutions . nodeDecoration $ refVec ! i


    -- Construct the directed subtree resolutions for each memoized datum index 'n' in the vector 'memo'.
    -- E.g., given node 'n' in the following undirected subgraph store the directed subtree resolutions of
    -- the complete graph: '[(i,n),(j,n),(k,n)]'
    --
    -- >    (i)
    -- >     |
    -- >    (n)
    -- >   /   \
    -- > (j)   (k)
    --
    -- However, for each directed edge we must apply filtering to the resolutions
    -- of the memoized subtree.
    -- WLOG let (i,n) be our directed edge.
    --
    -- > (memo ! n) ! (i, n) = mconcat . fmap (filterEdges i) $
    -- > [ liftA2 (<>) ((memo ! j) ! (n, j)) ((memo ! k) ! (n, k))
    -- > , (filterEdges k) $ (memo ! j) ! (n, j)
    -- > , (filterEdges j) $ (memo ! k) ! (n, k)
    -- > ]
    --

--    contextualNodeDatum :: Vector (Map EdgeReference (ResolutionCache (CharacterSequence u v w x y z)))
    contextNodeDatum = V.generate (length refVec) generateMemoizedDatum
      where

        -- Determine if the memoized point is a root node of the phylogenetic DAG
        -- component. If so, we do not generate memoized directional edge data
        -- for the component root, because the component root has a forced direction
        -- on its incident edges.
        --
        -- If the memoized point corresponds to a non-root vertex in the phylogentic
        -- DAG component we will consider the subtree resolutions for entering the
        -- node on each edge.
        --
        -- There should only be 0, 1, or 3 directed subtree values at each
        -- memoized point.
        --
        -- - 0 values if the point is a root node of the component
        --
        -- - 1 value  if the point is a leaf node of the component
        --
        -- - 3 values if the point is an internal node of the component
        --

--        generateMemoizedDatum :: Int -> Map EdgeReference (ResolutionCache (CharacterSequence u v w x y z))
--        generateMemoizedDatum n | trace ("Memo-gen: " <> show n) False = undefined
        generateMemoizedDatum n
          -- Root node case
          | n `elem` roots         = mempty
          -- Leaf node case
          | null unrootedChildRefs = HM.singleton (parentRef, n) $ getCache n
          -- Internal node case
          | otherwise              = HM.fromList $ foldMap deriveDirectedEdgeDatum edgeCombinations
          where

            -- These are the child edge references from the DAG context.
            unrootedChildRefs = IM.keys .  childRefs $ refVec ! n

            -- Here we remove any root nodes that were in the parents' references
            -- of the DAG and replace them with undirected edges references.
            -- We never reference a "root" node of the component which contains
            -- inherently directed edges, only the sister node on the undirected
            -- edge.
            unrootedParentRefs = g <$> otoList originalRootingParentRefs
              where
                g candidate
                  | candidate `notElem` rootRefs inputDag = candidate
                  | otherwise = sibling
                  where
                    sibling =
                      -- Kludge for single leaf forests with a surperfluous root node.
                      -- Shouldn't ever execute the empty list case, but here for safety.
                      case filter (/=n) . IM.keys .  childRefs $ refVec ! candidate of
                         []  -> candidate
                         x:_ -> x

            originalRootingParentRefs = parentRefs $ refVec ! n

            -- WLOG, single parent/child reference
            parentRef = head unrootedParentRefs
            --childRef  = head unrootedChildRefs

            -- The adjacent vertex indices
            unrootedAdjacentRefs = take 3 $ take 2 unrootedChildRefs <> take 2 unrootedParentRefs

            -- All the combinations of adjacent edges. The first position in the
            -- tuple is the only position that matters because the operation on
            -- the subtree is commutative, hence swapping the last two elements
            -- of the tuple will result in the same value.
            edgeCombinations :: NonEmpty (Int, Int, Int)
            edgeCombinations = f unrootedAdjacentRefs
              where
                f [i,j,k] = (i,j,k) :| [(j,k,i),(k,i,j)]
                f xs = error $ unlines
                    [ "There were not exactly 3 adjacent nodes in a non-root, non-leaf re-rooting context."
                    , "The node under inspection was index: " <> show n
                    , "Expected exactly 3 adjacent nodes."
                    , "Found: {" <> show (length xs) <> "} " <> show xs
                    , referenceRendering inputDag
                    ]

            -- Given the three adjacent edges, generate the subtree resolutions
            -- defined by the first element of the tuple being an incoming edge.
--            deriveDirectedEdgeDatum :: (Int, Int, Int) -> Map EdgeReference (ResolutionCache (CharacterSequence u v w x y z))
--            deriveDirectedEdgeDatum (i,j,k) | trace ("derive directional: " <> show (i,j,k)) False = undefined
            deriveDirectedEdgeDatum (i,j,k) = [((i, n), subtreeResolutions)]
              where
                lhsMemo       = (contextNodeDatum ! j) .!>. (n, j)
                rhsMemo       = (contextNodeDatum ! k) .!>. (n, k)
                lhsContext    = edgeReferenceFilter [(k,n)] lhsMemo
                rhsContext    = edgeReferenceFilter [(j,n)] rhsMemo

--                localResolutionApplication2 f xs ys = localResolutionApplication f (trace ("LHS: " <> show (n,j)) xs) (trace ("RHS: " <> show (n,k)) ys)
                subtreeResolutions
--                  | trace ( unwords [show (i,n), show (n,j), show (n,k), show (i `elem` unrootedParentRefs)] ) False = undefined
                  -- Check for the recursive memoized form's base case
                  | i `oelem` originalRootingParentRefs = getCache n
                  | otherwise =
                    case (isNetworkEdge (n,j), isNetworkEdge (n,k)) of
                      -- Neither are network edges
                      -- Perform standard tree operation
                      (False, False) ->
                          if   not $ isNetworkEdge (i,n)
                          then localResolutionApplication extensionTransformation meta lhsMemo rhsMemo
                          else case (lhsContext, rhsContext) of
                                 (  [],   []) -> error "Well, that's ALSO embarrassing..."
                                 (x:xs,   []) -> x:|xs
                                 (  [], y:ys) -> y:|ys
                                 (x:xs, y:ys) -> localResolutionApplication extensionTransformation meta (x:|xs) (y:|ys)

                      (False, True ) ->
                          case lhsContext of
                            []   -> rhsMemo
                            x:xs -> let lhsMemo' = x:|xs
                                    in  fold1  $ lhsMemo' :| [localResolutionApplication extensionTransformation meta lhsMemo' rhsMemo]

                      (True , False) ->
                          case rhsContext of
                            []   -> rhsMemo
                            x:xs -> let rhsMemo' = x:|xs
                                    in  fold1  $ rhsMemo' :| [localResolutionApplication extensionTransformation meta lhsMemo  rhsMemo']

                      (True , True ) ->
                          case (lhsContext, rhsContext) of
                             (  [],   []) -> error $ "Well, that's embarrassing...\nContext: " <> unwords ["Focus edge", show (i,n), "LHS edge", show (n,j), "RHS edge", show (n,k) ]
                             (x:xs,   []) -> x:|xs
                             (  [], y:ys) -> y:|ys
                             (x:xs, y:ys) -> fold1 $ (x:|xs) :| [y:|ys]


            -- Filter from the resolution cache all resolutions that have any of
            -- the supplied edges in the subtree.
--          edgeReferenceFilter :: [(Int,Int)] -> ResolutionCache (CharacterSequence u v w x y z) -> ResolutionCache (CharacterSequence u v w x y z)
--            edgeReferenceFilter es xs | trace (show es <> "  " <> show (fmap subtreeEdgeSet xs)) False = undefined
            edgeReferenceFilter es xs = filter (not . any (`elem` invalidEdges) . (^. _subtreeEdgeSet)) $ toList xs
              where
                invalidEdges       = toList es >>= getDirectedEdges
                getDirectedEdges e = [e, swap e]


    -- Here we have the minimal rerooting of dynamic characters mapped for each
    -- display tree. This is not a collection of the minimal display trees for
    -- each block, this is just the minimal re-rooting on each block for a given
    -- display tree.
    displayTreeRerooting :: HashMap TraversalTopology (NonEmpty (Double, Vector (Word, NonEmpty TraversalFocusEdge)))
    displayTreeRerooting = deriveMinimalSequenceForDisplayTree <$> displayTreeMapping
      where

        -- First we invert the Edge Cost Mapping to be keyed by display trees.
        -- This allows us to effciently use the results of the minimization.
        displayTreeMapping = transposeDisplayTrees edgeCostMapping

        -- We can invert the each 'ResolutionCache' element of the Edge Cost
        -- Mapping by creating a new mapping with keys for each display tree in
        -- the 'ResolutionCache' and assigning as the corresponding value the
        -- character sequence of the display tree with the rooting edge attached.
        --
        -- We could merge together the maps created by each element in the Edge
        -- Cost Mapping into our new mapping. However, we must take care to
        -- collect display tree key collisions into a list. To handle this
        -- correctly we perform nested folds that make an 'insertWith' call.
        transposeDisplayTrees :: HashMap TraversalFocusEdge (ResolutionCache s) -> HashMap TraversalTopology (NonEmpty (TraversalFocusEdge, s))
        transposeDisplayTrees = foldlWithKey' f mempty
          where
            f outerMapRef rootingEdge = foldl' g outerMapRef
              where
                g innerMapRef resInfo = HM.insertWith (<>) key val innerMapRef
                  where
                    key = (^. _topologyRepresentation) resInfo
                    val = pure (rootingEdge, (^. _characterSequence) resInfo)

        -- Once we have inverted the Edge Cost Mapping to be keyed by the
        -- display trees, we can perform a minimization on each display tree
        -- to determine which the minimal rooting edge for each dynamic character
        -- in each block.
        --
        -- It is important to remember that since this minimization is performed
        -- independently on each display tree, the traversal foci on the display
        -- tree can all be chosen independently also.
        deriveMinimalSequenceForDisplayTree
          :: HasBlockCost u v w x y z
          => NonEmpty (TraversalFocusEdge, CharacterSequence u v w x y z)
          -> NonEmpty (Double, Vector (Word, NonEmpty TraversalFocusEdge))
        deriveMinimalSequenceForDisplayTree = fmap recomputeCost . foldr1 (zipWith minimizeBlock) . fmap createZippableContext
          where
            minimizeBlock (static, dynCharVect1) (_, dynCharVect2) = (static, minimizedDynamicCharacterVector)
              where
                minimizedDynamicCharacterVector = zipWith minimizeDynamicCharacterRooting dynCharVect1 dynCharVect2
                minimizeDynamicCharacterRooting lhs@(c1, w, es1) rhs@(c2, _, es2) =
                    case c1 `compare` c2 of
                      LT -> lhs
                      GT -> rhs
                      EQ -> (c1, w, es1 <> es2)

        -- To create a readily zippable structure containing the contextual
        -- information to be minimized.
        --
        -- We unwrap the character sequence to a 'NonEmpty' list of blocks.
        -- Within each block we construct a minimization context.
        createZippableContext
          :: HasBlockCost u v w x y z
          => (e, CharacterSequence u v w x y z)
          -> NonEmpty (Double, Vector (Word, Double, NonEmpty e))
        createZippableContext (edge, charSeq) = toNonEmpty $ zipWith (toMinimalBlockContext edge) (meta ^. blockSequence) (charSeq ^. blockSequence)

        -- We create a minimization context for a given character block and a
        -- corresponding rooting edge (traversal focus) by extracting a vector
        -- of the dynamic characters in the block and record for each dynamic
        -- character extracted, its integral cost value, its real valued weight,
        -- and the current rooting edge that we are considering.
        --
        -- In addition to the vector of dynamic character information, we also
        -- extract the cumulative cost of all the static (non-dynamic characters)
        -- of the block.
        --
        -- We return the static cost and the vector to ??
        toMinimalBlockContext
          :: HasBlockCost u v w x y z
          => e
          -> MetadataBlock m
          -> CharacterBlock u v w x y z
          -> (Double, Vector (Word, Double, NonEmpty e))
        toMinimalBlockContext edge mBlock cBlock = (staticCost mBlock cBlock, dynCharVect)
          where
            dynCharVect = zipWith (\mVal dec -> (dec ^. characterCost, mVal ^. characterWeight, pure edge)) (mBlock ^. dynamicBin) $ cBlock ^. dynamicBin

        recomputeCost (staticCostVal, dynCharVect) = (staticCostVal + minDynCharCost, dynCharNoWeight)
          where
            minDynCharCost  = sum' $ (\(c, w,  _) -> fromIntegral c * w) <$> dynCharVect
            dynCharNoWeight =        (\(c, _, es) -> (c, es)           ) <$> dynCharVect

{-
    -- Here we calculate, for each character block, for each display tree in the
    -- phylogenetic DAG, the minimal traversal foci and the corresponding cost.
    -- Note that there could be many minimal traversal foci for each display tree.
    sequenceOfEdgesWithMinimalCost :: NonEmpty (Double, NonEmpty (TraversalTopology, Vector (Word, NonEmpty TraversalFocusEdge)))
    sequenceOfEdgesWithMinimalCost = mapWithKey blockLogic sequenceWLOG -- (\x -> trace (show $ (fmap (fmap costOfFoci)) <$> x) x) $

      where

        -- First we select an arbitrary character sequence from the DAG.
        -- We do this to produce a result that matches the structure of the
        -- character sequence in our DAG. Since all character sequences in the
        -- DAG have the same number of character blocks, and each character block
        -- has the same number of characters, we can select any character
        -- sequence without loss of generality.
        --
        -- By mapping over the structure we ensure that our result shares the
        -- the same structure because that's how Functors work.
        --
        -- The only structural difference is that character types other than
        -- dynamic characters are filtered from each character block.
        sequenceWLOG = toBlocks . characterSequence . NE.head $ getCache rootRefWLOG

        -- Second we collect the all the display trees obeserved in the DAG.
        -- We do this by unioning all the display trees from the root nodes of
        -- the DAG.
        --
        -- There is a chance that there may be a display tree observed in the
        -- "Edge Cost Mapping" that was not observed on one of the root nodes
        -- of the DAG. We hope that this isn't the case because we can save
        -- a lot of time by not folding over the entirely of the "Edge Cost
        -- Mapping" and instead folding over only the root nodes of the DAG.
        --
        -- The expected difference is between /O(n)/ and /O(e * n)/ where
        -- /n/ is the number of network nodes in the DAG and /e/ is the number
        -- of edges in the DAG.
        displayTreeSet :: NonEmpty TraversalTopology
        displayTreeSet = NE.fromList . toList $ foldMap (S.fromList . toList . fmap fst) rootEdgeInDAGToCostMapping

        -- A map from root edges in the DAG to the display trees and their cost.
        -- This is used to compute the minimal display tree of a block in the
        -- degenerative case where there are no dynamic characters in a block.
        --
        -- Also used to defined the display tree set which is in turn used for
        -- the "outer" most fold in the non-degenerative case when dynamic
        -- characters *are* present in a block.
        rootEdgeInDAGToCostMapping :: Map TraversalFocusEdge (NonEmpty (TraversalTopology, Vector Double))
        rootEdgeInDAGToCostMapping = foldMap1 f roots
          where
            f = M.singleton <$> getUnrootedEdgeReference <*> getRootResolutionContext

            getUnrootedEdgeReference i = fromMaybe (i,i) $ rootIndexToUnrootedIndex i
            getRootResolutionContext   = fmap (topologyRepresentation &&& getCostofEachBlock) . resolutions . nodeDecoration . (references inputDag !)
            getCostofEachBlock         = V.fromList' . fmap blockCost . toList . toBlocks . characterSequence

        -- For each block in the sequence of character we perform a
        -- multi-dimensional minimization. We must determine the minimal spanning
        -- tree (TopologyRepresentation) for each blockand it's corresponding
        -- cost.
        --
        -- The cost of a given spanning tree is defined by the sum of the costs
        -- on the minimum rooting edger for each dynamic character in the block.
        -- The minimimal rooting edge must be on the spanning tree we are
        -- quantifying.
        --
        -- Using the above method of quantification, we take the minimum spanning
        -- tree for each block and make note of the rooting edges for each
        -- dynamic character in the block that was associated with the minimal
        -- cost.
        blockLogic blockIndex blockValue = minimumContext
          where

            -- If the block has no dynamic characters, then we do the easy thing:
            --
            -- For each root node in the DAG,
            --   Find the minimum cost resolution for that root
            -- Select the root node with the minimum resolution.
            --
            -- If the block has one or more dynamic characters, we need to
            -- perform the arduous, multi-dimensional minimization:
            --
            -- For each spanning tree (observed) in the network,
            --   For each dynamic character in the current block,
            --     For each edge in the spanning tree under consideration
            --       Get the cost of placing the root on this edge for the
            --       current dynamic character.
            --     Take the minimum rooting edge for current dynamic character.
            --   Sum the minimum cost rooting for each dynamic character.
            -- Save the spanning tree context with the minimal cost.
            -- This spanning tree is the minimal TopologyRepresentation for the
            -- given block.
            minimumContext =
                case toList $ dynamicCharacters blockValue of
                  []   -> degenerateBlockContext blockIndex
                  x:xs -> fromMinimalTopologyContext $ foldMap1 (deriveMinimalSpanningTreeContext (x:|xs)) displayTreeSet

            -- In the case that there are no dynamic character in the block, we
            -- derive the degenerate block context.
            degenerateBlockContext i = (cost, pure (topo, mempty))
              where
                -- Degenerate Step 1:
                -- First collect the current block cost for each resolution at
                -- each root.
                mappingOfCost :: Map TraversalFocusEdge (NonEmpty (TraversalTopology, Double))
                mappingOfCost = fmap (fmap (fmap (! i))) rootEdgeInDAGToCostMapping

                -- Degenerate Step 2:
                -- Then find the minimum resolution for the current block at each
                -- root.
                mappingEdgeToMinTopo :: Map TraversalFocusEdge (TraversalTopology, Double)
                mappingEdgeToMinTopo = minimumBy (comparing snd) <$> mappingOfCost

                -- DegenerateStep 3:
                -- Lastly select the root (edge) with the minimum cost resolution.
                (_rootEdge, (topo, cost)) = minimumBy (comparing (snd . snd)) $ M.assocs mappingEdgeToMinTopo

            -- For the given spanning tree and the characters in the current
            -- block, we construct a 'MinimalTopologyContext' value and *will*
            -- use the 'Semigroup' operator '(<>)' to accumulate the minimal
            -- context for all the spanning trees in the 'foldMap1' call above.
            deriveMinimalSpanningTreeContext blockDynamicCharacters spanningTree = toMinimalTopologyContext minimalBlockCost spanningTree minimalRootsPerCharacter
              where
                minimalBlockCost               = sum $ fst <$> minimalCostAndRootPerCharacter
                minimalRootsPerCharacter       = V.fromList' . toList $ snd <$> minimalCostAndRootPerCharacter
                minimalCostAndRootPerCharacter = mapWithKey getMinimalCharacterRootInSpanningTree blockDynamicCharacters


                -- Determine the minimal rooting edge for the given dynamic
                -- character in the spanning tree by first constructing a
                -- 'MinimalDynamicCharacterRootContext' for each applicable edge
                -- in the spanning tree and then minimizing the root edge
                -- contexts using the 'Semigroup' instance of the
                -- 'MinimalDynamicCharacterRootContext' values in the 'fold1'
                -- call below.
                --
                -- We explicitly handle the "impossible" case that there was no
                -- rooting edge for a character in the spanning tree. How this
                -- could occur is currently beyond my comprehension, but it's
                -- good to give an explicit error message just in case.
                getMinimalCharacterRootInSpanningTree characterIndex characterDecoration =
                    case foldMapWithKey getEdgeCostInSpanningTree edgeCostMapping of
                      x:xs -> let r@(charCost, _) = fromMinimalDynamicCharacterRootContext . fold1 $ x:|xs
                              in  (charWeight * fromIntegral charCost, r)
                      []   -> error $ unwords
                                  [ "A very peculiar impossiblity occurred!"
                                  , "When determining the minimal rooting edge"
                                  , "for a given dynamic character"
                                  , "in a given display tree,"
                                  , "no such edge was found!"
                                  ]

                  where
                    charWeight = characterDecoration ^. characterWeight
                    getDynamicCharacteraterDecoration = (! characterIndex) . dynamicCharacters . (! blockIndex) . toBlocks . characterSequence

                    -- Possible construct a 'MinimalDynamicCharacterRootContext'
                    -- value for a given edge.
                    getEdgeCostInSpanningTree rootingEdge cache =
                      case NE.filter (\x -> spanningTree == topologyRepresentation x) cache of
                        []  -> []
                        x:_ -> [ toMinimalDynamicCharacterRootContext (getDynamicCharacterCost x) rootingEdge ]
                      where
                        getDynamicCharacterCost = (^. characterCost) . getDynamicCharacteraterDecoration
-}


    -- Step 4: Update the dynamic character decoration's cost & add an edge reference.
    modifiedRootRefs = (id &&& modifyRootCosts . (refVec !)) <$> rootRefs inputDag
      where
        modifyRootCosts idxData = idxData { nodeDecoration = nodeDatum }
          where
            node = nodeDecoration idxData
            nodeDatum =
                PNode2
                { resolutions          = f <$> resolutions node
                , nodeDecorationDatum2 = nodeDecorationDatum2 node
                }

        -- For each resolution we apply this transformation which updates each
        -- dynamic character in the resolution with the minimal cost and the
        -- spanning tree and rooting edges (collectively named the traversal foci)
        -- and also update the total cost of the resolution to reflect the lower
        -- dynamic character cost.
        f resInfo =
            resInfo & _totalSubtreeCost  .~ newTotalCost
                    & _characterSequence .~ modifiedSequence

          where
            resolutionTopology = (^. _topologyRepresentation) resInfo
            minimizedSequence  = NEV.fromNonEmpty $ minimalDisplayTreeRerootings ! resolutionTopology
            newTotalCost       = sequenceCost meta modifiedSequence
            modifiedSequence   = over blockSequence (zipWith g minimizedSequence) $ (^. _characterSequence) resInfo

            -- The "block-wise" transformation.
            --
            -- Expects a "context-block" containing the metadata of which
            -- spanning tree was minimal for the block and for each dynamic
            -- character in the block the rooting edges of which contributed to the
            -- minimal block cost.
            --
            -- Also expects a "data-block" with the old block data to be updated
            -- with information from the "context-block."
--            g :: (Double, Vector (Word, NonEmpty TraversalFocusEdge)) -> CharacterBlock u v w x y z -> CharacterBlock u v w x y z
            g (_, minBlockContexts) charBlock = charBlock & dynamicBin .~ modifiedDynamicChars
              where

                -- We take the first of the minimal contexts and distribute the
                -- associated spanning tree over the dynamic character vector
                -- to create a vector of associated "traversal foci" for each
                -- dynamic character in the block.
                --
                -- We use this vector to zip against the original dynamic
                -- character vector from the "data-block," updating the dynamic
                -- character decorations to contain the new minimal cost and
                -- corresponding traversal foci.
                vectorForZipping :: Vector Word
                vectorForZipping = fst <$> minBlockContexts

                modifiedDynamicChars = zipWith h vectorForZipping $ charBlock ^. dynamicBin

                h costVal originalDec =
                    originalDec
                      & characterCost .~ costVal


    -- Step 5: Update the metadata sequence with the TraversalFoci for each dynamic character
    modifiedMetadataSequence = over blockSequence (zipWith M.setFoci fociSequence) meta
      where
        rootTopologies :: NonEmpty TraversalTopology
        rootTopologies = do
            rootRef <- rootRefs inputDag
            resInfo <- resolutions . nodeDecoration $ refVec ! rootRef
            pure $ (^. _topologyRepresentation) resInfo

        rootContextVectors :: NonEmpty (TraversalTopology, NonEmpty (Double, Vector (Word, NonEmpty TraversalFocusEdge)))
        rootContextVectors = (id &&& (minimalDisplayTreeRerootings !)) <$> rootTopologies

        topologySequences :: NonEmpty (NonEmpty (Vector TraversalFoci))
        topologySequences = (\(topo, x) -> fmap (fmap (id &&& const topo) . snd) . snd <$> x) <$> rootContextVectors

        fociSequence :: NEV.Vector (Vector TraversalFoci)
        fociSequence = NEV.fromNonEmpty $ foldr1 (zipWith (zipWith (<>))) topologySequences


(.!>.) :: (Lookup f, Show (Key f)) => f a -> Key f -> a
(.!>.) s k = fromMaybe (error $ "Could not index: " <> show k) $ k `lookup` s
