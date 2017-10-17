------------------------------------------------------------------------------
-- |
-- Module      :  Bio.Graph.PhylogeneticDAG.DynamicCharacterRerooting
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

{-# LANGUAGE FlexibleContexts, MonoLocalBinds #-}

module Bio.Graph.PhylogeneticDAG.DynamicCharacterRerooting
  ( assignOptimalDynamicCharacterRootEdges
  ) where

import           Bio.Character.Decoration.Additive
import           Bio.Character.Decoration.Dynamic
import           Bio.Sequence
import           Bio.Graph.Node
--import           Bio.Graph.PhylogeneticDAG.Class
import           Bio.Graph.PhylogeneticDAG.Internal
import           Bio.Graph.ReferenceDAG.Internal
import           Control.Applicative
import           Control.Arrow             ((&&&))
import           Control.Lens
import           Control.Monad.State.Lazy
import           Data.Bifunctor            (second)
import           Data.Foldable
import           Data.HashMap.Lazy         (HashMap)
import qualified Data.HashMap.Lazy  as HM
import qualified Data.IntMap        as IM
import qualified Data.IntSet        as IS
import           Data.Key
import           Data.List.NonEmpty        (NonEmpty(..))
--import qualified Data.List.NonEmpty as NE
--import           Data.List.Utility
--import           Data.Map                  (Map)
--import qualified Data.Map           as M
import           Data.Maybe
import           Data.MonoTraversable
--import           Data.Ord                  (comparing)
--import           Data.Set                  (Set)
--import qualified Data.Set           as S
import           Data.Semigroup
import           Data.Semigroup.Foldable
--import           Data.TopologyRepresentation
import           Data.Tuple                (swap)
import           Data.Vector               (Vector)
import qualified Data.Vector        as V
import           Prelude            hiding (lookup, zipWith)


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
-- resolutions that contain the incident network edge contained on the current
-- network edge.

assignOptimalDynamicCharacterRootEdges
  :: ( HasBlockCost u v w x y z Word Double
     , HasTraversalFoci z (Maybe TraversalFoci)
{--
     , Show e
     , Show n
     , Show u
     , Show v
     , Show w
     , Show x
     , Show y
     , Show z
--}
     ) --x, Ord x, Show x)
  => (z -> [z] -> z)  -- ^ Post-order traversal function for Dynamic Characters.
  -> PhylogeneticDAG2 e n u v w x y z
  -> ( PhylogeneticDAG2 e n u v w x y z
     ,         HashMap EdgeReference (ResolutionCache (CharacterSequence u v w x y z))
     , Vector (HashMap EdgeReference (ResolutionCache (CharacterSequence u v w x y z)))
--     , NonEmpty (TraversalFoci)
     ) 
--assignOptimalDynamicCharacterRootEdges extensionTransformation x | trace (L.unpack . renderDot $ toDot x) False = undefined
--assignOptimalDynamicCharacterRootEdges extensionTransformation (PDAG2 x) | trace (referenceRendering x) False = undefined
assignOptimalDynamicCharacterRootEdges extensionTransformation pdag@(PDAG2 inputDag) =
    case toList inputDag of
      -- Degenarate cases
      []      ->     (pdag, mempty, mempty)
      [_]     ->     (pdag, mempty, mempty)
      -- Trivial case
      [_,_]   -> let r = ((0,1), (getCache 1))
                     c = ((1,0), (getCache 0))
                     m = HM.fromList [r, c]
                     d = setDefaultFoci <$> inputDag
                 in  (PDAG2 d, m, V.generate 2 (const m))
      -- Complex case, see four steps below.
      _:_:_:_ ->     (PDAG2 updatedDag, edgeCostMapping, contextualNodeDatum) 
  where
    
    -- Step 1: Construct a hashmap of all the *unrooted* edges.
    unrootedEdges = rootEdgeReferences <> otherUnrootedEdges
    
    -- Step 2: Create a lazy, memoized hashmap of the edge costs for each dynmaic character.
    edgeCostMapping = {- (\x -> trace ("edgeCostMapping length: " <> show (length x)) x) $ -} referenceEdgeMapping

    -- Step 3: For each display tree, for each dynamic character, find the
    -- minimal cost edge(s).
    minimalDisplayTreeRerootings = displayTreeRerooting
    
    -- Step 4: Update the dynamic character decoration's cost & add an edge reference.
    updatedDag = inputDag
        { references = refVec V.// toList modifiedRootRefs
        , graphData  = (graphData inputDag) { graphMetadata = (edgeCostMapping, contextualNodeDatum) }
        }


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
                    Just (lhs, rhs) -> (e, localResolutionApplication extensionTransformation lhs rhs)
                    Nothing         -> error errorContext
          where
            lhsContext = (i `lookup` contextualNodeDatum) >>= ((j,i) `lookup`)
            rhsContext = (j `lookup` contextualNodeDatum) >>= ((i,j) `lookup`)
            errorContext = unlines
                [ show e
                , show $ HM.keys <$> contextualNodeDatum
                ]
    


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


    -- Construct at each memoized datum index 'n' in the vector 'memo' such that
    -- for node 'n' the directed edge references '[(i,n),(j,n),(k,n)]' in the
    -- following undirected subgraph store the directed subtree resolutions  of
    -- the complete graph:
    -- >
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
    contextualNodeDatum = V.generate (length refVec) generateMemoizedDatum
      where

        -- Determine if the memoized point is a root node of the phylogenetic DAG
        -- component. If so, we do not generate an memoized directional edge data
        -- for the component root since the component root has a forced direction
        -- on it's incident edges.
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
            
            -- Here we remove any root nodes that were in the parents references
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
            -- the subtree is commutative. Hence swapping the last two elements
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
            -- defined by the first element of the tuple being an incomming edge.
--            deriveDirectedEdgeDatum :: (Int, Int, Int) -> Map EdgeReference (ResolutionCache (CharacterSequence u v w x y z))
--            deriveDirectedEdgeDatum (i,j,k) | trace ("derive directional: " <> show (i,j,k)) False = undefined
            deriveDirectedEdgeDatum (i,j,k) = [((i, n), subtreeResolutions)]
              where
                lhsMemo       = (contextualNodeDatum ! j) .!>. (n, j)
                rhsMemo       = (contextualNodeDatum ! k) .!>. (n, k)
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
                          then localResolutionApplication extensionTransformation lhsMemo rhsMemo 
                          else
                            case (lhsContext, rhsContext) of
                             (  [],   []) -> error "Well, that's ALSO embarassing..."
                             (x:xs,   []) -> x:|xs
                             (  [], y:ys) -> y:|ys
                             (x:xs, y:ys) -> localResolutionApplication extensionTransformation (x:|xs) (y:|ys)

                      (False, True ) ->
                          case lhsContext of
                            []   -> rhsMemo
                            x:xs -> let lhsMemo' = x:|xs
                                    in  sconcat  $ lhsMemo' :| [localResolutionApplication extensionTransformation lhsMemo' rhsMemo]

                      (True , False) ->
                          case rhsContext of
                            []   -> rhsMemo
                            x:xs -> let rhsMemo' = x:|xs
                                    in  sconcat  $ rhsMemo' :| [localResolutionApplication extensionTransformation lhsMemo  rhsMemo']

                      (True , True ) ->
                          case (lhsContext, rhsContext) of
                             (  [],   []) -> error $ "Well, that's embarassing...\nContext: " <> unwords ["Focus edge", show (i,n), "LHS edge", show (n,j), "RHS edge", show (n,k) ]
                             (x:xs,   []) -> x:|xs
                             (  [], y:ys) -> y:|ys
                             (x:xs, y:ys) -> sconcat $ (x:|xs) :| [y:|ys]


            -- Filter from the resolution cache all resolutions that have any of
            -- the supplied edges in the subtree.
--          edgeReferenceFilter :: [(Int,Int)] -> ResolutionCache (CharacterSequence u v w x y z) -> ResolutionCache (CharacterSequence u v w x y z)
--            edgeReferenceFilter es xs | trace (show es <> "  " <> show (fmap subtreeEdgeSet xs)) False = undefined
            edgeReferenceFilter es xs = filter (not . any (`elem` invalidEdges) . subtreeEdgeSet) $ toList xs
              where
                invalidEdges       = toList es >>= getDirectedEdges 
                getDirectedEdges e = [e, swap e]


    -- Here we have the minimal rerooting of dynamic characters mapped for each
    -- display tree. This is not a collection of the minimal display tree for
    -- each block, this is just the minimal re-rooting on each block for a given
    -- display tree.
    displayTreeRerooting :: HashMap TraversalTopology (NonEmpty (Double, Vector (Word, NonEmpty TraversalFocusEdge)))
    displayTreeRerooting = deriveMinimalSequenceForDisplayTree <$> displayTreeMapping
      where

        -- First we invert the Edge Cost Mapping to be keyed by display trees.
        -- This allows us to effciently use the results of the minimization.
        displayTreeMapping = transposeDisplayTrees edgeCostMapping

        -- We can invert the each Resolution Cache element of the Edge Cost
        -- Mapping by creating a new mapping with keys for each display tree in
        -- the Resolution Cache and assigning as the corresponding value the
        -- character sequence of the display tree with the rooting edge attached.
        -- 
        -- We could merge together the maps created by each element in the Edge
        -- Cost Mapping into our new mapping. However, we must take care to
        -- collect display tree key collisions into a list. To handle this
        -- correctly we perform nested folds that make an 'insertWith' call.
        transposeDisplayTrees :: HashMap TraversalFocusEdge (ResolutionCache s) -> HashMap TraversalTopology (NonEmpty (TraversalFocusEdge, s))
        transposeDisplayTrees = foldlWithKey' f mempty
          where
            f outerMapRef rootingEdge cache = foldl' g outerMapRef cache
              where
                g innerMapRef resInfo = HM.insertWith (<>) key val innerMapRef
                  where
                    key = topologyRepresentation resInfo
                    val = pure (rootingEdge, characterSequence resInfo)

        -- Once we have invereted the Edge Cost Mapping to be keyed by the
        -- display trees, we can perform a minimization on each display tree
        -- to determine which the minimal rooting edge for each dynamic character
        -- in each block.
        --
        -- It is important to rememebr that since this minimization is performed
        -- independantly on each display tree, the rooting edges on the display
        -- tree can all be choosen independantly also.
        deriveMinimalSequenceForDisplayTree
          :: HasBlockCost u v w x y z Word Double
          => NonEmpty (TraversalFocusEdge, CharacterSequence u v w x y z)
          -> NonEmpty (Double, Vector (Word, NonEmpty TraversalFocusEdge))
        deriveMinimalSequenceForDisplayTree = fmap recomputeCost . foldr1 (zipWith minimizeBlock) . fmap createZippableContext
          where
            minimizeBlock (static, dynCharVect1) (_, dynCharVect2) = (static, minimizedDynamicCharVector)
              where
                minimizedDynamicCharVector = zipWith minimizeDynamicCharRooting dynCharVect1 dynCharVect2
                minimizeDynamicCharRooting lhs@(c1, w, es1) rhs@(c2, _, es2) =
                    case c1 `compare` c2 of
                      LT -> lhs
                      GT -> rhs
                      EQ -> (c1, w, es1 <> es2)

        -- To create a readily zippable structure containing the contextual
        -- information to be minimized.
        --
        -- We unwrap the character sequence to a NonEmpty list of blocks.
        -- Within each block we construct a minimization context.
        createZippableContext 
          :: HasBlockCost u v w x y z Word Double
          => (e, CharacterSequence u v w x y z)
          -> NonEmpty (Double, Vector (Word, Double, NonEmpty e))
        createZippableContext (edge, charSeq) = toMinimalBlockContext edge <$> toBlocks charSeq

        -- We create a minimization context for a given character block and a
        -- corresponding rooting edge (traversal focus) by extracting a vector
        -- of the dynamic characters in the block and record for each dynamic 
        -- character extracted, it's integral cost value, it's real valued weight
        -- and the current rooting edge that we are considering.
        --
        -- In addition to the vector of dynamic character information, we also
        -- extract the cumulative cost of all the static (non-dynamic characters)
        -- of the block.
        --
        -- We return the static cost and the vector to 
        toMinimalBlockContext
          :: HasBlockCost u v w x y z Word Double
          => e
          -> CharacterBlock u v w x y z
          -> (Double, Vector (Word, Double, NonEmpty e))
        toMinimalBlockContext edge block = (staticCost block, dynCharVect)
          where
            dynCharVect = (\dec -> (dec ^. characterCost, dec ^. characterWeight, pure edge)) <$> dynamicCharacters block

        recomputeCost (staticCostVal, dynCharVect) = (staticCostVal + minDynCharCost, dynCharNoWeight)
          where
            minDynCharCost  = sum $ (\(c, w,  _) -> fromIntegral c * w) <$> dynCharVect
            dynCharNoWeight =       (\(c, _, es) -> (c, es)           ) <$> dynCharVect

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
        -- DAG have the same number of chracter blocks, and each character block
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
        -- The expected diffrence is between /O(n)/ and /O(e * n)/ where
        -- /n/ is the number of network nodes in the DAG and /e/ is the number
        -- of edges in the DAG.
        displayTreeSet :: NonEmpty TraversalTopology
        displayTreeSet = NE.fromList . toList $ foldMap (S.fromList . toList . fmap fst) rootEdgeInDAGToCostMapping

        -- A map from root edges in the DAG to the display trees and thier cost.
        -- This is used to comput the minimal display tree of a block in the
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
            getCostofEachBlock         = V.fromList . fmap blockCost . toList . toBlocks . characterSequence

        -- For each block in the sequence of character we perform a
        -- multi-dimensional minimization. We must determine the minimal spanning
        -- tree (TopologyRepresentation) for each blockand it's corresponsing
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
                minimalRootsPerCharacter       = V.fromList . toList $ snd <$> minimalCostAndRootPerCharacter
                minimalCostAndRootPerCharacter = mapWithKey getMinimalCharacterRootInSpanningTree blockDynamicCharacters


                -- Determine the minimal rooting edge for the given dynamic
                -- character in the spanning tree by first constructing a
                -- 'MinimalDynamicCharacterRootContext' for each applicable edge
                -- in the spanning tree and then minimizing the root edge
                -- contexts using the 'Semigroup' instance of the
                -- 'MinimalDynamicCharacterRootContext' values in the 'fold1'
                -- call below.
                --
                -- We explicitly hande the "impossible" case that there was no
                -- rooting edge for a character in the spanning tree. How this
                -- could occur is currently beyond my comprehension, but it's
                -- good to give an explict error message just in case.
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
                    getDynamicCharaterDecoration = (! characterIndex) . dynamicCharacters . (! blockIndex) . toBlocks . characterSequence

                    -- Possible construct a 'MinimalDynamicCharacterRootContext'
                    -- value for a given edge.
                    getEdgeCostInSpanningTree rootingEdge cache =
                      case NE.filter (\x -> spanningTree == topologyRepresentation x) cache of
                        []  -> []
                        x:_ -> [ toMinimalDynamicCharacterRootContext (getDynamicCharacterCost x) rootingEdge ]
                      where
                        getDynamicCharacterCost = (^. characterCost) . getDynamicCharaterDecoration
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

        -- For each resolution we apply this transformation which update each
        -- dynamic character in the resolution with the minimal cost and the
        -- spanning tree and rooting edges (collectively named the traversal foci)
        -- and also update the total cost of the resolution to reflect the lower
        -- dynamic character cost.
        f resInfo =
            resInfo
            { totalSubtreeCost  = newTotalCost
--            , localSequenceCost = newLocalCost
            , characterSequence = modifiedSequence
            }
          where
            resolutionTopology = topologyRepresentation resInfo
            minimizedSequence  = minimalDisplayTreeRerootings ! resolutionTopology
--            newLocalCost       = newTotalCost - sum (totalSubtreeCost <$> childResolutionContext)
            newTotalCost       = sequenceCost modifiedSequence
            modifiedSequence   = fromBlocks . zipWith g minimizedSequence . toBlocks $ characterSequence resInfo

            -- The "block-wise" transformation.
            --
            -- Expects a "context-block" containing the metadata of which
            -- spanning tree was minimal for the block and for each dynamic
            -- character in the block which rooting edges contributed to the
            -- minimal block cost.
            --
            -- Also expects a "data-block" with the old block data to be updated
            -- with information from the "context-block."
--            g :: (Double, Vector (Word, NonEmpty TraversalFocusEdge)) -> CharacterBlock u v w x y z -> CharacterBlock u v w x y z
            g (_, minBlockContexts) charBlock = charBlock { dynamicCharacters = modifiedDynamicChars }
              where

                -- We take the first of the minimal contexts and distribute the
                -- associated spanning tree over the dynamic character vector
                -- to create a vector of associated "traveral foci" for each
                -- dynamic character in the block.
                --
                -- We use this vector to zip against the original dynamic
                -- character vector from the "data-block," updating the dynamic
                -- character decorations to contain the new minimal cost and
                -- corresponding traversal foci.
                vectorForZipping :: Vector (Word, NonEmpty (TraversalFocusEdge, TraversalTopology))
                vectorForZipping = second (fmap (\e -> (e, resolutionTopology))) <$> minBlockContexts
                
                modifiedDynamicChars = zipWith h vectorForZipping $ dynamicCharacters charBlock
                
                h (costVal, foci) originalDec =
                    originalDec
                      & characterCost .~ costVal
                      & traversalFoci .~ Just foci


-- |
-- Used in the trivial case of single leaf component of a forest.
-- Updated the TraversalFoci to be the only edge in the DAG.
setDefaultFoci
  :: HasTraversalFoci z (Maybe TraversalFoci)
  => PhylogeneticNode2 (CharacterSequence u v w x y z) a
  -> PhylogeneticNode2 (CharacterSequence u v w x y z) a
setDefaultFoci =
    PNode2
      <$> fmap (fmap (hexmap id id id id id f)) . resolutions
      <*> nodeDecorationDatum2
  where
    f x = x & traversalFoci .~ (Just v :: Maybe TraversalFoci)
    e = (0,1)  -- The only edge in the DAG.
    t = mempty -- So there's no network edges in the DAG.
    v = pure (e,t)


(.!>.) :: (Lookup f, Show (Key f)) => f a -> Key f -> a
(.!>.) s k = fromMaybe (error $ "Could not index: " <> show k) $ k `lookup` s




{-

newtype MinimalDynamicCharacterRootContext c e = MDCRC (c, Set e) deriving (Show)


instance (Ord c, Ord e) => Semigroup (MinimalDynamicCharacterRootContext c e) where

    lhs@(MDCRC (lhsCost, lhsConext)) <> rhs@(MDCRC (rhsCost, rhsConext)) =
        case lhsCost `compare` rhsCost of
          GT -> rhs
          LT -> lhs
          EQ -> MDCRC (lhsCost, lhsConext <> rhsConext)


fromMinimalDynamicCharacterRootContext :: MinimalDynamicCharacterRootContext c e -> (c, NonEmpty e)
fromMinimalDynamicCharacterRootContext (MDCRC (cost, edges)) = (cost, NE.fromList $ toList edges)


toMinimalDynamicCharacterRootContext :: c -> e -> MinimalDynamicCharacterRootContext c e
toMinimalDynamicCharacterRootContext cost edge = MDCRC (cost, S.singleton edge)


-- |
-- The representation of a topology context for a block in a 'ChracterSequence'.
--
-- This type is designed to simplify the minimzation routine between two contexts
-- while preserving all relavent contextual infornmation.
--
-- Use the 'Semigroup' operator '(<>)' to perform a minimization between two
-- contexts.
data MinimalTopologyContext c i e
   = MW c (Map (TopologyRepresentation e) (Vector (i, Set e)))
   deriving (Show)


instance (Ord c, Ord e, Ord i) => Semigroup (MinimalTopologyContext c i e) where

    lhs@(MW lhsCost lhsConext) <> rhs@(MW rhsCost rhsConext) =
        case lhsCost `compare` rhsCost of
          GT -> rhs
          LT -> lhs
          EQ -> MW lhsCost $ M.unionWith (zipWith merger) lhsConext rhsConext
      where
        merger (c1, edges1) (c2, edges2) = (min c1 c2, edges1 <> edges2)


fromMinimalTopologyContext :: MinimalTopologyContext c i e -> (c, NonEmpty (TopologyRepresentation e, Vector (i, NonEmpty e)))
fromMinimalTopologyContext (MW cost context) = (cost, fmap nestedSetToNonEmptyList . NE.fromList $ M.assocs context)
  where
    -- fmap over the tuple, then over the vector, then over the other tuple, then coerce the Set to a NonEmpty list
    nestedSetToNonEmptyList = fmap (fmap (fmap (NE.fromList . toList)))


-- |
-- For our use cases /O(n)/ where /n/ is the length of the Vector.
toMinimalTopologyContext :: Ord e => c -> TopologyRepresentation e -> Vector (i, NonEmpty e) -> MinimalTopologyContext c i e
toMinimalTopologyContext cost topoRep dynCharRootEdges = MW cost . M.singleton topoRep $ second (S.fromList . toList) <$> dynCharRootEdges
-}
