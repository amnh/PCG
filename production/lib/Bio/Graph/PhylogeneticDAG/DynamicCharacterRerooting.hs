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

{-# LANGUAGE FlexibleContexts #-}

module Bio.Graph.PhylogeneticDAG.DynamicCharacterRerooting
  ( assignOptimalDynamicCharacterRootEdges
  ) where

import           Bio.Character.Decoration.Additive
import           Bio.Character.Decoration.Dynamic
import           Bio.Sequence
import           Bio.Graph.Node
import           Bio.Graph.PhylogeneticDAG.Internal
import           Bio.Graph.ReferenceDAG.Internal
import           Control.Applicative
import           Control.Arrow             ((&&&))
import           Control.Lens
import           Control.Monad.State.Lazy
import           Data.Foldable
import qualified Data.IntMap        as IM
import qualified Data.IntSet        as IS
import           Data.Key
import           Data.List.NonEmpty        (NonEmpty( (:|) ))
import qualified Data.List.NonEmpty as NE
--import           Data.List.Utility
--import           Data.Map                  (Map)
import qualified Data.Map           as M
import           Data.Maybe
import           Data.MonoTraversable
import           Data.Ord                  (comparing)
import           Data.Semigroup
import           Data.Semigroup.Foldable
import           Data.Tuple                (swap)
import qualified Data.Vector        as V
import           Prelude            hiding (lookup, zipWith)

--import Debug.Trace


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
-- resolutions that contain the "incident" network edge contained on the current
-- network edge.

assignOptimalDynamicCharacterRootEdges
  :: ( HasBlockCost u v w x y z Word Double
     , HasTraversalFoci z (Maybe TraversalFoci)
     , Show u
     , Show v
     , Show w
     , Show x
     , Show y
     , Show z
     ) --x, Ord x, Show x)
  => (z -> [z] -> z)
  -> PhylogeneticDAG2 e n u v w x y z
  -> PhylogeneticDAG2 e n u v w x y z
--assignOptimalDynamicCharacterRootEdges extensionTransformation (PDAG2 inputDag) = undefined
{--}
assignOptimalDynamicCharacterRootEdges extensionTransformation (PDAG2 inputDag) = PDAG2 updatedDag
  where

    -- Step 1: Construct a hashmap of all the edges.
    unrootedEdges = rootEdgeReferences <> otherUnrootedEdges
    
    -- Step 2: Create a lazy memoized hashmap of the edge costs for each dynmaic character.

    edgeCostMapping = referenceEdgeMapping

    -- Step 3: For each dynamic character, find the minimal cost edge(s).
    minimalCostSequence = sequenceOfEdgesWithMinimalCost
    
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
        f i =
          case IM.keys . childRefs $ refVec ! i of
            []    -> []
            [_]   -> []
            x:y:_ -> [(x,y)]

    refVec = references inputDag

    roots  = rootRefs   inputDag

    isNetworkEdge (a,b) = parentCount > 1
      where
        parentCount = olength . parentRefs $ refVec ! y
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

{-
    referenceEdgeMapping :: HashMap EdgeReference IncidentEdges
    referenceEdgeMapping = HM.fromList $ foldMap f otherUnrootedEdges <> foldMap g rootEdgeReferences
      where
        f e@(i,j) = [(e, parRefs <> cldRefs)]
          where
            parRefs = ofoldMap (\k -> [(k,i)])           . parentRefs $ refVec ! i
            cldRefs =  foldMap (\k -> [(j,k)]) . IM.keys .  childRefs $ refVec ! j
        g e@(i,j) = [(e, lhsRefs <> rhsRefs)]
          where
            lhsRefs =  foldMap (\k -> [(i,k)]) . IM.keys .  childRefs $ refVec ! i
            rhsRefs = ofoldMap (\k -> [(j,k)]) . IM.keys .  childRefs $ refVec ! j
-}

--    referenceEdgeMapping :: HashMap EdgeReference (ResolutionCache (CharacterSequence u v w x y z))
    referenceEdgeMapping = foldMap f unrootedEdges
      where
        f e@(i,j) = 
            case getRootingNode e of
              Just r  -> M.singleton e $ getCache r
              Nothing ->
                  case liftA2 (,) lhsContext rhsContext of
                    Just (lhs, rhs) -> M.singleton e $ localResolutionApplication extensionTransformation lhs rhs
                    Nothing         -> error errorContext
          where
            lhsContext = (i `lookup` contextualNodeDatum) >>= ((j,i) `lookup`)
            rhsContext = (j `lookup` contextualNodeDatum) >>= ((i,j) `lookup`)
            errorContext = unlines
                [ show e
                , show $ M.keys <$> contextualNodeDatum
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


    -- |
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
        -- DAG component we will consider the subtree resolutions for enteing the
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
        generateMemoizedDatum n
          -- Root node case
          | n `elem` roots         = mempty
          -- Leaf node case
          | null unrootedChildRefs = M.singleton (parentRef, n) $ getCache n
          -- Internal node case
          | otherwise              = foldMap deriveDirectedEdgeDatum edgeCombinations
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
                    sibling   = head . filter (/=n) . IM.keys .  childRefs $ refVec ! candidate

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
                    , "Expected exactly 3 adjacent nodes."
                    , "Found: {" <> show (length xs) <> "} " <> show xs
                    ]

            -- Given the three adjacent edges, generate the subtree resolutions
            -- defined by the first element of the tuple being an incomming edge.
--            deriveDirectedEdgeDatum :: (Int, Int, Int) -> Map EdgeReference (ResolutionCache (CharacterSequence u v w x y z))
            deriveDirectedEdgeDatum (i,j,k) = M.singleton (i, n) subtreeResolutions
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
            edgeReferenceFilter es xs = filter (not . any (`elem` invalidEdges) . subtreeEdgeSet) $ toList xs
              where
                invalidEdges       = toList es >>= getDirectedEdges 
                getDirectedEdges e = [e, swap e]


    rootRefWLOG  = NE.head $ rootRefs inputDag

    -- Here we calculate, for each character block, for each display tree in the
    -- phylogenetic DAG, the minimal traversal foci and the corresponding cost.
    -- Note that there could be many minimal traversal foci for each display tree.
 -- sequenceOfEdgesWithMinimalCost :: NonEmpty (NonEmpty (Topology, Minimal Cost, NonEmpty (Minimal Foci)))
    sequenceOfEdgesWithMinimalCost = -- (\x -> trace (show $ (fmap (fmap costOfFoci)) <$> x) x) $
                                     foldMapWithKey1 blockLogic sequenceWLOG
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
        sequenceWLOG = fmap dynamicCharacters . toBlocks . characterSequence . NE.head $ getCache rootRefWLOG

        -- Generate a vector of characters that mirrors the dynamic character
        -- vector of the given block.
        blockLogic k v = V.generate (length v) deriveMinimalCharacterContexts :| []
          where

            -- 
            deriveMinimalCharacterContexts i = result
              where
                result = fromMinimalTopologyContext . foldMap1 gatherMinimalLoci . NE.fromList $ M.assocs edgeIndexCostMapping
                edgeIndexCostMapping   = fmap (fmap (((^. characterCost) . (! i) . dynamicCharacters . (! k) . toBlocks . characterSequence) &&& subtreeEdgeSet)) edgeCostMapping
                gatherMinimalLoci (e, xs) = toMinimalTopologyContext $ (\(c, es) -> (es, c, e)) <$> xs


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

        -- TODO: Only apply logic in the appropriate resolutions.
        f resInfo =
            resInfo
            { totalSubtreeCost  = newTotalCost
--            , localSequenceCost = newLocalCost
            , characterSequence = modifiedSequence
            }
          where
            newTotalCost       = sequenceCost modifiedSequence
--            newLocalCost       = newTotalCost - sum (totalSubtreeCost <$> childResolutionContext) 
            modifiedSequence   = fromBlocks . foldMapWithKey1 g . toBlocks $ characterSequence resInfo
            resolutionTopology = subtreeEdgeSet resInfo
            
            g k charBlock = pure $ charBlock { dynamicCharacters = modifiedDynamicChars }
              where
                modifiedDynamicChars = zipWith h (minimalCostSequence ! k) $ dynamicCharacters charBlock
                h topologyContexts originalDec =
                    originalDec
                      & characterCost .~ costVal
                      & traversalFoci .~ (Just foci :: Maybe TraversalFoci)
                  where
                    (es, costVal, fociEdges) = fromJust $ find ((resolutionTopology ==) . firstOfThree) topologyContexts
                    foci = (\x -> (x, es)) <$> fociEdges
--                    minimaContext   = NE.fromList $ minimaBy (comparing costOfFoci) topologyContexts
--                    (_, costVal, _) = NE.head minimaContext

{--}

(.!>.) :: (Lookup f, Show (Key f)) => f a -> Key f -> a
(.!>.) s k = fromMaybe (error $ "Could not index: " <> show k) $ k `lookup` s


newtype MinimalTopologyContext e = MW { fromMinimalTopologyContext :: (NonEmpty (EdgeSet e, Word, NonEmpty e)) }


instance Ord e => Semigroup (MinimalTopologyContext e) where

    (MW lhs) <> (MW rhs) = MW . NE.fromList $ mergeMin (toList lhs) (toList rhs)
      where
        mergeMin    []     []  = []
        mergeMin    []     ys  = ys
        mergeMin    xs     []  = xs
        mergeMin (x:xs) (y:ys) =
            case comparing firstOfThree x y of
              GT -> y : mergeMin (x:xs)    ys
              LT -> x : mergeMin    xs  (y:ys)
              EQ ->
                let mergedValue =
                      case comparing costOfFoci x y of
                        GT -> x
                        LT -> y
                        EQ -> mergeFoci x y
                in mergedValue : mergeMin xs ys
          where
            mergeFoci (es, c, a) (_, _, b) = (es, c, a <> b)


toMinimalTopologyContext :: Ord e => NonEmpty (EdgeSet e, Word, e) -> MinimalTopologyContext e
toMinimalTopologyContext = MW . fmap (\(x,y,z) -> (x, y, pure z)) . NE.sortWith firstOfThree 


costOfFoci :: (a, b, c) -> b
costOfFoci (_,c,_) = c

firstOfThree :: (a, b, c) -> a
firstOfThree (x, _, _) = x
