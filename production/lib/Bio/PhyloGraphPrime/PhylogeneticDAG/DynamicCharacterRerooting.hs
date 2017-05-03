------------------------------------------------------------------------------
-- |
-- Module      :  Bio.PhyloGraphPrime.PhylogeneticDAG.DynamicCharacterRerooting
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

{-# LANGUAGE BangPatterns, FlexibleContexts #-}

module Bio.PhyloGraphPrime.PhylogeneticDAG.DynamicCharacterRerooting
  ( assignOptimalDynamicCharacterRootEdges
  ) where

import           Bio.Character.Decoration.Additive
import           Bio.Character.Decoration.Dynamic
import           Bio.Sequence
import           Bio.PhyloGraphPrime.Node
import           Bio.PhyloGraphPrime.PhylogeneticDAG.Internal
import           Bio.PhyloGraphPrime.ReferenceDAG.Internal
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
import           Data.Map                  (Map)
import qualified Data.Map           as M
import           Data.Maybe
import           Data.MonoTraversable
import           Data.Semigroup
import           Data.Tuple                (swap)
import qualified Data.Vector        as V
import           Prelude            hiding (lookup, zipWith)

import Debug.Trace


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
     , HasTraversalLocus z (Maybe TraversalLocusEdge)
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
assignOptimalDynamicCharacterRootEdges extensionTransformation (PDAG2 inputDag) = traceShow (show $ (contextualNodeDatum ! 1) ! (2,1)) $ PDAG2 updatedDag
  where

    -- Step 1: Construct a hashmap of all the edges.
    unrootedEdges = rootEdgeReferences <> otherUnrootedEdges
    
    -- Step 2: Create a lazy memoized hashmap of the edge costs for each dynmaic character.

    edgeCostMapping = referenceEdgeMapping

    -- Step 3: For each dynamic character, find the minimal cost edge(s).
    minimalCostSequence = sequenceOfEdgesWithMinimalCost
    
    -- Step 4: Update the dynamic character decoration's cost & add an edge reference.
    updatedDag = inputDag { references = refVec V.// toList modifiedRootRefs }


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
        (x,y) =
          if   (b `elem`) . IM.keys . childRefs $ refVec ! a
          then (a,b)
          else (b,a)

    getRootingNode e | trace ("getRooting @ " <> show e) False = undefined
    getRootingNode (i,j)
      | (i,j) == (1,4) = trace (unwords ["1 parents: ", show iRefs, "4 parents: ", show jRefs, "Intersection", show intersected]) value
      | otherwise = value
      where
        value = headMay . otoList $intersected
        intersected = iRefs `IS.intersection` jRefs
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
        f e | trace ("f @ " <> show e) False = undefined
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
            unrootedParentRefs = fmap g $ otoList originalRootingParentRefs
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
            unrootedAdjacentRefs = -- (\x -> trace ("(" <> show n <> ", _) <-< "<> show x) x) $
                                   take 3 $ take 2 unrootedChildRefs <> take 2 unrootedParentRefs

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
--            deriveDirectedEdgeDatum (i,j,k) | trace (show (i,j,k)) False = undefined
            deriveDirectedEdgeDatum (i,j,k) = M.singleton (i, n) subtreeResolutions
              where
                lhsMemo       = (contextualNodeDatum ! j) .!>. (n, j) 
                rhsMemo       = (contextualNodeDatum ! k) .!>. (n, k) 
                lhsContext    = trace (unwords ["filtering", show [k], "from", show (n, j)]) $
                                edgeReferenceFilter [k] lhsMemo
                rhsContext    = trace (unwords ["filtering", show [j], "from", show (n, k)]) $
                                edgeReferenceFilter [j] rhsMemo

                localResolutionApplication2 f xs ys = localResolutionApplication f (trace ("LHS: " <> show (n,j)) xs) (trace ("RHS: " <> show (n,k)) ys)
                subtreeResolutions
                  | trace ( unwords [show (i,n), show (n,j), show (n,k), show (i `elem` unrootedParentRefs)] ) False = undefined
                  -- Check for the recursive memoized form's base case
                  | i `oelem` originalRootingParentRefs = trace ("Assuming cache for: " <> show (i,n)) $ getCache n
                  | otherwise = trace ("NOT assuming cache for: " <> show (i,n)) $
                    case (isNetworkEdge (n,j), isNetworkEdge (n,k)) of
                      -- Neither are network edges
                      -- Perform standard tree operation
                      (False, False) ->
                          if   not $ isNetworkEdge (i,n)
                          then localResolutionApplication2 extensionTransformation lhsMemo rhsMemo 
                          else trace ("IN HERE " <> show (i,n)) $
                            case (lhsContext, rhsContext) of
                             (  [],   []) -> error "Well, that's ALSO embarassing..."
                             (x:xs,   []) -> x:|xs
                             (  [], y:ys) -> y:|ys
                             (x:xs, y:ys) -> localResolutionApplication2 extensionTransformation (x:|xs) (y:|ys)

                      (False,  True) ->
                          case lhsContext of
                            []   -> rhsMemo
                            x:xs -> let lhsMemo' = x:|xs
                                    in sconcat $ lhsMemo' :| [localResolutionApplication2 extensionTransformation lhsMemo' rhsMemo]

                      (True , False) ->
                          case rhsContext of
                            []   -> rhsMemo
                            x:xs -> let rhsMemo' = x:|xs
                                    in sconcat $ rhsMemo' :| [localResolutionApplication2 extensionTransformation lhsMemo  rhsMemo']

                      (True ,  True) ->
                          case (lhsContext, rhsContext) of
                             (  [],   []) -> error "Well, that's embarassing..."
                             (x:xs,   []) -> x:|xs
                             (  [], y:ys) -> y:|ys
                             (x:xs, y:ys) -> sconcat $ (x:|xs) :| [y:|ys]


{-                
                joinedContext = traceShowId $ 
                    -- Check for the recursive memoized form's base case
                    if   [i] == unrootedParentRefs
                    then getCache n
                    else localResolutionApplication extensionTransformation ((contextualNodeDatum ! j) .!>. (n, j)) ((contextualNodeDatum ! k) .!>. (n, k))
-}

            -- Filter from the resolution cache all resolutions that have 'x'
            -- connected to the subtree.
--          edgeReferenceFilter :: Int -> ResolutionCache (CharacterSequence u v w x y z) -> ResolutionCache (CharacterSequence u v w x y z)
            edgeReferenceFilter es xs = (\x -> trace ("Good edgeSets: " <> show (subtreeEdgeSet <$> x)) x) $
                                        filter (not . any (`elem` invalidEdges) . subtreeEdgeSet) $ toList xs
{--
                case filter (any (`elem` invalidEdges) . subtreeEdgeSet) $ toList xs of
                  []   -> error "OHHH NOOOES!"
                  y:ys -> y:|ys
--}
              where
                invalidEdges     = (\x -> trace ("Bad edges: " <> show x) x) $ toList es >>= getDirectedEdges 
                getDirectedEdges = uncurry (<>) . (id &&& fmap swap) . M.keys . (contextualNodeDatum !)


    rootRefWLOG  = NE.head $ rootRefs inputDag
    sequenceWLOG = fmap dynamicCharacters . toBlocks . characterSequence . NE.head $ getCache rootRefWLOG


    sequenceOfEdgesWithMinimalCost = foldMapWithKey1 f sequenceWLOG
      where
        f k v = V.generate (length v) g :| []
          where
            g i = result
              where
                result@(_minimumEdge, _minimumCost) = fromJust $ foldlWithKey h Nothing edgeIndexCostMapping
                edgeIndexCostMapping = fmap (fmap ((^. characterCost) . (! i) . dynamicCharacters . (! k) . toBlocks . characterSequence)) edgeCostMapping
                h acc e cs =
                    case acc of
                      Nothing      -> Just (e, c)
                      Just (_e', c') ->
                        if   c < c'
                        then Just (e, c)
                        else acc
                  where
                    c = minimum cs


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
        f resInfo = resInfo { characterSequence = modifiedSequence  }
          where
            modifiedSequence = fromBlocks . foldMapWithKey1 g . toBlocks $ characterSequence resInfo
        g k charBlock = pure $ charBlock { dynamicCharacters = modifiedDynamicChars }
          where
            modifiedDynamicChars = zipWith h (minimalCostSequence ! k) $ dynamicCharacters charBlock
            h (edgeVal, costVal) originalDec = originalDec
                                                 & characterCost  .~ costVal
                                                 & traversalLocus .~ Just edgeVal
{--}

(.!>.) :: (Lookup f, Show (Key f)) => f a -> Key f -> a
(.!>.) _ k | trace ("x ! " <> show k) False = undefined
(.!>.) s k =
  case k `lookup` s of
    Just v  -> v
    Nothing -> error $ "Could not index: " <> show k
