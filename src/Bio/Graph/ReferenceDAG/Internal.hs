-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Graph.ReferenceDAG.Internal
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveFunctor, DeriveGeneric, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeFamilies #-}

module Bio.Graph.ReferenceDAG.Internal where

import           Bio.Graph.LeafSet
import           Bio.Graph.Component
import           Control.Arrow                    ((&&&),(***))
import           Control.DeepSeq
import           Control.Lens                     (lens)
import           Control.Monad.State.Lazy
import           Data.Bifunctor
import           Data.EdgeSet
import           Data.Foldable
import           Data.Functor                     ((<$))
import           Data.GraphViz.Attributes
import           Data.GraphViz.Printing    hiding ((<>)) -- Seriously, why is this redefined?
import           Data.GraphViz.Types       hiding (attrs)
import           Data.GraphViz.Types.Graph hiding (node)
import           Data.Hashable                    (Hashable)
import qualified Data.HashMap.Strict       as HM
import           Data.IntMap                      (IntMap)
import qualified Data.IntMap               as IM
import           Data.IntSet                      (IntSet)
import qualified Data.IntSet               as IS
import           Data.Key
import           Data.List                        (intercalate)
import           Data.List.NonEmpty               (NonEmpty ((:|)))
import qualified Data.List.NonEmpty        as NE
import           Data.List.Utility                (isSingleton)
import           Data.Monoid                      ((<>))
import           Data.MonoTraversable
import           Data.Semigroup.Foldable
import           Data.Set                         (Set)
import qualified Data.Set                  as S
import           Data.String
import           Data.Tree                        (unfoldTree)
import           Data.Tree.Pretty                 (drawVerticalTree)
import           Data.Vector                      (Vector)
import qualified Data.Vector               as V
import           Data.Vector.Instances            ()
import           GHC.Generics
import           Numeric.Extended.Real
import           Prelude                   hiding (lookup, zipWith)
import           Text.Newick.Class
import           Text.XML.Custom


-- |
-- A constant time access representation of a directed acyclic graph.
data  ReferenceDAG d e n
    = RefDAG
    { references :: Vector (IndexData e n)
    , rootRefs   :: NonEmpty Int
    , graphData  :: GraphData d
    } deriving (Generic)


-- |
-- A labeled record for each "node" in the graph containing the node decoration,
-- a set of parent references, and a set of child references with edge decorations.
--
-- Type annotations:
-- * e = edge decorations, as yet unused
-- * n = node label: 'Maybe'('String')
data  IndexData e n
    = IndexData
    { nodeDecoration :: n
    , parentRefs     :: IntSet
    , childRefs      :: IntMap e
    } deriving (Generic, Show)


-- |
-- Annotations which are global to the graph
--
-- -- Type annotations:
-- * d = graph metadata
data  GraphData d
    = GraphData
    { dagCost           :: {-# UNPACK #-} !ExtendedReal
    , networkEdgeCost   :: {-# UNPACK #-} !ExtendedReal
    , rootingCost       :: {-# UNPACK #-} !Double
    , totalBlockCost    :: {-# UNPACK #-} !Double
    , graphMetadata     :: d
    } deriving (Functor, Generic)


-- |
-- This will be used below to print the node type to XML and Newick.
data NodeClassification
    = NodeClassification
    | LeafNode
    | NetworkNode
    | RootNode
    | TreeNode
   deriving (Eq, Show)


-- |
-- A reference to a node within the 'ReferenceDAG'.
newtype NodeRef = NR Int deriving (Eq, Enum)


type instance Key (ReferenceDAG d e) = Int


-- | (✔)
instance Bifunctor (ReferenceDAG d) where

    bimap f g dag =
        RefDAG
        { references = h <$> references dag
        , rootRefs   = rootRefs  dag
        , graphData  = graphData dag
        }
      where
        h (IndexData node parentRefs' childRefs') = IndexData (g node) parentRefs' $ f <$> childRefs'


-- | (✔)
instance Foldable (ReferenceDAG d e) where

    foldMap f = foldMap (f . nodeDecoration) . references


-- | (✔)
instance FoldableWithKey (ReferenceDAG d e) where

    {-# INLINE foldrWithKey #-}
    foldrWithKey f e = V.ifoldr' (\i n a -> f i (nodeDecoration n) a) e . references

    {-# INLINE foldlWithKey #-}
    foldlWithKey f e = V.ifoldl' (\a i -> f a i . nodeDecoration) e . references


-- | (✔)
instance Functor (ReferenceDAG d e) where

    fmap f dag =
        RefDAG
        { references = g <$> references dag
        , rootRefs   = rootRefs  dag
        , graphData  = graphData dag
        }
      where
        g (IndexData node parentRefs' childRefs') = IndexData (f node) parentRefs' childRefs'


-- | (✔)
instance HasLeafSet (ReferenceDAG d e n) (LeafSet n) where

    leafSet = lens getter undefined
        where
            getter (RefDAG v _ _) = LeafSet $ foldMap f v

            f e | null (childRefs e) = [nodeDecoration e]
                | otherwise          = mempty


-- | (✔)
instance (NFData d, NFData e, NFData n) => NFData (ReferenceDAG d e n)


-- | (✔)
instance (NFData e, NFData n) => NFData (IndexData e n)


-- | (✔)
instance (NFData d) => NFData (GraphData d)


-- | (✔)
instance PhylogeneticComponent (ReferenceDAG d e n) NodeRef e n where

    parents   i dag = fmap toEnum . otoList . parentRefs $ references dag ! fromEnum i

    children  i dag = fmap toEnum . IM.keys . childRefs  $ references dag ! fromEnum i

    roots           = fmap toEnum . rootRefs

    leaves          = foldMapWithKey f . references
      where
        f i x
          | null $ childRefs x = [toEnum i]
          | otherwise          = mempty

    nodeCount       = length . references

    nodeDatum i dag = nodeDecoration $ references dag ! fromEnum i

    edgeDatum (i,j) dag =  fromEnum j `lookup` childRefs (references dag ! fromEnum i)

    isComponentNode = isNetworkNode

    isNetworkNode i dag = olength ps == 2 && length cs == 1
      where
        iPoint = references dag ! fromEnum i
        ps = parentRefs iPoint
        cs = childRefs  iPoint

    isTreeNode i dag = olength ps == 1 && length cs == 2
      where
        iPoint = references dag ! fromEnum i
        ps = parentRefs iPoint
        cs = childRefs  iPoint

    isLeafNode i dag =  null . childRefs  $ references dag ! fromEnum i

    isRootNode i dag = onull . parentRefs $ references dag ! fromEnum i

    networkResolutions = pure


-- | (✔)
instance PhylogeneticNetwork (ReferenceDAG d e n) NodeRef e n where

    root = toEnum . NE.head . rootRefs

    -- TODO: Broken
    treeResolutions = pure


-- | (✔)
instance PhylogeneticTree (ReferenceDAG d e n) NodeRef e n where

    parent i dag = fmap toEnum . headMay . otoList . parentRefs $ references dag ! fromEnum i


-- | (✔)
instance Foldable f => PrintDot (ReferenceDAG d e (f String)) where

    unqtDot       = unqtDot . uncurry mkGraph . getDotContext 0 0

    toDot         = toDot   . uncurry mkGraph . getDotContext 0 0

    unqtListToDot = unqtDot . uncurry mkGraph . bimap mconcat mconcat . unzip . fmap (getDotContext 0 0)

    listToDot     = toDot   . uncurry mkGraph . bimap mconcat mconcat . unzip . fmap (getDotContext 0 0)


-- | (✔)
instance Show (GraphData m) where

    show x = unlines
        [ "DAG total cost:           " <> show (dagCost x)
        , "DAG network edge cost:    " <> show (networkEdgeCost x)
        , "DAG mutli-rooting cost:   " <> show (rootingCost     x)
        , "DAG character block cost: " <> show (totalBlockCost  x)
        ]


-- | (✔)
instance {- (Show e, Show n) => -} Show (ReferenceDAG d e n) where

    show dag = intercalate "\n" [topologyRendering dag, "", referenceRendering dag]


-- | (✔)
instance Foldable f => ToNewick (ReferenceDAG d e (f String)) where

    toNewick refDag = mconcat [ newickString, "[", show cost, "]" ]
      where
        (_,newickString) = generateNewick namedVec rootRef mempty
        cost     = dagCost $ graphData refDag
        rootRef  = NE.head $ rootRefs refDag
        vec      = references refDag

        namedVec = zipWith (\x n -> n { nodeDecoration = x }) labelVec vec
        labelVec = (`evalState` (1,1,1)) $ mapM deriveLabel vec -- All network nodes have "htu\d" as nodeDecoration.
        deriveLabel :: Foldable f => IndexData e (f String) -> State (Int, Int, Int) String
        deriveLabel node =
            case toList $ nodeDecoration node of
              x:_ -> pure x
              []  -> do
                  (lC, nC, tC) <- get
                  case getNodeType node of
                    LeafNode    -> do
                        put (lC+1, nC, tC)
                        pure $ "Leaf_" <> show lC
                    NetworkNode -> do
                        put (lC, nC+1, tC)
                        pure $ "HTU_"  <> show nC
                    _           -> do
                        put (lC, nC, tC+1)
                        pure $ "Node_" <> show tC


-- | (✔)
instance ToXML (GraphData m) where

    toXML gData = xmlElement "Graph_data" attrs contents
        where
            attrs = []
            contents = [ Left ( "DAG_total_cost"          , show $ dagCost         gData)
                       , Left ( "DAG_network_edge_cost"   , show $ networkEdgeCost gData)
                       , Left ( "DAG_rooting_cost"        , show $ rootingCost     gData)
                       , Left ( "DAG_character_block_cost", show $ totalBlockCost  gData)
                       ]


-- | (✔)
instance ToXML n => ToXML (IndexData e n) where

   toXML indexData = toXML $ nodeDecoration indexData
   -- ("Node_type", show $ getNodeType indexData)


-- | (✔)
instance Foldable f => ToXML (ReferenceDAG d e (f String)) where
--instance (ToXML n) => ToXML (ReferenceDAG d e n) where

    toXML dag = xmlElement "Directed_acyclic_graph" [] [newick, meta, vect]
      where
          -- leafs    = Right $ collapseElemList "Leaf set" [] [(dag ^. leafSet)]
          -- fmap id . (^. leafSet) <$> forests
          meta   = Right . toXML $ graphData dag
          newick = Left ("Newick_representation", toNewick dag)
          vect   = Right $ collapseElemList "Nodes" [] dag  -- Because ReferenceDAG is Foldable over Vector(IndexData)


-- |
-- Produces a set of directed references representing all edges in the DAG.
-- Equivelant to:
--
-- > referenceTreeEdgeSet dag `union` referenceTreeEdgeSet dag
referenceEdgeSet :: ReferenceDAG d e n -> EdgeSet (Int, Int)
referenceEdgeSet = foldMapWithKey f . references
  where
    f i = foldMap (\e -> singletonEdgeSet (i,e)) . IM.keys . childRefs


-- |
-- Produces a set of directed references representing all /tree/ edges in the DAG.
-- Omits /network/ edges in the DAG. The resulting 'EdgeSet' may not be connected.
--
-- Equivelant to:
--
-- > referenceEdgeSet dag `difference` referenceNetworkEdgeSet dag
--
-- The follwoing will always hold:
--
-- > null (referenceTreeEdgeSet dag `intersecttion` referenceTreeEdgeSet dag)
referenceTreeEdgeSet :: ReferenceDAG d e n -> EdgeSet (Int, Int)
referenceTreeEdgeSet dag = foldMapWithKey f refs
  where
    refs = references dag
    f i = foldMap (\e -> singletonEdgeSet (i,e)) . filter childHasOnlyOneParent . IM.keys . childRefs
    childHasOnlyOneParent = isSingleton . otoList . parentRefs . (refs !)


-- |
-- Produces a set of directed references representing all /Netowrk/ edges in the DAG.
-- Omits /tree/ edges in the DAG. The resulting 'EdgeSet' *will not* be connected.
--
-- Equivelant to:
--
-- > referenceEdgeSet dag `difference` referenceTreeEdgeSet dag
--
-- The follwoing will always hold:
--
-- > null (referenceTreeEdgeSet dag `intersecttion` referenceTreeEdgeSet dag)
referenceNetworkEdgeSet :: ReferenceDAG d e n -> EdgeSet (Int, Int)
referenceNetworkEdgeSet dag = foldMapWithKey f refs
  where
    refs = references dag
    f i = foldMap (\e -> singletonEdgeSet (i,e)) . filter childHasMoreThanOneParent . IM.keys . childRefs
    childHasMoreThanOneParent = not . isSingleton . otoList . parentRefs . (refs !)


-- |
-- Produces a set of undirected references representing all undirected edges that
-- have root edges in the DAG, if the root(s) were removed an to create an
-- undirected network. Omits all edges in the undirected network representation
-- of the DAG that are not specified as a root of the DAG. The resulting 'EdgeSet'
-- (unless of trivial cardinality) *will not* be connected.
undirectedRootEdgeSet :: ReferenceDAG d e n -> EdgeSet (Int, Int)
undirectedRootEdgeSet dag = foldMap f $ rootRefs dag
  where
    refs = references dag
    f i  = makeRootEdge i . IM.keys . childRefs $ refs ! i
    makeRootEdge r kids =
        case kids of
          []    -> mempty
          [x]   -> singletonEdgeSet (r,x)
          x:y:_ -> singletonEdgeSet (x,y)


-- |
-- Given two edges and two functions describing the way to construct new node
-- values, adds a new edge between the two supplied edges and constructs two
-- intermediary nodes.
connectEdge
  :: Monoid e
  => ReferenceDAG d e n
  -> (n -> n -> n -> n) -- ^ Function describing how to construct the new origin internal node: parent, existing child, new child
  -> (n -> n -> n) -- ^ Function describing how to construct the new target internal node: existing parent, existing child
  -> (Int, Int) -- ^ Origin edge (coming from)
  -> (Int, Int) -- ^ Target edge (going to)
  -> ReferenceDAG d e n
--connectEdge dag _ _ origin target | trace (unlines  ["Origin: " <> show origin, "Target: " <> show target, "Input:", show dag ]) False = undefined
connectEdge dag originTransform targetTransform (ooRef, otRef) (toRef, ttRef) = {- (\x -> trace ("Output:\n"<>show x) x) -} newDag
  where
    refs    = references dag
    oldLen  = length refs
    oNewRef = oldLen -- synonym
    tNewRef = oldLen + 1
    newDag  =
      RefDAG
        <$> const newVec
        <*> rootRefs
        <*> graphData
        $ dag

    getDatum = nodeDecoration . (refs !)

    newVec = V.generate (oldLen + 2) g
      where
        g i
          | i <  oldLen = f i $ refs ! i
          -- This is the new node on the origin edge
          | i == oldLen = IndexData
                            (originTransform (getDatum ooRef) (getDatum otRef) (nodeDecoration $ newVec ! tNewRef))
                            (IS.singleton ooRef)
                            (IM.singleton otRef mempty <> IM.singleton tNewRef mempty)
          -- This is the new node on the target edge
          | otherwise   = IndexData
                            (targetTransform (getDatum toRef) (getDatum ttRef))
                            (IS.singleton toRef <> IS.singleton oNewRef)
                            (IM.singleton ttRef mempty)

    f i x = IndexData (nodeDecoration x) pRefs cRefs
      where
        ps = parentRefs x
        cs = childRefs  x
        pRefs
          | i == otRef && ooRef `oelem` ps = IS.insert oNewRef $ IS.delete ooRef ps
          | i == ttRef && toRef `oelem` ps = IS.insert tNewRef $ IS.delete toRef ps
          | otherwise = ps
        cRefs =
          case otRef `lookup` cs of
            Just v  -> IM.insert oNewRef v $ IM.delete otRef cs
            Nothing ->
              case ttRef `lookup` cs of
                Just v  -> IM.insert tNewRef v $ IM.delete ttRef cs
                Nothing -> cs


-- |
-- Add a node on the supplied edge, creating two new incident edges.
invadeEdge
  :: Monoid e
  => ReferenceDAG d e n
  -> (n -> n -> n -> n) -- ^ Function describing how to construct the new internal node, parent, old child, and new child
  -> n
  -> (Int, Int)
  -> ReferenceDAG d e n
invadeEdge dag transformation node (oRef, iRef) = newDag
  where
    oldLen  = length refs
    newRef  = oldLen -- synonym
    refs    = references dag
    newDag  =
      RefDAG
        <$> const newVec
        <*> rootRefs
        <*> graphData
        $ dag

    getDatum = nodeDecoration . (refs !)

    newVec = V.generate (oldLen + 2) g
      where
        g i
          | i <  oldLen = f i $ refs ! i
          | i == oldLen = IndexData
                            (transformation (getDatum oRef) (getDatum iRef) node)
                            (IS.singleton oRef)
                            (IM.singleton iRef mempty <> IM.singleton (newRef + 1) mempty)
          | otherwise   = IndexData node (IS.singleton newRef) mempty

    f i x = IndexData (nodeDecoration x) pRefs cRefs
      where
        ps = parentRefs x
        cs = childRefs  x
        pRefs
          | i == iRef && oRef `oelem` ps = IS.insert newRef $ IS.delete oRef ps
          | otherwise = ps
        cRefs =
          case iRef `lookup` cs of
            Just v  -> IM.insert newRef v $ IM.delete iRef cs
            Nothing -> cs


-- |
-- /O(n*i)/ where /i/ is the number of missing indicies.
-- Assuming all indicies in the input /x/ are positive, /i/ = 'findMax x - length x'.
--
-- Takes an 'IntMap' that might not have a contiguous index range and makes the
-- range contiguous [0, length n - 1].
contractToContiguousVertexMapping :: IntMap (IntSet, t, IntMap a) -> IntMap (IntSet, t, IntMap a)
contractToContiguousVertexMapping inputMap = foldMapWithKey contractIndices inputMap
  where
    missingIndicies = otoList . snd . foldl' f (0, mempty) $ IM.keys inputMap
      where
        f (expectedValue, missing) actualValue
          | expectedValue == actualValue = (expectedValue + 1, missing)
          | otherwise                    = (  actualValue + 1, missing <> IS.fromList [expectedValue .. actualValue - 1])

    decrementIndex originalIndex = originalIndex - subtrahend
      where
        subtrahend = length $ takeWhile (<originalIndex) missingIndicies

    contractIndices key (iSet, datum, iMap) = IM.singleton key' value'
      where
        key'   = decrementIndex key
        value' = (IS.map decrementIndex iSet, datum, IM.mapKeysMonotonic decrementIndex iMap)


-- |
-- Set the metadata to a "default" value.
--
-- Default in the function's name is used as a verb, not a noun.
defaultGraphMetadata :: Monoid m => GraphData d -> GraphData m
defaultGraphMetadata =
    GraphData
      <$> dagCost
      <*> networkEdgeCost
      <*> rootingCost
      <*> totalBlockCost
      <*> const mempty


-- |
-- Overwrite the current graph metadata with a default value.
--
-- Default in the function's name is used as a verb, not a noun.
defaultMetadata :: Monoid m => ReferenceDAG d e n -> ReferenceDAG m e n
defaultMetadata =
    RefDAG
      <$> references
      <*> rootRefs
      <*> defaultGraphMetadata . graphData


-- |
-- Ensure that each vertex has either:
--
--   - In-degree 0, out degree 1
--
--   - In-degree 0, out degree 2
--
--   - In-degree 1, out degree 2
--
--   - In-degree 1, out degree 0
--
--   - In-degree 2, out degree 1
--
-- Expand or contract edges as necessary to enforce the above invariant.
expandVertexMapping :: (Monoid a, Monoid t) => IntMap (IntSet, t, IntMap a) -> IntMap (IntSet, t, IntMap a)
expandVertexMapping unexpandedMap = snd . foldl' expandEdges (initialCounter+1, unexpandedMap) $ IM.keys unexpandedMap
  where
    (initialCounter, _) = IM.findMax unexpandedMap

    expandEdges acc@(counter, mapping) key =
        case parentCount of
          0 -> if   childCount <= 2
               then acc
               else handleTooManyChildren

          1 -> case childCount of
                  0 -> acc
                  1 -> collapseEdge
                  2 -> acc
                  -- One too many childern
                  3 -> expandOutExtraChild
                  -- Far too many children
                  _ -> handleTooManyChildren

          2 -> case childCount of
                  0 -> expandOutVertexToChild
                  1 -> acc
                  -- Too many children
                  _ -> expandEdges expandOutVertexToChild counter

          -- One too many parents
          3 -> expandEdges expandOutExtraParent counter

          -- Far too many parents
          _ -> expandEdges handleTooManyParents key

      where
        (iSet, nDatum, iMap) = mapping ! key

        parentCount = olength iSet
        childCount  =  length iMap

        -- To collapse an edge
        collapseEdge = (counter, collapsedEdgeMapping)
          where
            updateAncestoralEdge (x,y,z) = (IS.insert parentKey $ IS.delete key x, y, z)
            updateDescendentEdge (x,y,z) = (x, y, IM.insert childKey edgeValue $ IM.delete key z)
            parentKey             = IS.findMin iSet
            (childKey, edgeValue) = IM.findMin iMap
            collapsedEdgeMapping  = IM.adjust updateAncestoralEdge  childKey
                                  . IM.adjust updateDescendentEdge parentKey
                                  . IM.delete key
                                  $ mapping


        expandOutVertexToChild = (counter + 1, expandedMapping)
          where
            ancestoralVertex = (            iSet, mempty, IM.singleton counter mempty)
            descendentVertex = (IS.singleton key, nDatum,                        iMap)

            updateParent = flip (adjust setParent)
              where
                setParent (_,y,z) = (IS.singleton counter, y, z)

            expandedMapping  = IM.insert counter descendentVertex
                             . replace   key     ancestoralVertex
                             . foldl' updateParent mapping
                             $ IM.keys iMap


        expandOutExtraChild  = (counter + 1, expandedMapping)
          where
            ancestoralVertex = (            iSet, nDatum, singledChildMap)
            descendentVertex = (IS.singleton key, mempty, reducedChildMap)

            singledChildMap  = IM.singleton counter mempty <> IM.singleton minChildKey edgeValue
            reducedChildMap  = IM.delete minChildKey iMap

            (minChildKey, edgeValue) = IM.findMin iMap

            updateParent = flip (adjust setParent)
              where
                setParent (_,y,z) = (IS.singleton counter, y, z)

            expandedMapping  = IM.insert counter descendentVertex
                             . replace   key     ancestoralVertex
                             . foldl' updateParent mapping
                             $ IM.keys reducedChildMap


        handleTooManyChildren = rhsRecursiveResult
          where
            (lhsChildMap, rhsChildMap) = (IM.fromList *** IM.fromList) . splitAt (childCount `div` 2) $ toKeyedList iMap

            ancestoralVertex = (            iSet, nDatum, newChildMap)
            lhsNewVertex     = (IS.singleton key, mempty, lhsChildMap)
            rhsNewVertex     = (IS.singleton key, mempty, rhsChildMap)

            newChildMap      = IM.singleton counter mempty <> IM.singleton (counter+1) mempty

            lhsUpdateParent = flip (adjust setParent)
              where
                setParent (_,y,z) = (IS.singleton  counter  , y, z)

            rhsUpdateParent = flip (adjust setParent)
              where
                setParent (_,y,z) = (IS.singleton (counter+1), y, z)

            expandedMapping  = IM.insert  counter    lhsNewVertex
                             . IM.insert (counter+1) rhsNewVertex
                             . replace    key        ancestoralVertex
                             . flip (foldl' rhsUpdateParent) (IM.keys rhsChildMap)
                             . foldl' lhsUpdateParent mapping
                             $ IM.keys lhsChildMap

            lhsRecursiveResult = expandEdges (counter+2, expandedMapping) counter
            rhsRecursiveResult = expandEdges lhsRecursiveResult (counter+1)


        -- The third parent node will point to a new node
        -- The child will, point to the new node
        -- The new node will point to the original subtree
        expandOutExtraParent = (counter + 1, expandedMapping)
          where
            ancestoralVertex = (reducedParentMap, nDatum, singledChildMap)
            -- Should pointed at by the current node and the pruned parent
            -- Sholud point   to the original subtree
            descendentVertex = (singledParentMap, mempty, iMap)

            singledChildMap  = IM.singleton counter mempty
            singledParentMap = IS.fromList [key, maxParentKey]
            reducedParentMap = IS.delete    maxParentKey iSet

            maxParentKey = IS.findMax iSet

            updateChildRef (x,y,z) = (x, y, IM.insert counter (z ! key) $ IM.delete key z)

            updateParent = flip (adjust setParent)
              where
                setParent (_,y,z) = (IS.singleton counter, y, z)

            expandedMapping  = IM.insert counter        descendentVertex
                             . replace   key            ancestoralVertex
                             . adjust    updateChildRef maxParentKey
                             . foldl'    updateParent   mapping
                             $ IM.keys iMap


        handleTooManyParents = rhsRecursiveResult
          where
            (lhsParentSet, rhsParentSet) = (IS.fromList *** IS.fromList) . splitAt (parentCount `div` 2) $ otoList iSet

            descendantVertex = (newParentSet, nDatum, iMap)
            lhsNewVertex     = (lhsParentSet, mempty, IM.singleton key mempty)
            rhsNewVertex     = (rhsParentSet, mempty, IM.singleton key mempty)

            newParentSet = IS.fromList [counter, counter+1]

            lhsUpdateChildren = flip (adjust setChild)
              where
                setChild (x,y,z) = (x, y, IM.insert  counter    (z ! key) $ IM.delete key z)

            rhsUpdateChildren = flip (adjust setChild)
              where
                setChild (x,y,z) = (x, y, IM.insert (counter+1) (z ! key) $ IM.delete key z)

            expandedMapping = IM.insert  counter    lhsNewVertex
                            . IM.insert (counter+1) rhsNewVertex
                            . replace    key        descendantVertex
                            . flip (ofoldl' rhsUpdateChildren) rhsParentSet
                            . ofoldl' lhsUpdateChildren mapping
                            $ lhsParentSet

            lhsRecursiveResult = expandEdges (counter+2, expandedMapping) counter
            rhsRecursiveResult = expandEdges lhsRecursiveResult (counter+1)


-- |
-- 'generateNewick' recursively retrieves the node name at a given index in an 'IndexData' vector.
-- The set acts as an accumulator to remember which network nodes have been referenced thus far.
-- Each network node has already been assigned an index. That index will be used as the node reference in the eNewick output.
generateNewick :: Vector (IndexData e String) -> Int -> S.Set String -> (S.Set String, String)
generateNewick refs idx htuNumSet = (finalNumSet, finalStr)
  where
    node = refs ! idx

    (finalNumSet, finalStr) =
        case getNodeType node of
          LeafNode    -> (htuNumSet, nodeDecoration node)
          NetworkNode ->
            let childIdx          = head . toList . IM.keys $ childRefs node
                htuNumberStr      = nodeDecoration node
                updatedHtuNumSet  = S.insert htuNumberStr htuNumSet
                (updatedHtuNumSet', subtreeNewickStr) = generateNewick refs childIdx updatedHtuNumSet
            in if   htuNumberStr `elem` htuNumSet
               -- If the node is already a member, no update to htuNumberSet.
               then (         htuNumSet, mconcat [ "#", htuNumberStr ])
               -- Network node but not yet in set of traversed nodes, so update htuNumberSet.
               else ( updatedHtuNumSet', mconcat [ subtreeNewickStr, "#", htuNumberStr ])

              -- Both root and tree node. Originally root was a separate case that resolved to an error,
              -- but the first call to this fn is always root, so can't error out on that.
              -- In no case does the htuNumberSet update.
          _           ->
             case IM.keys $ childRefs node of
                 []              -> error "Graph construction should prevent a 'root' node or 'tree' node with no children."
                 lhsIdx:rhsIdx:_ -> ( updatedHtuNumSet'
                                    , mconcat [ "(", lhsReturnString, ", ", rhsReturnString, ")" ]
                                    )
                   where
                     (updatedHtuNumSet , lhsReturnString) = generateNewick refs lhsIdx htuNumSet
                     (updatedHtuNumSet', rhsReturnString) = generateNewick refs rhsIdx updatedHtuNumSet
                    -- Next should happen only under network node, but here for completion.
                 [singleChild]   -> ( updatedHtuNumSet'
                                    , mconcat [ "(", updatedNewickStr, ")" ]
                                    )
                   where
                     (updatedHtuNumSet', updatedNewickStr) = generateNewick refs singleChild htuNumSet


-- |
-- Get the edge set of the DAG. Includes edges to root nodes.
getEdges :: ReferenceDAG d e n -> EdgeSet (Int, Int)
getEdges dag = foldMap1 f $ rootRefs dag
  where
    refs = references dag
    f i  = foldMap f childKeys <> currentEdges
      where
        childKeys    = IM.keys . childRefs $ refs ! i
        currentEdges = foldMap (\x -> singletonEdgeSet (i,x)) childKeys


-- |
-- Takes in 'IndexData' and returns 'NodeClassification' based on number of parents and children of node.
getNodeType :: IndexData e n -> NodeClassification
getNodeType e =
    case (olength $ parentRefs e, length $ childRefs e) of
      (0,_) -> RootNode
      (_,0) -> LeafNode
      (1,2) -> TreeNode
      (2,1) -> NetworkNode
      (p,c) -> error $ "Incoherently constructed graph when determining NodeClassification: parents " <> show p <> " children " <> show c


-- |
-- Use the supplied transformation to fold the Node values of the DAG into a
-- 'Monoid' result. The fold is *loosely* ordered from *a* root node toward the
-- leaves.
nodeFoldMap :: Monoid m => (n -> m) -> ReferenceDAG d e n -> m
nodeFoldMap f = foldMap f . fmap nodeDecoration . references


-- |
-- Applies a traversal logic function over a 'ReferenceDAG' in a /post-order/ manner.
--
-- The logic function takes a current node decoration and
-- a list of child node decorations with the logic function already applied,
-- and returns the new decoration for the current node.
nodePostOrder :: (n -> [n'] -> n') -> ReferenceDAG d e n -> ReferenceDAG d e n'
nodePostOrder f dag = RefDAG <$> const newReferences <*> rootRefs <*> graphData $ dag
  where
    dagSize       = length $ references dag
    newReferences = V.generate dagSize h
      where
        h i = IndexData <$> const (memo ! i) <*> parentRefs <*> childRefs $ references dag ! i
    memo = V.generate dagSize h
      where
        h i = f datum $ (memo !) <$> childIndices
          where
            datum        = nodeDecoration node
            node         = references dag ! i
            childIndices = IM.keys $ childRefs node


-- |
-- Applies a traversal logic function over a 'ReferenceDAG' in a /pre-order/ manner.
--
-- The logic function takes a current node decoration and
-- a list of parent node decorations with the logic function already applied,
-- and returns the new decoration for the current node.
nodePreOrder :: (n -> [(Word, n')] -> n') -> ReferenceDAG d e n -> ReferenceDAG d e n'
nodePreOrder f dag = RefDAG <$> const newReferences <*> rootRefs <*> graphData $ dag
  where
    dagSize       = length $ references dag
    newReferences = V.generate dagSize h
      where
        h i = IndexData <$> const (memo ! i) <*> parentRefs <*> childRefs $ references dag ! i
    memo = V.generate dagSize h
      where
        h i = f datum $ (childPosition &&& (memo !)) <$> parentIndices
          where
            node            = references dag ! i
            datum           = nodeDecoration node
            parentIndices   = otoList $ parentRefs node
            -- In sparsely connected graphs (like ours) this will be effectively constant.
            childPosition j = toEnum . length . takeWhile (/=i) . IM.keys . childRefs $ references dag ! j


-- |
-- Renders the 'ReferenceDAG' without showing the node or edge decorations.
-- Displays a multi-line, tabular reference map of the 'ReferenceDAG'.
referenceRendering :: ReferenceDAG d e n -> String
referenceRendering dag = unlines $ [shownRootRefs] <> toList shownDataLines
  where
    shownRootRefs   = listShow . toList $ rootRefs dag

    shownRefs       = f <$> references dag
      where
        f (IndexData _ pRefs cRefs) = (listShow $ otoList pRefs, listShow $ IM.keys cRefs)

    shownTrimmedParentRefs = fst <$> shownRefs

    shownTrimmedChildRefs  = snd <$> shownRefs

    shownPaddedParentRefs  = pad maxParentWidth <$> shownTrimmedParentRefs

    shownPaddedChildRefs   = pad maxChildWidth  <$> shownTrimmedChildRefs

    maxParentWidth = maximum $ length <$> shownTrimmedParentRefs

    maxChildWidth  = maximum $ length <$> shownTrimmedChildRefs

    maxIndexWidth  = length . show . pred . length $ references dag

    shownDataLines = zipWithKey f shownPaddedParentRefs shownPaddedChildRefs
      where
        f i p c = "  " <> unwords [ pad maxIndexWidth $ show i, p, c]

    listShow = (\x -> "{" <> x <> "}") . intercalate "," . fmap show

    pad n    []  = replicate n ' '
    pad 0    xs  = xs
    pad n (x:xs) = x : pad (n-1) xs


-- |
-- Displays a tree-like rendering of the 'ReferenceDAG'.
topologyRendering :: ReferenceDAG d e n -> String
topologyRendering dag = drawVerticalTree . unfoldTree f . NE.head $ rootRefs dag
  where
    f i = (show i, IM.keys . childRefs $ references dag ! i)


-- |
-- /O(n)/
--
-- Constructs a 'ReferenceDAG' from a "list" of references.
--
-- Generally regarded as *UNSAFE*! Prefer 'unfoldDAG'.
--
-- * Does not check for connectivity of the DAG.
--
-- * Does not check for the lack of cycles.
--
-- * Does not normalize nodes for proper in-degree values.
--
-- * Does not normalize nodes for proper out-degree values.
fromList :: Foldable f => f (IntSet, n, IntMap e) -> ReferenceDAG () e n
fromList xs =
    RefDAG
    { references = referenceVector
    , rootRefs   = rootSet
    , graphData  = GraphData 0 0 0 0 ()
    }
  where
    listValue = toList xs
    referenceVector = V.fromList $ (\(pSet, datum, cMap) -> IndexData datum pSet cMap) <$> listValue
    rootSet =
      case foldMapWithKey (\k (pSet,_,_) -> [ k | onull pSet ]) listValue of
        []   -> error "No root nodes supplied in call to ReferenceDAG.fromList"
        y:ys -> y:|ys


-- |
-- Probably, hopefully /O(n)/.
--
-- Build the graph functionally from a generating function.
--
-- The generating function produces three results:
--
-- 1. A list of parent edge decorations and corresponding ancestral values
--
-- 2. The node decoration for the input value
--
-- 3. A list of child edge decorations and corresponding descendent values
unfoldDAG :: (Eq a, Hashable a, Monoid e, Monoid n)
          => (a -> ([(e, a)], n, [(e, a)])) -- ^ Unfolding function
          -> a                              -- ^ Seed value
          -> ReferenceDAG () e n
unfoldDAG f origin =
    RefDAG
    { references = referenceVector
    , rootRefs   = NE.fromList roots2 -- otoList rootIndices
    , graphData  = GraphData 0 0 0 0 ()
    }
  where
    referenceVector = V.fromList . fmap h $ toList expandedMap
      where
        h (iSet, nDatum, iMap) =
            IndexData
            { nodeDecoration = nDatum
            , parentRefs     = iSet
            , childRefs      = iMap
            }

    expandedMap = contractToContiguousVertexMapping $ expandVertexMapping resultMap
--    expandedMap = resultMap

    -- TODO:
    -- _rootIndices seems to be wrong so we do this.
    -- slightly inefficient, see if we can correct the _rootIndices value.
    roots2 = foldMapWithKey h resultMap
      where
        h k (v,_,_)
          | onull v   = [k]
          | otherwise = []

    initialAccumulator = (-1, -1, (Nothing, mempty), mempty, mempty)
    (_, _, _, _rootIndices, resultMap) = g initialAccumulator origin
    g (counter, _otherIndex, previousContext@(previousIndex, previousSeenSet), currentRoots, currentMap) currentValue =
        case currentValue `lookup` previousSeenSet of
          -- If this value is in the previously seen set we don't recurse,
          -- we just return the supplied accumulator with a mutated otherIndex value.
          Just i  -> (counter, i, previousContext, currentRoots, currentMap)
          Nothing -> result
      where
        result = ( cCounter
                 , currentIndex
                 , resultContext
                 , currentRoots <> pRoots <> cRoots <> localRoots
                 , cMap <> mapWithLocalChildren <> mapWithLocalParents <> mapWithLocalValues
                 )

        (fullParentPairs, newDatum, fullChildPairs) = f currentValue
        (omittedParentPairs, parentPairs) = omitOriginPath fullParentPairs
        (omittedChildPairs , childPairs ) = omitOriginPath fullChildPairs

        currentIndex   = counter + 1
        currentContext = (Just currentIndex, HM.insert currentValue currentIndex previousSeenSet)

        resultContext  = (previousIndex, snd cContext)

        omitOriginPath = foldr h ([],[])
          where
            h y@(e,v) (xs,ys) =
              case v `lookup` previousSeenSet of
                Nothing -> (        xs, y:ys)
                Just i  -> ((e,v,i):xs,   ys)

        parentResursiveResult          = NE.scanr (\e a -> second (g (snd a)) e) (undefined, (currentIndex, undefined, currentContext, currentRoots, currentMap)) parentPairs
        (pCounter, _, pContext, pRoots, pMap) = snd $ NE.head parentResursiveResult
        childResursiveResult           = NE.scanr (\e a -> second (g (snd a)) e) (undefined, (pCounter, undefined, pContext, pRoots, pMap)) childPairs
        (cCounter, _, cContext, cRoots, cMap) = snd $ NE.head childResursiveResult

        mapWithLocalParents = foldMap h $ NE.init parentResursiveResult
          where
            h (_,(_,c,_,_,_)) = IM.insertWith insWith cCounter (IS.singleton c, newDatum, mempty) cMap
              where
                insWith (niSet, _, niMap) (oiSet, dec, oiMap) = (niSet <> oiSet, dec, oiMap <> niMap)

        mapWithLocalChildren = foldMap h $ NE.init childResursiveResult
          where
            h (e,(_,c,_,_,_)) = IM.insertWith insWith cCounter (mempty, newDatum, IM.singleton c e) cMap
              where
                insWith (niSet, _, niMap) (oiSet, dec, oiMap) = (niSet <> oiSet, dec, oiMap <> niMap)

        mapWithLocalValues  = IM.singleton currentIndex
                            ( otherParents  <> resultParents
                            , newDatum
                            , otherChildren <> resultChildren
                            )
          where
            otherParents   = foldMap (\(_,_,i)         -> IS.singleton i  ) omittedParentPairs
            otherChildren  = foldMap (\(e,_,i)         -> IM.singleton i e) omittedChildPairs
            resultParents  = foldMap (\(_,(_,c,_,_,_)) -> IS.singleton c  ) $ NE.init parentResursiveResult
            resultChildren = foldMap (\(e,(_,c,_,_,_)) -> IM.singleton c e) $ NE.init childResursiveResult

        localRoots
          | null fullParentPairs = IS.singleton cCounter
          | otherwise            = mempty




-- A test for unfoldDAG containing all node types!
{--

dataDef1 :: [(Char,String)]
dataDef1 = [('R',"AB"),('A',"CD"),('B',"CE"),('C',[]),('D',[]),('E',[]),('Z',"BF"),('F',"GH"),('G',[]),('H',[])]

gen1 :: Char -> ([(Int,Char)], String, [(Int,Char)])
gen1 x = (pops, show x, kids)
  where
    pops = foldMap (\(i,xs) -> if x `elem` xs then [(-1,i)] else []) dataDef1
    kids =
      case Pre.lookup x dataDef1 of
        Nothing -> []
        Just xs -> (\y -> (-1,y)) <$> xs
--}


-- |
-- Extract a context from the 'ReferenceDAG' that can be used to create a dot
-- context for rendering.
getDotContext
  :: Foldable f
  => Int -- ^ Base over which the Unique
  -> Int
  -> ReferenceDAG d e (f String)
  -> ([DotNode GraphID], [DotEdge GraphID])
--getDotContext dag | trace ("About to render this to DOT:\n\n" <> show dag) False = undefined
getDotContext uniqueIdentifierBase mostSignificantDigit dag = second mconcat . unzip $ foldMapWithKey f vec
  where
    idOffest = uniqueIdentifierBase * mostSignificantDigit

    vec = references dag

    toId :: Int -> GraphID
    toId = Num . Int . (+ idOffest)

    toAttributes :: Foldable f => f String -> Attributes
    toAttributes x =
      case toList x of
        []  -> []
        s:_ -> [ toLabel s ]



    f :: Foldable f => Int -> IndexData e (f String) -> [(DotNode GraphID, [DotEdge GraphID])]
    f k v = [ (toDotNode, toDotEdge <$> kidRefs) ]
      where
        datum       = nodeDecoration v
        nodeId      = toId k
        nodeAttrs   = toAttributes datum
        kidRefs     = IM.keys $ childRefs v
        toDotNode   = DotNode nodeId nodeAttrs
        toDotEdge x = DotEdge (toId x) nodeId nodeAttrs


-- |
-- Generate the set of candidate network edges for a given DAG.
candidateNetworkEdges :: ReferenceDAG d e n -> Set ( (Int, Int), (Int,Int) )
candidateNetworkEdges dag = S.filter correctnessCriterion $ foldMapWithKey f mergedVector
  where
    mergedVector  = zipWith mergeThem ancestoralEdgeSets descendantEdgeSets

    mergeThem a d =
        IndexData
        { nodeDecoration = nodeDecoration a
        , parentRefs     = parentRefs a
        , childRefs      = zipWith (<>) (childRefs a) (childRefs d)
        }

    correctnessCriterion x = doesNotShareNode x && notNetworkEdges x

    doesNotShareNode ((a,b),(c,d)) = a /= c && a /= d && b /= c && b /= d

    notNetworkEdges  ((_,b),(_,d)) = isNotNetworkNode b && isNotNetworkNode d
      where
        isNotNetworkNode i = (<=1) . olength . parentRefs $ refs ! i
        refs = references ! dag

    rootEdges           = tabulateRootIncidentEdgeset dag
    ancestoralEdgeSets  = references $ tabulateAncestoralEdgesets dag
    descendantEdgeSets  = references $ tabulateDescendantEdgesets dag
    completeEdgeSet     = getEdges dag `difference` rootEdges
    f k     = foldMapWithKey (g k) . mapWithKey (h k) . childRefs
    g j k   = foldMap (\x -> S.singleton ((j,k), x))
    h j k v = possibleEdgeSet j k `difference` v
    possibleEdgeSet i j = completeEdgeSet `difference` (singletonEdgeSet (i,j) <> singletonEdgeSet (j,i))
{-
    renderVector  = unlines . mapWithKey (\k v -> show k <> " " <> show (childRefs v)) . toList

    renderContext = unlines [refsStr, anstSet, descSet, edgeset]
      where
        refsStr = referenceRendering (dag { references = mergedVector })
        edgeset = renderVector mergedVector
        anstSet = renderVector ancestoralEdgeSets
        descSet = renderVector descendantEdgeSets
-}


-- |
-- Find all edges adjacent to root nodes.
tabulateRootIncidentEdgeset :: ReferenceDAG d e n -> EdgeSet (Int,Int)
tabulateRootIncidentEdgeset dag = foldMap f $ rootRefs dag
  where
    f i = foldMap (\e -> singletonEdgeSet (i,e)) kids
      where
        kids = IM.keys . childRefs $ references dag ! i


-- |
-- Gather all paths from a root node to each node in the graph.
tabulateAncestoralEdgesets :: ReferenceDAG d e n -> ReferenceDAG () (EdgeSet (Int,Int)) ()
tabulateAncestoralEdgesets dag =
    RefDAG
    { references = memo
    , rootRefs   = rootRefs dag
    , graphData  = defaultGraphMetadata $ graphData dag
    }
  where
    refs = references dag
    memo = V.generate (length refs) g
    g i =
        IndexData
        { nodeDecoration = ()
        , parentRefs     = parentVals
        , childRefs      = zipWith (<>) childShape (getNetworkEdgeDatum i)
        }
      where
        childShape = ancestorDatum <$ childRefs point
        point      = refs ! i
        parentVals = parentRefs point
        ancestorDatum =
            case otoList parentVals of
              []    -> mempty
              [x]   -> getPreviousDatums x i
              x:y:_ -> getPreviousDatums x i `union` getPreviousDatums y i

    getPreviousDatums i j = childRefs point ! j <> other
      where
        point = memo ! i
        -- This is the step where new information is added to the accumulator
        other = singletonEdgeSet (i,j)

    -- We can't let a network edge form a new network edge with it's incident
    -- network edge. Down that road lies infinite recursion.
    getNetworkEdgeDatum i = mapWithKey f . childRefs $ refs ! i
      where
        f k _ =
          case otoList . parentRefs $ refs ! k of
            []  -> mempty
            [_] -> mempty
            xs  ->
              case filter (/=i) xs of
                []  -> mempty
                x:_ -> singletonEdgeSet (x,k)
--                x:_ -> foldMap (`getPreviousDatums` x) . otoList . parentRefs $ memo ! x


-- |
-- Gather all paths from a leaf node to each node in the graph.
tabulateDescendantEdgesets :: ReferenceDAG d e n -> ReferenceDAG () (EdgeSet (Int,Int)) ()
tabulateDescendantEdgesets dag =
    RefDAG
    { references = memo
    , rootRefs   = rootRefs dag
    , graphData  = defaultGraphMetadata $ graphData dag
    }
  where
    refs = references dag
    memo = V.generate (length refs) g
    g i =
        IndexData
        { nodeDecoration = ()
        , parentRefs     = parentRefs point
        , childRefs      = descendantDatum <$ childVals
        }
      where
        point     = refs ! i
        childVals = childRefs point
        descendantDatum =
            case IM.keys childVals of
              []    -> mempty
              [x]   -> getPreviousDatums i x
              x:y:_ -> getPreviousDatums i x `union` getPreviousDatums i y

    getPreviousDatums _ j = foldMap id (childRefs point) <> other
      where
        point = memo ! j
        -- This is the step where new information is added to the accumulator
        other = foldMap (\x -> singletonEdgeSet (j,x)) . IM.keys $ childRefs point
