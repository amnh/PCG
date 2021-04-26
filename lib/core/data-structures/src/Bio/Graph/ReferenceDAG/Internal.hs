-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Graph.DAG.Internal
-- Copyright   :  (c) 2015-2021 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UnboxedSums                #-}

module Bio.Graph.ReferenceDAG.Internal where

import           Bio.Graph.BinaryRenderingTree
import           Bio.Graph.Component
import           Bio.Graph.LeafSet
import           Bio.Graph.Node.Context
import           Control.Arrow                 ((***))
import           Control.DeepSeq
import           Control.Lens                  as Lens (lens, to)
import           Control.Lens.Fold             (Fold, folding)
import           Control.Lens.Operators        ((%~), (.~), (^.))
import           Control.Lens.Type             (Lens, Lens')
import           Control.Monad.State.Lazy
import           Data.Bifunctor
import           Data.Binary                   (Binary)
import qualified Data.DList                    as DL
import           Data.EdgeSet
import           Data.Foldable
import           Data.Foldable.Custom
import           Data.GraphViz.Attributes
import           Data.GraphViz.Printing
import           Data.GraphViz.Types           hiding (attrs)
import           Data.GraphViz.Types.Graph     hiding (node, (&))
import qualified Data.HashMap.Strict           as HM
import           Data.Hashable                 (Hashable)
import           Data.IntMap                   (IntMap)
import qualified Data.IntMap                   as IM
import           Data.IntSet                   (IntSet)
import qualified Data.IntSet                   as IS
import           Data.Key
import           Data.List                     (intercalate)
import           Data.List.NonEmpty            (NonEmpty(..), intersperse)
import qualified Data.List.NonEmpty            as NE
import           Data.List.Utility             (isSingleton)
import           Data.MonoTraversable
import           Data.Monoid                   hiding ((<>))
import           Data.Semigroup                hiding (First(..))
import           Data.Semigroup.Foldable
import qualified Data.Set                      as S
import           Data.String
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import qualified Data.TextShow.Custom          as TextShow (intercalateB)
import           Data.Traversable
import           Data.Tree                     (unfoldTree)
import           Data.Tree.Pretty              (drawVerticalTree)
import           Data.Vector                   (Vector)
import qualified Data.Vector                   as V
import           Data.Vector.Binary            ()
import qualified Data.Vector.Custom            as V (fromList')
import           Data.Vector.Instances         ()
import           GHC.Generics
import           Numeric.Extended.Real
import           Prelude                       hiding (lookup, zipWith)
import           Text.Newick.Class
import           Text.XML.Custom
import           TextShow                      (TextShow(..), toString, unlinesB)


-- |
-- A constant time access representation of a directed acyclic graph.
data  ReferenceDAG d e n
    = ReferenceDAG
    { references :: {-# UNPACK #-} !(Vector (IndexData e n))
    , rootRefs   :: !(NonEmpty Int)
    , graphData  :: !(GraphData d)
    }
    deriving stock    (Generic)
    deriving anyclass (NFData)


-- |
-- A labeled record for each "node" in the graph containing the node decoration,
-- a set of parent references, and a set of child references with edge decorations.
--
-- Type annotations:
-- * e = edge decorations, as yet unused
-- * n = node label: 'Maybe'('String')
data  IndexData e n
    = IndexData
    { nodeDecoration :: !n
    , parentRefs     :: !IntSet
    , childRefs      :: !(IntMap e)
    }
    deriving stock    (Generic, Show)
    deriving anyclass (NFData)


-- |
-- Annotations which are global to the graph
--
-- -- Type annotations:
-- * d = graph metadata
data  GraphData d
    = GraphData
    { dagCost         :: {-# UNPACK #-} !ExtendedReal
    , networkEdgeCost :: {-# UNPACK #-} !ExtendedReal
    , rootingCost     :: {-# UNPACK #-} !Double
    , totalBlockCost  :: {-# UNPACK #-} !Double
    , graphMetadata   :: d
    }
    deriving stock    (Functor, Generic)
    deriving anyclass (NFData)


-- |
-- This will be used below to print the node type to XML and Newick.
data NodeClassification
    = NodeClassification
    | LeafNode
    | NetworkNode
    | RootNode
    | TreeNode
    deriving stock    (Eq, Generic, Show)
    deriving anyclass (NFData)


-- |
-- A reference to a node within the 'ReferenceDAG'.
newtype NodeRef = NR Int
  deriving stock   (Eq)
  deriving newtype (Enum)


type instance Key (ReferenceDAG d e) = Int


-- |
-- A 'Lens' for the 'nodeDecoration' field in 'IndexData'
{-# SPECIALISE _nodeDecoration :: Lens (IndexData e n) (IndexData e n') n n' #-}
class HasNodeDecoration s t a b | s -> a, t -> b, s b -> t, t a -> s where

    _nodeDecoration :: Lens s t a b


-- |
-- A 'Lens' for the 'parentRefs' field in 'IndexData'
{-# SPECIALIZE _parentRefs :: Lens' (IndexData e n) IntSet #-}
class HasParentRefs s a | s -> a where

    _parentRefs :: Lens' s a


-- |
-- A 'Lens' for the 'childRefs' field in 'IndexData'
{-# SPECIALISE _childRefs :: Lens' (IndexData e n) IntSet #-}
class HasChildRefs s t a b | s -> a, t -> b, s b -> t, t a -> s where

    _childRefs :: Lens s t a b


-- |
-- A 'Lens' for the 'graphData' field.
{-# SPECIALISE _graphData :: Lens (ReferenceDAG d e n) (ReferenceDAG d' e n) (GraphData d) (GraphData d') #-}
class HasGraphData s t a b | s -> a, t -> b,  s b -> t, t a -> s where

    _graphData :: Lens s t a b


-- |
-- A 'Lens' for the 'references' field
{-# SPECIALISE  _references :: Lens (ReferenceDAG d e n) (ReferenceDAG d e' n') (Vector (IndexData e n)) (Vector (IndexData e' n')) #-}
class HasReferenceVector s t a b | s -> a, b s -> t where

    _references :: Lens s t a b


-- |
-- A 'Lens' for the 'rootRefs' field
{-# SPECIALISE _rootRefs :: Lens' (ReferenceDAG d e n) (NonEmpty Int) #-}
class HasRootReferences s a | s -> a where

    _rootRefs :: Lens' s a


-- |
-- A 'Fold' for folding over a structure containing node decorations.
{-# SPECIALISE foldNodeDecoration :: Fold (ReferenceDAG d e n) n #-}
class FoldNodeDecoration s a | s -> a where

    foldNodeDecoration :: Fold s a


-- |
-- A 'Lens' for the 'dagCost' field
{-# SPECIALISE _dagCost :: (GraphData d) ExtendedReal #-}
class HasDagCost s a | s -> a where

    _dagCost :: Lens' s a


-- |
-- A 'Lens' for the 'networkEdgeCost' field.
{-# SPECIALISE _networkEdgeCost :: Lens' (GraphData d) ExtendedReal #-}
class HasNetworkEdgeCost s a | s -> a where

    _networkEdgeCost :: Lens' s a


-- |
-- a 'Lens' for the 'rootingCost' field.
{-# SPECIALISE _rootingCost :: Lens' (GraphData d) Double #-}
class HasRootingCost s a | s -> a where

    _rootingCost :: Lens' s a


-- |
-- A 'Lens' for 'totalBlockCost' field.
{-# SPECIALISE _totalBlockCost :: Lens' (GraphData d) Double #-}
class HasTotalBlockCost s a | s -> a where

    _totalBlockCost :: Lens' s a


-- |
-- a 'Lens' for the 'graphMetadata' field.
{-# SPECIALISE _graphMetadata :: Lens' (GraphData d) d #-}
class HasGraphMetadata s t a b | s -> a, t -> b, s b -> t, t a -> s where

    _graphMetadata :: Lens s t a b


instance HasNodeDecoration (IndexData e n) (IndexData e n') n n' where
    {-# INLINE _nodeDecoration #-}

    _nodeDecoration = lens nodeDecoration (\i n' -> i {nodeDecoration = n'})


instance HasParentRefs (IndexData e n) IntSet where
    {-# INLINE _parentRefs #-}

    _parentRefs = lens parentRefs (\i p -> i {parentRefs = p})


instance HasChildRefs (IndexData e n) (IndexData e' n) (IntMap e) (IntMap e') where
    {-# INLINE _childRefs #-}

    _childRefs = lens childRefs (\i c -> i {childRefs = c})


instance HasRootReferences (ReferenceDAG d e n) (NonEmpty Int) where
    {-# INLINE _rootRefs #-}

    _rootRefs = lens rootRefs (\r v -> r {rootRefs = v})


instance FoldNodeDecoration (ReferenceDAG d e n) n where
    {-# INLINE foldNodeDecoration #-}

    foldNodeDecoration = _references . folding id . _nodeDecoration


instance HasDagCost (GraphData d) ExtendedReal where
    {-# INLINE _dagCost #-}

    _dagCost = lens dagCost (\g d -> g {dagCost = d})


instance HasNetworkEdgeCost (GraphData d) ExtendedReal where
    {-# INLINE _networkEdgeCost #-}

    _networkEdgeCost = lens networkEdgeCost (\g n -> g {networkEdgeCost = n})


instance HasRootingCost (GraphData d) Double where
    {-# INLINE _rootingCost #-}

    _rootingCost = lens rootingCost (\g r -> g {rootingCost = r})


instance HasTotalBlockCost (GraphData d) Double where
    {-# INLINE _totalBlockCost #-}

    _totalBlockCost = lens totalBlockCost (\g t -> g {totalBlockCost = t})


instance HasGraphMetadata (GraphData d) (GraphData d') d d' where
    {-# INLINE _graphMetadata #-}

    _graphMetadata = lens graphMetadata (\g m -> g {graphMetadata = m})



instance HasGraphData (ReferenceDAG d e n) (ReferenceDAG d' e n) (GraphData d) (GraphData d') where
    {-# INLINE _graphData #-}

    _graphData = lens graphData (\r g -> r {graphData = g})


instance HasReferenceVector (ReferenceDAG d e n) (ReferenceDAG d e' n') (Vector (IndexData e n)) (Vector (IndexData e' n')) where
    {-# INLINE _references #-}

    _references = lens references (\r v -> r {references = v})


instance Bifunctor (ReferenceDAG d) where

    bimap f g dag =
        ReferenceDAG
        { references = h <$> references dag
        , rootRefs   = rootRefs  dag
        , graphData  = graphData dag
        }
      where
        h (IndexData node parentRefs' childRefs') = IndexData (g node) parentRefs' $ f <$> childRefs'


instance Binary d => Binary (GraphData d)


instance (Binary e, Binary n) => Binary (IndexData e n)


instance (Binary d, Binary e, Binary n) => Binary (ReferenceDAG d e n)


instance Foldable (ReferenceDAG d e) where

    foldMap f = foldMap (f . nodeDecoration) . references


instance FoldableWithKey (ReferenceDAG d e) where

    {-# INLINE foldrWithKey #-}
    foldrWithKey f e = V.ifoldr' (\i n a -> f i (nodeDecoration n) a) e . references

    {-# INLINE foldlWithKey #-}
    foldlWithKey f e = V.ifoldl' (\a i -> f a i . nodeDecoration) e . references


instance Functor (ReferenceDAG d e) where

    fmap f dag =
        ReferenceDAG
        { references = g <$> references dag
        , rootRefs   = rootRefs  dag
        , graphData  = graphData dag
        }
      where
        g (IndexData node parentRefs' childRefs') = IndexData (f node) parentRefs' childRefs'


instance HasLeafSet (ReferenceDAG d e n) (LeafSet n) where

    leafSet = Lens.to getter
        where
            getter :: ReferenceDAG d e n -> LeafSet n
            getter (ReferenceDAG v _ _) = LeafSet $ foldMap f v

            f e | null (childRefs e) = pure $ nodeDecoration e
                | otherwise          = mempty


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
        (ps,cs) = parentsAndChildren i dag

    isTreeNode i dag = olength ps == 1 && length cs == 2
      where
        (ps,cs) = parentsAndChildren i dag

    isLeafNode i dag =  null . childRefs  $ references dag ! fromEnum i

    isRootNode i dag = onull . parentRefs $ references dag ! fromEnum i

    networkResolutions = pure


instance PhylogeneticNetwork (ReferenceDAG d e n) NodeRef e n where

    root = toEnum . NE.head . rootRefs

    treeResolutions = pure


instance PhylogeneticTree (ReferenceDAG d e n) NodeRef e n where

    parent i dag = fmap toEnum . headMay . otoList . parentRefs $ references dag ! fromEnum i


instance TextShow n => PrintDot (ReferenceDAG d e n) where

    unqtDot       = unqtDot . uncurry mkGraph . getDotContext 0 0

    toDot         = toDot   . uncurry mkGraph . getDotContext 0 0

    unqtListToDot = unqtDot . uncurry mkGraph . bimap fold fold . unzip . fmap (getDotContext 0 0)

    listToDot     = toDot   . uncurry mkGraph . bimap fold fold . unzip . fmap (getDotContext 0 0)


instance Show (GraphData m) where

    show x = unlines
        [ "DAG total cost:           " <> show (dagCost x)
        , "DAG network edge cost:    " <> show (networkEdgeCost x)
        , "DAG multi-rooting cost:   " <> show (rootingCost     x)
        , "DAG character block cost: " <> show (totalBlockCost  x)
        ]


instance (TextShow d, TextShow n) => Show (ReferenceDAG d e n) where

    show = toString . showb


instance TextShow d => TextShow (GraphData d) where

    showb x = unlinesB
        [ "DAG total cost:           " <> showb (dagCost x)
        , "DAG network edge cost:    " <> showb (networkEdgeCost x)
        , "DAG multi-rooting cost:   " <> showb (rootingCost     x)
        , "DAG character block cost: " <> showb (totalBlockCost  x)
        ]


instance (TextShow d, TextShow n) => TextShow (ReferenceDAG d e n) where

    showb dag = TextShow.intercalateB "\n"
        [ fromString . topologyRendering $ dag
        , ""
        , fromString . fold1 . intersperse "\n" $ horizontalRendering <$> toBinaryRenderingTree (toString . showb) dag
        , ""
        , fromString . referenceRendering $ dag
        , showb $ graphData dag
        ]

instance Semigroup d => Semigroup (GraphData d) where
  (<>)
    (GraphData dagCost1 networkEdgeCost1 rootingCost1 totalBlockCost1 graphMetadata1)
    (GraphData dagCost2 networkEdgeCost2 rootingCost2 totalBlockCost2 graphMetadata2)
      = GraphData
          (dagCost1         `addPositive` dagCost2)
          (networkEdgeCost1 `addPositive` networkEdgeCost2)
          (rootingCost1     + rootingCost2    )
          (totalBlockCost1  + totalBlockCost2 )
          (graphMetadata1  <> graphMetadata2  )

instance Monoid d => Monoid (GraphData d) where
  mempty = GraphData
             { dagCost         = 0
             , networkEdgeCost = 0
             , rootingCost     = 0
             , totalBlockCost  = 0
             , graphMetadata   = mempty
             }

instance TextShow n => ToNewick (ReferenceDAG d e n) where

    toNewick refDag = T.concat [ newickString, "[", showt cost, "]" ]
      where
        (_,newickString) = generateNewick namedVec rootRef mempty
        cost     = dagCost $ graphData refDag
        rootRef  = NE.head $ rootRefs refDag
        vec      = references refDag

        namedVec = zipWith (\x n -> n { nodeDecoration = x }) labelVec vec
        labelVec = (`evalState` (1,1,1)) $ mapM deriveLabel vec -- All network nodes have "htu\d" as nodeDecoration.

        deriveLabel :: IndexData e n -> State (Int, Int, Int) Text
        deriveLabel node
          | shownLabel /= "{Unlabeled Node}" = pure shownLabel
          | otherwise = do
              (lC, nC, tC) <- get
              case getNodeType node of
                LeafNode    -> do
                    put (lC+1, nC, tC)
                    pure $ "Leaf_" <> showt lC
                NetworkNode -> do
                    put (lC, nC+1, tC)
                    pure $ "HTU_"  <> showt nC
                _           -> do
                    put (lC, nC, tC+1)
                    pure $ "Node_" <> showt tC
          where
            shownLabel = showt $ nodeDecoration node


instance ToXML (GraphData m) where

    toXML gData = xmlElement "Graph_data" attrs contents
        where
            attrs = []
            contents = [ Left ( "DAG_total_cost"          , show $ dagCost         gData)
                       , Left ( "DAG_network_edge_cost"   , show $ networkEdgeCost gData)
                       , Left ( "DAG_rooting_cost"        , show $ rootingCost     gData)
                       , Left ( "DAG_character_block_cost", show $ totalBlockCost  gData)
                       ]


instance Show n => ToXML (IndexData e n) where

   toXML indexData = toXML . show $ nodeDecoration indexData


instance (TextShow n, ToXML n) => ToXML (ReferenceDAG d e n) where

    toXML dag = xmlElement "Directed_acyclic_graph" [] [newick, meta, vect]
      where
          meta   = Right . toXML $ graphData dag
          newick = Left ("Newick_representation", T.unpack $ toNewick dag)
          vect   = Right . collapseElemList "Nodes" [] $ dag


-- |
-- Produces a set of directed references representing all edges in the DAG.
-- Equivalent to:
--
-- > referenceTreeEdgeSet dag `union` referenceNetworkEdgeSet dag
referenceEdgeSet :: ReferenceDAG d e n -> EdgeSet (Int, Int)
referenceEdgeSet = foldMapWithKey f . references
  where
    f i = foldMap (\e -> singletonEdgeSet (i,e)) . IM.keys . childRefs


-- |
-- Produces a set of directed references representing all /Network/ edges in the DAG.
-- Omits /tree/ edges in the DAG. The resulting 'EdgeSet' *will not* be connected.
--
-- Equivalent to:
--
-- > referenceEdgeSet dag `difference` referenceTreeEdgeSet dag
--
-- The following will always hold:
--
-- > null (referenceTreeEdgeSet dag `intersection` referenceTreeEdgeSet dag)
referenceNetworkEdgeSet :: ReferenceDAG d e n -> EdgeSet (Int, Int)
referenceNetworkEdgeSet dag = foldMapWithKey f refs
  where
    refs = references dag
    f i = foldMap (\e -> singletonEdgeSet (i,e)) . filter childHasMoreThanOneParent . IM.keys . childRefs
    childHasMoreThanOneParent = not . isSingleton . otoList . parentRefs . (refs !)


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
      ReferenceDAG
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
      ReferenceDAG
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
-- /O(n*i)/ where /i/ is the number of missing indices.
-- Assuming all indices in the input /x/ are positive, /i/ = 'findMax x - length x'.
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
defaultGraphMetadata :: forall m d . Monoid m => GraphData d -> GraphData m
{-# INLINE defaultGraphMetadata #-}
defaultGraphMetadata = _graphMetadata .~ mempty


-- |
-- Overwrite the current graph metadata with a default value.
--
-- Default in the function's name is used as a verb, not a noun.
defaultMetadata :: Monoid m => ReferenceDAG d e n -> ReferenceDAG m e n
{-# INLINE defaultMetadata #-}
defaultMetadata = _graphData %~ defaultGraphMetadata

-- |
-- Zero cost graph data with 'graphMetadata' set to 'mempty'.
zeroCostGraphData :: Monoid m => GraphData m
{-# INLINE zeroCostGraphData #-}
zeroCostGraphData
  = mempty


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
                  -- One too many children
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
generateNewick :: Vector (IndexData e Text) -> Int -> S.Set Text -> (S.Set Text, Text)
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
               then (         htuNumSet, fold [ "#", htuNumberStr ])
               -- Network node but not yet in set of traversed nodes, so update htuNumberSet.
               else ( updatedHtuNumSet', fold [ subtreeNewickStr, "#", htuNumberStr ])

              -- Both root and tree node. Originally root was a separate case that resolved to an error,
              -- but the first call to this fn is always root, so can't error out on that.
              -- In no case does the htuNumberSet update.
          _           ->
             case IM.keys $ childRefs node of
                 []              -> error "Graph construction should prevent a 'root' node or 'tree' node with no children."
                 lhsIdx:rhsIdx:_ -> ( updatedHtuNumSet'
                                    , fold [ "(", lhsReturnString, ", ", rhsReturnString, ")" ]
                                    )
                   where
                     (updatedHtuNumSet , lhsReturnString) = generateNewick refs lhsIdx htuNumSet
                     (updatedHtuNumSet', rhsReturnString) = generateNewick refs rhsIdx updatedHtuNumSet
                    -- Next should happen only under network node, but here for completion.
                 [singleChild]   -> ( updatedHtuNumSet'
                                    , fold [ "(", updatedNewickStr, ")" ]
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
-- Displays a tree-like rendering of the 'ReferenceDAG' as a 'String'
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
    ReferenceDAG
    { references = referenceVector
    , rootRefs   = rootSet
    , graphData  = GraphData 0 0 0 0 ()
    }
  where
    listValue   = toList xs
    indexedList = Prelude.zip [0..] listValue
    referenceVector =
      V.fromList . fmap (\ (pSet, datum, cMap) -> IndexData datum pSet cMap) $ listValue
    rootSet =
      let isRootDL (k, (pSet,_,_)) = if onull pSet then DL.singleton k else mempty in
      case DL.toList $ foldMap isRootDL indexedList of
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
    ReferenceDAG
    { references = referenceVector
    , rootRefs   = NE.fromList roots2 -- otoList rootIndices
    , graphData  = GraphData 0 0 0 0 ()
    }
  where
    referenceVector = V.fromList' . fmap h $ toList expandedMap
      where
        h (iSet, nDatum, iMap) =
            IndexData
            { nodeDecoration = nDatum
            , parentRefs     = iSet
            , childRefs      = iMap
            }

    expandedMap = contractToContiguousVertexMapping $ expandVertexMapping resultMap

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


-- |
-- Extract a context from the 'ReferenceDAG' that can be used to create a dot
-- context for rendering.
getDotContext
  :: TextShow n
  => Int -- ^ Base over which the Unique
  -> Int
  -> ReferenceDAG d e n
  -> ([DotNode GraphID], [DotEdge GraphID])
--getDotContext dag | trace ("About to render this to DOT:\n\n" <> show dag) False = undefined
getDotContext uniqueIdentifierBase mostSignificantDigit dag =
    second fold . unzip $ foldMapWithKey f vec
  where
    idOffest = uniqueIdentifierBase * mostSignificantDigit

    vec = references dag

    toId :: Int -> GraphID
    toId = Num . Int . (+ idOffest)

    toAttributes :: TextShow a => a -> Attributes
    toAttributes x =
      let txt = showt x
      in  [ toLabel $ T.unpack txt | not (T.null txt) ]

    f :: TextShow n => Int -> IndexData e n -> [(DotNode GraphID, [DotEdge GraphID])]
    f k v = [ (toDotNode, toDotEdge <$> kidRefs) ]
      where
        datum       = nodeDecoration v
        nodeId      = toId k
        nodeAttrs   = toAttributes datum
        kidRefs     = IM.keys $ childRefs v
        toDotNode   = DotNode nodeId nodeAttrs
        toDotEdge x = DotEdge nodeId (toId x) nodeAttrs


-- |
-- Construct the intermediate 'BinaryRenderingTree' data type for a given 'ReferenceDAG'.
--
-- The first parameter is a rendering function for the leaves.
toBinaryRenderingTree :: (n -> String) -> ReferenceDAG d e n -> NonEmpty BinaryRenderingTree
toBinaryRenderingTree nodeRenderer dag = (`evalState` initialState) . traverse subtreeToRendering $ rootRefs dag
  where
    refVec = references dag

    -- |
    -- Holds a counter for the next symbolic reference in the first position of
    -- the tuple.
    --
    -- Holds a map from indices in the reference vector to symbolic references.
    --
    -- Symbolic references are used only on in-degree 2 edges.
    initialState :: (Int, IntMap Int)
    initialState = (0, mempty)

    subtreeToRendering :: Int -> State (Int, IntMap Int) BinaryRenderingTree
    subtreeToRendering i =
        if   parentCount < 2
        then do
             subtrees <- mapM subtreeToRendering kids
             pure $ case subtrees of
                      []   -> Leaf shownNode
                      x:xs -> Node (sum' $ subtreeSize <$> x:xs) Nothing $ x:|xs
        else do
             (ctr, symRefs) <- get
             case i `lookup` symRefs of
               Just sym -> pure . Leaf $ "@" <> show sym
               Nothing  -> do
                 put (succ ctr, IM.insert i ctr symRefs)
                 subtrees <- mapM subtreeToRendering kids
                 pure $ case subtrees of
                          []   -> Leaf shownNode
                          x:xs -> Node (sum' $ subtreeSize <$> x:xs) (Just (show ctr)) $ x:|xs
      where
        context     = refVec ! i
        kids        = IM.keys $ childRefs context
        parentCount = olength $ parentRefs context
        shownNode = takeWhile (/='\n') . nodeRenderer $ nodeDecoration context


-- |
-- get the parents and children references for a given node index.
parentsAndChildren :: Enum a => a -> ReferenceDAG d e n -> (IntSet, IntMap e)
parentsAndChildren i dag = (ps, cs)
  where
    iPoint = references dag ! fromEnum i
    ps = parentRefs iPoint
    cs = childRefs  iPoint


-- |
-- Get the indices of all leaf nodes in the ReferenceDAG.
leafIndices :: ReferenceDAG d e n -> IntSet
leafIndices dag = foldMapWithKey leafTest $ dag ^. _references
  where
    leafTest :: Int -> IndexData e n -> IntSet
    leafTest index nodeDatum =
      if null (childRefs nodeDatum)
        then IS.singleton index
        else  mempty


-- |
-- Get the child context of a node at the given index.
{-# INLINE getChildContext #-}
getChildContext :: forall e n . Vector (IndexData e n) -> Int -> ChildContext Int
getChildContext refs ind = otoChildContext . IM.keysSet $ (refs ! ind) ^. _childRefs


-- |
-- Get the parent context of a node at the given index.
{-# INLINE getParentContext #-}
getParentContext :: forall e n . Vector (IndexData e n) -> Int -> ParentContext Int
getParentContext refs ind = otoParentContext $ (refs ! ind) ^. _parentRefs


-- |
-- Create a DAG with one root node connected to one leaf node.
trivialRefDAG :: IndexData e n ->  IndexData e n -> ReferenceDAG () e n
{-# INLINE trivialRefDAG #-}
trivialRefDAG root node =
    ReferenceDAG
    { references = V.fromList [root, node]
    , rootRefs   = 0 :| []
    , graphData  = GraphData 0 0 0 0 ()
    }
