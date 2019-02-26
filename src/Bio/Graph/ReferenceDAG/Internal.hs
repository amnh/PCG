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

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
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

import Debug.Trace

import           Analysis.Parsimony.Internal
import           Bio.Graph.BinaryRenderingTree
import           Bio.Graph.Component
import           Bio.Graph.LeafSet
import           Control.Applicative           as Alt (Alternative (empty, (<|>)))
import           Control.Arrow                 ((&&&), (***))
import           Control.DeepSeq
import           Control.Lens                  as Lens (Lens, Lens', lens, to)
import           Control.Lens.Fold             (Fold, folding)
import           Control.Lens.Operators        ((%~), (.~), (^.))
import           Control.Monad.State.Lazy
import           Data.Bifunctor
import           Data.Binary                   (Binary)
import           Data.EdgeSet
import           Data.Foldable
import           Data.Foldable.Custom
import           Data.GraphViz.Attributes
import           Data.GraphViz.Printing
import           Data.GraphViz.Types           hiding (attrs)
import           Data.GraphViz.Types.Graph     hiding (node)
import           Data.Hashable                 (Hashable)
import qualified Data.HashMap.Strict           as HM
import           Data.IntMap                   (IntMap)
import qualified Data.IntMap                   as IM
import           Data.IntSet                   (IntSet)
import qualified Data.IntSet                   as IS
import           Data.Key
import           Data.List                     (intercalate, tails)
import           Data.List.NonEmpty            (NonEmpty (..), intersperse)
import qualified Data.List.NonEmpty            as NE
import           Data.List.Utility             (isSingleton)
import           Data.Monoid                   hiding ((<>))
import           Data.MonoTraversable
import           Data.Semigroup                hiding (First(..))
import           Data.Semigroup.Foldable
import           Data.Set                      (Set)
import qualified Data.Set                      as S
import           Data.String
import qualified Data.TextShow.Custom          as TextShow (intercalateB)
import           Data.Traversable
import           Data.Tree                     (unfoldTree)
import           Data.Tree.Pretty              (drawVerticalTree)
import           Data.Tuple.Utility
import           Data.Vector                   (Vector)
import qualified Data.Vector                   as V
import           Data.Vector.Binary            ()
import qualified Data.Vector.Custom            as V (fromList')
import           Data.Vector.Instances         ()
import           Data.Vector.Utility           as DV
import           GHC.Generics
import           Numeric.Extended.Real
import           Prelude                       hiding (lookup, zipWith)
import           Text.Newick.Class
import           Text.XML.Custom
import           TextShow                      (TextShow (..), toString, unlinesB)


-- |
-- A constant time access representation of a directed acyclic graph.
data  ReferenceDAG d e n
    = ReferenceDAG
    { references :: {-# UNPACK #-} !(Vector (IndexData e n))
    , rootRefs   :: !(NonEmpty Int)
    , graphData  :: !(GraphData d)
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
    { nodeDecoration :: !n
    , parentRefs     :: !IntSet
    , childRefs      :: !(IntMap e)
    } deriving (Generic, Show)


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


-- |
-- A 'Lens' for the 'nodeDecoration' field in 'IndexData'
class HasNodeDecoration s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _nodeDecoration :: Lens s t a b

{-# SPECIALISE _nodeDecoration :: Lens (IndexData e n) (IndexData e n') n n' #-}

instance HasNodeDecoration (IndexData e n) (IndexData e n') n n' where
  {-# INLINE _nodeDecoration #-}
  _nodeDecoration = lens nodeDecoration (\i n' -> i {nodeDecoration = n'})

-- |
-- A 'Lens' for the 'parentRefs' field in 'IndexData'
class HasParentRefs s a | s -> a where
  _parentRefs :: Lens' s a

{-# SPECIALISE _parentRefs :: Lens' (IndexData e n) IntSet #-}

instance HasParentRefs (IndexData e n) IntSet where
  {-# INLINE _parentRefs #-}
  _parentRefs = lens parentRefs (\i p -> i {parentRefs = p})

-- |
-- A 'Lens' for the 'childRefs' field in 'IndexData'
class HasChildRefs s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _childRefs :: Lens s t a b

{-# SPECIALISE _childRefs :: Lens' (IndexData e n) IntSet #-}

instance HasChildRefs (IndexData e n) (IndexData e' n) (IntMap e) (IntMap e') where
  {-# INLINE _childRefs #-}
  _childRefs = lens childRefs (\i c -> i {childRefs = c})


-- |
-- A 'Lens' for the 'graphData' field.
class HasGraphData s t a b | s -> a, t -> b,  s b -> t, t a -> s where
  _graphData :: Lens s t a b

{-# SPECIALISE _graphData
                 :: Lens
                      (ReferenceDAG d e n)
                      (ReferenceDAG d' e n)
                      (GraphData d)
                      (GraphData d')
  #-}

instance HasGraphData
  (ReferenceDAG d e n) (ReferenceDAG d' e n) (GraphData d) (GraphData d')
  where
  {-# INLINE _graphData #-}
  _graphData = lens graphData (\r g -> r {graphData = g})



-- |
-- A 'Lens' for the 'references' field
class HasReferenceVector s t a b | s -> a, b s -> t where
  _references :: Lens s t a b

{-# SPECIALISE
  _references :: Lens
                   (ReferenceDAG d e n)
                   (ReferenceDAG d e' n')
                   (Vector (IndexData e n))
                   (Vector (IndexData e' n'))
  #-}

instance HasReferenceVector
  (ReferenceDAG d e n)
  (ReferenceDAG d e' n')
  (Vector (IndexData e n))
  (Vector (IndexData e' n'))
    where
  {-# INLINE _references #-}
  _references = lens references (\r v -> r {references = v})

-- |
-- A 'Lens' for the 'rootRefs' field
class HasRootReferences s a | s -> a where
  _rootRefs :: Lens' s a

{-# SPECIALISE _rootRefs :: Lens' (ReferenceDAG d e n) (NonEmpty Int) #-}

instance HasRootReferences (ReferenceDAG d e n) (NonEmpty Int) where
  {-# INLINE _rootRefs #-}
  _rootRefs = lens rootRefs (\r v -> r {rootRefs = v})


-- |
-- A 'Fold' for folding over a structure containing node decorations.
class FoldNodeDecoration s a | s -> a where
  foldNodeDecoration :: Fold s a
{-# SPECIALISE foldNodeDecoration :: Fold (ReferenceDAG d e n) n #-}

instance FoldNodeDecoration (ReferenceDAG d e n) n where
  {-# INLINE foldNodeDecoration #-}
  foldNodeDecoration = _references . folding id . _nodeDecoration

-- |
-- A 'Lens' for the 'dagCost' field
class HasDagCost s a | s -> a where
  _dagCost :: Lens' s a
{-# SPECIALISE _dagCost :: (GraphData d) ExtendedReal #-}

instance HasDagCost (GraphData d) ExtendedReal where
  {-# INLINE _dagCost #-}
  _dagCost = lens dagCost (\g d -> g {dagCost = d})

-- |
-- A 'Lens' for the 'networkEdgeCost' field.
class HasNetworkEdgeCost s a | s -> a where
  _networkEdgeCost :: Lens' s a
{-# SPECIALISE _networkEdgeCost :: Lens' (GraphData d) ExtendedReal #-}


instance HasNetworkEdgeCost (GraphData d) ExtendedReal where
  {-# INLINE _networkEdgeCost #-}
  _networkEdgeCost = lens networkEdgeCost (\g n -> g {networkEdgeCost = n})

-- |
-- a 'Lens' for the 'rootingCost' field.
class HasRootingCost s a | s -> a where
  _rootingCost :: Lens' s a
{-# SPECIALISE _rootingCost :: Lens' (GraphData d) Double #-}

instance HasRootingCost (GraphData d) Double where
  {-# INLINE _rootingCost #-}
  _rootingCost = lens rootingCost (\g r -> g {rootingCost = r})

-- |
-- A 'Lens' for 'totalBlockClost' field.
class HasTotalBlockCost s a | s -> a where
  _totalBlockCost :: Lens' s a
{-# SPECIALISE _totalBlockCost :: Lens' (GraphData d) Double #-}

instance HasTotalBlockCost (GraphData d) Double where
  {-# INLINE _totalBlockCost #-}
  _totalBlockCost = lens totalBlockCost (\g t -> g {totalBlockCost = t})

-- |
-- a 'Lens' for the 'graphMetadata' field.
class HasGraphMetadata s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _graphMetadata :: Lens s t a b
{-# SPECIALISE _graphMetadata :: Lens' (GraphData d) d #-}

instance HasGraphMetadata (GraphData d) (GraphData d') d d' where
  {-# INLINE _graphMetadata #-}
  _graphMetadata = lens graphMetadata (\g m -> g {graphMetadata = m})



type instance Key (ReferenceDAG d e) = Int


-- | (✔)
instance Bifunctor (ReferenceDAG d) where

    bimap f g dag =
        ReferenceDAG
        { references = h <$> references dag
        , rootRefs   = rootRefs  dag
        , graphData  = graphData dag
        }
      where
        h (IndexData node parentRefs' childRefs') = IndexData (g node) parentRefs' $ f <$> childRefs'


-- | (✔)
instance Binary d => Binary (GraphData d)


-- | (✔)
instance (Binary e, Binary n) => Binary (IndexData e n)


-- | (✔)
instance (Binary d, Binary e, Binary n) => Binary (ReferenceDAG d e n)


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
        ReferenceDAG
        { references = g <$> references dag
        , rootRefs   = rootRefs  dag
        , graphData  = graphData dag
        }
      where
        g (IndexData node parentRefs' childRefs') = IndexData (f node) parentRefs' childRefs'


-- | (✔)
instance HasLeafSet (ReferenceDAG d e n) (LeafSet n) where

    leafSet = Lens.to getter
        where
            getter :: ReferenceDAG d e n -> LeafSet n
            getter (ReferenceDAG v _ _) = LeafSet $ foldMap f v

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
        (ps,cs) = parentsAndChildren i dag

    isTreeNode i dag = olength ps == 1 && length cs == 2
      where
        (ps,cs) = parentsAndChildren i dag

    isLeafNode i dag =  null . childRefs  $ references dag ! fromEnum i

    isRootNode i dag = onull . parentRefs $ references dag ! fromEnum i

    networkResolutions = pure


-- | (✔)
instance PhylogeneticNetwork (ReferenceDAG d e n) NodeRef e n where

    root = toEnum . NE.head . rootRefs

    treeResolutions = pure


-- | (✔)
instance PhylogeneticTree (ReferenceDAG d e n) NodeRef e n where

    parent i dag = fmap toEnum . headMay . otoList . parentRefs $ references dag ! fromEnum i


-- | (✔)
instance Show n => PrintDot (ReferenceDAG d e n) where

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
instance Show n => Show (ReferenceDAG d e n) where

    show dag = intercalate "\n"
        [ topologyRendering dag
        , ""
        ,   sconcat
          . intersperse "\n"
          $ horizontalRendering <$> toBinaryRenderingTree show dag
        , ""
        , referenceRendering dag
        ]


-- | (✔)
instance TextShow d => TextShow (GraphData d) where
    showb x = unlinesB
        [ "DAG total cost:           " <> showb (dagCost x)
        , "DAG network edge cost:    " <> showb (networkEdgeCost x)
        , "DAG mutli-rooting cost:   " <> showb (rootingCost     x)
        , "DAG character block cost: " <> showb (totalBlockCost  x)
        ]


-- | (✔)
instance TextShow n => TextShow (ReferenceDAG d e n) where

    showb dag = TextShow.intercalateB "\n"
        [ showb . topologyRendering $ dag
        , ""
        , showb . sconcat . intersperse "\n" $ horizontalRendering <$> toBinaryRenderingTree (toString . showb) dag
        , ""
        , showb . referenceRendering $ dag
        ]

-- | (✔)
instance Semigroup d => Semigroup (GraphData d) where
  (<>)
    (GraphData dagCost1 networkEdgeCost1 rootingCost1 totalBlockCost1 graphMetadata1)
    (GraphData dagCost2 networkEdgeCost2 rootingCost2 totalBlockCost2 graphMetadata2)
      = GraphData
          (dagCost1         + dagCost2        )
          (networkEdgeCost1 + networkEdgeCost2)
          (rootingCost1     + rootingCost2    )
          (totalBlockCost1  + totalBlockCost2 )
          (graphMetadata1  <> graphMetadata2  )

-- | (✔)
instance Monoid d => Monoid (GraphData d) where
  mempty = GraphData
             { dagCost         = 0
             , networkEdgeCost = 0
             , rootingCost     = 0
             , totalBlockCost  = 0
             , graphMetadata   = mempty
             }

-- | (✔)
instance Show n => ToNewick (ReferenceDAG d e n) where

    toNewick refDag = mconcat [ newickString, "[", show cost, "]" ]
      where
        (_,newickString) = generateNewick namedVec rootRef mempty
        cost     = dagCost $ graphData refDag
        rootRef  = NE.head $ rootRefs refDag
        vec      = references refDag

        namedVec = zipWith (\x n -> n { nodeDecoration = x }) labelVec vec
        labelVec = (`evalState` (1,1,1)) $ mapM deriveLabel vec -- All network nodes have "htu\d" as nodeDecoration.

        deriveLabel :: IndexData e n -> State (Int, Int, Int) String
        deriveLabel node
          | shownLabel /= "{Unlabeled Node}" = pure shownLabel
          | otherwise = do
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
          where
            shownLabel = show $ nodeDecoration node


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
instance Show n => ToXML (IndexData e n) where

   toXML indexData = toXML . show $ nodeDecoration indexData


-- | (✔)
instance (Show n, ToXML n) => ToXML (ReferenceDAG d e n) where

    toXML dag = xmlElement "Directed_acyclic_graph" [] [newick, meta, vect]
      where
          meta   = Right . toXML $ graphData dag
          newick = Left ("Newick_representation", toNewick dag)
          vect   = Right . collapseElemList "Nodes" [] $ dag


-- |
-- Produces a set of directed references representing all edges in the DAG.
-- Equivelant to:
--
-- > referenceTreeEdgeSet dag `union` referenceNetworkEdgeSet dag
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
-- The following will always hold:
--
-- > null (referenceTreeEdgeSet dag `intersection` referenceTreeEdgeSet dag)
referenceTreeEdgeSet :: ReferenceDAG d e n -> EdgeSet (Int, Int)
referenceTreeEdgeSet dag = foldMapWithKey f refs
  where
    refs = references dag
    f i = foldMap (\e -> singletonEdgeSet (i,e)) . filter childHasOnlyOneParent . IM.keys . childRefs
    childHasOnlyOneParent = isSingleton . otoList . parentRefs . (refs !)


-- |
-- Produces a set of directed references representing all /Network/ edges in the DAG.
-- Omits /tree/ edges in the DAG. The resulting 'EdgeSet' *will not* be connected.
--
-- Equivelant to:
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
nodePostOrder f dag = ReferenceDAG <$> const newReferences <*> rootRefs <*> graphData $ dag
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
nodePreOrder f dag = ReferenceDAG <$> const newReferences <*> rootRefs <*> graphData $ dag
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
    listValue = toList xs
    referenceVector = V.fromList' $ (\(pSet, datum, cMap) -> IndexData datum pSet cMap) <$> listValue
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
  :: Show n
  => Int -- ^ Base over which the Unique
  -> Int
  -> ReferenceDAG d e n
  -> ([DotNode GraphID], [DotEdge GraphID])
--getDotContext dag | trace ("About to render this to DOT:\n\n" <> show dag) False = undefined
getDotContext uniqueIdentifierBase mostSignificantDigit dag = second mconcat . unzip $ foldMapWithKey f vec
  where
    idOffest = uniqueIdentifierBase * mostSignificantDigit

    vec = references dag

    toId :: Int -> GraphID
    toId = Num . Int . (+ idOffest)

    toAttributes :: Show a => a -> Attributes
    toAttributes x =
      case show x of
        ""  -> []
        str -> [ toLabel str ]

    f :: Show n => Int -> IndexData e n -> [(DotNode GraphID, [DotEdge GraphID])]
    f k v = [ (toDotNode, toDotEdge <$> kidRefs) ]
      where
        datum       = nodeDecoration v
        nodeId      = toId k
        nodeAttrs   = toAttributes datum
        kidRefs     = IM.keys $ childRefs v
        toDotNode   = DotNode nodeId nodeAttrs
        toDotEdge x = DotEdge nodeId (toId x) nodeAttrs


-- |
-- Contruct the intermediate 'BinaryRenderingTree' data type for a given 'ReferenceDAG'.
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
    -- Holds a map from indicies in the reference vector to symbolic references.
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
-- A function that recursively builds  a generating function
-- to be consumed as reference data. The function returned uses open recursion
-- (for memoization purposes), in the form of a 'DVector'.
dVectorPostorderWithContext
  :: forall a d e n
   . (  ChildContext (a , Int, IndexData e n) --  Child values and index information
     -> ParentContext Int                     --  Parent indices
     -> (Int, IndexData e n)                  --  Current index information
     -> a                                     --  Index data
     )
  -> ReferenceDAG d e n -> DVector a
dVectorPostorderWithContext indexFn dag = DVector f
  where
    refs        = dag ^. _references
    leafInds    = leafIndices dag

 -- A generate function with open recursion
    f :: (Int -> a) -> Int -> a
    f recurseFn ind =
      let parentContext = getParentContext refs ind in
        if ind `IS.member` leafInds
            -- inductive case updating leaves
          then indexFn NoChildren (getParentContext refs ind) (ind, refs ! ind)
          else
            case getChildContext refs ind of
              NoChildren
                -> error "Non-leaf node without children!"
           -- Recursively apply the function
              OneChild childInd
                -> indexFn (OneChild (g childInd)) parentContext (ind, refs ! ind)
              TwoChildren childInd1 childInd2
                -> indexFn
                     (TwoChildren (g childInd1) (g childInd2))
                     parentContext
                     (ind, refs ! ind)
      where
        g :: Int -> (a, Int, IndexData e n)
        g n = (recurseFn n, n, refs ! n)
-- |
-- A function that recursively builds (in a postorder fashion) a generating function
-- to be consumed as reference data. The function returned uses open recursion
-- (for memoization purposes), in the form of a 'DVector'.
dVectorPostorder
  :: forall a d e n
   . (  ChildContext a       --  Child values
     -> (Int, IndexData e n) --  Current index information
     -> a                    --  Index data
     )
  -> ReferenceDAG d e n -> DVector a
dVectorPostorder indexFn dag = DVector f
  where
    refs        = dag ^. _references
    leafInds    = leafIndices dag

 -- A generate function with open recursion
    f :: (Int -> a) -> Int -> a
    f recurseFn ind =
      if ind `IS.member` leafInds
        then indexFn NoChildren (ind, refs ! ind)  -- inductive case updating leaves
        else
          case otoChildContext . IM.keysSet $ (refs ! ind) ^. _childRefs of
            NoChildren
              -> error "Non-leaf node without children!"
            OneChild childInd                           -- recursively apply the function
              -> indexFn
                   (OneChild (recurseFn childInd))
                   (ind, refs ! ind)
            TwoChildren childInd1 childInd2                 -- Same as above.
              -> indexFn
                   (TwoChildren (recurseFn childInd1) (recurseFn childInd2))
                   (ind, refs ! ind)




-- |
-- A function that recursively builds (in a preorder fashion) a generating function
-- to be consumed as reference data. The function returned uses open recursion
-- (for memoization purposes), in the form of a 'DVector'.
dVectorPreorder
  :: forall a d e n
   . (  ParentContext a      --  Parent data
     -> (Int, IndexData e n) --  Current index information
     -> a                    --  Index data
     )
  -> ReferenceDAG d e n -> DVector a
dVectorPreorder indexFn dag = DVector f
  where
    refs        = dag ^. _references
    rootInds    = rootRefs dag

 -- A generate function with open recursion
    f :: (Int -> a) -> Int -> a
    f recurseFn ind =
      if ind `elem` rootInds
        then indexFn NoParent (ind, refs ! ind)  -- base case updating roots
        else
          case otoParentContext $ (refs ! ind) ^. _parentRefs of
            NoParent
              -> error "Non-root node without parents!"
            OneParent parInd                           -- recursively apply the function
              -> indexFn
                   (OneParent (recurseFn parInd))
                   (ind, refs ! ind)
            TwoParents parInd1 parInd2                 -- Same as above.
              -> indexFn
                   (TwoParents (recurseFn parInd1) (recurseFn parInd2))
                   (ind, refs ! ind)

-- |
-- A function that recursively builds (in a preorder fashion) a generating function
-- to be consumed as reference data. The function returned uses open recursion
-- (for memoization purposes), in the form of a 'DVector'.
dVectorPreorderWithContext
  :: forall a d e n
   . (  ParentContext (a, Int, IndexData e n)  -- Parent data with their index information
     -> ChildContext Int                       -- Child indicies 
     -> (Int, IndexData e n)                   -- Current index information
     -> a                                      -- Index data
     )
  -> ReferenceDAG d e n -> DVector a
dVectorPreorderWithContext indexFn dag = DVector f
  where
    refs        = dag ^. _references
    rootInds    = rootRefs dag

 -- A generate function with open recursion
    f :: (Int -> a) -> Int -> a
    f recurseFn ind =
      let childContext = getChildContext refs ind in
        if ind `elem` rootInds
            -- base case updating roots
          then indexFn NoParent childContext (ind, refs ! ind)
          else
              -- apply openly recursive argument
            case getParentContext refs ind of
              NoParent
                -> error "Non-root node without parents!"
              OneParent parInd                           
                -> indexFn
                     (OneParent (g parInd)) childContext (ind, refs ! ind)
              TwoParents parInd1 parInd2
                -> indexFn (TwoParents (g parInd1) (g parInd2)) childContext (ind, refs ! ind)

      where
        g :: Int -> (a, Int, IndexData e n)
        g n = (recurseFn n, n, refs ! n)



data NetworkContext = NetworkContext
  { netNode    :: Int
  , netParent1 :: Int
  , netParent2 :: Int
  }
  deriving (Eq, Ord, Show)

-- |
-- This computes, in a nodal context, the set of ancestral
-- Network contexts from the parent edge sets and current node data.
ancestralNetworkContextContextFn
  :: ParentContext (Set NetworkContext, Int, IndexData e n)  -- ^ Parent ancestral network contexts
  -> ChildContext Int
  -> (Int, IndexData e n)                                    -- ^ Current node data
  -> Set NetworkContext                                     -- ^ Current ancestral network contexts
ancestralNetworkContextContextFn parentNetContexts _ (currInd, nodeDatum) =
    case parentNetContexts of
      NoParent                              -> mempty
      OneParent (ancestralNetContext, _, _) -> ancestralNetContext
      TwoParents (ancestralNetContext1, parInd1, _) (ancestralNetContext2, parInd2, _) ->
        let
          currNetContext = NetworkContext
                          { netNode    = currInd
                          , netParent1 = parInd1
                          , netParent2 = parInd2
                          }
        in
               S.singleton currNetContext
            <> ancestralNetContext1
            <> ancestralNetContext2          
            
      
-- |
-- This computes, in a nodal context, the set of ancestral
-- edges from the parent edge sets and current node data.
ancestralEdgeSetContextFn
  :: ParentContext (EdgeSet (Int, Int))  -- ^ Parent ancestral edge sets
  -> (Int, IndexData e n)                -- ^ Current node data
  -> EdgeSet (Int, Int)                  -- ^ Current node edge sets
ancestralEdgeSetContextFn ancestralEdgeSets (currInd, nodeDatum) =
    case ancestralEdgeSets of
      NoParent                   -> mempty
      OneParent ancestralEdgeSet -> currentEdgeSet <> ancestralEdgeSet

      TwoParents ancestralEdgeSet1 ancestralEdgeSet2
        -> currentEdgeSet <> ancestralEdgeSet1 <> ancestralEdgeSet2
  where
    parRefs        = nodeDatum ^. _parentRefs
    currentEdgeSet = makeParentEdgeSet currInd parRefs

-- |
-- This computes, in a nodal context, the set of ancestral
-- nodes from the parent edge sets and current node data.
ancestralNodeSetContextFn
  :: ParentContext IntSet  -- ^ Parent ancestral nodes
  -> (Int, IndexData e n)      -- ^ Current node data
  -> IntSet                  -- ^ Current ancestral nodes
ancestralNodeSetContextFn ancestralNodes (currInd, nodeDatum) =
    case ancestralNodes of
      NoParent              -> mempty
      OneParent parentAncestralNodes -> (IS.singleton currInd) <> parentAncestralNodes

      TwoParents ancestralEdgeSet1 ancestralEdgeSet2
        -> IS.singleton currInd <> ancestralEdgeSet1 <> ancestralEdgeSet2


-- |
-- This computes, in a nodal context, the set of descendent
-- network nodes from the child descendent network sets and current node data.
-- It does this via a 'traversal with state' which keeps track of whether a child
-- node has only a single child and so is a descendant network node.
descendantNetworkNodesContextFn
  :: ChildContext IntSet  -- ^ Child descendent network node set
  -> (Int, IndexData e n) -- ^ Current node data
  ->  IntSet              -- ^ Current descendant network node sets
descendantNetworkNodesContextFn descendantNetworkNodes (currInd, _) =
    case descendantNetworkNodes of
    NoChildren                              -> mempty
 -- If a node has a single child then it is a network node and so is added
 -- to the set of nodes to be included in the parent descendant sets.
    OneChild descNetworkNodes               ->     IS.singleton currInd
                                                <> descNetworkNodes
    TwoChildren networkNodes1 networkNodes2 -> networkNodes1 <> networkNodes2


-- |
-- This computes, in a nodal context, the set of descendent
-- network edges from the child descendent network sets and current node data.
-- It does this via a 'traversal with state' which keeps track of whether a child
-- node has only a single child and so is a descendant network node.
descendantNetworkEdgesContextFn
  ::  ChildContext (Set (Int, Int), Int, IndexData e n) -- ^ Child descendent network edge set and indexInfo
  -> ParentContext Int
  -> (Int, IndexData e n)                               -- ^ Current node data
  -> Set (Int, Int)                                    -- ^ Current descendant network edge sets
descendantNetworkEdgesContextFn descendantNetworkNodes parentContext (currInd, _) =
   case descendantNetworkNodes of
    NoChildren                  -> mempty
 -- If a node has a single child then it is a network node and so is added
 -- to the set of network nodes.
    OneChild (networkNodes, _, _)  ->
      case parentContext of
        TwoParents parInd1 parInd2 ->
              S.singleton (parInd1, currInd)
          <>  S.singleton (parInd2, currInd)
          <> networkNodes
       -- This case should never happen as it violates the network invariants
        _  ->
          error "This graph contains a node with out-degree 1 and in-degree other than 2"
    TwoChildren (networkNodes1, _, _) (networkNodes2, _, _)
      -> networkNodes1 <> networkNodes2

               

-- |
-- This computes, in a nodal context, the set of ancestral
-- nodes which are incident to a root node set. It does this
-- via a 'traversal with state' passing a boolean value of
-- whether a node is a root node.
ancestralRootIncidentNodesContextFn
  :: ParentContext (IntSet,Bool)  -- ^ parent root node set and state information
  -> (Int, IndexData e n)         -- ^ Current node data
  -> (IntSet, Bool)               -- ^ Current node edge sets
ancestralRootIncidentNodesContextFn ancestralRootNodes (currInd, nodeDatum) =
  case ancestralRootNodes of
    NoParent
      -> (mempty, True)
    OneParent (parAncestralSet, incidentToRoot)
      -> if incidentToRoot
           then (IS.singleton currInd, False)
           else (parAncestralSet, False)
    TwoParents
      (parAncestralSet1, incidentToRoot1)
      (parAncestralSet2, incidentToRoot2)
        -> if incidentToRoot1 && incidentToRoot2
             then (IS.singleton currInd <> parAncestralSet1 <> parAncestralSet2, False)
             else (parAncestralSet1 <> parAncestralSet2, False)

-- |
-- Generate a vector of graph data for finding the candidate network edges in a memoized
-- fashion.
tabulateNetworkInformation
  :: ReferenceDAG d e n
  -> Vector
        ( EdgeSet (Int, Int)  -- Ancestral Edge Set
        , IntSet              -- Descendant Network Nodes
        , IntSet              -- Ancestral Node Set
        , Set NetworkContext  -- Ancestral Network Contexts
        )
tabulateNetworkInformation dag =
  let
    dVectorAncestralEdge    = dVectorPreorder  ancestralEdgeSetContextFn       dag
    dVectorDescendantNet    = dVectorPostorder descendantNetworkNodesContextFn dag
    dVectorAncestralNodes   = dVectorPreorder  ancestralNodeSetContextFn       dag    
    dVectorAncestralNetCtxt = dVectorPreorderWithContext ancestralNetworkContextContextFn dag


    dVectorNetInfo
      = DV.zip4
          dVectorAncestralEdge
          dVectorDescendantNet
          dVectorAncestralNodes
          dVectorAncestralNetCtxt          
  in
    generateMemo lengthRefs dVectorNetInfo
  where
    lengthRefs = length $ dag ^. _references


data RootStatus = IncludeRoot | ExcludeRoot

-- |
-- Find all candgidate network edges in a DAG.
candidateNetworkEdges'
  :: RootStatus     -- ^ Whether we include nodes adjacent to the root
  -> ReferenceDAG d e n
  -> Set ((Int, Int), (Int,Int))
candidateNetworkEdges' rootStatus dag = S.fromList candidateEdgesList
  where
    completeEdges      = toList $ getEdges dag
    rootIndices        = IS.fromList . toList . rootRefs $ dag
    leafInds           = leafIndices dag
    networkNodes       = gatherDescendantNetworkNodes rootIndices networkInformation
    networkContexts    = gatherAncestralNetworkContexts leafInds networkInformation
--    (networkParents, networkNodes) = S.fromList . unzip . S.toList $ networkEdges

  -- This vector contains all the information needed for the various edge
 -- compatibility criteria.
    networkInformation = tabulateNetworkInformation dag
    


 -- Gets all pairs of distinct edges from the edge set that can be
 -- compatably added to the network.
    candidateEdgesList :: [((Int, Int), (Int, Int))]
    candidateEdgesList =
        do
       -- collect distinct edge pairs.
          (e1@(src1,tgt1) : es)       <- tails completeEdges
          e2@(src2, tgt2) <- es
          guard $ symmetricTest e1 e2
          let
            e1e2Bool = not (hasIncidentNetworkNode e2) && posetalTest e1 e2
            e2e1Bool = not (hasIncidentNetworkNode e1) && posetalTest e2 e1
          case (e1e2Bool, e2e1Bool) of
                (True, True)   -> pure (e1,e2) <|> pure (e2,e1)
                (True, False)  -> pure (e1, e2)
                (False, True)  -> pure (e2, e1)
                (False, False) -> Alt.empty

      where
     -- Checks historical compatability in a symmetrical fashion.                    
        symmetricTest :: (Int, Int) -> (Int, Int) -> Bool
        symmetricTest e1@(src1,tgt1) e2@(src2, tgt2) =
     -- Network Info
            let
              e1AncestralEdges, e2AncestralEdges :: EdgeSet (Int, Int)  
              e1AncestralEdges  = proj4_1 $ networkInformation ! tgt1  
              e2AncestralEdges  = proj4_1 $ networkInformation ! tgt2
  
              e1TgtAncestralNodes, e2TgtAncestralNodes :: IntSet
              e1TgtAncestralNodes = proj4_3 $ networkInformation ! tgt1
              e2TgtAncestralNodes = proj4_3 $ networkInformation ! tgt2
              
        -- Boolean tests
    --          e1NetworkEdgeTest, e2NetworkEdgeTest :: Bool
    --          e1NetworkEdgeTest = networkDescendantTest src1 e2TgtAncestralNodes
      --
            in    
                -- First check if the two edges are from the same parent to short circuit
              -- faster in this case.
                  src1 /= src2
              -- or if either edge is ancestral to the other.
              && (singletonEdgeSet e1 `disjoint` e2AncestralEdges)
              && (singletonEdgeSet e2 `disjoint` e1AncestralEdges)
              -- finally check if either node fails the network edge test.
            --  && e1NetworkEdgeTest
            --  && e2NetworkEdgeTest
          

   
--            e1NetworkEdgeTest =
--                 (\x -> trace (unlines
--                   ["e1 : " <> show e1
--                   ,"e2 : " <> show e2
--                  -- , "otherNetPar: " <> show otherNetworkParent e1
--                   , "e2TgtAnc : " <> show e2TgtAncestralNodes
--                   , "netNodes: " <> show networkNodes
--                   , "netContexts: " <> show networkContexts
--                   , "othernetPare1: " <> (show $ getOtherNetworkParent src1 networkContexts)
--                   , "othernetPare2: " <> (show $ getOtherNetworkParent src2 networkContexts)                       , "x :" <> show x
--                   ]) x) $
--                   networkDescendantTest src1

        
        -- We check historical compatibility by checking if the
        -- descendant network node of tgt1 is "historically compatible" to the descendant
        -- network nodes of src2. This is because these are the historical
        -- coniditons the new nodes will inherit:
        --                o  ── src1     ┌─────────────────────────────────────────────────┐
        --               /             ┌─│ New parent network node of newTgt has the same  │
        --              /              │ │ descendant network events as tgt1.              │
        --             o  ── newSrc ───┘ └─────────────────────────────────────────────────┘
        --            /  \______________________
        --           /                          \
        --          o ── tgt1                    \
        --                                     [...]
        --                                         \              ┌───────── o ── src2
        --                                          \____________ │ __      /
        --    ┌────────────────────────────────────────────┐      │   \    /
        --    │  src2 is the other new parent network node │      │    \  /
        --    │  of newTgt and so must be potentially      │──────┘      o  ── newTgt
        --    │  historically coincident with newSrc.      │            /
        --    └────────────────────────────────────────────┘           /
        --                                                            o  ── tgt2
        --   ┌─────────────────────────────────────┐
        --   │ Potentially historically coincident │
        --   └─────────────────────────────────────┘
        --   This  means that any descendant network node of src2 cannot also be a descendant
        --   network node to a non-root node ancestral to src1 as this would lead to
        --   non-transitivty in the implied oredering. Similarly we cannot have that a
        --   descendant network node of tgt1 (which has the same  descendant nodes as
        --   newSrc other than newTgt) equal to a descendant network node of a non-root
        --   node ancestral to src2.
        posetalTest :: (Int, Int) -> (Int, Int) -> Bool
        posetalTest e1@(src1, tgt1) e2@(src2, tgt2) = 
          let
         -- Network Information
         
            e1SrcAncestralNodes, e2SrcAncestralNodes :: IntSet
            e1SrcAncestralNodes = proj4_3 $ networkInformation ! src1
            e2SrcAncestralNodes = proj4_3 $ networkInformation ! src2
            e1TgtAncestralNodes = proj4_3 $ networkInformation ! tgt1
            e2TgtAncestralNodes = proj4_3 $ networkInformation ! tgt2
            
            e1SrcAncestralNetworkNodes, e2SrcAncestralNetworkNodes :: IntSet
            e1SrcAncestralNetworkNodes = e1SrcAncestralNodes `IS.intersection` networkNodes
            e2SrcAncestralNetworkNodes = e2SrcAncestralNodes `IS.intersection` networkNodes

            e1TgtDescendantNetworkNodes = proj4_2 $ networkInformation ! tgt1
            e2SrcDescendantNetworkNodes = proj4_2 $ networkInformation ! src2

            

         -- Tests
            e1HasRootSource, e2HasRootSource :: Bool
            e1HasRootSource = src1 `IS.member` rootIndices
            e2HasRootSource = src2 `IS.member` rootIndices
 
            e1AncestralTest, e2AncestralTest :: Bool
            e1AncestralTest
              = e2SrcDescendantNetworkNodes `IS.disjoint` e1SrcAncestralNetworkNodes
            e2AncestralTest
              = e1TgtDescendantNetworkNodes `IS.disjoint` e2SrcAncestralNetworkNodes

            
            networkParentSource2Test :: Int -> IntSet -> Bool
            networkParentSource2Test src1 e2TgtAncNodes = 
              case getOtherNetworkParentFromNode src1 networkContexts of
                Just netPar -> netPar `IS.notMember` e2TgtAncNodes
                Nothing     -> True

            networkEdgeSource1Test :: (Int, Int) -> IntSet -> Bool
            networkEdgeSource1Test e e2TgtAncNodes = 
              case getOtherNetworkParentFromEdge e networkContexts of
                Just netPar -> netPar `IS.notMember` e2TgtAncNodes
                Nothing     -> True

         -- TODO: better name and document  -- networkParentSource2Test?
         -- checks if src2 is already a network parent node with a node ancestral to newSrc
            e2NetworkEdgeTest = networkParentSource2Test src2 e1TgtAncestralNodes

         -- TODO: same   --networkEdgeSource1Test
         -- checks if (src1, tgt1) is a network edge which is in a context which is ancestral
         -- to src2
            e1NetworkEdgeTest = networkEdgeSource1Test e1 e2TgtAncestralNodes

          in
       -- TODO: Revisit whether root adjacent edges.
       -- We will re-evaluate how to add network edges incident to the root after
       -- some of the core data sttructures have changed.
            case rootStatus of
              ExcludeRoot ->     not e1HasRootSource
                              && not e2HasRootSource
                              && e1AncestralTest
                              && e2AncestralTest
                              && e1NetworkEdgeTest
                              && e2NetworkEdgeTest
              IncludeRoot ->
                case (e1HasRootSource, e2HasRootSource) of
                  (True , True ) -> False
                  (True , False) -> e2AncestralTest
                  (False, True ) -> e1AncestralTest
                  (False, False) -> e1AncestralTest
                                    && e2AncestralTest
                


     -- helper functions
        hasIncidentNetworkNode :: (Int, Int) -> Bool
        hasIncidentNetworkNode e@(src,tgt) =
             tgt `IS.member` networkNodes
          || src `IS.member` networkNodes

        getOtherNetworkParentFromNode :: Int -> Set NetworkContext -> Maybe Int
        getOtherNetworkParentFromNode src = getFirst . foldMap getNetPar
          where
            getNetPar :: NetworkContext -> First Int
            getNetPar (NetworkContext networkNode netPar1 netPar2)
              | src == netPar1  = pure netPar2
              | src == netPar2  = pure netPar1
              | otherwise       = mempty

        getOtherNetworkParentFromEdge :: (Int, Int) -> Set NetworkContext -> Maybe Int
        getOtherNetworkParentFromEdge (src, tgt) = getFirst . foldMap getNetPar
          where
            getNetPar :: NetworkContext -> First Int
            getNetPar (NetworkContext networkNode netPar1 netPar2)
              | src == netPar1 && tgt == networkNode  = pure netPar2
              | src == netPar2 && tgt == networkNode  = pure netPar1
              | otherwise       = mempty


candidateNetworkEdges = candidateNetworkEdges' ExcludeRoot



-- |
-- Helper function to get all descendent network nodes from an `IntSet` of nodes.
gatherDescendantNetworkNodes
  :: IntSet                    -- ^ Node set
  -> Vector (a, IntSet, c, d)  -- ^ Vector tuple with network node indices
  -> IntSet                    -- ^ All descendant network nodes
gatherDescendantNetworkNodes inds vect
  = ofoldMap (\ind -> proj4_2 $ vect ! ind) inds

-- |
-- Helper function to get all descendent network contexts from an `IntSet` of nodes.
gatherAncestralNetworkContexts
  :: IntSet                                -- ^ Node set
  -> Vector (a, b, c, Set NetworkContext)  -- ^ Vector tuple with network node indices
  -> Set NetworkContext                    -- ^ All descendant network nodes
gatherAncestralNetworkContexts inds vect
  = ofoldMap (\ind -> proj4_4 $ vect ! ind) inds



-- |
-- Gets all edges from a `ReferenceDAG` which are incident to a network
-- node.
getNetworkEdges :: ReferenceDAG d e n -> Set (Int, Int)
getNetworkEdges dag = fold descendantNetworkEdges
  where
    descendantNetworkEdges = generateMemo numberOfNodes dVectorDescendantNet
    dVectorDescendantNet   = dVectorPostorderWithContext descendantNetworkEdgesContextFn dag
    numberOfNodes          = length $ dag ^. _references







getChildContext :: forall e n . Vector (IndexData e n) -> Int -> ChildContext Int
{-# INLINE getChildContext #-}
getChildContext refs ind = otoChildContext . IM.keysSet $ (refs ! ind) ^. _childRefs


getParentContext :: forall e n . Vector (IndexData e n) -> Int -> ParentContext Int
{-# INLINE getParentContext #-}
getParentContext refs ind = otoParentContext $ (refs ! ind) ^. _parentRefs
