-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Graph.Node.Internal
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveFunctor, DeriveGeneric, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}

module Bio.Graph.Node.Internal
  ( EdgeSet
  , NewickSerialization()
  , PhylogeneticNode (..)
  , PhylogeneticNode2(..)
  , ResolutionCache
  , ResolutionInformation(..)
  , SubtreeLeafSet()
  -- , hasLeafSet
  , addEdgeToEdgeSet
  , addNetworkEdgeToTopology
  , singletonEdgeSet
  , singletonNewickSerialization
  , singletonSubtreeLeafSet
  , pNode2
  ) where


-- import Bio.Graph.LeafSet
import Control.DeepSeq
import Control.Lens
-- import Data.Bifunctor
import Data.Bits
import Data.BitVector.LittleEndian
import Data.EdgeSet
import Data.Foldable
import Data.List.NonEmpty       (NonEmpty(..))
import Data.TopologyRepresentation
import GHC.Generics
import Text.Newick.Class
import Text.XML


-- |
-- This serves as a computation /invariant/ node decoration designed to hold node
-- information such as name and later a subtree structure.
data  PhylogeneticNode n s
    = PNode
    { nodeDecorationDatum :: n
    , sequenceDecoration  :: s
    } deriving (Eq, Functor, Generic, Show)


-- |
-- This serves as a computation /dependant/ node decoration designed to hold node
-- information for a a phylogenetic network (or tree).
data  PhylogeneticNode2 s n
    = PNode2
    { resolutions          :: ResolutionCache s
    , nodeDecorationDatum2 :: n
    } deriving (Eq, Functor, Generic)


-- |
-- A collection of information used to memoize network optimizations.
data  ResolutionInformation s
    = ResInfo
    { totalSubtreeCost       :: Double
    , localSequenceCost      :: Double
    , leafSetRepresentation  :: SubtreeLeafSet
    , subtreeRepresentation  :: NewickSerialization
    , subtreeEdgeSet         :: EdgeSet (Int, Int)
    , topologyRepresentation :: TopologyRepresentation (Int, Int)
    , characterSequence      :: s
    } deriving (Functor, Generic)


-- |
-- A collection of subtree resolutions. Represents a non-deterministic collection
-- of subtree choices.
type ResolutionCache s = NonEmpty (ResolutionInformation s)


-- |
-- A newick representation of a subtree. Semigroup instance used for subtree
-- joining.
newtype NewickSerialization = NS String
  deriving (Eq, Generic, Ord)


-- |
-- An arbitrarily ordered collection of leaf nodes in a subtree. Leaves in the
-- tree are uniquely identified by an index across the entire DAG. Set bits
-- represent a leaf uniquily identified by that index being present in the
-- subtree.
--
-- Use the 'Semigroup' operation '(<>)' to union the leaves included in a leaf
-- set.
newtype SubtreeLeafSet = LS BitVector
  deriving (Bits, Eq, Generic, Ord)


instance Bifunctor PhylogeneticNode where

    bimap g f =
      PNode <$> g . nodeDecorationDatum
            <*> f . sequenceDecoration


instance Eq  (ResolutionInformation s) where

    lhs == rhs = leafSetRepresentation lhs == leafSetRepresentation rhs
              && subtreeRepresentation lhs == subtreeRepresentation rhs


instance (NFData n, NFData s) => NFData (PhylogeneticNode n s)


instance (NFData s, NFData n) => NFData (PhylogeneticNode2 s n)


instance NFData NewickSerialization


instance NFData s => NFData (ResolutionInformation s)


instance NFData SubtreeLeafSet


instance Ord (ResolutionInformation s) where

    lhs `compare` rhs =
        case leafSetRepresentation lhs `compare` leafSetRepresentation lhs of
          EQ -> subtreeRepresentation lhs `compare` subtreeRepresentation rhs
          c  -> c

-- -- |
-- -- A 'Lens' for the 'transitionCostMatrix' field
-- instance (Element d ~ c) => HasSparseTransitionCostMatrix (DynamicDecorationDirectOptimization d) MemoizedCostMatrix where

--     sparseTransitionCostMatrix = lens getter setter
--       where
--          getter e   = dynamicDecorationDirectOptimizationMetadata e ^. sparseTransitionCostMatrix
--          setter e f = e { dynamicDecorationDirectOptimizationMetadata = dynamicDecorationDirectOptimizationMetadata e & sparseTransitionCostMatrix .~ f }


instance Semigroup NewickSerialization where

    (NS lhs) <> (NS rhs) = NS $ "(" <> lhs <> "," <> rhs <> ")"


instance Semigroup SubtreeLeafSet where

    (<>) = (.|.)


instance Show NewickSerialization where

    show (NS s) = s


instance (Show n, Show s) => Show (PhylogeneticNode2 s n) where

    show node = unlines
        [ show $ nodeDecorationDatum2 node
        , "Resolutions: {" <> (show . length . resolutions) node <> "}\n"
        , unlines . fmap show . toList $ resolutions node
        ]


instance Show s => Show (ResolutionInformation s) where

    show resInfo = unlines tokens
      where
        tokens =
           [ "Total Cost: "    <> show (totalSubtreeCost      resInfo)
           , "Local Cost: "    <> show (localSequenceCost     resInfo)
           , "Edge Set  : "    <> show (subtreeEdgeSet        resInfo)
           , "Leaf Set  : "    <> show (leafSetRepresentation resInfo)
           , "Subtree   : "    <> show (subtreeRepresentation resInfo)
           , "Decoration:\n\n" <> show (characterSequence     resInfo)
           ]


instance Show SubtreeLeafSet where

    show (LS bv) = foldMap f $ toBits bv
      where
        f x = if x then "1" else "0"


instance Show s => ToNewick (PhylogeneticNode2 n s) where
    toNewick node = show $ nodeDecorationDatum2 node {- case nodeDecorationDatum2 node of
                        Just str -> show str
                        _        -> "" -}


instance (ToXML n) => ToXML (PhylogeneticNode2 n s) where

    toXML node = xmlElement "Phylogenetic_node" nodeAttrs contents
        where
            nodeAttrs       = []
            resolutionAttrs = []
            contents        = [ Right ( collapseElemList "Resolutions" resolutionAttrs (resolutions node) )
                              ]


instance (ToXML s) => ToXML (ResolutionInformation s) where

    toXML info = xmlElement "Resolution_info" attrs contents
        where
            -- (ES edgeSet)  = subtreeEdgeSet info
            attrs         = []
            contents      = [ Right . toXML $ characterSequence info
                            , Left  ("Total_subtree_cost" , show $ totalSubtreeCost  info)
                            , Left  ("Local_sequence_cost", show $ localSequenceCost info)
                            , Right subtree
                            ]
            subtree       = xmlElement "Subtree_fields" [] subtreeFields
            subtreeFields = [ Left ("Subtree_leaf_set"      , show $ leafSetRepresentation info)
                            , Left ("Subtree_representation", show $ subtreeRepresentation info)
                            , Left ("Subtree_edge_set"      , show $ subtreeEdgeSet        info)
                            ]


-- |
-- Adds an edge reference to an existing subtree resolution.
addEdgeToEdgeSet :: (Int, Int) -> ResolutionInformation s -> ResolutionInformation s
addEdgeToEdgeSet e r = r { subtreeEdgeSet = singletonEdgeSet e <> subtreeEdgeSet r }


-- |
-- Updates the 'TopologyRepresentation' to include a new network edge present in
-- to spanning tree the node is a subtree of.
addNetworkEdgeToTopology
  :: (Int, Int) -- ^ Applied network edge identifier
  -> (Int, Int) -- ^ Excluded network edge identifier
  -> ResolutionInformation s
  -> ResolutionInformation s
addNetworkEdgeToTopology e x r = r { topologyRepresentation = isolatedNetworkEdgeContext e x <> topologyRepresentation r }


-- |
-- A safe constructor of a 'PhylogeneticNode2'.
pNode2 :: n -> ResolutionCache s -> PhylogeneticNode2 s n
pNode2 = flip PNode2


-- |
-- Construct a singleton newick string with a unique identifier that can be
-- rendered to a string through it's 'Show' instance.
singletonNewickSerialization :: Show i => i -> NewickSerialization
singletonNewickSerialization i = NS $ show i


-- |
-- Construct a singleton leaf set by supplying the number of leaves and the
-- unique leaf index.
singletonSubtreeLeafSet
  :: Int -- ^ Leaf count
  -> Int -- ^ Leaf index
  -> SubtreeLeafSet
singletonSubtreeLeafSet n i = LS . (`setBit` i) $ toEnum n `fromNumber` (0 :: Integer)
