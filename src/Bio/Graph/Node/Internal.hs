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

{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}

module Bio.Graph.Node.Internal
  ( EdgeSet
  , NewickSerialization()
  , PhylogeneticNode (..)
  , PhylogeneticNode2(..)
  , ResolutionCache
  , ResolutionInformation(..)
  , HasNodeDecorationDatum(..)
  , HasResolutions(..)
  , HasTotalSubtreeCost(..)
  , HasLocalSequenceCost(..)
  , HasLeafSetRepresentation(..)
  , HasSubtreeRepresentation(..)
  , HasSubtreeEdgeSet(..)
  , HasTopologyRepresentation(..)
  , HasCharacterSequence(..)
  , addEdgeToEdgeSet
  , addNetworkEdgeToTopology
  , singletonEdgeSet
  , singletonNewickSerialization
  , singletonSubtreeLeafSet
  , pNode2
  ) where


import Control.DeepSeq
import Control.Lens
import Control.Lens.Lens           (Lens)
import Data.Bits
import Data.BitVector.LittleEndian
import Data.EdgeSet
import Data.Foldable
import Data.List.NonEmpty          (NonEmpty (..))
import Data.Text.Lazy              (Text, pack)
import Data.TopologyRepresentation
import Data.UnionSet
import GHC.Generics
import Text.Newick.Class
import Text.XML


-- |
-- This serves as a computation /invariant/ node decoration designed to hold node
-- information such as name and later a subtree structure.
data  PhylogeneticNode n s
    = PNode
    { nodeDecorationDatum :: !n
    , sequenceDecoration  :: !s
    } deriving (Eq, Functor, Generic, Show)


-- |
-- This serves as a computation /dependant/ node decoration designed to hold node
-- information for a phylogenetic network (or tree).
data  PhylogeneticNode2 s n
    = PNode2
    { resolutions          :: !(ResolutionCache s)
    , nodeDecorationDatum2 :: !n
    } deriving (Eq, Functor, Generic)

-- |
-- A 'Lens' for the 'resoluions' field.
class HasResolutions s t a b | s -> a, b s -> t where
  _resolutions :: Lens s t a b

{-# SPECIALISE _resolutions
                 :: Lens (PhylogeneticNode2 s n) (PhylogeneticNode2 s' n) (ResolutionCache s) (ResolutionCache s')
  #-}

instance HasResolutions
  (PhylogeneticNode2 s n) (PhylogeneticNode2 s' n) (ResolutionCache s) (ResolutionCache s')
  where
  {-# INLINE _resolutions #-}
  _resolutions = lens resolutions (\p s -> p {resolutions = s})

-- |
-- A 'Lens' for the 'nodeDecorationDatum' field.
class HasNodeDecorationDatum s t a b | s -> a, b s -> t where
  _nodeDecorationDatum :: Lens s t a b

{-# SPECIALISE _nodeDecorationDatum :: Lens (PhylogeneticNode2 s n) (PhylogeneticNode2 s n') n n' #-}

instance HasNodeDecorationDatum (PhylogeneticNode2 s n) (PhylogeneticNode2 s n') n n' where
  {-# INLINE _nodeDecorationDatum #-}
  _nodeDecorationDatum = lens nodeDecorationDatum2 (\p n -> p {nodeDecorationDatum2 = n})

-- |
-- A collection of information used to memoize network optimizations.
data  ResolutionInformation s
    = ResInfo
    { totalSubtreeCost       :: {-# UNPACK #-} !Double
    , localSequenceCost      :: {-# UNPACK #-} !Double
    , leafSetRepresentation  :: {-# UNPACK #-} !UnionSet
    , subtreeRepresentation  ::                !NewickSerialization
    , subtreeEdgeSet         ::                !(EdgeSet (Int, Int))
    , topologyRepresentation :: {-# UNPACK #-} !(TopologyRepresentation (Int, Int))
    , characterSequence      :: !s
    } deriving (Functor, Foldable, Traversable, Generic)


-- |
-- A collection of subtree resolutions. Represents a non-deterministic collection
-- of subtree choices.
type ResolutionCache s = NonEmpty (ResolutionInformation s)


-- |
-- A newick representation of a subtree. 'Semigroup' instance used for subtree
-- joining.
newtype NewickSerialization = NS Text
  deriving newtype Eq
  deriving         Generic
  deriving newtype Ord
  deriving newtype Show

-- |
-- A 'Lens' for the 'totalSubtreeCost' field in 'ResolutionInformation'
class HasTotalSubtreeCost s a | s -> a where
  _totalSubtreeCost :: Lens' s a

{-# SPECIALISE _totalSubtreeCost :: Lens' (ResolutionInformation s) Double #-}


instance HasTotalSubtreeCost (ResolutionInformation s) Double where
  {-# INLINE _totalSubtreeCost #-}
  _totalSubtreeCost = lens totalSubtreeCost (\r t -> r {totalSubtreeCost = t})

-- |
-- A 'Lens' for the 'localSequencecost' field in 'ResolutionInformation'
class HasLocalSequenceCost s a | s -> a where
  _localSequenceCost :: Lens' s a

{-# SPECIALISE _localSequenceCost :: Lens' (ResolutionInformation s) Double #-}


instance HasLocalSequenceCost (ResolutionInformation s) Double where
  {-# INLINE _localSequenceCost #-}
  _localSequenceCost = lens localSequenceCost (\r t -> r {localSequenceCost = t})

-- |
-- A 'Lens' for the 'LeafSetRepresentation' field in 'ResolutionInformation'
class HasLeafSetRepresentation s a | s -> a where
  _leafSetRepresentation :: Lens' s a

{-# SPECIALISE _leafSetRepresentation :: Lens' (ResolutionInformation s) UnionSet #-}


instance HasLeafSetRepresentation (ResolutionInformation s) UnionSet where
  {-# INLINE _leafSetRepresentation #-}
  _leafSetRepresentation = lens leafSetRepresentation (\r l -> r {leafSetRepresentation = l})

-- |
-- A 'Lens' for the 'subtreeRepresentation' field in 'ResolutionInformation'
class HasSubtreeRepresentation s a | s -> a where
  _subtreeRepresentation :: Lens' s a

{-# SPECIALISE _subtreeRepresentation :: Lens' (ResolutionInformation s) NewickSerialization #-}


instance HasSubtreeRepresentation (ResolutionInformation s) NewickSerialization where
  {-# INLINE _subtreeRepresentation #-}
  _subtreeRepresentation = lens subtreeRepresentation (\r s -> r {subtreeRepresentation = s})

-- |
-- A 'Lens' for the 'subtreeEdgeSet' field in 'ResolutionInformation'
class HasSubtreeEdgeSet s a | s -> a where
  _subtreeEdgeSet :: Lens' s a

{-# SPECIALISE _subtreeEdgeSet :: Lens' (ResolutionInformation s) (EdgeSet (Int, Int)) #-}


instance HasSubtreeEdgeSet (ResolutionInformation s) (EdgeSet (Int, Int)) where
  {-# INLINE _subtreeEdgeSet #-}
  _subtreeEdgeSet = lens subtreeEdgeSet (\r s -> r {subtreeEdgeSet = s})

-- |
-- A 'Lens' for the 'topologyRepresentation' field in 'ResolutionInformation'
class HasTopologyRepresentation s a | s -> a where
  _topologyRepresentation :: Lens' s a

{-# SPECIALISE
      _topologyRepresentation :: Lens' (ResolutionInformation s) (TopologyRepresentation (Int, Int))  #-}

instance HasTopologyRepresentation (ResolutionInformation s) (TopologyRepresentation (Int, Int))
  where
  {-# INLINE _topologyRepresentation #-}
  _topologyRepresentation = lens topologyRepresentation (\r t -> r {topologyRepresentation = t})

-- |
-- A 'Lens' for the 'characterSequence' field in 'ResolutionInformation'
class HasCharacterSequence s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _characterSequence :: Lens s t a b

{-# SPECIALISE
      _characterSequence
        :: Lens'
             (ResolutionInformation s)
             (ResolutionInformation s')
             s
             s'
  #-}

instance HasCharacterSequence
           (ResolutionInformation s)
           (ResolutionInformation s')
           s
           s'
  where
  {-# INLINE _characterSequence #-}
  _characterSequence = lens characterSequence (\r c -> r {characterSequence = c})


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



instance Ord (ResolutionInformation s) where

    lhs `compare` rhs =
        case leafSetRepresentation lhs `compare` leafSetRepresentation lhs of
          EQ -> subtreeRepresentation lhs `compare` subtreeRepresentation rhs
          c  -> c


instance Semigroup NewickSerialization where

    (NS lhs) <> (NS rhs) = NS $ "(" <> lhs <> "," <> rhs <> ")"


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


instance Show s => ToNewick (PhylogeneticNode2 n s) where

    toNewick node = show $ nodeDecorationDatum2 node


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
-- the spanning tree the node is a subtree of.
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
-- rendered to a string through its 'Show' instance.
singletonNewickSerialization :: Show i => i -> NewickSerialization
singletonNewickSerialization = NS . pack . show


-- |
-- Construct a singleton leaf set by supplying the number of leaves and the
-- unique leaf index.
singletonSubtreeLeafSet
  :: Int -- ^ Leaf count
  -> Int -- ^ Leaf index
  -> UnionSet
singletonSubtreeLeafSet = singletonSet
