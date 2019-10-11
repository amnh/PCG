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
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}

module Bio.Graph.Node.Internal
  ( EdgeSet
  , NewickSerialization()
  , PhylogeneticFreeNode (..)
  , PhylogeneticNode(..)
  , ResolutionCache
  , ResolutionInformation(..)
  , ResolutionMetadata(..)
  , HasNodeDecorationDatum(..)
  , HasResolutions(..)
  , HasSequenceDecoration(..)
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
import Control.Lens                hiding (_head)
import Control.Lens.Lens           (Lens)
import Data.EdgeSet
import Data.Foldable
import Data.Functor.Apply
import Data.List.NonEmpty          (NonEmpty (..))
import Data.List.Utility           (HasHead (..))
import Data.Ord                    (comparing)
import Data.Text.Lazy              (Text, pack)
import Data.TopologyRepresentation
import Data.UnionSet
import GHC.Generics
import Text.Newick.Class
import Text.XML
import TextShow                    (TextShow (showb, showt), toString, unlinesB)


-- |
-- This serves as a computation /invariant/ node decoration designed to hold node
-- information such as name and later a subtree structure.
data  PhylogeneticFreeNode n s
    = PNode
    { nodeDecorationDatum :: !n
    , sequenceDecoration  :: !s
    } deriving (Eq, Functor, Generic, Show)


-- |
-- This serves as a computation /dependant/ node decoration designed to hold node
-- information for a phylogenetic network (or tree).
data  PhylogeneticNode s n
    = PNode2
    { resolutions          :: !(ResolutionCache s)
    , nodeDecorationDatum2 :: !n
    } deriving (Eq, Functor, Generic)


-- |
-- A 'Lens' for the 'resoluions' field.
{-# SPECIALISE _resolutions :: Lens (PhylogeneticNode s n) (PhylogeneticNode s' n) (ResolutionCache s) (ResolutionCache s') #-}
class HasResolutions s t a b | s -> a, b s -> t where

    _resolutions :: Lens s t a b


-- |
-- A 'Lens' for the 'nodeDecorationDatum' field.
{-# SPECIALISE _nodeDecorationDatum :: Lens (PhylogeneticNode s n) (PhylogeneticNode s n') n n' #-}
class HasNodeDecorationDatum s t a b | s -> a, b s -> t where

    _nodeDecorationDatum :: Lens s t a b


-- |
-- A 'Lens' for the 'sequenceDecoration' field.
{-# SPECIALISE _sequenceDecoration :: Lens (PhylogeneticNode s n) (PhylogeneticNode s' n) s s' #-}
class HasSequenceDecoration s a | s -> a where

    _sequenceDecoration :: Lens' s a


instance HasResolutions (PhylogeneticNode s n) (PhylogeneticNode s' n) (ResolutionCache s) (ResolutionCache s') where

    {-# INLINE _resolutions #-}
    _resolutions = lens resolutions (\p s -> p {resolutions = s})


instance HasNodeDecorationDatum (PhylogeneticNode s n) (PhylogeneticNode s n') n n' where

    {-# INLINE _nodeDecorationDatum #-}
    _nodeDecorationDatum = lens nodeDecorationDatum2 (\p n -> p {nodeDecorationDatum2 = n})


instance HasSequenceDecoration (PhylogeneticFreeNode n s) s where

    {-# INLINE _sequenceDecoration #-}
    _sequenceDecoration = lens sequenceDecoration (\p s -> p {sequenceDecoration = s})


instance forall s n . HasSequenceDecoration (PhylogeneticNode s n) s where

    {-# INLINE _sequenceDecoration #-}
    _sequenceDecoration = _resolutions @_ @_ @(ResolutionCache s) @(ResolutionCache s)
                        . _head @(ResolutionCache s) @(ResolutionInformation s)
                        . _characterSequence


-- |
-- A collection of information used to memoize network optimizations.
data  ResolutionInformation s
    = ResInfo
    { resolutionMetadata :: ResolutionMetadata
    , characterSequence  :: !s
    } deriving (Functor, Foldable, Traversable, Generic)


-- |
-- The metadata of a subtree resolution.
data ResolutionMetadata
    = ResolutionMetadata
    { totalSubtreeCost       :: {-# UNPACK #-} !Double
    , localSequenceCost      :: {-# UNPACK #-} !Double
    , leafSetRepresentation  :: {-# UNPACK #-} !UnionSet
    , subtreeRepresentation  ::                !NewickSerialization
    , subtreeEdgeSet         ::                !(EdgeSet (Int, Int))
    , topologyRepresentation :: {-# UNPACK #-} !(TopologyRepresentation (Int, Int))
    } deriving (Eq, Ord, Generic)


-- |
-- A collection of subtree resolutions. Represents a non-deterministic collection
-- of subtree choices.
type ResolutionCache s = NonEmpty (ResolutionInformation s)


-- |
-- A newick representation of a subtree. 'Semigroup' instance used for subtree
-- joining.
newtype NewickSerialization = NS Text
  deriving newtype (Eq, Ord, Show, TextShow)
  deriving stock   (Generic)



-- |
-- A 'Lens' for the 'totalSubtreeCost' field in 'ResolutionMetadata'
{-# SPECIALISE _totalSubtreeCost :: Lens' ResolutionMetadata Double #-}
{-# SPECIALISE _totalSubtreeCost :: Lens' (ResolutionInformation s) Double #-}
class HasTotalSubtreeCost s a | s -> a where

    _totalSubtreeCost :: Lens' s a


instance HasTotalSubtreeCost ResolutionMetadata Double where

    {-# INLINE _totalSubtreeCost #-}
    _totalSubtreeCost = lens totalSubtreeCost (\r t -> r {totalSubtreeCost = t})


instance HasTotalSubtreeCost (ResolutionInformation s) Double where

    {-# INLINE _totalSubtreeCost #-}
    _totalSubtreeCost = _resolutionMetadata . _totalSubtreeCost


-- |
-- A 'Lens' for the 'localSequencecost' field in 'ResolutionMetadata'
{-# SPECIALISE _localSequenceCost :: Lens' ResolutionMetadata Double #-}
{-# SPECIALISE _localSequenceCost :: Lens' (ResolutionInformation s) Double #-}
class HasLocalSequenceCost s a | s -> a where

    _localSequenceCost :: Lens' s a


instance HasLocalSequenceCost ResolutionMetadata Double where

    {-# INLINE _localSequenceCost #-}
    _localSequenceCost = lens localSequenceCost (\r t -> r {localSequenceCost = t})


instance HasLocalSequenceCost (ResolutionInformation s) Double where

    {-# INLINE _localSequenceCost #-}
    _localSequenceCost = _resolutionMetadata . _localSequenceCost


-- |
-- A 'Lens' for the 'LeafSetRepresentation' field in 'ResolutionMetadata'
{-# SPECIALISE _leafSetRepresentation :: Lens' ResolutionMetadata UnionSet #-}
{-# SPECIALISE _leafSetRepresentation :: Lens' (ResolutionInformation s) UnionSet #-}
class HasLeafSetRepresentation s a | s -> a where

    _leafSetRepresentation :: Lens' s a


instance HasLeafSetRepresentation ResolutionMetadata UnionSet where

    {-# INLINE _leafSetRepresentation #-}
    _leafSetRepresentation = lens leafSetRepresentation (\r l -> r {leafSetRepresentation = l})


instance HasLeafSetRepresentation (ResolutionInformation s) UnionSet where

    {-# INLINE _leafSetRepresentation #-}
    _leafSetRepresentation = _resolutionMetadata . _leafSetRepresentation


-- |
-- A 'Lens' for the 'subtreeRepresentation' field in 'ResolutionMetadata'
{-# SPECIALISE _subtreeRepresentation :: Lens' ResolutionMetadata NewickSerialization #-}
{-# SPECIALISE _subtreeRepresentation :: Lens' (ResolutionInformation s) NewickSerialization #-}
class HasSubtreeRepresentation s a | s -> a where

    _subtreeRepresentation :: Lens' s a


instance HasSubtreeRepresentation ResolutionMetadata NewickSerialization where

    {-# INLINE _subtreeRepresentation #-}
    _subtreeRepresentation = lens subtreeRepresentation (\r s -> r {subtreeRepresentation = s})


instance HasSubtreeRepresentation (ResolutionInformation s) NewickSerialization where

    {-# INLINE _subtreeRepresentation #-}
    _subtreeRepresentation = _resolutionMetadata . _subtreeRepresentation


-- |
-- A 'Lens' for the 'subtreeEdgeSet' field in 'ResolutionMetadata'
{-# SPECIALISE _subtreeEdgeSet :: Lens' ResolutionMetadata (EdgeSet (Int, Int)) #-}
{-# SPECIALISE _subtreeEdgeSet :: Lens' (ResolutionInformation s) (EdgeSet (Int, Int)) #-}
class HasSubtreeEdgeSet s a | s -> a where

    _subtreeEdgeSet :: Lens' s a


instance HasSubtreeEdgeSet ResolutionMetadata (EdgeSet (Int, Int)) where

    {-# INLINE _subtreeEdgeSet #-}
    _subtreeEdgeSet = lens subtreeEdgeSet (\r s -> r {subtreeEdgeSet = s})


instance HasSubtreeEdgeSet (ResolutionInformation s) (EdgeSet (Int, Int)) where

    {-# INLINE _subtreeEdgeSet #-}
    _subtreeEdgeSet = _resolutionMetadata . _subtreeEdgeSet


-- |
-- A 'Lens' for the 'topologyRepresentation' field in 'ResolutionMetadata'
{-# SPECIALISE _topologyRepresentation :: Lens' ResolutionMetadata (TopologyRepresentation (Int, Int))  #-}
{-# SPECIALISE _topologyRepresentation :: Lens' (ResolutionInformation s) (TopologyRepresentation (Int, Int))  #-}
class HasTopologyRepresentation s a | s -> a where

    _topologyRepresentation :: Lens' s a


instance HasTopologyRepresentation ResolutionMetadata (TopologyRepresentation (Int, Int)) where

    {-# INLINE _topologyRepresentation #-}
    _topologyRepresentation = lens topologyRepresentation (\r t -> r {topologyRepresentation = t})


instance HasTopologyRepresentation (ResolutionInformation s) (TopologyRepresentation (Int, Int)) where

    {-# INLINE _topologyRepresentation #-}
    _topologyRepresentation = _resolutionMetadata . _topologyRepresentation


-- |
-- A 'Lens' for the 'characterSequence' field in 'ResolutionInformation'
{-# SPECIALISE _characterSequence :: Lens (ResolutionInformation s) (ResolutionInformation s') s s' #-}
class HasCharacterSequence s t a b | s -> a, t -> b, s b -> t, t a -> s where

    _characterSequence :: Lens s t a b


instance HasCharacterSequence (ResolutionInformation s) (ResolutionInformation s') s s' where

    {-# INLINE _characterSequence #-}
    _characterSequence = lens characterSequence (\r c -> r {characterSequence = c})


-- |
-- A 'Lens' for the 'resolutionMetadata' field in 'ResolutionInformation'
{-# SPECIALISE  _resolutionMetadata :: Lens' (ResolutionInformation s) ResolutionMetadata  #-}
class HasResolutionMetadata s a| s -> a where

    _resolutionMetadata :: Lens' s a


instance HasResolutionMetadata (ResolutionInformation s) ResolutionMetadata where

    {-# INLINE _resolutionMetadata #-}
    _resolutionMetadata = lens resolutionMetadata (\r m -> r {resolutionMetadata = m})


instance Apply ResolutionInformation where

    (<.>) f r =
      ResInfo
      { resolutionMetadata =  f ^.  _resolutionMetadata <> r ^. _resolutionMetadata
      , characterSequence  = (f ^. _characterSequence) $ r ^. _characterSequence
      }

    (.>) ra rb =
      ResInfo
      { resolutionMetadata = ra ^. _resolutionMetadata <> rb ^. _resolutionMetadata
      , characterSequence  = rb ^. _characterSequence
      }

    (<.) ra rb =
      ResInfo
      { resolutionMetadata = ra ^. _resolutionMetadata <> rb ^. _resolutionMetadata
      , characterSequence  = ra ^. _characterSequence
      }


instance Bifunctor PhylogeneticFreeNode where

    bimap g f =
      PNode <$> g . nodeDecorationDatum
            <*> f . sequenceDecoration


instance Eq  (ResolutionInformation s) where

    lhs == rhs =
                  lhs ^. _leafSetRepresentation == rhs ^. _leafSetRepresentation
               && lhs ^. _subtreeRepresentation == rhs ^. _subtreeRepresentation


instance (NFData n, NFData s) => NFData (PhylogeneticFreeNode n s)


instance (NFData s, NFData n) => NFData (PhylogeneticNode s n)


instance NFData NewickSerialization


instance NFData s => NFData (ResolutionInformation s)


instance NFData ResolutionMetadata


instance Ord (ResolutionInformation s) where

    compare =  comparing (^. _leafSetRepresentation)
            <> comparing (^. _subtreeRepresentation)
           -- using pointwise (<>) for Ordering for lexicographic comparison


instance Semigroup NewickSerialization where

    (NS lhs) <> (NS rhs) = NS $ "(" <> lhs <> "," <> rhs <> ")"


instance Semigroup ResolutionMetadata where
    (<>) l r =
      ResolutionMetadata
      { totalSubtreeCost       = l ^. _totalSubtreeCost       +  r ^. _totalSubtreeCost
      , localSequenceCost      = 0
      , leafSetRepresentation  = l ^. _leafSetRepresentation  <> r ^. _leafSetRepresentation
      , subtreeRepresentation  = l ^. _subtreeRepresentation  <> r ^. _subtreeRepresentation
      , subtreeEdgeSet         = l ^. _subtreeEdgeSet         <> r ^. _subtreeEdgeSet
      , topologyRepresentation = l ^. _topologyRepresentation <> r ^. _topologyRepresentation
      }


instance (TextShow n, TextShow s) => Show (PhylogeneticNode s n) where

    show = toString . showb


instance (TextShow n, TextShow s) => TextShow (PhylogeneticFreeNode s n) where

    showb node = unlinesB
        [ "PNode {"
        , "  " <> showb (nodeDecorationDatum node)
        , "  " <> showb (sequenceDecoration  node)
        , "}"
        ]


instance (TextShow n, TextShow s) => TextShow (PhylogeneticNode s n) where

    showb node = unlinesB
        [ showb $ nodeDecorationDatum2 node
        , "Resolutions: {" <> (showb . length . resolutions) node <> "}\n"
        , unlinesB . fmap showb . toList $ resolutions node
        ]


instance Show s => Show (ResolutionInformation s) where

    show resInfo = unlines tokens
      where
        tokens =
           [ "Total Cost: "    <> show (resInfo ^. _totalSubtreeCost     )
           , "Local Cost: "    <> show (resInfo ^. _localSequenceCost    )
           , "Edge Set  : "    <> show (resInfo ^. _subtreeEdgeSet       )
           , "Leaf Set  : "    <> show (resInfo ^. _leafSetRepresentation)
           , "Subtree   : "    <> show (resInfo ^. _subtreeRepresentation)
           , "Decoration:\n\n" <> show (resInfo ^. _characterSequence    )
           ]


instance TextShow s => TextShow (ResolutionInformation s) where

    showb resInfo = unlinesB tokens
      where
        tokens =
           [ "Total Cost: "    <> showb (resInfo ^. _totalSubtreeCost     )
           , "Local Cost: "    <> showb (resInfo ^. _localSequenceCost    )
           , "Edge Set  : "    <> showb (resInfo ^. _subtreeEdgeSet       )
           , "Leaf Set  : "    <> showb (resInfo ^. _leafSetRepresentation)
           , "Subtree   : "    <> showb (resInfo ^. _subtreeRepresentation)
           , "Decoration:\n\n" <> showb (resInfo ^. _characterSequence    )
           ]


instance TextShow s => ToNewick (PhylogeneticNode n s) where

    toNewick = showt . nodeDecorationDatum2


instance (ToXML n) => ToXML (PhylogeneticNode n s) where

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
                        , Left  ("Total_subtree_cost" , show $ info ^. _totalSubtreeCost )
                        , Left  ("Local_sequence_cost", show $ info ^. _localSequenceCost)
                        , Right subtree
                        ]
        subtree       = xmlElement "Subtree_fields" [] subtreeFields
        subtreeFields = [ Left ("Subtree_leaf_set"
                               , show $ info ^. _leafSetRepresentation)
                        , Left ("Subtree_representation"
                               , show $ info ^. _subtreeRepresentation)
                        , Left ("Subtree_edge_set"
                               , show $ info ^. _subtreeEdgeSet       )
                        ]


-- |
-- Adds an edge reference to an existing subtree resolution.
addEdgeToEdgeSet :: (Int, Int) -> ResolutionInformation s -> ResolutionInformation s
addEdgeToEdgeSet e r =
  r &  _resolutionMetadata . _subtreeEdgeSet
    %~ (singletonEdgeSet e <>)


-- |
-- Updates the 'TopologyRepresentation' to include a new network edge present in
-- the spanning tree the node is a subtree of.
addNetworkEdgeToTopology
  :: (Int, Int) -- ^ Applied network edge identifier
  -> (Int, Int) -- ^ Excluded network edge identifier
  -> ResolutionInformation s
  -> ResolutionInformation s
addNetworkEdgeToTopology e x r
  = r &    _resolutionMetadata . _topologyRepresentation
      %~  (isolatedNetworkEdgeContext e x <>)
        -- prepend network edge to topologyRepresentation


-- |
-- A safe constructor of a 'PhylogeneticNode'.
pNode2 :: n -> ResolutionCache s -> PhylogeneticNode s n
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
