
------------------------------------------------------------------------------
-- |
-- Module      :  Bio.Graph.PhylogeneticDAG.Internal
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

{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MonoLocalBinds         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}


module Bio.Graph.PhylogeneticDAG.Internal
  ( EdgeReference
  , PhylogeneticFreeDAG(..)
  , PostorderContextualData(..)
  , PhylogeneticDAG(..)
  , applySoftwireResolutions
  , generateLocalResolutions
  , getDotContextWithBaseAndIndex
  , localResolutionApplication
  , renderSummary
  , resolutionsDoNotOverlap
  , HasPhylogeneticForest(..)
  , HasColumnMetadata(..)
  , HasMinimalNetworkContext(..)
  , HasVirtualNodeMapping(..)
  , setDefaultMetadata
  ) where


import           Bio.Character.Decoration.Shared
import           Bio.Character.Encodable
import           Bio.Graph.LeafSet
import           Bio.Graph.Node
import           Bio.Graph.Node.Context
import           Bio.Graph.ReferenceDAG.Internal
import           Bio.Metadata.Continuous
import           Bio.Metadata.Discrete
import           Bio.Metadata.DiscreteWithTCM
import           Bio.Metadata.Dynamic
import           Bio.Sequence
import           Control.Arrow                   ((***))
import           Control.DeepSeq
import           Control.Lens                    as Lens hiding ((<.>))
import           Data.Binary
import           Data.Binary.Instances.UnorderedContainers ()
import           Data.Bits
import           Data.Foldable
import           Data.Foldable.Custom            (sum')
import           Data.Functor.Apply              (Apply ((<.>)))
import           Data.GraphViz.Printing
import           Data.GraphViz.Types
import           Data.HashMap.Lazy               (HashMap)
import qualified Data.IntMap                     as IM
import           Data.IntSet                     (IntSet)
import qualified Data.IntSet                     as IS
import           Data.Key
import           Data.List                       (zip4)
import           Data.List.NonEmpty              (NonEmpty (..))
import qualified Data.List.NonEmpty              as NE
import           Data.Maybe                      (fromMaybe)
import           Data.MonoTraversable
import           Data.String
import qualified Data.Text                       as T (Text, filter, length, unlines)
import           Data.TopologyRepresentation
import           Data.Vector                     (Vector)
import           GHC.Generics
import           Prelude                         hiding (zip)
import           Text.Newick.Class
import           Text.XML
import           TextShow                        (Builder, TextShow (showb, showt), fromText, toString, unlinesB)
import           Type.Reflection                 (Typeable)


-- |
-- Wrapper for ReferenceDAG (deprecated)
--
-- Type annotations (metadata types):
--
-- * e = 'Data.EdgeLength'
-- * n = node labels: 'Maybe'('String')
-- * u = various (initial, post-order, pre-order) 'Bio.Character.Decoration.Continuous' specified as 'ContinuousChar'  or 'Bio.Metadata.General'
-- * v = various (initial, post-order, pre-order) 'Bio.Character.Decoration.Fitch'      specified as 'StaticCharacter' or 'Bio.Metadata.Discrete'
-- * w = various (initial, post-order, pre-order) 'Bio.Character.Decoration.Additive'   specified as 'StaticCharacter' or 'Bio.Metadata.Discrete'
-- * x = various (initial, post-order, pre-order) 'Bio.Character.Decoration.Sankoff'    specified as 'StaticCharacter' or 'Bio.Metadata.Discrete'
-- * y = various (initial, post-order, pre-order) 'Bio.Character.Decoration.Sankoff'    specified as 'StaticCharacter' or 'Bio.Metadata.Discrete'
-- * z = various (initial, post-order, pre-order) 'Bio.Character.Decoration.Dynamic'    specified as 'DynamicCharacter'     or 'Bio.Metadata.DiscreteWithTCM'
data  PhylogeneticFreeDAG m e n u v w x y z
    = PDAG
    { simpleColumnMetadata     :: MetadataSequence m
    , simplePhylogeneticForest :: ReferenceDAG () e (PhylogeneticFreeNode n (CharacterSequence u v w x y z))
    }
    deriving anyclass (Binary, NFData)
    deriving stock    (Generic)


-- |
-- Wrapper for ReferenceDAG
-- Type annotations (metadata types):
--
-- * e = edge info, as yet undetermined
-- * n = node labels: 'Maybe'('String')
-- * u = various (initial, post-order, pre-order) 'Bio.Character.Decoration.Continuous' specified as 'ContinuousChar'  or 'Bio.Metadata.General'
-- * v = various (initial, post-order, pre-order) 'Bio.Character.Decoration.Fitch'      specified as 'StaticCharacter' or 'Bio.Metadata.Discrete'
-- * w = various (initial, post-order, pre-order) 'Bio.Character.Decoration.Additive'   specified as 'StaticCharacter' or 'Bio.Metadata.Discrete'
-- * x = various (initial, post-order, pre-order) 'Bio.Character.Decoration.Sankoff'    specified as 'StaticCharacter' or 'Bio.Metadata.Discrete'
-- * y = various (initial, post-order, pre-order) 'Bio.Character.Decoration.Sankoff'    specified as 'StaticCharacter' or 'Bio.Metadata.Discrete'
-- * z = various (initial, post-order, pre-order) 'Bio.Character.Decoration.Dynamic'    specified as 'DynamicCharacter'     or 'Bio.Metadata.DiscreteWithTCM'


data  PostorderContextualData t = PostorderContextualData
    { virtualNodeMapping    :: HashMap EdgeReference (ResolutionCache t)
    , contextualNodeDatum   :: Vector (HashMap EdgeReference (ResolutionCache t))
    , minimalNetworkContext :: Maybe (NonEmpty (TraversalTopology, Double, Double, Double, Vector (NonEmpty TraversalFocusEdge)))
    }
    deriving stock    (Eq, Show, Generic, Functor, Foldable, Traversable)
    deriving anyclass (Binary, NFData)


-- |
-- The primitive phylogenetic object.
--
-- Contains each taxon exactly once, though the leaves do not need to be connected.
-- The graph object allows for multiple roots and recticulation events.
data  PhylogeneticDAG m e n u v w x y z
    = PDAG2
    { phylogeneticForest :: ReferenceDAG
                              (PostorderContextualData (CharacterSequence u v w x y z))
                              e
                              (PhylogeneticNode (CharacterSequence u v w x y z) n)
    , columnMetadata     :: MetadataSequence m
    }
    deriving anyclass (Binary, NFData)
    deriving stock    (Generic, Typeable)


-- |
-- Reference to a edge in the DAG
type EdgeReference = (Int, Int)


-- |
-- A 'Lens' for the 'virtualNodeMapping' field in 'PostorderContextualData'
{-# SPECIALISE  _virtualNodeMapping :: Lens' (PostorderContextualData t) (HashMap EdgeReference (ResolutionCache t)) #-}
class HasVirtualNodeMapping s a | s -> a where

    _virtualNodeMapping :: Lens' s a


-- |
-- A 'Lens' for the 'minimalNetworkContext' field in 'PostorderContextualData'
{-# SPECIALISE  _minimalNetworkContext :: Lens' (PostorderContextualData t) (Maybe (NonEmpty (TraversalTopology, Double, Double, Double, Vector (NonEmpty TraversalFocusEdge)))) #-}
class HasMinimalNetworkContext s a | s -> a where

    _minimalNetworkContext :: Lens' s a


-- |
-- A 'Lens' for the 'phyogeneticForest' field in 'PhylogeneticDAG'
class HasPhylogeneticForest s t a b | s -> a, t -> b, s b -> t, t a -> s where

    _phylogeneticForest :: Lens s t a b


-- |
-- A 'Lens' for the 'columnMetadata' field in 'PhylogeneticDAG'
class HasColumnMetadata s t a b | s -> a, t -> b, s b -> t, t a -> s where

    _columnMetadata :: Lens s t a b


instance HasVirtualNodeMapping (PostorderContextualData t) (HashMap EdgeReference (ResolutionCache t)) where

    {-# INLINE _virtualNodeMapping #-}
    _virtualNodeMapping = lens virtualNodeMapping (\p v -> p {virtualNodeMapping = v})


instance HasVirtualNodeMapping (PhylogeneticDAG m e n u v w x y z) (HashMap EdgeReference (ResolutionCache (CharacterSequence u v w x y z))) where

    {-# INLINE _virtualNodeMapping #-}
    _virtualNodeMapping = lens
        (virtualNodeMapping . graphMetadata . graphData . phylogeneticForest)
        (\p v -> p & _phylogeneticForest . _graphData . _graphMetadata . _virtualNodeMapping .~ v)


instance HasMinimalNetworkContext (PostorderContextualData t) (Maybe (NonEmpty (TraversalTopology, Double, Double, Double, Vector (NonEmpty TraversalFocusEdge)))) where

    {-# INLINE _minimalNetworkContext #-}
    _minimalNetworkContext = lens minimalNetworkContext (\p m -> p {minimalNetworkContext = m})


instance HasPhylogeneticForest
           (PhylogeneticDAG m e n u v w x y z)
           (PhylogeneticDAG m e' n' u' v' w' x' y' z')
           (ReferenceDAG (PostorderContextualData (CharacterSequence u v w x y z)) e (PhylogeneticNode (CharacterSequence u v w x y z) n))
           (ReferenceDAG (PostorderContextualData (CharacterSequence u' v' w' x' y' z')) e' (PhylogeneticNode (CharacterSequence u' v' w' x' y' z') n')) where

    {-# INLINE _phylogeneticForest #-}
    _phylogeneticForest = lens phylogeneticForest (\p pf -> p {phylogeneticForest = pf})


instance HasColumnMetadata
           (PhylogeneticDAG m  e n u v w x y z)
           (PhylogeneticDAG m' e n u v w x y z)
           (MetadataSequence m                 )
           (MetadataSequence m'                ) where
    {-# INLINE _columnMetadata #-}
    _columnMetadata = lens columnMetadata (\p c -> p {columnMetadata = c})


instance HasLeafSet (PhylogeneticDAG m e n u v w x y z) (LeafSet (PhylogeneticNode (CharacterSequence u v w x y z) n)) where

    leafSet = Lens.to getter
        where
            getter ::
              PhylogeneticDAG m e n u v w x y z
              -> LeafSet (PhylogeneticNode (CharacterSequence u v w x y z) n)
            getter (PDAG2 e _) =  e ^. leafSet


instance TextShow n => PrintDot (PhylogeneticDAG m e n u v w x y z) where

    unqtDot       = unqtDot . discardCharacters

    toDot         = toDot . discardCharacters

    unqtListToDot = unqtListToDot . fmap discardCharacters

    listToDot     = listToDot . fmap discardCharacters


instance Semigroup (PostorderContextualData t) where
   (<>)
     PostorderContextualData
       { virtualNodeMapping    = v1
       , contextualNodeDatum   = c1
       , minimalNetworkContext = m1
       }
     PostorderContextualData
       { virtualNodeMapping    = v2
       , contextualNodeDatum   = c2
       , minimalNetworkContext = m2
       }
    = PostorderContextualData
        { virtualNodeMapping    = v1 <> v2
        , contextualNodeDatum   = c1 <> c2
        , minimalNetworkContext = m1 <> m2
        }


instance ( TextShow e
         , TextShow n
         , TextShow u
         , TextShow v
         , TextShow w
         , TextShow x
         , TextShow y
         , TextShow z
         ) => Show (PhylogeneticFreeDAG m e n u v w x y z) where

    show = toString . showb


instance ( TextShow e
         , TextShow n
         , TextShow u
         , TextShow v
         , TextShow w
         , TextShow x
         , TextShow y
         , TextShow z
         ) => TextShow (PhylogeneticFreeDAG m e n u v w x y z) where

    showb (PDAG _ dag) = showb dag <> "\n" <> foldMapWithKey f dag
      where
        f i (PNode n sek) = fold [ "Node {", showb i, "}:\n\n", unlinesB [showb n, showb sek] ]


instance ( HasBlockCost u v w x y z
         , TextShow m
         , TextShow e
         , TextShow n
         , TextShow u
         , TextShow v
         , TextShow w
         , TextShow x
         , TextShow y
         , TextShow z
         ) => Show (PhylogeneticDAG m e n u v w x y z) where

    show = toString . showb


instance TextShow n => ToNewick (PhylogeneticDAG m e n u v w x y z) where

    toNewick = toNewick . discardCharacters


instance TextShow t => TextShow (PostorderContextualData t) where

    showb (PostorderContextualData virtual contextual minimal) = unlinesB
        [ showb virtual
        , showb contextual
        , showb minimal
        ]


instance ( HasBlockCost u v w x y z
         , TextShow m
         , TextShow e
         , TextShow n
         , TextShow u
         , TextShow v
         , TextShow w
         , TextShow x
         , TextShow y
         , TextShow z
         ) => TextShow (PhylogeneticDAG m e n u v w x y z) where

    showb p@(PDAG2 dag m) = unlinesB
        [ renderSummary  p
        , renderMetadata m
        , foldMapWithKey f dag
        ]
      where
        f i n = fold [ "Node {", showb i, "}:\n\n", showb n ]



instance ( TextShow n
         , TextShow u
         , TextShow v
         , TextShow w
         , TextShow y
         , TextShow x
         , TextShow z
         , ToXML u
         , ToXML v
         , ToXML w
         , ToXML y
         , ToXML z
         ) => ToXML (PhylogeneticDAG m e n u v w x y z)  where

    toXML (PDAG2 refDag _) = toXML refDag


-- |
-- Get the dot context of a 'PhylogeneticDAG' with useful internal node decorations.
getDotContextWithBaseAndIndex
  :: TextShow n
  => Int -- ^ Base over which the Unique
  -> Int
  -> PhylogeneticDAG m e n u v w x y z
  -> ([DotNode GraphID], [DotEdge GraphID])
getDotContextWithBaseAndIndex i j (PDAG2 dag _) = getDotContext i j $ nodeDecorationDatum2 <$> dag


-- |
-- Generate all the possible, consistent combinatorial patterns of the subtree.
applySoftwireResolutions
  :: forall s t
  .  ResolutionInformation t                    -- ^ Parent node information
  -> ChildContext (ResolutionCache s, IntSet)   -- ^ Possible subtree resolution information
  -> NonEmpty
       (ResolutionInformation
         (PostorderContext t s)
       )                                        -- ^ Potential subtree contexts
applySoftwireResolutions nodeInfo =
    \case
      NoChildren                     -> pure $ LeafContext <$> nodeInfo

      OneChild (resCache, _)         -> fmap PostNetworkContext <$> resCache

      TwoChildren leftCtxt rightCtxt -> pairingLogic (leftCtxt, rightCtxt)
  where
    multipleParents :: IntSet -> Bool
    multipleParents = (/= 1) . olength

    pairingLogic
      :: ((ResolutionCache s, IntSet) , (ResolutionCache s, IntSet))
      -> NonEmpty
           (ResolutionInformation
             (PostorderContext t s)
           )
    pairingLogic
      ((leftResolutionCache , lPars), (rightResolutionCache , rPars)) =
        case (multipleParents lPars, multipleParents rPars) of
          (False, False) -> pairedResolutions
          (False, True ) -> pairedResolutions <> lhsNetResolutions
          (True , False) -> pairedResolutions <> rhsNetResolutions
          (True , True ) -> pairedResolutions <> lhsNetResolutions <> rhsNetResolutions
       where
         lhsNetResolutions
           = fmap PostNetworkContext <$> leftResolutionCache
         rhsNetResolutions
           = fmap PostNetworkContext <$> rightResolutionCache

         pairedResolutions =
             case makeProductBinaryContexts leftResolutionCache rightResolutionCache of
               x:xs -> x:|xs
               []   -> error errorContext  -- this shouldn't ever happen
           where
             errorContext = unlines
                 [ "The impossible happened!"
                 , "LHS:"
                 , shownLHS
                 , "RHS:"
                 , shownRHS
                 ]
               where
                 shownLHS = unlines . toList
                              $ show . (^. _leafSetRepresentation) <$> leftResolutionCache
                 shownRHS = unlines . toList
                              $ show . (^. _leafSetRepresentation) <$> rightResolutionCache

      -- The apply operator here correctly updates the 'ResolutionMetadata' with a
      -- pointwise semigroup operator in the relevant fields.
         makeProductBinaryContexts ls rs =
           [ PostBinaryContext <$> l <.> r
           | l <- toList ls
           , r <- toList rs
           , resolutionsDoNotOverlap l r
           ]

-- |
-- Given a pre-order transformation for each type parameter, apply the
-- transformations to each possible resolution that is not inconsistent.
generateLocalResolutions
  :: HasBlockCost u' v' w' x' y' z'
  => (ContinuousCharacterMetadataDec                         -> PostorderContext u u' -> u')
  -> (DiscreteCharacterMetadataDec                           -> PostorderContext v v' -> v')
  -> (DiscreteCharacterMetadataDec                           -> PostorderContext w w' -> w')
  -> (DiscreteWithTCMCharacterMetadataDec StaticCharacter    -> PostorderContext x x' -> x')
  -> (DiscreteWithTCMCharacterMetadataDec StaticCharacter    -> PostorderContext y y' -> y')
  -> (DynamicCharacterMetadataDec (Element DynamicCharacter) -> PostorderContext z z' -> z')
  ->  MetadataSequence m
  ->  ResolutionInformation
        (PostorderContext
          (CharacterSequence u  v  w  x  y  z )
          (CharacterSequence u' v' w' x' y' z')
        )
  ->  ResolutionInformation (CharacterSequence u' v' w' x' y' z')
generateLocalResolutions f1 f2 f3 f4 f5 f6 meta resolutionContext =
  let
    (f1Leaf, f1Bin) = (leafFunction <$> f1 , postBinaryFunction <$> f1)
    (f2Leaf, f2Bin) = (leafFunction <$> f2 , postBinaryFunction <$> f2)
    (f3Leaf, f3Bin) = (leafFunction <$> f3 , postBinaryFunction <$> f3)
    (f4Leaf, f4Bin) = (leafFunction <$> f4 , postBinaryFunction <$> f4)
    (f5Leaf, f5Bin) = (leafFunction <$> f5 , postBinaryFunction <$> f5)
    (f6Leaf, f6Bin) = (leafFunction <$> f6 , postBinaryFunction <$> f6)
  in  case resolutionContext ^. _characterSequence of
        LeafContext leafCharSequence ->
          let newCharacterSequence
                = hexZipMeta
                    f1Leaf
                    f2Leaf
                    f3Leaf
                    f4Leaf
                    f5Leaf
                    f6Leaf
                    meta
                    leafCharSequence
              newTotalCost = sequenceCost meta newCharacterSequence
          in
            resolutionContext & _characterSequence .~ newCharacterSequence
                              & _totalSubtreeCost  .~ newTotalCost
                              & _localSequenceCost .~ newTotalCost

        PostNetworkContext netChildCharSequence ->
          resolutionContext & _characterSequence .~ netChildCharSequence
          -- Want to propagate what is stored in the network child resolution
          -- to the parent.

        PostBinaryContext
          { -- binNode    = parentalCharSequence
            leftChild  = leftCharSequence
          , rightChild = rightCharSequence
          } ->
          let
            totalChildCost = resolutionContext ^. _totalSubtreeCost
            newCharacterSequence
                = hexZipMeta
                    f1Bin
                    f2Bin
                    f3Bin
                    f4Bin
                    f5Bin
                    f6Bin
                    meta
--                    parentalCharSequence
                    (hexZip
                      leftCharSequence
                      rightCharSequence
                    )
            newTotalCost      = sequenceCost meta newCharacterSequence
            newLocalCost      = newTotalCost - totalChildCost
              in
            resolutionContext
              & _characterSequence .~ newCharacterSequence
              & _totalSubtreeCost  .~ newTotalCost
              & _localSequenceCost .~ newLocalCost


-- |
-- Given a transformation for the last type parameter and two resolution caches,
-- apply the transformation to all possible resolution combinations.
localResolutionApplication
  :: HasBlockCost u v w x y d
  => (DynamicCharacterMetadataDec (Element DynamicCharacter) -> PostorderContext d d -> d)
  -> MetadataSequence m
  -> NonEmpty (ResolutionInformation (CharacterSequence u v w x y d))
  -> NonEmpty (ResolutionInformation (CharacterSequence u v w x y d))
  -> NonEmpty (ResolutionInformation (CharacterSequence u v w x y d))
localResolutionApplication dynFunction meta leftResolutions rightResolutions =
    fmap
      ( generateLocalResolutions
          (const extractNode)
          (const extractNode)
          (const extractNode)
          (const extractNode)
          (const extractNode)
          dynFunction
          meta
      ) resolutionContext
  where

    resolutionContext
      = applySoftwireResolutions
          cleanLeftRes
          (TwoChildren
            (leftResolutions , IS.singleton 0)
            (rightResolutions, IS.singleton 0)
          )

    cleanLeftResMeta =
        ResolutionMetadata
        { totalSubtreeCost       = 0
        , localSequenceCost      = 0
        , subtreeEdgeSet         = mempty
        , leafSetRepresentation  = zeroBits
        , subtreeRepresentation  = singletonNewickSerialization (0 :: Word)
        , topologyRepresentation = mempty
        }

    cleanLeftRes =
        ResInfo
        { resolutionMetadata = cleanLeftResMeta
        , characterSequence  = characterSequence $ NE.head leftResolutions
        }






-- |
-- Assert that two resolutions do not overlap.
resolutionsDoNotOverlap :: ResolutionInformation a -> ResolutionInformation b -> Bool
resolutionsDoNotOverlap x y
  = popCount ((x ^. _leafSetRepresentation) .&. (y ^. _leafSetRepresentation)) == 0


-- |
-- Retrieve only 'ReferenceDAG' from 'PhylogeneticDAG'.
discardCharacters :: PhylogeneticDAG m e n u v w x y z -> ReferenceDAG () e n
discardCharacters (PDAG2 x _) = defaultMetadata $ nodeDecorationDatum2 <$> x


-- |
-- Set 'GraphData' to a default value.
setDefaultMetadata
  :: GraphData m
  -> GraphData (PostorderContextualData t')
{-# INLINE setDefaultMetadata #-}
setDefaultMetadata gd = gd & _graphMetadata .~ defaultMetadataValue


-- |
-- A polymorphic default value for 'PostorderContextualData'.
defaultMetadataValue :: PostorderContextualData t
{-# INLINE defaultMetadataValue #-}
defaultMetadataValue =
  PostorderContextualData
  { virtualNodeMapping    = mempty
  , contextualNodeDatum   = mempty
  , minimalNetworkContext = Nothing
  }

{-
┌───────────────────────────────┐
│                               │
│  Rendering Utility Functions  │
│                               │
└───────────────────────────────┘
-}

-- |
-- Nicely show the DAG information.
renderSummary
  :: ( TextShow n
     , TextShow u
     , TextShow v
     , TextShow w
     , TextShow x
     , TextShow y
     , TextShow z
     , HasBlockCost u v w x y z
     )
  => PhylogeneticDAG m e n u v w x y z
  -> Builder
renderSummary pdag@(PDAG2 dag _) = unlinesB
    [ showb dag
    , showb $ graphData dag
    , renderSequenceSummary pdag
    ]


renderMetadata :: TextShow m => MetadataSequence m -> Builder
renderMetadata = unlinesB . fmap (showb . (^. blockMetadata)) . toList . (^. blockSequence)


-- |
-- Render a "summary" of a sequence consisting of a summary for each block.
renderSequenceSummary
  :: ( TextShow n
     , HasBlockCost u v w x y z
     )
  => PhylogeneticDAG m e n u v w x y z
  -> Builder
renderSequenceSummary pdag@(PDAG2 dag _meta)
  = ("Sequence Summary\n\n" <>)
  . unlinesB
  $ mapWithKey (renderBlockSummary pdag) sequenceContext
  where
    refVec = references dag
    roots  = rootRefs dag

    sequenceWLOG   = getSequence $ NE.head roots
    getSequence    = otoList . characterSequence . NE.head . resolutions . nodeDecoration . (refVec !)
    displayForests =
      f . minimalNetworkContext . graphMetadata . graphData $ dag
        where
          f = (fmap (\(y,r,n,_,_) -> (r,n,y)) <$>)

    sequenceContext =
        case displayForests of
          Nothing  -> (\x -> (Nothing, Nothing, Nothing, x)) <$> sequenceWLOG
          Just ctx -> let (a,b,c) = unzip3 $ toList ctx
                      in  zip4 (Just <$> a) (Just <$> b) (Just <$> c) sequenceWLOG


-- |
-- Render a block's "summary" in a legible manner.
-- Includes:
--
--   * cost incurred from the rooting context
--
--   * cost incurred from the network context
--
--   * cumulative cost of all characters in the block
--
--   * total cost of the block
--
--   * display forest of the block
--
--   * brief summary of each character in the block
--
renderBlockSummary
  :: ( HasBlockCost u v w x y z
     , TextShow n
     )
  => PhylogeneticDAG m e n u v w x y z
  -> Int
  -> (Maybe Double, Maybe Double, Maybe TraversalTopology, CharacterBlock u v w x y z)
  -> Builder
renderBlockSummary (PDAG2 dag meta) key (costOfRooting, costOfNetworking, displayMay, block)
  = fold . (renderedPrefix:) .
    (renderBlockMeta pair :) $
    [ unlinesB . fmap renderStaticCharacterSummary              . toList . uncurry zip . ((^.  continuousBin) *** (^.  continuousBin))
    , unlinesB . fmap renderStaticCharacterWithAlphabetSummary  . toList . uncurry zip . ((^. nonAdditiveBin) *** (^. nonAdditiveBin))
    , unlinesB . fmap renderStaticCharacterWithAlphabetSummary  . toList . uncurry zip . ((^.    additiveBin) *** (^.    additiveBin))
    , unlinesB . fmap renderStaticCharacterWithAlphabetSummary  . toList . uncurry zip . ((^.      metricBin) *** (^.      metricBin))
    , unlinesB . fmap renderStaticCharacterWithAlphabetSummary  . toList . uncurry zip . ((^.   nonMetricBin) *** (^.   nonMetricBin))
    , unlinesB . fmap renderDynamicCharacterSummary             . toList . uncurry zip . ((^.     dynamicBin) *** (^.     dynamicBin))
    ] <*> [(mBlock, block)]
  where
    pair@(mBlock, _) = ((meta ^. blockSequence) ! key, block)

    renderedPrefix = "Block " <> showb key <> "\n\n"

    renderBlockMeta (mValue, bValue) = unlinesB
        [ "  Rooting Cost: " <> maybe "<Unavailable>" showb costOfRooting
        , "  Network Cost: " <> maybe "<Unavailable>" showb costOfNetworking
        , "  Block   Cost: " <> showb bCost
        , "  Total   Cost: " <> showb totalCost
        , "  Display Tree: " <> inferDisplayForest
        , ""
        ]
      where
        bCost     = blockCost mValue bValue
        totalCost = sum'
            [ fromMaybe 0 costOfRooting
            , fromMaybe 0 costOfNetworking
            , bCost
            ]

    renderStaticCharacterSummary (m, c) = unlinesB
        [ "    Name:     " <> showb (m ^. characterName)
        , "    Weight:   " <> showb (m ^. characterWeight)
        , "    Cost:     " <> showb (c ^. characterCost)
        ]

    renderStaticCharacterWithAlphabetSummary (m, c) = unlinesB
        [ "    Name:     " <> showb (m ^. characterName)
        , "    Weight:   " <> showb (m ^. characterWeight)
        , "    Cost:     " <> showb (c ^. characterCost)
        , "    "           <> showb (m ^. characterAlphabet)
        ]

    renderDynamicCharacterSummary (m, c) = unlinesB
        [ "    Name:   " <> showb (m ^. characterName)
        , "    Weight: " <> showb (m ^. characterWeight)
        , "    Cost:   " <> showb (c ^. characterCost)
        , "    Foci:   " <> maybe "<Unavailable>" renderFoci (m ^. traversalFoci)
        ]
      where
        renderFoci (x:|[]) = showb $ fst x
        renderFoci xs      = showb . fmap fst $ toList xs

    inferDisplayForest = maybe "<Unavailable>" renderFunction displayMay

    renderFunction = renderDisplayForestNewick (nodeDecorationDatum2 <$> dag)



-- |
-- Render a display forest to a newick string.
renderDisplayForestNewick :: TextShow n => ReferenceDAG d e n -> TraversalTopology -> Builder
renderDisplayForestNewick dag topo = fromText . T.unlines $ renderDisplayTree <$> toList (rootRefs dag)
  where
    refVec = references dag

    renderDisplayTree :: Int -> T.Text
    renderDisplayTree nodeIdx =
      case kidRefs of
        []    -> renderLeaf nodeIdx $ nodeDecoration nodeVal
        [x]   -> renderDisplayTree x
        x:y:_ -> let x' = renderDisplayTree x
                     y' = renderDisplayTree y
                     (l, r) -- Do this to bias parens right
                       | openParensIn x' > openParensIn y' = (y', x')
                       | otherwise                         = (x', y')
                 in fold ["(", l, ",", r, ")"]
      where
        nodeVal = refVec ! nodeIdx
        kidRefs = filter (\i -> (nodeIdx, i) `isEdgePermissibleWith` topo) . IM.keys $ childRefs nodeVal

        openParensIn = T.length . T.filter (== '(')

    renderLeaf _k = showt
