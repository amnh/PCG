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

{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MonoLocalBinds         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE OverloadedStrings      #-}


module Bio.Graph.PhylogeneticDAG.Internal
  ( EdgeReference
  , PhylogeneticDAG(..)
  , PostorderContextualData(..)
  , PhylogeneticDAG2(..)
  , applySoftwireResolutions
  , generateLocalResolutions
  , getDotContextWithBaseAndIndex
  , localResolutionApplication
  , renderSummary
  , resolutionsDoNotOverlap
  , HasPhylogeneticForest(..)
  , HasMinimalNetworkContext(..)
  , setDefaultMetadata
  ) where


import           Analysis.Parsimony.Internal
import           Bio.Character.Decoration.Shared
import           Bio.Character.Encodable
import           Bio.Graph.LeafSet
import           Bio.Graph.Node
import           Bio.Graph.ReferenceDAG.Internal
import           Bio.Metadata.Continuous
import           Bio.Metadata.Discrete
import           Bio.Metadata.DiscreteWithTCM
import           Bio.Metadata.Dynamic
import           Bio.Sequence
import           Control.Arrow                   ((***))
import           Control.DeepSeq
import           Control.Lens                    as Lens hiding ((<.>))
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
import           Data.TopologyRepresentation
import           Data.Vector                     (Vector)
import           GHC.Generics
import           Prelude                         hiding (zip)
import           Text.Newick.Class
import           Text.XML
import           Type.Reflection                 (Typeable)
import Bio.Graph.PhylogeneticDAG.Render


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
data  PhylogeneticDAG m e n u v w x y z
    = PDAG
    { simpleColumnMetadata     :: MetadataSequence m
    , simplePhylogeneticForest :: ReferenceDAG () e (PhylogeneticNode n (CharacterSequence u v w x y z))
    } deriving (Generic)


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


data PostorderContextualData t = PostorderContextualData
  { virtualNodeMapping    :: HashMap EdgeReference (ResolutionCache t)
  , contextualNodeDatum   :: Vector (HashMap EdgeReference (ResolutionCache t))
  , minimalNetworkContext :: Maybe (NonEmpty (TraversalTopology, Double, Double, Double, Vector (NonEmpty TraversalFocusEdge)))
  }
  deriving
    ( Eq
    , Show
    , Generic
    , Functor
    , Foldable
    , Traversable
    )


-- | (✔)
instance NFData t => NFData (PostorderContextualData t)


-- TODO: RENAME THIS to PhylogeneticForest
-- |
-- The primitive phylogenetic object.
--
-- Contains each taxon exactly once, though the leaves do not need to be connected.
-- The graph object allows for multiple roots and recticulation events.
data  PhylogeneticDAG2 m e n u v w x y z
    = PDAG2
    { phylogeneticForest :: ReferenceDAG
                              (PostorderContextualData (CharacterSequence u v w x y z))
                              e
                              (PhylogeneticNode2 (CharacterSequence u v w x y z) n)
    , columnMetadata     :: MetadataSequence m
    } deriving (Generic, Typeable)


-- |
-- Reference to a edge in the DAG
type EdgeReference = (Int, Int)


-- |
-- A 'Lens' for the 'minimalNetworkContext' field in 'PostorderContextualData'
{-# SPECIALISE  _minimalNetworkContext :: Lens' (PostorderContextualData t) (Maybe (NonEmpty (TraversalTopology, Double, Double, Double, Vector (NonEmpty TraversalFocusEdge)))) #-}
class HasMinimalNetworkContext s a | s -> a where

    _minimalNetworkContext :: Lens' s a


instance HasMinimalNetworkContext (PostorderContextualData t) (Maybe (NonEmpty (TraversalTopology, Double, Double, Double, Vector (NonEmpty TraversalFocusEdge)))) where

    {-# INLINE _minimalNetworkContext #-}
    _minimalNetworkContext = lens minimalNetworkContext (\p m -> p {minimalNetworkContext = m})


-- |
-- A 'Lens' for the 'phyogeneticForest' field in 'PhylogeneticDAG2'
class HasPhylogeneticForest s t a b | s -> a, t -> b, s b -> t, t a -> s where

    _phylogeneticForest :: Lens s t a b


instance HasPhylogeneticForest
           (PhylogeneticDAG2 m e n u v w x y z)
           (PhylogeneticDAG2 m e n u' v' w' x' y' z')
           (ReferenceDAG (PostorderContextualData (CharacterSequence u v w x y z)) e (PhylogeneticNode2 (CharacterSequence u v w x y z) n))
           (ReferenceDAG (PostorderContextualData (CharacterSequence u' v' w' x' y' z')) e (PhylogeneticNode2 (CharacterSequence u' v' w' x' y' z') n)) where

    {-# INLINE _phylogeneticForest #-}
    _phylogeneticForest = lens phylogeneticForest (\p pf -> p {phylogeneticForest = pf})


-- | (✔)
instance HasLeafSet (PhylogeneticDAG2 m e n u v w x y z) (LeafSet (PhylogeneticNode2 (CharacterSequence u v w x y z) n)) where

    leafSet = Lens.to getter
        where
            getter ::
              PhylogeneticDAG2 m e n u v w x y z
              -> LeafSet (PhylogeneticNode2 (CharacterSequence u v w x y z) n)
            getter (PDAG2 e _) =  e ^. leafSet


-- | (✔)
instance ( NFData m
         , NFData e
         , NFData n
         , NFData u
         , NFData v
         , NFData w
         , NFData x
         , NFData y
         , NFData z
         ) => NFData (PhylogeneticDAG2 m e n u v w x y z)


-- | (✔)
instance Show n => PrintDot (PhylogeneticDAG2 m e n u v w x y z) where

    unqtDot       = unqtDot . discardCharacters

    toDot         = toDot . discardCharacters

    unqtListToDot = unqtListToDot . fmap discardCharacters

    listToDot     = listToDot . fmap discardCharacters


-- | (✔)
instance ( Show e
         , Show n
         , Show u
         , Show v
         , Show w
         , Show x
         , Show y
         , Show z
         ) => Show (PhylogeneticDAG m e n u v w x y z) where

    show (PDAG _ dag) = show dag <> "\n" <> foldMapWithKey f dag
      where
        f i (PNode n sek) = mconcat [ "Node {", show i, "}:\n\n", unlines [show n, show sek] ]


-- | (✔)
instance ( HasBlockCost u v w x y z
         , Show m
         , Show e
         , Show n
         , Show u
         , Show v
         , Show w
         , Show x
         , Show y
         , Show z
         ) => Show (PhylogeneticDAG2 m e n u v w x y z) where

    show p@(PDAG2 dag m) = unlines
        [ renderSummary  p
        , renderMetadata m
        , foldMapWithKey f dag
        ]
      where
        f i n = mconcat [ "Node {", show i, "}:\n\n", show n ]

-- | (✔)
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
         ) => TextShow (PhylogeneticDAG2 m e n u v w x y z) where

    showb p@(PDAG2 dag m) = unlinesB
        [ renderSummaryB  p
        , renderMetadataB m
        , foldMapWithKey f dag
        ]
      where
        f i n = fold [ "Node {", showb i, "}:\n\n", showb n ]




-- | (✔)
instance Show n => ToNewick (PhylogeneticDAG2 m e n u v w x y z) where

    toNewick = toNewick . discardCharacters


-- | (✔)
instance ( Show  n
         , Show  u
         , Show  v
         , Show  w
         , Show  y
         , Show  x
         , Show  z
         , ToXML u
         , ToXML v
         , ToXML w
         , ToXML y
         , ToXML z
         ) => ToXML (PhylogeneticDAG2 m e n u v w x y z)  where

    toXML (PDAG2 refDag _) = toXML refDag


-- |
-- Get the dot context of a 'PhylogeneticDAG' with useful internal node decorations.
getDotContextWithBaseAndIndex
  :: Show n
  => Int -- ^ Base over which the Unique
  -> Int
  -> PhylogeneticDAG2 m e n u v w x y z
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
           [ PostBinaryContext (nodeInfo ^. _characterSequence) <$> l <.> r
             | l <- toList ls
             , r <- toList rs
             , resolutionsDoNotOverlap l r
             ]

-- |
-- Given a pre-order transformation for each type parameter, apply the
-- transformations to each possible resolution that is not inconsistent.
generateLocalResolutions
  :: HasBlockCost u' v' w' x' y' z'
  => (ContinuousCharacterMetadataDec                      -> PostorderContext u u' -> u')
  -> (DiscreteCharacterMetadataDec                        -> PostorderContext v v' -> v')
  -> (DiscreteCharacterMetadataDec                        -> PostorderContext w w' -> w')
  -> (DiscreteWithTCMCharacterMetadataDec StaticCharacter -> PostorderContext x x' -> x')
  -> (DiscreteWithTCMCharacterMetadataDec StaticCharacter -> PostorderContext y y' -> y')
  -> (DynamicCharacterMetadataDec (Element DynamicCharacter)   -> PostorderContext z z' -> z')
  ->  MetadataSequence m
  ->  ResolutionInformation
        (PostorderContext
          (CharacterSequence u  v  w  x  y  z)
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
  in
    case resolutionContext ^. _characterSequence of
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
       -- Want to propogate what is stored in the network child resolution
       -- to the parent.

      PostBinaryContext
        { binNode    = parentalCharSequence
        , leftChild  = leftCharSequence
        , rightChild = rightCharSequence
        } ->
        let
          totalChildCost = resolutionContext ^. _totalSubtreeCost
          newCharacterSequence
              = hexZipWithMeta
                  f1Bin
                  f2Bin
                  f3Bin
                  f4Bin
                  f5Bin
                  f6Bin
                  meta
                  parentalCharSequence
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
-- Retrieve only 'ReferenceDAG' from 'PhylogeneticDAG2'.
discardCharacters :: PhylogeneticDAG2 m e n u v w x y z -> ReferenceDAG () e n
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
