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

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- Because I'm sick of dealing with the typechecker.
{-# LANGUAGE UndecidableInstances  #-}

module Bio.Graph.PhylogeneticDAG.Internal
  ( EdgeReference
  , PhylogeneticDAG(..)
  , PhylogeneticDAG2(..)
  , applySoftwireResolutions
  , generateLocalResolutions
  , getDotContextWithBaseAndIndex
  , localResolutionApplication
  , renderSummary
  , resolutionsDoNotOverlap
  ) where
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
import           Bio.Sequence.Block.Character    (CharacterBlock (..))
import           Bio.Sequence.Block.Internal
import           Bio.Sequence.Block.Metadata     (MetadataBlock (..))
import           Bio.Sequence.Metadata           (MetadataSequence, getBlockMetadata)
import qualified Bio.Sequence.Metadata           as M
import           Control.Applicative             (liftA2)
import           Control.Arrow                   ((***))
import           Control.DeepSeq
import           Control.Lens                    as Lens
import           Data.Bits
import           Data.Foldable
import           Data.GraphViz.Printing          hiding ((<>))
import           Data.GraphViz.Types
import           Data.HashMap.Lazy               (HashMap)
import qualified Data.IntMap                     as IM
import           Data.IntSet                     (IntSet)
import qualified Data.IntSet                     as IS
import           Data.Key
import           Data.List                       (zip4)
import           Data.List.NonEmpty              (NonEmpty (..))
import qualified Data.List.NonEmpty              as NE
import           Data.List.Utility
import           Data.Maybe                      (fromMaybe)
import           Data.MonoTraversable
import           Data.Semigroup.Foldable
import           Data.TopologyRepresentation
import           Data.Vector                     (Vector)
import           GHC.Generics
import           Prelude                         hiding (zip)
import           Text.Newick.Class
import           Text.XML


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
-- * z = various (initial, post-order, pre-order) 'Bio.Character.Decoration.Dynamic'    specified as 'DynamicChar'     or 'Bio.Metadata.DiscreteWithTCM'
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
-- * z = various (initial, post-order, pre-order) 'Bio.Character.Decoration.Dynamic'    specified as 'DynamicChar'     or 'Bio.Metadata.DiscreteWithTCM'

-- TODO: RENAME THIS to PhylogeneticForest
data  PhylogeneticDAG2 m e n u v w x y z
    = PDAG2
    { phylogeneticForest :: ReferenceDAG
                              (         HashMap EdgeReference (ResolutionCache (CharacterSequence u v w x y z))
                              , Vector (HashMap EdgeReference (ResolutionCache (CharacterSequence u v w x y z)))
                              , Maybe  (NonEmpty (TraversalTopology, Double, Double, Double, Vector (NonEmpty TraversalFocusEdge)))
                              )
                              e
                              (PhylogeneticNode2 (CharacterSequence u v w x y z) n)
    , columnMetadata     :: MetadataSequence m
    } deriving (Generic)


-- |
-- Reference to a edge in the DAG
type EdgeReference = (Int, Int)


-- | (✔)
instance HasLeafSet
  (PhylogeneticDAG2 m e n u v w x y z)
  (LeafSet (PhylogeneticNode2 (CharacterSequence u v w x y z) n)) where

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
applySoftwireResolutions :: [(ResolutionCache s, IntSet)] -> NonEmpty [ResolutionInformation s]
applySoftwireResolutions inputContexts =
    case inputContexts of
      []    -> pure []
      [x]   -> pure <$> fst x
      x:y:_ -> pairingLogic (x,y)
  where
    multipleParents = not . isSingleton . otoList . snd

    pairingLogic (lhs, rhs) =
        case (multipleParents lhs, multipleParents rhs) of
          (False, False) -> pairedSet
          (False, True ) -> pairedSet <> lhsSet
          (True , False) -> pairedSet <> rhsSet
          (True , True ) -> pairedSet <> lhsSet <> rhsSet
       where
         lhsSet = pure <$> lhs'
         rhsSet = pure <$> rhs'
         lhs'   = fst lhs
         rhs'   = fst rhs
         pairedSet =
             case cartesianProduct lhs' rhs' of
               x:xs -> x:|xs
               []   -> error errorContext -- pure [] -- This shouldn't ever happen
           where
             errorContext = unlines
                 [ "The impossible happened!"
                 , "LHS:"
                 , shownLHS
                 , "RHS:"
                 , shownRHS
                 ]
               where
                 shownLHS = unlines . toList $ show . leafSetRepresentation <$> fst lhs
                 shownRHS = unlines . toList $ show . leafSetRepresentation <$> fst rhs

         cartesianProduct xs ys =
             [ [x,y]
             | x <- toList xs
             , y <- toList ys
             , resolutionsDoNotOverlap x y
             ]


-- |
-- Given a pre-order transformation for each type parameter, apply the
-- transformations to each possible resolution that is not inconsistent.
generateLocalResolutions
  :: HasBlockCost u'' v'' w'' x'' y'' z''
  => (ContinuousCharacterMetadataDec        -> u -> [u'] -> u'')
  -> (DiscreteCharacterMetadataDec          -> v -> [v'] -> v'')
  -> (DiscreteCharacterMetadataDec          -> w -> [w'] -> w'')
  -> (DiscreteWithTCMCharacterMetadataDec StaticCharacter
      -> x -> [x'] -> x'')
  -> (DiscreteWithTCMCharacterMetadataDec StaticCharacter
      -> y -> [y'] -> y'')
  -> (DynamicCharacterMetadataDec (Element DynamicChar)
      -> z -> [z'] -> z'')
  ->  MetadataSequence m
  ->  ResolutionInformation (CharacterSequence u   v   w   x   y   z  )
  -> [ResolutionInformation (CharacterSequence u'  v'  w'  x'  y'  z' )]
  ->  ResolutionInformation (CharacterSequence u'' v'' w'' x'' y'' z'')
generateLocalResolutions f1 f2 f3 f4 f5 f6 meta parentalResolutionContext childResolutionContext =
                ResInfo
                { totalSubtreeCost       = newTotalCost
                , localSequenceCost      = newLocalCost
                , subtreeEdgeSet         = newSubtreeEdgeSet
                , leafSetRepresentation  = newLeafSetRep
                , subtreeRepresentation  = newSubtreeRep
                , topologyRepresentation = newTopologyRep
                , characterSequence      = newCharacterSequence
                }
              where
                newTotalCost = sequenceCost meta newCharacterSequence

                newLocalCost = newTotalCost - sum (totalSubtreeCost <$> childResolutionContext)

                newCharacterSequence = transformation (characterSequence parentalResolutionContext) (characterSequence <$> childResolutionContext)
                newSubtreeEdgeSet    = foldMap subtreeEdgeSet childResolutionContext

                (newLeafSetRep, newSubtreeRep, newTopologyRep) =
                    case childResolutionContext of
                      []   -> (,,) <$>          leafSetRepresentation
                                   <*>          subtreeRepresentation
                                   <*>          topologyRepresentation
                                   $ parentalResolutionContext
                      x:xs -> (,,) <$> foldMap1 leafSetRepresentation
                                   <*> foldMap1 subtreeRepresentation
                                   <*> foldMap1 topologyRepresentation
                                   $ x:|xs

                transformation pSeq cSeqs = hexZipWithMeta f1 f2 f3 f4 f5 f6 meta pSeq transposition
                  where
                    transposition =
                        case cSeqs of
                          x:xs -> hexTranspose $ x:|xs
                          []   -> let c = const []
                                  in hexmap c c c c c c pSeq


-- |
-- Given a transformation for the last type parameter and two resolution caches,
-- apply the transformation to all possible resolution combinations.
localResolutionApplication
  :: HasBlockCost u v w x y d'
  => (DynamicCharacterMetadataDec (Element DynamicChar)
      -> d -> [d] -> d')
  -> MetadataSequence m
  -> NonEmpty (ResolutionInformation (CharacterSequence u v w x y d))
  -> ResolutionCache (CharacterSequence u v w x y d)
  -> NonEmpty (ResolutionInformation (CharacterSequence u v w x y d'))
localResolutionApplication f m x y =
    liftA2 (generateLocalResolutions id3 id3 id3 id3 id3 f m) mutalatedChild relativeChildResolutions
  where
    relativeChildResolutions = applySoftwireResolutions
        [ (x, IS.singleton 0)
        , (y, IS.singleton 0)
        ]
    id3 _ z _ = z
    mutalatedChild = pure
        ResInfo
        { totalSubtreeCost       = 0
        , localSequenceCost      = 0
        , subtreeEdgeSet         = mempty
        , leafSetRepresentation  = zeroBits
        , subtreeRepresentation  = singletonNewickSerialization (0 :: Word)
        , topologyRepresentation = mempty
        , characterSequence      = characterSequence $ NE.head x
        }


-- |
-- Nicely show the DAG information.
renderSummary
  :: ( Show n
     , Show u
     , Show v
     , Show w
     , Show x
     , Show y
     , Show z
     , HasBlockCost u v w x y z
     )
  => PhylogeneticDAG2 m e n u v w x y z
  -> String
renderSummary pdag@(PDAG2 dag _) = unlines
    [ show dag
    , show $ graphData dag
    , renderSequenceSummary pdag
    ]


renderMetadata :: Show m => MetadataSequence m -> String
renderMetadata = unlines . fmap (show . getBlockMetadata) . toList . M.toBlocks


-- |
-- Render a "summary" of a sequence consisting of a summary for each block.
renderSequenceSummary
  :: ( Show n
     , HasBlockCost u v w x y z
     )
  => PhylogeneticDAG2 m e n u v w x y z
  -> String
renderSequenceSummary pdag@(PDAG2 dag _meta) = ("Sequence Summary\n\n" <>) . unlines $ mapWithKey (renderBlockSummary pdag) sequenceContext
  where
    refVec = references dag
    roots  = rootRefs dag

    sequenceWLOG   = getSequence $ NE.head roots
    getSequence    = otoList . characterSequence . NE.head . resolutions . nodeDecoration . (refVec !)
    displayForests = (\(_,_,x) -> fmap (fmap (\(y,r,n,_,_) -> (r,n,y))) x) . graphMetadata $ graphData dag

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
     , Show n
     )
  => PhylogeneticDAG2 m e n u v w x y z
  -> Int
  -> (Maybe Double, Maybe Double, Maybe TraversalTopology, CharacterBlock u v w x y z)
  -> String
renderBlockSummary (PDAG2 dag meta) key (costOfRooting, costOfNetworking, displayMay, block) = mconcat . (renderedPrefix:) .
    (renderBlockMeta pair :) $
    [ unlines . fmap renderStaticCharacterSummary  . toList . uncurry zip . ( continuousBins ***  continuousBins)
    , unlines . fmap renderStaticCharacterSummary  . toList . uncurry zip . (nonAdditiveBins *** nonAdditiveBins)
    , unlines . fmap renderStaticCharacterSummary  . toList . uncurry zip . (   additiveBins ***    additiveBins)
    , unlines . fmap renderStaticCharacterSummary  . toList . uncurry zip . (     metricBins ***      metricBins)
    , unlines . fmap renderStaticCharacterSummary  . toList . uncurry zip . (  nonMetricBins ***   nonMetricBins)
    , unlines . fmap renderDynamicCharacterSummary . toList . uncurry zip . (    dynamicBins ***     dynamicBins)
    ] <*> [(mBlock, cBlock)]
  where
    pair = (M.toBlocks meta ! key, block)
    (MB mBlock, CB cBlock) = pair

    renderedPrefix = "Block " <> show key <> "\n\n"

    renderBlockMeta (mValue, bValue) = unlines
        [ "  Rooting Cost: " <> maybe "<Unavailible>" show costOfRooting
        , "  Network Cost: " <> maybe "<Unavailible>" show costOfNetworking
        , "  Block   Cost: " <> show bCost
        , "  Total   Cost: " <> show totalCost
        , "  Display Tree: " <> inferDisplayForest
        , ""
        ]
      where
        bCost     = blockCost mValue bValue
        totalCost = sum
            [ fromMaybe 0 costOfRooting
            , fromMaybe 0 costOfNetworking
            , bCost
            ]

    renderStaticCharacterSummary (m, c) = unlines
        [ "    Name:   " <> show (m ^. characterName)
        , "    Weight: " <> show (m ^. characterWeight)
        , "    Cost:   " <> show (c ^. characterCost)
        ]

    renderDynamicCharacterSummary (m, c) = unlines
        [ "    Name:   " <> show (m ^. characterName)
        , "    Weight: " <> show (m ^. characterWeight)
        , "    Cost:   " <> show (c ^. characterCost)
        , "    Foci:   " <> maybe "<Unavailible>" renderFoci (m ^. traversalFoci)
        ]
      where
        renderFoci (x:|[]) = show $ fst x
        renderFoci xs      = show . fmap fst $ toList xs

    inferDisplayForest = maybe "<Unavailible>" renderFunction displayMay

    renderFunction = renderDisplayForestNewick (nodeDecorationDatum2 <$> dag)


-- |
-- Render a display forest to a newick string.
renderDisplayForestNewick :: Show n => ReferenceDAG d e n -> TraversalTopology -> String
renderDisplayForestNewick dag topo = unlines $ renderDisplayTree <$> toList (rootRefs dag)
  where
    refVec = references dag

    renderDisplayTree :: Int -> String
    renderDisplayTree nodeIdx =
      case kidRefs of
        []    -> renderLeaf nodeIdx $ nodeDecoration nodeVal
        [x]   -> renderDisplayTree x
        x:y:_ -> let x' = renderDisplayTree x
                     y' = renderDisplayTree y
                     (l, r) -- Do this to bias parens right
                       | openParensIn x' > openParensIn y' = (y', x')
                       | otherwise                         = (x', y')
                 in mconcat ["(", l, ",", r, ")"]
      where
        nodeVal = refVec ! nodeIdx
        kidRefs = filter (\i -> (nodeIdx, i) `isEdgePermissibleWith` topo) . IM.keys $ childRefs nodeVal

        openParensIn = length . filter (== '(')

    renderLeaf _k = show


-- |
-- Assert that two resolutions do not overlap.
resolutionsDoNotOverlap :: ResolutionInformation a -> ResolutionInformation b -> Bool
resolutionsDoNotOverlap x y = popCount (leafSetRepresentation x .&. leafSetRepresentation y) == 0


-- |
-- Retrieve only 'ReferenceDAG' from 'PhylogeneticDAG2'.
discardCharacters :: PhylogeneticDAG2 m e n u v w x y z -> ReferenceDAG () e n
discardCharacters (PDAG2 x _) = defaultMetadata $ nodeDecorationDatum2 <$> x
