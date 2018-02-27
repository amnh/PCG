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

{-# LANGUAGE DeriveGeneric, FlexibleContexts, FlexibleInstances, MonoLocalBinds, MultiParamTypeClasses, ScopedTypeVariables #-}

-- Because I'm sick of dealing with the typechecker.
{-# LANGUAGE UndecidableInstances #-}

module Bio.Graph.PhylogeneticDAG.Internal where

import           Bio.Character.Decoration.Shared
import           Bio.Graph.LeafSet
import           Bio.Graph.Node
import           Bio.Graph.ReferenceDAG.Internal
import           Bio.Metadata
import           Bio.Metadata.CharacterName
import           Bio.Metadata.Dynamic
import           Bio.Sequence
import           Control.Applicative              (liftA2)
import           Control.DeepSeq
import           Control.Lens
import           Data.Bits
import           Data.Foldable
import           Data.GraphViz.Printing    hiding ((<>)) -- Seriously, why is this redefined?
import           Data.GraphViz.Types
import           Data.HashMap.Lazy                (HashMap)
import           Data.IntSet                      (IntSet)
import qualified Data.IntSet               as IS
import           Data.Key
import           Data.List                        (zip4)
import           Data.List.NonEmpty               (NonEmpty( (:|) ))
import qualified Data.List.NonEmpty        as NE
import           Data.List.Utility
import           Data.Maybe                       (fromMaybe)
import           Data.MonoTraversable
import           Data.Semigroup
import           Data.Semigroup.Foldable
import           Data.Vector                      (Vector)
import           GHC.Generics
import           Text.Newick.Class
import           Text.XML
import           Prelude                   hiding (zip)

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
newtype PhylogeneticDAG e n u v w x y z
     = PDAG (ReferenceDAG () e (PhylogeneticNode n (CharacterSequence u v w x y z)))


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
newtype PhylogeneticDAG2 e n u v w x y z
     = PDAG2 ( ReferenceDAG
                 (         HashMap EdgeReference (ResolutionCache (CharacterSequence u v w x y z))
                 , Vector (HashMap EdgeReference (ResolutionCache (CharacterSequence u v w x y z)))
                 , Maybe  (NonEmpty (TraversalTopology, Double, Double, Vector (NonEmpty TraversalFocusEdge)))
                 )
                 e
                 (PhylogeneticNode2 (CharacterSequence u v w x y z) n)
             )
     deriving (Generic)


-- |
-- Reference to a edge in the DAG
type EdgeReference = (Int, Int)


--instance HasLeafSet (PhylogeneticDAG2 e n u v w x y z) (LeafSet n) where
-- | (✔)
instance HasLeafSet (PhylogeneticDAG2 e n u v w x y z) (LeafSet (PhylogeneticNode2 (CharacterSequence u v w x y z) n)) where

    leafSet = lens getter undefined
        where
--            getter :: (PhylogeneticDAG2 e n u v w x y z) -> (LeafSet n)
            getter (PDAG2 e) =  e ^. leafSet


-- | (✔)
instance (NFData e, NFData n, NFData u, NFData v, NFData w, NFData x, NFData y, NFData z) => NFData (PhylogeneticDAG2 e n u v w x y z)


-- | (✔)
instance Foldable f => PrintDot (PhylogeneticDAG2 e (f String) u v w x y z) where

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
         , HasBlockCost u v w x y z Word Double
         ) => Show (PhylogeneticDAG e n u v w x y z) where

    show (PDAG dag) = show dag <> "\n" <> foldMapWithKey f dag
      where
        f i (PNode n sek) = mconcat [ "Node {", show i, "}:\n\n", unlines [show n, show sek] ]


-- | (✔)
instance ( HasBlockCost u v w x y z Word Double
         , HasCharacterName u CharacterName
         , HasCharacterName v CharacterName
         , HasCharacterName w CharacterName
         , HasCharacterName x CharacterName
         , HasCharacterName y CharacterName
         , HasCharacterName z CharacterName
         , HasTraversalFoci z (Maybe TraversalFoci)
         , Show e
         , Show n
         , Show u
         , Show v
         , Show w
         , Show x
         , Show y
         , Show z
         ) => Show (PhylogeneticDAG2 e n u v w x y z) where

    show p@(PDAG2 dag) = unlines
        [ renderSummary p
        , foldMapWithKey f dag
        ]
      where
--        f i (PNode2 n sek) = mconcat [ "Node {", show i, "}:\n\n", unlines [show n, show sek], "\n\n" ]
        f i n = mconcat [ "Node {", show i, "}:\n\n", show n ]


-- | (✔)
instance Foldable f => ToNewick (PhylogeneticDAG2 e (f String) u v w x y z) where

    toNewick = toNewick . discardCharacters


-- | (✔)
instance ( ToXML u
         , ToXML v
         , ToXML w
         , ToXML y
         , ToXML z
         ) => ToXML (PhylogeneticDAG2 e n u v w x y z)  where

    toXML (PDAG2 refDag) = toXML refDag


-- |
-- Get the dot context of a 'PhylogeneticDAG' with useful internal node decorations.
getDotContextWithBaseAndIndex
  :: Foldable f
  => Int -- ^ Base over which the Unique
  -> Int
  -> PhylogeneticDAG2 e (f String) u v w x y z
  -> ([DotNode GraphID], [DotEdge GraphID])
getDotContextWithBaseAndIndex i j (PDAG2 dag) = getDotContext i j $ nodeDecorationDatum2 <$> dag


-- |
-- Generate all the possible, consistent combinatorial patterns of the subtree.
applySoftwireResolutions :: [(ResolutionCache s, IntSet)] -> NonEmpty [ResolutionInformation s]
applySoftwireResolutions inputContexts =
    case inputContexts of
      []   -> pure []
      [x]  -> pure <$> fst x
{-
          let y = pure <$> fst x
          in  if   multipleParents x
              then y <> pure []
              else y
-}
      x:y:_ -> pairingLogic (x,y)
  where
    multipleParents = not . isSingleton . otoList . snd
{-
    pairingLogic :: ( (ResolutionCache s), IntSet)
                    , (ResolutionCache s), IntSet)
                    )
                 -> NonEmpty [ResolutionInformation s]
-}
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
               x:xs -> {- NE.fromList . ensureNoLeavesWereOmmitted $ -} x:|xs
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
{-
           where
             xMask = foldMap1 leafSetRepresentation xs
             yMask = foldMap1 leafSetRepresentation ys
             overlapMask = xMask .&. yMask
             properOverlapInclusion x y =
               (leafSetRepresentation x .&. overlapMask) `xor` (leafSetRepresentation y .&. overlapMask) == zeroBits
-}


-- |
-- Given a pre-order transformation for each type parameter, apply the
-- transformations to each possible resolution that is not inconsistent.
generateLocalResolutions :: HasBlockCost u'' v'' w'' x'' y'' z'' Word Double
                         => (u -> [u'] -> u'')
                         -> (v -> [v'] -> v'')
                         -> (w -> [w'] -> w'')
                         -> (x -> [x'] -> x'')
                         -> (y -> [y'] -> y'')
                         -> (z -> [z'] -> z'')
                         ->  ResolutionInformation (CharacterSequence u   v   w   x   y   z  )
                         -> [ResolutionInformation (CharacterSequence u'  v'  w'  x'  y'  z' )]
                         ->  ResolutionInformation (CharacterSequence u'' v'' w'' x'' y'' z'')
generateLocalResolutions f1 f2 f3 f4 f5 f6 parentalResolutionContext childResolutionContext =
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
                newTotalCost = sequenceCost newCharacterSequence

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

                transformation pSeq cSeqs = hexZipWith f1 f2 f3 f4 f5 f6 pSeq transposition
                  where
                    transposition =
                        case cSeqs of
                          x:xs -> hexTranspose $ x:|xs
                          []   -> let c = const []
                                  in hexmap c c c c c c pSeq


-- |
-- Given a transformation for the last type parameter, and two resolution caches,
-- apply the transformation to all possible resolution combinations.
localResolutionApplication
  :: HasBlockCost u v w x y d' Word Double
  => (d -> [d] -> d')
  -> NonEmpty (ResolutionInformation (CharacterSequence u v w x y d))
  -> ResolutionCache (CharacterSequence u v w x y d)
  -> NonEmpty (ResolutionInformation (CharacterSequence u v w x y d'))
localResolutionApplication f x y =
    liftA2 (generateLocalResolutions id2 id2 id2 id2 id2 f) mutalatedChild relativeChildResolutions
  where
    relativeChildResolutions = applySoftwireResolutions
        [ (x, IS.singleton 0)
        , (y, IS.singleton 0)
        ]
    id2 z _ = z
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
-- Given a foldable structure, generate a list of all possible pairs in the
-- structure. Does not check for uniqueness of elements.
pairs :: Foldable f => f a -> [(a, a)]
pairs = f . toList
  where
    f    []  = []
    f   [_]  = []
    f (x:xs) = ((\y -> (x, y)) <$> xs) <> f xs


-- |
-- Nicely show the DAG information.
renderSummary
  :: ( HasBlockCost u v w x y z Word Double
     , HasCharacterName u CharacterName
     , HasCharacterName v CharacterName
     , HasCharacterName w CharacterName
     , HasCharacterName x CharacterName
     , HasCharacterName y CharacterName
     , HasCharacterName z CharacterName
     , HasTraversalFoci z (Maybe TraversalFoci)
     )
  => PhylogeneticDAG2 e n u v w x y z
  -> String
renderSummary pdag@(PDAG2 dag) = unlines
    [ show dag
    , show $ graphData dag
    , renderSequenceSummary pdag
    ]


-- |
-- Render a "summary" of a sequence consisting of a summary for each block
renderSequenceSummary
  :: ( HasBlockCost u v w x y z Word Double
     , HasCharacterName u CharacterName
     , HasCharacterName v CharacterName
     , HasCharacterName w CharacterName
     , HasCharacterName x CharacterName
     , HasCharacterName y CharacterName
     , HasCharacterName z CharacterName
     , HasTraversalFoci z (Maybe TraversalFoci)
     )
  => PhylogeneticDAG2 e n u v w x y z
  -> String
renderSequenceSummary pdag@(PDAG2 dag) = ("Sequence Summary\n\n" <>) . unlines $ mapWithKey (renderBlockSummary pdag) sequenceContext
  where
    refVec = references dag
    roots  = rootRefs dag
    
    sequenceWLOG   = getSequence $ NE.head roots
    getSequence    = otoList . characterSequence . NE.head . resolutions . nodeDecoration . (refVec !)
    displayForests = (\(_,_,x) -> fmap (fmap (\(y,r,n,_) -> (r,n,y))) x) . graphMetadata $ graphData dag

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
  :: ( HasBlockCost u v w x y z Word Double
     , HasCharacterName u CharacterName
     , HasCharacterName v CharacterName
     , HasCharacterName w CharacterName
     , HasCharacterName x CharacterName
     , HasCharacterName y CharacterName
     , HasCharacterName z CharacterName
     , HasTraversalFoci z (Maybe TraversalFoci)
     )
  => PhylogeneticDAG2 e n u v w x y z
  -> Int
  -> (Maybe Double, Maybe Double, Maybe TraversalTopology, CharacterBlock u v w x y z)
  -> String
renderBlockSummary (PDAG2 dag) key (rootingCost, networkingCost, displayMay, block) = mconcat . (renderedPrefix:) $
    [ renderBlockMeta
    , unlines . fmap renderStaticCharacterSummary  . toList . continuousCharacterBins
    , unlines . fmap renderStaticCharacterSummary  . toList . nonAdditiveCharacterBins
    , unlines . fmap renderStaticCharacterSummary  . toList . additiveCharacterBins
    , unlines . fmap renderStaticCharacterSummary  . toList . metricCharacterBins
    , unlines . fmap renderStaticCharacterSummary  . toList . nonMetricCharacterBins
    , unlines . fmap renderDynamicCharacterSummary . toList . dynamicCharacters
    ] <*> [block]
  where
    renderedPrefix = "Block " <> show key <> "\n\n"

    renderBlockMeta bValue = unlines
        [ "  Rooting Cost: " <> maybe "<Unavailible>" show rootingCost
        , "  Network Cost: " <> maybe "<Unavailible>" show networkingCost
        , "  Block   Cost: " <> show (blockCost bValue)
        , "  Total   Cost: " <> show totalCost
        , "  Display Tree: " <> inferDisplayForest bValue
        , ""
        ]
      where
        totalCost = sum
          [ fromMaybe 0 rootingCost
          , fromMaybe 0 networkingCost
          , blockCost bValue
          ]
        
    renderStaticCharacterSummary sc = unlines
        [ "    Name:   " <> show (sc ^. characterName)
        , "    Weight: " <> show (sc ^. characterWeight)
        , "    Cost:   " <> show (sc ^. characterCost)
        ]

    renderDynamicCharacterSummary dc = unlines
        [ "    Name:   " <> show (dc ^. characterName)
        , "    Weight: " <> show (dc ^. characterWeight)
        , "    Cost:   " <> show (dc ^. characterCost)
        , "    Foci:   " <> (maybe "<Unavailible>" renderFoci $ dc ^. traversalFoci)
        ]
      where
        renderFoci (x:|[]) = show $ fst x
        renderFoci xs      = show . fmap fst $ toList xs

    inferDisplayForest =
        case displayMay of
          Nothing -> const "<Unavailible>"
          Just df -> const "<Not yet rendered>"

  
-- |
-- Assert that two resolutions do not overlap.
resolutionsDoNotOverlap :: ResolutionInformation a -> ResolutionInformation b -> Bool
resolutionsDoNotOverlap x y = popCount (leafSetRepresentation x .&. leafSetRepresentation y) == 0


-- |
-- Retrieve only 'ReferenceDAG' from 'PhylogeneticDAG2'.
discardCharacters :: PhylogeneticDAG2 e n u v w x y z -> ReferenceDAG () e n
discardCharacters (PDAG2 x) = defaultMetadata $ nodeDecorationDatum2 <$> x


