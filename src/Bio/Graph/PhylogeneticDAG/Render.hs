------------------------------------------------------------------------------
-- |
-- Module      :  Bio.Graph.PhylogeneticDAG.Render
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Module for functions related to rendering a PhylogeneticDAG
--
-----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}


module Bio.Graph.PhylogeneticDAG.Render
  ( renderSummary
  , renderSummaryB
  , renderMetadata
  , renderMetadataB
  , renderSequenceSummary
  , renderSequenceSummaryB
  , renderBlockSummary
  , renderBlockSummaryB
  )
  where
import {-# SOURCE #-} Bio.Graph.PhylogeneticDAG.Internal
import Control.Arrow ((***))
import Data.TopologyRepresentation
import           Bio.Sequence
import Data.Foldable
import Data.Foldable.Custom
import           Data.List.NonEmpty              (NonEmpty (..))
import qualified Data.List.NonEmpty              as NE
import Bio.Metadata.Dynamic
import Bio.Graph.ReferenceDAG.Internal
import qualified Data.IntMap                     as IM
import Data.Key
import Control.Lens.Operators ((^.))
import Prelude hiding (zip)
import Bio.Graph.Node
import Bio.Character.Decoration.Metric
import Data.Maybe (fromMaybe)
import Data.List (zip4)
import Data.MonoTraversable
import qualified Data.Text as T (Text, unlines, filter, length)
import TextShow (TextShow (showb, showt), fromText, Builder, unlinesB)


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

-- |
-- Nicely show the DAG information as a 'Builder'
renderSummaryB
  :: ( TextShow n
     , TextShow u
     , TextShow v
     , TextShow w
     , TextShow x
     , TextShow y
     , TextShow z
     , HasBlockCost u v w x y z
     )
  => PhylogeneticDAG2 m e n u v w x y z
  -> Builder
renderSummaryB pdag@(PDAG2 dag _) = unlinesB
    [ showb dag
    , showb $ graphData dag
    , renderSequenceSummaryB pdag
    ]


renderMetadata :: Show m => MetadataSequence m -> String
renderMetadata = unlines . fmap (show . (^. blockMetadata)) . toList . (^. blockSequence)


renderMetadataB :: TextShow m => MetadataSequence m -> Builder
renderMetadataB = unlinesB . fmap (showb . (^. blockMetadata)) . toList . (^. blockSequence)




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
-- Render a "summary" of a sequence consisting of a summary for each block as a 'Builder' type.
renderSequenceSummaryB
  :: ( TextShow n
     , HasBlockCost u v w x y z
     )
  => PhylogeneticDAG2 m e n u v w x y z
  -> Builder
renderSequenceSummaryB pdag@(PDAG2 dag _meta)
  = ("Sequence Summary\n\n" <>)
  . unlinesB
  $ mapWithKey (renderBlockSummaryB pdag) sequenceContext
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
     , Show n
     )
  => PhylogeneticDAG2 m e n u v w x y z
  -> Int
  -> (Maybe Double, Maybe Double, Maybe TraversalTopology, CharacterBlock u v w x y z)
  -> String
renderBlockSummary (PDAG2 dag meta) key (costOfRooting, costOfNetworking, displayMay, block) = mconcat . (renderedPrefix:) .
    (renderBlockMeta pair :) $
    [ unlines . fmap renderStaticCharacterSummary              . toList . uncurry zip . ((^.  continuousBin) *** (^.  continuousBin))
    , unlines . fmap renderStaticCharacterWithAlphabetSummary  . toList . uncurry zip . ((^. nonAdditiveBin) *** (^. nonAdditiveBin))
    , unlines . fmap renderStaticCharacterWithAlphabetSummary  . toList . uncurry zip . ((^.    additiveBin) *** (^.    additiveBin))
    , unlines . fmap renderStaticCharacterWithAlphabetSummary  . toList . uncurry zip . ((^.      metricBin) *** (^.      metricBin))
    , unlines . fmap renderStaticCharacterWithAlphabetSummary  . toList . uncurry zip . ((^.   nonMetricBin) *** (^.   nonMetricBin))
    , unlines . fmap renderDynamicCharacterSummary             . toList . uncurry zip . ((^.     dynamicBin) *** (^.     dynamicBin))
    ] <*> [(mBlock, block)]
  where
    pair@(mBlock, _) = ((meta ^. blockSequence) ! key, block)

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
        totalCost = sum'
            [ fromMaybe 0 costOfRooting
            , fromMaybe 0 costOfNetworking
            , bCost
            ]

    renderStaticCharacterSummary (m, c) = unlines
        [ "    Name:     " <> show (m ^. characterName)
        , "    Weight:   " <> show (m ^. characterWeight)
        , "    Cost:     " <> show (c ^. characterCost)
        ]

    renderStaticCharacterWithAlphabetSummary (m, c) = unlines
        [ "    Name:     " <> show (m ^. characterName)
        , "    Weight:   " <> show (m ^. characterWeight)
        , "    Cost:     " <> show (c ^. characterCost)
        , "    "           <> show (m ^. characterAlphabet)
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
-- Same as renderBlockSummary but for a render the summary as a 'Builder'
renderBlockSummaryB
  :: ( HasBlockCost u v w x y z
     , TextShow n
     )
  => PhylogeneticDAG2 m e n u v w x y z
  -> Int
  -> (Maybe Double, Maybe Double, Maybe TraversalTopology, CharacterBlock u v w x y z)
  -> Builder
renderBlockSummaryB (PDAG2 dag meta) key (costOfRooting, costOfNetworking, displayMay, block)
  = mconcat . (renderedPrefix:) .
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
        [ "  Rooting Cost: " <> maybe "<Unavailible>" showb costOfRooting
        , "  Network Cost: " <> maybe "<Unavailible>" showb costOfNetworking
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
        , "    Foci:   " <> maybe "<Unavailible>" renderFoci (m ^. traversalFoci)
        ]
      where
        renderFoci (x:|[]) = showb $ fst x
        renderFoci xs      = showb . fmap fst $ toList xs

    inferDisplayForest = maybe "<Unavailible>" renderFunction displayMay

    renderFunction = renderDisplayForestNewickB (nodeDecorationDatum2 <$> dag)





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
-- Render a display forest to a newick string.
renderDisplayForestNewickB :: TextShow n => ReferenceDAG d e n -> TraversalTopology -> Builder
renderDisplayForestNewickB dag topo = fromText . T.unlines $ renderDisplayTree <$> toList (rootRefs dag)
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
                 in mconcat ["(", l, ",", r, ")"]
      where
        nodeVal = refVec ! nodeIdx
        kidRefs = filter (\i -> (nodeIdx, i) `isEdgePermissibleWith` topo) . IM.keys $ childRefs nodeVal

        openParensIn = T.length . T.filter (== '(')

    renderLeaf _k = showt
