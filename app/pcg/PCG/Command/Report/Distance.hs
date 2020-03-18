-----------------------------------------------------------------------------
-- |
-- Module      :  PCG.Command.Types.Report.Metadata
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Functionality to output a csv containing all the metadata
--
-----------------------------------------------------------------------------

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

module PCG.Command.Report.Distance
  ( outputDistanceMatrix
  ) where

import           Analysis.Distance
import           Bio.Character
import           Bio.Character.Decoration.Additive
import           Bio.Character.Decoration.Continuous
import           Bio.Character.Decoration.Dynamic
import           Bio.Character.Decoration.Fitch
import           Bio.Character.Decoration.Metric
import           Bio.Graph
import           Bio.Graph.LeafSet
import           Bio.Graph.Node
import           Bio.Sequence
import           Control.Lens
import           Data.Coerce
import           Data.Foldable
import           Data.List                           (intersperse)
import qualified Data.List.NonEmpty                  as NonEmpty
import           Data.Matrix.Unboxed                 (Matrix)
import qualified Data.Matrix.Unboxed                 as Matrix
import           Data.NodeLabel
import           Data.Text.Lazy                      (Text)
import qualified Data.Text.Lazy                      as Text
import qualified Data.Text.Lazy.Builder              as Builder
import qualified Data.Text.Lazy.Builder.RealFloat    as Builder
import           Data.Vector                         (Vector)
import qualified Data.Vector                         as Vector
import           TextShow



outputDistanceMatrix :: DecoratedCharacterResult -> Text
outputDistanceMatrix solution = distanceMatrixCSV leaves meta
  where
    meta = view _columnMetadata . extractSolution $  solution

    leaves = fromLeafSet $ view leafSet solution

-- |
-- This function outputs the distance matrix of each of the leaves
-- in csv format.
distanceMatrixCSV
  :: Vector
       (PhylogeneticNode
       (CharacterSequence
         (ContinuousOptimizationDecoration ContinuousCharacter)
         (FitchOptimizationDecoration StaticCharacter)
         (AdditiveOptimizationDecoration StaticCharacter)
         (SankoffOptimizationDecoration StaticCharacter)
         (SankoffOptimizationDecoration StaticCharacter)
         (DynamicDecorationDirectOptimization DynamicCharacter))
       NodeLabel)
  -> MetadataSequence m
  -> Text
distanceMatrixCSV l meta = Text.unlines $ rowNames : rows

  where
 -- coerce to use a type amenable to the characterDistanceMatrix
    leaves :: Vector (DecoratedCharacterNode Identity)
    leaves = coerce l

 -- We convert the distance matrix to rows and then map each row to text
    rows :: [Text]
    rows =
        fmap toRow
      . Matrix.toLists
      $ distanceMatrix

 -- Convert a list of doubles to a comma-separated piece of text
    toRow :: [Double] -> Text
    toRow =
        toLazyText
      . intercalateM (Builder.singleton ',')
      . fmap Builder.realFloat

 -- From the leaves get a comma-seprated list of row names
    rowNames :: Text
    rowNames =
        Text.intercalate ","
      . Vector.toList
      . fmap (nodeLabelToLazyText . view _nodeDecorationDatum)
      $ leaves

 -- The distance matrix of the character sequences on the leaves
    distanceMatrix :: Matrix Double
    distanceMatrix = characterDistanceMatrix charSeqs meta

 -- Get the character sequences from the leaf nodes.
    charSeqs = fmap (view _characterSequence . NonEmpty.head . view _resolutions) leaves


-- |
-- A function which intercalates a monoid value between each of the entries
-- in a list.
intercalateM :: Monoid m => m -> [m] -> m
intercalateM xs = fold . intersperse xs
