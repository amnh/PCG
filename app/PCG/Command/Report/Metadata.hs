{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

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

module PCG.Command.Report.Metadata
  ( outputMetadata
  )
  where

import           Bio.Character.Type         (CharacterType (..))
import           Bio.Graph
import           Bio.Graph.PhylogeneticDAG
import           Bio.Graph.Solution
import           Bio.Metadata
import           Bio.Metadata.CharacterName
import           Bio.Sequence.Metadata
import           Control.Arrow              ((&&&))
import           Control.Lens.Operators     ((?~), (^.))
import qualified Data.ByteString.Lazy       as BS
import           Data.ByteString.Lazy.Char8 (unpack)
import           Data.Compact
import           Data.Csv
import           Data.Foldable
import           Data.Function              ((&))
import           Data.List                  (nub)
import qualified Data.List.NonEmpty         as NE
import           Data.Maybe
import           Data.Monoid                ((<>))
import           Data.Text                  (Text, pack)
import           Data.Vector                (Vector)


data CharacterReportMetadata =
  CharacterReportMetadata
  { characterNameRM :: String
  , sourceFileRM    :: FilePath
  , characterTypeRM :: CharacterType
  }

instance ToNamedRecord CharacterReportMetadata where
  toNamedRecord CharacterReportMetadata {..} =
    namedRecord
      [ "Character Name" .= characterNameRM
      , "Source File"    .= sourceFileRM
      , "Character Type" .= characterTypeRM
      ]

instance DefaultOrdered CharacterReportMetadata where
  headerOrder _ = header ["Character Name", "Source File", "Character Type"]




-- | Wrapper function to output a metadata csv
outputMetadata :: DecoratedCharacterResult -> BS.ByteString
outputMetadata =
  encodeDefaultOrderedByName . characterMetadataOutput


characterMetadataOutput :: DecoratedCharacterResult -> [CharacterReportMetadata]
characterMetadataOutput decCharRes = getCharacterReportMetadata metaSeq
  where
 -- Extract a generic solution and its metadata sequence
    pdag2        = extractSolution decCharRes
    metaSeq      = pdag2 ^. _columnMetadata



getCharacterReportMetadata :: MetadataSequence m -> [CharacterReportMetadata]
getCharacterReportMetadata =
    hexFoldMap
      continuousMeta
      nonAdditiveMeta
      additiveMeta
      metricMeta
      nonMetricMeta
      dynamicBin

  where
    charName :: HasCharacterName s CharacterName =>  s -> String
    charName = show . (^. characterName)

    sourceFilePath :: HasCharacterName s CharacterName =>  s -> FilePath
    sourceFilePath = sourceFile . (^. characterName)

    f :: HasCharacterName s CharacterName => CharacterType -> s -> CharacterReportMetadata
    f ch = CharacterReportMetadata
        <$> charName
        <*> sourceFilePath
        <*> const ch

    continuousMeta  = pure . f Continuous
    nonAdditiveMeta = pure . f NonAdditive
    additiveMeta    = pure . f Additive
    metricMeta      = pure . f Metric
    nonMetricMeta   = pure . f NonMetric
    dynamicBin      = pure . f Dynamic



