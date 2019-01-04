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

import           Bio.Character              (CharacterType (..))
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

data CharacterMetadata =
  CharacterMetadata
  { characterNameM :: String
  , sourceFileM    :: FilePath
  , characterTypeM :: CharacterType
  }

instance ToNamedRecord CharacterMetadata where
  toNamedRecord CharacterMetadata {..} =
    namedRecord
      [ "Character Name" .= characterNameM
      , "Source File"    .= sourceFileM
      , "Character Type" .= characterTypeM
      ]

instance DefaultOrdered CharacterMetadata where
  headerOrder _ = header ["Character Name", "Source File", "Character Type"]




-- | Wrapper function to output a metadata csv
outputMetadata :: DecoratedCharacterResult -> BS.ByteString
outputMetadata =
  encodeDefaultOrderedByName . characterMetadataOutput


characterMetadataOutput :: DecoratedCharacterResult -> [CharacterMetadata]
characterMetadataOutput decCharRes = getCharacterMetadata metaSeq
  where
 -- Extract a generic solution and its metadata sequence
    pdag2        = extractSolution decCharRes
    metaSeq      = pdag2 ^. _columnMetadata



getCharacterMetadata :: MetadataSequence m -> [CharacterMetadata]
getCharacterMetadata =
    hexFoldMap
      continuousMeta
      nonAdditiveMeta
      additiveMeta
      metricMeta
      nonMetricMeta
      dynamicBin

  where
    filePath :: HasCharacterName s CharacterName =>  s -> String
    filePath = show . (^. characterName)

    sourceFilePath :: HasCharacterName s CharacterName =>  s -> FilePath
    sourceFilePath = sourceFile . (^. characterName)

    f :: HasCharacterName s CharacterName => CharacterType -> s -> CharacterMetadata
    f ch = CharacterMetadata
        <$> filePath
        <*> sourceFilePath
        <*> const ch

    continuousMeta  = pure . f Continuous
    nonAdditiveMeta = pure . f NonAdditive
    additiveMeta    = pure . f Additive
    metricMeta      = pure . f Metric
    nonMetricMeta   = pure . f NonMetric
    dynamicBin      = pure . f Dynamic



