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

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- Included for ToField instance of FileSource.
-- I didn't want the cassava package dependency for the library that defines FileSource.
{-# OPTIONS_GHC -fno-warn-orphans #-}

module PCG.Command.Report.Metadata
  ( outputMetadata
  ) where

import           Bio.Graph
import           Bio.Graph.PhylogeneticDAG
import           Bio.Metadata
import           Bio.Metadata.CharacterName
import           Bio.Sequence.Metadata
import           Control.Lens.Operators     ((^.))
import qualified Data.ByteString.Lazy       as BS
import           Data.Csv
import           Data.FileSource
import           Data.Text.Short            (toByteString)


data  CharacterReportMetadata
    = CharacterReportMetadata
    { characterNameRM  :: String
    , charsourceFileRM :: FileSource
    , characterTypeRM  :: CharacterType
    , tcmSourceFile    :: FileSource
    }

-- |
-- A label for the different types of characters.
data  CharacterType
    = Continuous
    | NonAdditive
    | Additive
    | Metric
    | NonMetric
    | Dynamic
    deriving Show


instance ToField CharacterType where

    toField = \case
      Continuous  -> "Continuous"
      NonAdditive -> "NonAdditive"
      Additive    -> "Additive"
      Metric      -> "Metric"
      NonMetric   -> "NonMetric"
      Dynamic     -> "Dynamic"


instance ToNamedRecord CharacterReportMetadata where
  toNamedRecord CharacterReportMetadata {..} =
    namedRecord
      [ "Character Name"        .= characterNameRM
      , "Character Source File" .= charsourceFileRM
      , "Character Type"        .= characterTypeRM
      , "TCM Source File"       .= tcmSourceFile
      ]


instance DefaultOrdered CharacterReportMetadata where
  headerOrder _ =
    header
      [ "Character Name"
      , "Character Source File"
      , "Character Type"
      , "TCM Source File"
      ]

instance ToField FileSource where

    toField = toByteString . toShortText

-- |
-- Wrapper function to output a metadata csv as a 'ByteString'
outputMetadata :: DecoratedCharacterResult -> BS.ByteString
outputMetadata = encodeDefaultOrderedByName . characterMetadataOutput

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
    charName :: HasCharacterName s CharacterName => s -> String
    charName = show . (^. characterName)

    charSourceFilePath :: HasCharacterName s CharacterName => s -> FileSource
    charSourceFilePath = sourceFile . (^. characterName)

    tcmSourceFilePath :: HasTcmSourceFile s FileSource => s -> FileSource
    tcmSourceFilePath = (^. _tcmSourceFile)


    f :: (HasCharacterName s CharacterName, HasTcmSourceFile s FileSource)
      => CharacterType
      -> s
      -> CharacterReportMetadata
    f ch = CharacterReportMetadata
        <$> charName
        <*> charSourceFilePath
        <*> const ch
        <*> tcmSourceFilePath

    continuousMeta  = pure . f Continuous
    nonAdditiveMeta = pure . f NonAdditive
    additiveMeta    = pure . f Additive
    metricMeta      = pure . f Metric
    nonMetricMeta   = pure . f NonMetric
    dynamicBin      = pure . f Dynamic
