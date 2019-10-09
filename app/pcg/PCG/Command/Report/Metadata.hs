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

module PCG.Command.Report.Metadata
  ( outputMetadata
  ) where

import Bio.Graph
import Bio.Graph.PhylogeneticDAG
import Bio.Metadata
import Bio.Metadata.CharacterName
import Bio.Sequence.Metadata
import Control.Lens.Operators     ((^.))
import Data.FileSource
import Data.Text.Lazy             (Text)
import Data.Text.Short
import Prelude             hiding (filter)
import TextShow
import TextShow.Custom


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


-- |
-- Wrapper function to output a metadata csv as a 'ByteString'
outputMetadata :: DecoratedCharacterResult -> Text
outputMetadata = toLazyText . unlinesB . fmap (intercalateB ",") . (headerRow:) . fmap toFields . characterMetadataOutput
  where
    toFields x =
        [ showb . characterNameRM
        , showb . charsourceFileRM
        , renderCharacterType . characterTypeRM
        , showb . stripCommas . tcmSourceFile
        ] <*> [x]
      
    headerRow =
        [ "Character Name"
        , "Character Source File"
        , "Character Type"
        , "TCM Source File"
        ]

    renderCharacterType = \case
        Continuous  -> "Continuous"
        NonAdditive -> "NonAdditive"
        Additive    -> "Additive"
        Metric      -> "Metric"
        NonMetric   -> "NonMetric"
        Dynamic     -> "Dynamic"

    stripCommas = filter (','/=) . toShortText


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
