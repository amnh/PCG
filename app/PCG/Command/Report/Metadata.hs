{-# LANGUAGE OverloadedStrings #-}
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

import           Bio.Graph
import           Bio.Graph.PhylogeneticDAG
import           Bio.Graph.Solution
import           Bio.Metadata
import           Bio.Metadata.CharacterName
import           Bio.Sequence.Metadata
import           Codec.Xlsx
import           Control.Arrow              ((&&&))
import           Control.Lens.Operators     ((?~), (^.))
import qualified Data.ByteString.Lazy       as BS
import           Data.ByteString.Lazy.Char8 (unpack)
import           Data.Compact
import           Data.Foldable
import           Data.Function              ((&))
import           Data.List                  (nub)
import qualified Data.List.NonEmpty         as NE
import           Data.Maybe
import           Data.Monoid                ((<>))
import           Data.Text                  (Text, pack)
import           Data.Time.Clock.POSIX
import           Data.Vector                (Vector)


-- | Wrapper function to output a metadata csv
outputMetadata :: DecoratedCharacterResult -> String
outputMetadata decGraph =
  let
    xlsxWorkSheet = characterSourcefileOutput decGraph
    xlsxOutput    = def & atSheet "CharacterSourceFiles" ?~ xlsxWorkSheet
  in
    unpack $ fromXlsx startOfTime xlsxOutput



characterSourcefileOutput :: DecoratedCharacterResult -> Worksheet
characterSourcefileOutput decCharRes =
    let
      srcFileSheet        = foldr srcFileFn def indSrcFiles
      srcFileAndCharSheet = foldr charNameFn srcFileSheet indCharNames
      markedSpreadSheet   = foldr charSrcFileFn srcFileAndCharSheet indCharNames
    in
      markedSpreadSheet
  where
 -- Extract a generic solution with metadata
    pdag2        = extractSolution decCharRes
    metaSeq      = pdag2 ^. _columnMetadata

 -- Character name information
    charNameInfo = getCharacterNames metaSeq
    srcFileNames = nub . fmap snd $ charNameInfo
    charNames    = fmap fst charNameInfo

 -- indexed rows and columns respectively
    indSrcFiles  = zip srcFileNames [2..]
    indCharNames = zip charNames    [2..]

 -- Folding functions
    srcFileFn  (name, rowInd) acc = acc & cellValueAt (rowInd, 1) ?~ CellText name
    charNameFn (name, colInd) acc = acc & cellValueAt (1, colInd) ?~ CellText name
    charSrcFileFn (name, colInd) acc =
      let
        assocSrcFile = fromJust $ lookup name charNameInfo
      in
        case lookup assocSrcFile indSrcFiles of
          Nothing     -> error . fold $
                           ["Src file of character "
                           ,  show name
                           , " not found in "
                           , show indSrcFiles
                           ]
          Just rowInd -> acc & cellValueAt (rowInd, colInd) ?~ CellText cellMarker


--                                             ┌ Character Name
--                                             │
--                                             │     ┌ Filepath
--                                             │     │
getCharacterNames :: MetadataSequence m -> [(Text, Text)]
getCharacterNames =
    hexFoldMap
      (filePathAndName . (^. characterName))
      (filePathAndName . (^. characterName))
      (filePathAndName . (^. characterName))
      (filePathAndName . (^. characterName))
      (filePathAndName . (^. characterName))
      (filePathAndName . (^. characterName))
  where
    filePathAndName :: CharacterName -> [(Text, Text)]
    filePathAndName = pure . ((pack . show) &&& (pack . sourceFile))

cellMarker :: Text
cellMarker = "x" --"✓"

-- This should give 1970-01-01 00:00 UTC
startOfTime :: POSIXTime
startOfTime = 0
