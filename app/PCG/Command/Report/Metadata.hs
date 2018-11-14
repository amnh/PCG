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

module PCG.Command.Report.Metadata where

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
outputMetadata :: FilePath -> GraphState -> IO ()
outputMetadata fileName compactGraph = do
  currTime <- getPOSIXTime
  let graphOpt = getCompact compactGraph
  case graphOpt of
    Left  topGraph -> pure ()
    Right decGraph ->
      do
        let xlsxWorkSheet = characterSourcefileOutput decGraph
        BS.writeFile fileName $ fromXlsx currTime xlsxOutput



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
    srcFileNames = nub . fmap fst $ charNameInfo
    charNames    = fmap snd charNameInfo

 -- indexed rows and columns respectively
    indSrcFiles  = zip srcFileNames [2..]
    indCharNames = zip charNames    [2..]

 -- Folding functions
    srcFileFn  (name, rowInd) acc = acc & cellValueAt (rowInd, 1) ?~ CellText name
    charNameFn (name, colInd) acc = acc & cellValueAt (1, colInd) ?~ CellText name
    charSrcFileFn (name, colInd) acc =
      let rowInd = fromJust $ lookup name indSrcFiles in
        acc & cellValueAt (rowInd, colInd) ?~ CellText cellMarker

--                                             ┌ Filepath
--                                             │
--                                             │     ┌ Character Name
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
    filePathAndName = pure . ((pack . sourceFile) &&& (pack . show))

cellMarker :: Text
cellMarker = "✓"
