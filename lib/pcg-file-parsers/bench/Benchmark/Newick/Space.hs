{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Benchmark.Newick.Space
  ( benchSpace
  , newickFilePath
  , newickInlineSequenceFiles
  ) where

import           Benchmark.Internal    (measureParserSpace)
import           Data.Foldable
--import qualified Data.Text.IO          as T
import qualified Data.Text.Lazy.IO     as TL
import           File.Format.Newick
import           System.FilePath.Posix
import           Text.Megaparsec
import           Weigh


benchSpace :: [Weigh ()]
benchSpace = fold
    [ parserBenchmark ("lazy-text", TL.readFile) <$> newickInlineSequenceFiles
--    , parserBenchmark (     "text",  T.readFile) <$> newickInlineSequenceFiles
    ]


parserBenchmark
  :: ( Stream s
     , Token s ~ Char
     )
  => (String, FilePath -> IO s)
  -> FilePath
  -> Weigh ()
parserBenchmark (prefix, reader) filePath = measureParserSpace prefix filePath reader newickStreamParser


newickInlineSequenceFiles :: [FilePath]
newickInlineSequenceFiles = do
    taxaCount      <- taxaCounts
    let fileName = fold [ show taxaCount, ".tree" ]
    pure $ newickFilePath </> fileName


newickFilePath :: FilePath
newickFilePath =  "bench" </> "data-sets" </>"newick"


taxaCounts :: [Word]
taxaCounts = (8 *) . (2^) <$> [0 .. 5 :: Word]
