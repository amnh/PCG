module Benchmark.FASTA.Space
  ( main
  , fastaFilePath
  , fastaInlineSequenceFiles
  ) where

import           Benchmark.Internal    (measureParserSpace)
import           Data.Foldable
import qualified Data.Text.IO          as T
import qualified Data.Text.Lazy.IO     as TL
import           File.Format.Fasta
import           System.FilePath.Posix
import           Weigh


main :: IO ()
main = mainWith $ do
    setColumns [Case, Allocated, GCs, Max]
    traverse_ parserBenchmark  fastaInlineSequenceFiles
    traverse_ parserBenchmark' fastaInlineSequenceFiles


parserBenchmark :: FilePath -> Weigh ()
parserBenchmark filePath = measureParserSpace "text" filePath T.readFile fastaStreamParser


parserBenchmark' :: FilePath -> Weigh ()
parserBenchmark' filePath = measureParserSpace "lazy-text" filePath TL.readFile fastaStreamParser


fastaInlineSequenceFiles :: [FilePath]
fastaInlineSequenceFiles = do
    taxaCount      <- taxaCounts
    sequenceLength <- sequenceLengths
    let fileName = fold [ show taxaCount, "x", show sequenceLength, ".fasta" ]
    pure $ fastaFilePath </> fileName


fastaFilePath :: FilePath
fastaFilePath =  "bench" </> "data-sets" </>"fasta"


taxaCounts :: [Word]
taxaCounts = (8 *) . (2^) <$> [0 .. 5 :: Word]


sequenceLengths :: [Word]
sequenceLengths = (8 *) . (2^) <$> [0 .. 9 :: Word]
