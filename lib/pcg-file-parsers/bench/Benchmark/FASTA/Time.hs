module Benchmark.FASTA.Time
  ( main
  , fastaFilePath
  , fastaInlineSequenceFiles
  ) where

import           Benchmark.Internal    (measureParserTime)
import           Criterion.Main
import           Data.Foldable
import qualified Data.Text.IO          as T
import qualified Data.Text.Lazy.IO     as TL
import           File.Format.Fasta
import           System.FilePath.Posix


main :: IO ()
main = defaultMain $ 
    fmap parserBenchmark  fastaInlineSequenceFiles <>
    fmap parserBenchmark' fastaInlineSequenceFiles


parserBenchmark :: FilePath -> Benchmark
parserBenchmark filePath = measureParserTime "text" filePath T.readFile fastaStreamParser


parserBenchmark' :: FilePath -> Benchmark
parserBenchmark' filePath = measureParserTime "lazy-text" filePath TL.readFile fastaStreamParser


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
