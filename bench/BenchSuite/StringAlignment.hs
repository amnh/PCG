module BenchSuite.StringAlignment
  (
  ) where

import Criterion.Main
import Bio.Graph.Constructions
import Control.Monad (join)
import Data.Vector.NonEmpty    (Vector)

benchSuite =
  defaultMain
    [ bench "Score and writeFile" $ nfIO (streamBench1)
    ]


inputDirectories :: [FilePath]
inputDirectories = ("bench/strings/" <>) <$>
    [ "dna"
    , "protein"
    , "slashes"
    , "two-hex"
    , "utter-gibberish"
    ]


buildSequences :: Validation UnificationError CharacterResult
buildSequences = do
    pids <- (readPartialInput <=< gatherInputFiles) <$> inputDirectories
    fromRight <$> unifyPartialInputs pids 


gatherInputFiles :: FilePath -> ValidationT ReadCommandError IO (FileSource, [FileSource])
gatherInputFiles path = liftIO . fmap groupFiles . listdirectory
  where
    groupFiles = first head . patrition isTCM . fromString
    isTCM = (== fromString "tcm") . extractExtension


readPartialInput :: (FileSource, [FileSource]) -> ValidationT ReadCommandError IO [PartialInputData]
readPartialInput (sequencesSources, metricSources) = do
    charMap <- rebuildSequence <$> progressiveParse sequencesSources
    metaPID <- traverse (flip parseAndSetTCM [charMap]) metricSources
    pure $ join metaPID
  where
    rebuildSequence :: PartialInputData -> PartialInputData
    rebuildSequence pid = mo . unzip3 . go . toList . parsedChars $ pid
      where
        mkPair (n,x) (m,y) = (mkMeta $ n <> " X " <> m, x, y)

        go :: [(Identifier, a)] -> [(NormalizedMetadata, a, a)] 
        go    []  = mempty
        go   [x]  = mempty
        go (x:xs) = (mkPair x <$> xs) <> go xs

        mo (ms, xs, ys) = pid
            { parsedChars = Map.fromList [("Left", fold xs), ("Right", fold ys)]
            , parsedMetas = Just $ V.fromList ms
            }

        mkMeta n = NormalizedMetadata
            { alphabet      = fromSymbols ["-"]
            , characterName = n
            , weight        = 1
            , parsedTCM     = Nothing
            , isDynamic     = True
            , isIgnored     = False
            }
