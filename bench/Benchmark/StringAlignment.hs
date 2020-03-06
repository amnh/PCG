{-# LANGUAGE OverloadedStrings #-}

module Benchmark.StringAlignment
  ( benchStringAlignment
  ) where

import           Analysis.Parsimony.Dynamic.DirectOptimization
import           Bio.Character
import           Bio.Character.Decoration.Dynamic              hiding (characterName)
import           Bio.Graph
import           Bio.Graph.Node
import           Bio.Graph.ReferenceDAG
import           Bio.Metadata.Dynamic
import           Bio.Sequence
import           Control.DeepSeq
import           Control.Lens                                  ((^.))
import           Control.Monad                                 (guard, join, (<=<))
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Validation
import           Control.Parallel.Custom
import           Control.Parallel.Strategies
import           Criterion.Main
import           Criterion.Types                               (Config (..))
import           Data.Alphabet
import           Data.Bifunctor
import           Data.Char
import           Data.EdgeLength
import           Data.Either
import           Data.FileSource
import           Data.Foldable
import           Data.Functor                                  (($>))
import           Data.Key
import           Data.List                                     (partition, sortOn, unzip4)
import           Data.List.NonEmpty                            (NonEmpty, nonEmpty)
import qualified Data.List.NonEmpty                            as NE
import qualified Data.Map                                      as Map
import           Data.Maybe
import           Data.MonoTraversable
import           Data.NodeLabel
import           Data.Normalization.Character
import           Data.Normalization.Metadata                   hiding (characterName)
import           Data.Semigroup.Foldable
import           Data.String
import           Data.Text.Short                               (toString)
import           Data.Unification
import           Data.Validation
import qualified Data.Vector                                   as V
import           Data.Vector.NonEmpty                          (Vector)
import qualified Data.Vector.NonEmpty                          as V
import           PCG.Command.Read.ParseStreams
import           PCG.Command.Read.ReadCommandError
import           Prelude                                       hiding (zip, zipWith)
import           System.Directory
import           System.FilePath


benchStringAlignment :: ((DynamicCharacterMetadataDec AmbiguityGroup, DynamicCharacter, DynamicCharacter) -> Bool) -> IO ()
benchStringAlignment f = do
    points <- force <$> gatherBenchmarkData f
    guard . not $ null points
    defaultMainWith cfg [ bgroup "main" $ parmap rpar measureStringAlignment points ]
  where
    cfg = defaultConfig { csvFile = Just "bench-string-alignment.csv" }


inputDirectories :: NonEmpty FilePath
inputDirectories = ("bench/strings/" <>) <$> NE.fromList
    [ "dna"
    , "protein"
    , "slashes"
    , "two-hex"
    , "unfamiliar"
    ]


measureStringAlignment
  :: (DynamicCharacterMetadataDec AmbiguityGroup, DynamicCharacter, DynamicCharacter)
  -> Benchmark
measureStringAlignment (metadata, lhs, rhs) =
    bench label $ nf (uncurry metric) (lhs, rhs)
  where
    label  = otoList $ metadata ^. characterName
    metric = selectDynamicMetric metadata


gatherBenchmarkData
  :: ((DynamicCharacterMetadataDec AmbiguityGroup, DynamicCharacter, DynamicCharacter) -> Bool)
  -> IO [(DynamicCharacterMetadataDec AmbiguityGroup, DynamicCharacter, DynamicCharacter)]
gatherBenchmarkData f = do
    res <- buildSequences
    case res of
      Left  err  -> putStrLn err $> mempty
      Right seqs ->
        case filter f $ toList seqs of
          [] -> putStrLn "No benchmarking points after applying filtering predicate" $> mempty
          xs -> pure xs


buildSequences :: IO (Either String (Vector (DynamicCharacterMetadataDec AmbiguityGroup, DynamicCharacter, DynamicCharacter)))
buildSequences = do
    inputResult <- liftIO . runValidationT $
                       traverse (readPartialInput <=< liftIO . gatherInputFiles) inputDirectories
    pure $ case inputResult of
             Failure pErr -> Left $ show pErr
             Success pids -> case unifyPartialInputs $ join pids <> pure tree of
                               Failure uErr -> Left $ show uErr
                               Success cDAG -> bimap (const "No characters read") extractVector cDAG
  where
    tree =
        PID
        { parsedChars   = mempty
        , parsedMetas   = Nothing
        , parsedForests = tVal
        , relatedTcm    = Nothing
        , sourceFile    = "cherry.tree"
        }
      where
        tVal = Just . pure . PhylogeneticForest . pure $ unfoldDAG f 0

        f :: Word -> ([(EdgeLength, Word)], Maybe NodeLabel, [(EdgeLength, Word)])
        f 0 = ([], Nothing, [(mempty, 1), (mempty, 2)])
        f 1 = ([(mempty, 0)], Just "Left" , [])
        f _ = ([(mempty, 0)], Just "Right", [])


extractVector :: CharacterResult -> Vector (DynamicCharacterMetadataDec AmbiguityGroup, DynamicCharacter, DynamicCharacter)
extractVector s =
    let pdag    = extractSolution s
        metaSeq = columnMetadata pdag
        metaVec = ofoldMap (^. dynamicBin) metaSeq
        nodeVec = toList $ phylogeneticForest pdag
        lhs     = getNodeVec $ nodeVec ! 1
        rhs     = getNodeVec $ nodeVec ! 2
        getNodeVec = fmap ((^. encoded) . fromJust) . ofoldMap (^. dynamicBin) . characterSequence . NE.head . resolutions
    in  V.fromNonEmpty . NE.fromList . toList $ V.zip3 metaVec lhs rhs


gatherInputFiles :: FilePath -> IO (FileSource, NonEmpty FileSource)
gatherInputFiles path = groupFiles . fmap (path </>) <$> listDirectory path
  where
    groupFiles :: [FilePath] -> (FileSource, NonEmpty FileSource)
    groupFiles = bimap head organizeTCMs . partition isTCM . fmap fromString

    isTCM = (/= Just "tcm") . extractExtension

    -- Sort tcm files, case-insensitively
    organizeTCMs = NE.fromList . sortOn (omap toLower)


readPartialInput :: (FileSource, NonEmpty FileSource) -> ValidationT ReadCommandError IO (NonEmpty PartialInputData)
readPartialInput (sequencesSources, metricSources) = do
    charMap <- rebuildSequence <$> progressiveParse sequencesSources
    metaPID <- traverse (assignTCM charMap) metricSources
    pure $ join metaPID
  where

    assignTCM charMap tcmPath = do
        xs <- parseAndSetTCM tcmPath $ pure charMap
        pure $ prependName <$> xs
      where
        prependName pid = pid { parsedMetas = fmap g <$> parsedMetas pid }
        metricPrefix = fromString . takeBaseName . toString $ toShortText tcmPath
        g (NormalizedMetadata a b c d e f) =
            NormalizedMetadata a (metricPrefix <> ": " <> b) c d e f

    rebuildSequence :: PartialInputData -> PartialInputData
    rebuildSequence pid = mo . unzip4 . go . sortStrings . Map.assocs . parsedChars $ pid
      where
        mkPair (n,x) (m,y) = (n, m, x, y)

        sortStrings = sortOn (getNumberChunk . fst)

        getNumberChunk :: Identifier -> Word
        getNumberChunk = read . takeWhile isDigit . dropWhile (not . isDigit) . toString

        -- Create the n(n+1)/2 unique pairs of strings
        go :: [(Identifier, a)] -> [(Identifier, Identifier, a, a)]
        go    []  = mempty
        go   [_]  = mempty
        go (x:xs) = (mkPair x <$> xs) <> go xs

        mo :: ([Identifier], [Identifier], [NormalizedCharacterCollection], [NormalizedCharacterCollection])
           -> PartialInputData
        mo (ms, ns, xs, ys) = pid
            { parsedChars = Map.fromList [("Left", fold1 $ NE.fromList xs), ("Right", fold1 $ NE.fromList ys)]
            , parsedMetas = fmap V.fromNonEmpty . nonEmpty $ zipWith mkMeta ms ns
            }

        mkMeta m n =
            NormalizedMetadata
              (fromSymbols ["-"])
              (m <> " X " <> n)
              1 Nothing True False
