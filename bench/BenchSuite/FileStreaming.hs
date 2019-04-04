module BenchSuite.FileStreaming
  (
  ) where

import Criterion.Main
import Bio.Graph.Constructions
import Control.Monad (join)
import System.FilePath.Posix ((</>), takeExtension)


benchSuite =
  defaultMain
    [ bench "Score and writeFile" $ nfIO (streamBench1)
    ]

filePaths :: [FilePath]
filePaths = []

streamBench1 = streamingAction (putStrLn "TO DO")

streamingAction :: (GraphState -> IO ()) -> IO ()
streamingAction streamFunction = do
  evalGraph <- runEvaluation . unifyBenchFiles $ benchDirectory
  join $ evaluation (pure ()) (const (pure ())) streamFunction


unifyBenchFiles :: FilePath -> IO (Either TopologicalResult DecoratedCharacterResult)
unifyBenchFiles dir = do
    files <- listdirectory dir
    let fastaFiles = filter isFastaFile
    if isNull fastaFiles then
      fail "No fasta files found in " <> (dir)
    else
      do
      let parseFiles =   runExceptT
                       . eitherTValidation
                       . parmap rpar (fmap removeGaps . parseSpecifiedFile)
      result <- parseFiles fastafiles
      case result of
        Left pErr -> fail $ show pErr
        Right xs ->
          case decoration . masterUnify $ sconcat xs of
            Left uErr -> fail $ show uErr
            Right g   -> g
   
  where
    isFastaFile :: FilePath -> Bool
    isFastaFile = (== ".fasta"). takeExtension

    decoration     = fmap (fmap initializeDecorations2)

benchDirectory :: FilePath
benchDirectory = "Benchmark" </> "datasets"
