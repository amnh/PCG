{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE TypeFamilies      #-}

module TestSuite.ScriptTests
  ( commandTestSuite
  , costTestSuite
  , failureTestSuite
  ) where

import Bio.Graph
import Bio.Graph.ReferenceDAG (_dagCost, _graphData)
import Control.DeepSeq
import Control.Lens           (Getter, (^.))
import Control.Monad.Except   (ExceptT(..), runExceptT)
import Data.Binary            (decodeOrFail)
import Data.ByteString.Lazy   (ByteString, readFile)
import Data.Either
import Data.Foldable
import Data.List              (intercalate)
import Data.List.NonEmpty     (NonEmpty(..))
import Data.List.Utility      (equalityOf)
import Data.Semigroup.Foldable
import Numeric.Extended.Real
import Prelude                hiding (readFile, writeFile)
import System.Directory       (doesFileExist, makeAbsolute)
import System.Exit
import System.FilePath.Posix  (splitFileName, takeFileName)
import System.Process
import Test.Tasty
import Test.Tasty.HUnit
import TestSuite.SubProcess


-- |
-- This test-suite exists to ensure that a sequence of PCG command work correctly
-- together.
commandTestSuite :: TestTree
commandTestSuite = testGroup "Command Interoperability"
  [ -- Test saving out to a file, then reading the file back in.
    scriptsAllSucceed
      [ "datasets/commands/save-load/saving.pcg", "datasets/commands/save-load/reload.pcg" ]
    -- Report data and save state, load save-state and re-report.
  , scriptDiffOutputFiles
      [ "datasets/commands/report-reload/saving.pcg", "datasets/commands/report-reload/reload.pcg" ]
      [ "datasets/commands/report-reload/fst.data"  , "datasets/commands/report-reload/snd.data"   ]
  ]


-- |
-- This test-suite exists to ensure that the resulting cost from analysing the
-- input has not changed from the expected value for a variety of data-sets.
costTestSuite :: TestTree
costTestSuite = testGroup "Cost Analysis"
  [ scriptCheckCost 50.46
        "datasets/continuous/single-block/arthropods.pcg"
        "datasets/continuous/single-block/graph.bin"
  ,  scriptCheckCost 7
        "datasets/continuous/missing/test.pcg"
        "datasets/continuous/missing/graph.bin"
  , scriptCheckCost 8
        "datasets/non-additive/missing/test.pcg"
        "datasets/non-additive/missing/graph.bin"
  , scriptCheckCost 1665
        "datasets/non-additive/single-block/arthropods.pcg"
        "datasets/non-additive/single-block/graph.bin"
  , scriptCheckCost 56
        "datasets/additive/missing/test.pcg"
        "datasets/additive/missing/graph.bin"
  , scriptCheckCost 2642
        "datasets/additive/single-block/arthropods.pcg"
        "datasets/additive/single-block/graph.bin"
  , scriptCheckCost 1
        "datasets/additive/case-1/case-1.pcg"
        "datasets/additive/case-1/graph.bin"
  , scriptCheckCost 3
        "datasets/additive/case-2a/case-2a.pcg"
        "datasets/additive/case-2a/graph.bin"
  , scriptCheckCost 5
        "datasets/additive/case-2b/case-2b.pcg"
        "datasets/additive/case-2b/graph.bin"
  , scriptCheckCost 4
        "datasets/additive/case-3/case-3.pcg"
        "datasets/additive/case-3/graph.bin"
  , scriptCheckCost 16
      "datasets/sankoff/multi-block/missing/missing.pcg"
      "datasets/sankoff/multi-block/missing/graph.bin"
  , scriptCheckCost 12
      "datasets/sankoff/single-block/missing/missing-values.pcg"
      "datasets/sankoff/single-block/missing/graph.bin"
  , scriptCheckCost 914
        "datasets/sankoff/single-block/dna/discrete/arthropods.pcg"
        "datasets/sankoff/single-block/dna/discrete/graph.bin"
  , scriptCheckCost 1713
        "datasets/sankoff/single-block/dna/L1-norm/arthropods.pcg"
        "datasets/sankoff/single-block/dna/L1-norm/graph.bin"
  , scriptCheckCost 914
        "datasets/sankoff/single-block/dna/1-2/arthropods.pcg"
        "datasets/sankoff/single-block/dna/1-2/graph.bin"
  , scriptCheckCost 1789
        "datasets/sankoff/single-block/dna/2-1/arthropods.pcg"
        "datasets/sankoff/single-block/dna/2-1/graph.bin"
  , scriptCheckCost 1143
        "datasets/sankoff/single-block/protein/discrete/invertebrates.pcg"
        "datasets/sankoff/single-block/protein/discrete/graph.bin"
  , scriptCheckCost 11813
        "datasets/sankoff/single-block/protein/L1-norm/invertebrates.pcg"
        "datasets/sankoff/single-block/protein/L1-norm/graph.bin"
  , scriptCheckCost 2012
        "datasets/sankoff/single-block/protein/1-2/invertebrates.pcg"
        "datasets/sankoff/single-block/protein/1-2/graph.bin"
  , scriptCheckCost 1304
        "datasets/sankoff/single-block/protein/2-1/invertebrates.pcg"
        "datasets/sankoff/single-block/protein/2-1/graph.bin"
  , scriptCheckCost 89
        "datasets/sankoff/single-block/slashes/discrete/test.pcg"
        "datasets/sankoff/single-block/slashes/discrete/graph.bin"
  , scriptCheckCost 2089
        "datasets/sankoff/single-block/slashes/L1-norm/test.pcg"
        "datasets/sankoff/single-block/slashes/L1-norm/graph.bin"
  , scriptCheckCost 89
        "datasets/sankoff/single-block/slashes/1-2/test.pcg"
        "datasets/sankoff/single-block/slashes/1-2/graph.bin"
  , scriptCheckCost 154
        "datasets/sankoff/single-block/slashes/2-1/test.pcg"
        "datasets/sankoff/single-block/slashes/2-1/graph.bin"
  , scriptCheckCost 240
        "datasets/sankoff/single-block/slashes/hamming/test.pcg"
        "datasets/sankoff/single-block/slashes/hamming/graph.bin"
  , scriptCheckCost 176
        "datasets/sankoff/single-block/slashes/levenshtein/test.pcg"
        "datasets/sankoff/single-block/slashes/levenshtein/graph.bin"
  , scriptCheckCost 158
        "datasets/sankoff/single-block/large-mix/discrete/test.pcg"
        "datasets/sankoff/single-block/large-mix/discrete/graph.bin"
  , scriptCheckCost 11004
        "datasets/sankoff/single-block/large-mix/L1-norm/test.pcg"
        "datasets/sankoff/single-block/large-mix/L1-norm/graph.bin"
  , scriptCheckCost 158
        "datasets/sankoff/single-block/large-mix/1-2/test.pcg"
        "datasets/sankoff/single-block/large-mix/1-2/graph.bin"
  , scriptCheckCost 251
        "datasets/sankoff/single-block/large-mix/2-1/test.pcg"
        "datasets/sankoff/single-block/large-mix/2-1/graph.bin"
  , scriptCheckCost 348
        "datasets/sankoff/single-block/large-mix/hamming/test.pcg"
        "datasets/sankoff/single-block/large-mix/hamming/graph.bin"
  , scriptCheckCost 200
        "datasets/sankoff/single-block/large-mix/levenshtein/test.pcg"
        "datasets/sankoff/single-block/large-mix/levenshtein/graph.bin"
  , scriptCheckCost 117
        "datasets/sankoff/single-block/huge-mix/discrete/test.pcg"
        "datasets/sankoff/single-block/huge-mix/discrete/graph.bin"
  , scriptCheckCost 12681
        "datasets/sankoff/single-block/huge-mix/L1-norm/test.pcg"
        "datasets/sankoff/single-block/huge-mix/L1-norm/graph.bin"
  , scriptCheckCost 117
        "datasets/sankoff/single-block/huge-mix/1-2/test.pcg"
        "datasets/sankoff/single-block/huge-mix/1-2/graph.bin"
  , scriptCheckCost 181
        "datasets/sankoff/single-block/huge-mix/2-1/test.pcg"
        "datasets/sankoff/single-block/huge-mix/2-1/graph.bin"
  , scriptCheckCost 275
        "datasets/sankoff/single-block/huge-mix/hamming/test.pcg"
        "datasets/sankoff/single-block/huge-mix/hamming/graph.bin"
  , scriptCheckCost 230
        "datasets/sankoff/single-block/huge-mix/levenshtein/test.pcg"
        "datasets/sankoff/single-block/huge-mix/levenshtein/graph.bin"
  , scriptCheckCost 45
      "datasets/dynamic/multi-block/missing/missing.pcg"
      "datasets/dynamic/multi-block/missing/graph.bin"
  , scriptCheckCost 2042
      "datasets/dynamic/multi-block/dna/arthropods.pcg"
      "datasets/dynamic/multi-block/dna/graph.bin"
  , scriptCheckCost 28
      "datasets/dynamic/single-block/missing/missing-values.pcg"
      "datasets/dynamic/single-block/missing/graph.bin"
  , scriptCheckCost 11036
      "datasets/dynamic/single-block/protein/L1-norm/invertebrates.pcg"
      "datasets/dynamic/single-block/protein/L1-norm/graph.bin"
  , scriptCheckCost 1132
      "datasets/dynamic/single-block/protein/discrete/invertebrates.pcg"
      "datasets/dynamic/single-block/protein/discrete/graph.bin"
  , scriptCheckCost 1948
        "datasets/dynamic/single-block/protein/1-2/invertebrates.pcg"
        "datasets/dynamic/single-block/protein/1-2/graph.bin"
  , scriptCheckCost 1241
        "datasets/dynamic/single-block/protein/2-1/invertebrates.pcg"
        "datasets/dynamic/single-block/protein/2-1/graph.bin"
  , scriptCheckCost 3413
      "datasets/dynamic/single-block/slashes/L1-norm/test.pcg"
      "datasets/dynamic/single-block/slashes/L1-norm/graph.bin"
  , scriptCheckCost 197
        "datasets/dynamic/single-block/slashes/discrete/test.pcg"
        "datasets/dynamic/single-block/slashes/discrete/graph.bin"
  , scriptCheckCost 254
        "datasets/dynamic/single-block/slashes/1-2/test.pcg"
        "datasets/dynamic/single-block/slashes/1-2/graph.bin"
  , scriptCheckCost 228
        "datasets/dynamic/single-block/slashes/2-1/test.pcg"
        "datasets/dynamic/single-block/slashes/2-1/graph.bin"
  , scriptCheckCost 671
        "datasets/dynamic/single-block/slashes/hamming/test.pcg"
        "datasets/dynamic/single-block/slashes/hamming/graph.bin"
  , scriptCheckCost 488
        "datasets/dynamic/single-block/slashes/levenshtein/test.pcg"
        "datasets/dynamic/single-block/slashes/levenshtein/graph.bin"
  , scriptCheckCost 133
        "datasets/dynamic/single-block/large-mix/discrete/test.pcg"
        "datasets/dynamic/single-block/large-mix/discrete/graph.bin"
  , scriptCheckCost 7185
        "datasets/dynamic/single-block/large-mix/L1-norm/test.pcg"
        "datasets/dynamic/single-block/large-mix/L1-norm/graph.bin"
  , scriptCheckCost 164
        "datasets/dynamic/single-block/large-mix/1-2/test.pcg"
        "datasets/dynamic/single-block/large-mix/1-2/graph.bin"
  , scriptCheckCost 172
        "datasets/dynamic/single-block/large-mix/2-1/test.pcg"
        "datasets/dynamic/single-block/large-mix/2-1/graph.bin"
  , scriptCheckCost 367
        "datasets/dynamic/single-block/large-mix/hamming/test.pcg"
        "datasets/dynamic/single-block/large-mix/hamming/graph.bin"
  , scriptCheckCost 213
        "datasets/dynamic/single-block/large-mix/levenshtein/test.pcg"
        "datasets/dynamic/single-block/large-mix/levenshtein/graph.bin"
  , scriptCheckCost 246
        "datasets/dynamic/single-block/huge-mix/discrete/test.pcg"
        "datasets/dynamic/single-block/huge-mix/discrete/graph.bin"
  , scriptCheckCost 21753
        "datasets/dynamic/single-block/huge-mix/L1-norm/test.pcg"
        "datasets/dynamic/single-block/huge-mix/L1-norm/graph.bin"
  , scriptCheckCost 325
        "datasets/dynamic/single-block/huge-mix/1-2/test.pcg"
        "datasets/dynamic/single-block/huge-mix/1-2/graph.bin"
  , scriptCheckCost 284
        "datasets/dynamic/single-block/huge-mix/2-1/test.pcg"
        "datasets/dynamic/single-block/huge-mix/2-1/graph.bin"
  , scriptCheckCost 872
        "datasets/dynamic/single-block/huge-mix/hamming/test.pcg"
        "datasets/dynamic/single-block/huge-mix/hamming/graph.bin"
  , scriptCheckCost 698
        "datasets/dynamic/single-block/huge-mix/levenshtein/test.pcg"
        "datasets/dynamic/single-block/huge-mix/levenshtein/graph.bin"
  ]


-- |
-- This test suite-exists to list input data sets that PCG should reject
-- as input due to inconsistencies.
failureTestSuite :: TestTree
failureTestSuite = testGroup "Expected Failures"
  [ scriptFailure "datasets/failure/unmatched-leaf-taxon/test.pcg"
  , scriptFailure "datasets/failure/unmatched-tree-taxon/test.pcg"
  , scriptFailure "datasets/failure/duplicate-leaf-taxon/test.pcg"
-- We omit this test because the DAG.unfoldr function in the ParsedForest call
-- will ensure that there is only one leaf in the graph. It may have multiple
-- parents however.
--  , scriptFailure "datasets/duplicate-tree-taxon/test.pcg"
  , scriptFailure "datasets/failure/no-data-in-graph/test.pcg"
  ]


-- |
-- Most general and primative script test-case builder.
--
-- Runs PCG with the specified script file. After execution completes, collects
-- the contents of the specified output files.
runScripts
  :: ( Foldable1 f
     , Traversable t
     , NFData (t ByteString)
     )
  => f FilePath -- ^ Paths to the script files to run (in order)
  -> t FilePath -- ^ Paths to the generated output files to inspect
  -> IO (Either (FilePath, Int) (t ByteString)) -- ^ Resulting file contents of the
                                                --   specified output files, or the
                                                --   file path and exit code of the
                                                --   script that failed
runScripts inputScripts outputPaths = do
    let (x:|xs) = toNonEmpty inputScripts
    result <- runExceptT $ foldl' runNextScript (runScript x) xs
    case result of
      Left  v -> pure  $ Left v
      Right _ -> force . Right <$> traverse nicelyReadFile outputPaths
  where
    runNextScript :: ExceptT (FilePath, Int) IO () -> FilePath -> ExceptT (FilePath, Int) IO ()
    runNextScript v s = v >> runScript s
    
    runScript :: FilePath -> ExceptT (FilePath, Int) IO ()
    runScript script = ExceptT $ do
        ctx <- constructProcess script
        (exitCode, _, _) <- readCreateProcessWithExitCode (process ctx) mempty
        _   <- destructProcess ctx
        pure $ case exitCode of
                 ExitFailure v -> Left (script, v)
                 _             -> Right ()
    
    nicelyReadFile :: FilePath -> IO ByteString
    nicelyReadFile filePath = do
        fileExist   <- doesFileExist filePath
        absFilePath <- makeAbsolute  filePath
        if fileExist
        then readFile filePath
        else fail $ unlines
               [ "No file found with the specified filepath:"
               , absFilePath
               ]


-- |
-- Compare the graph cost against an expected value
scriptCheckCost
  :: ExtendedReal -- ^ Expected cost ∈ [0, ∞]
  -> FilePath     -- ^ Script File
  -> FilePath     -- ^ Serialized output file
  -> TestTree
scriptCheckCost = scriptCheckValue (_graphData . _dagCost)


-- |
-- Use a getter to check a value serialized to disk
scriptCheckValue
  :: (Eq a, Show a)
  => Getter UndecoratedReferenceDAG a -- ^ Value accessor
  -> a                                -- ^ Expected value
  -> FilePath                         -- ^ Script File
  -> FilePath                         -- ^ Serialized output file
  -> TestTree
scriptCheckValue getter expectedValue scriptPath outputPath = testCase scriptPath $ do
    v <- runScripts (scriptPath:|[]) [outputPath]
    case v of
      Left  (path, exitCode) -> assertFailure $ mconcat
                                  ["Script '", path, "'failed with exit code: ", show exitCode]
      Right               [] -> assertFailure "No files were returned despite supplying one path!"
      Right (   binStream:_) ->
          case decodeOrFail binStream of
            Left  (_, _, errMsg) -> assertFailure $ "Binary decoding error: " <> errMsg
            Right (_, _, graph ) -> let foundValue = graph ^. getter
                                    in  foundValue @?= expectedValue


-- |
-- Run one of more scripts, then assert that all the output files are equal.
scriptDiffOutputFiles
  :: [FilePath] -- ^ Script Files to run
  -> [FilePath] -- ^ Serialized output files to diff
  -> TestTree
scriptDiffOutputFiles is os =
    case is of
      []   -> testCase "[]" $ assertBool "No scripts provided, vacuous success." True
      x:xs ->
          let scripts = x:|xs
          in  case os of
                []   -> testCase "[]" $ assertBool "No outputs provided, vacuous success." True
                y:ys ->
                    let outFiles = y:|ys
                    in  testCase (makeTitle scripts) $ do
                        v <- runScripts scripts outFiles
                        case v of
                          Left  (path, exitCode) -> assertFailure $ mconcat
                              ["Script '", path, "'failed with exit code: ", show exitCode]
                          Right  binStreams      -> assertBool "Not all outputs were the same!" $
                                                        equalityOf id binStreams


-- |
-- Expects the PCG script to return a non-zero exitcode.
scriptFailure :: String -> TestTree
scriptFailure scriptPath = testCase scriptPath $ do
    v <- runScripts (scriptPath:|[]) []
    assertBool "Expected script success" $ isLeft v


-- |
-- Expects the each of the PCG scripts to succeed.
scriptsAllSucceed :: [FilePath] -> TestTree
scriptsAllSucceed xs =
    case xs of
      []   -> testCase "[]" $ assertBool "No scripts provided, vacuous success." True
      y:ys -> let scripts = y:|ys
              in  testCase (makeTitle scripts) $ do
        v <- runScripts scripts []
        case v of
          Right e -> assertBool "No files were to be collected" $ e == []
          Left (failedScript, ec) -> assertFailure $ mconcat
                                       [ "Expected success of script '"
                                       , failedScript
                                       , "', but failed with exit code: "
                                       , show ec
                                       ]


-- |
-- Takes multiple file pathes and combines thier base names tinto a shorter title.
makeTitle :: NonEmpty FilePath -> String 
makeTitle (x:|xs) = (prefix <>) . intercalate "," . (name:) $ takeFileName <$> xs
   where
     (prefix, name) = splitFileName x


