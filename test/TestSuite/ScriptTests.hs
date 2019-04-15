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
import Bio.Graph.ReferenceDAG  (_dagCost, _graphData)
import Control.Lens            (Getter, (^.))
import Control.Monad.Except    (ExceptT (..), runExceptT)
import Data.Binary             (decodeOrFail)
import Data.Bits
import Data.ByteString.Lazy    (ByteString)
import Data.Either
import Data.Foldable
import Data.List               (intercalate)
import Data.List.NonEmpty      (NonEmpty (..))
import Data.List.Utility       (equalityOf)
import Data.Semigroup.Foldable
import Numeric.Extended.Real
import Prelude                 hiding (writeFile)
import System.Exit
import System.FilePath.Posix   (splitFileName, takeFileName)
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
      [ "commands/save-load/saving.pcg", "commands/save-load/reload.pcg" ]
    -- Report data and save state, load save-state and re-report.
  , scriptDiffOutputFiles
      [ "commands/report-reload/saving.pcg", "commands/report-reload/reload.pcg" ]
      [ "commands/report-reload/fst.data"  , "commands/report-reload/snd.data"   ]
  ]


-- |
-- This test-suite exists to ensure that the resulting cost from analysing the
-- input has not changed from the expected value for a variety of data-sets.
costTestSuite :: TestTree
costTestSuite = testGroup "Cost Analysis"
  [ scriptCheckCost 50.46
        "continuous/single-block/arthropods.pcg"
        "continuous/single-block/graph.bin"
  ,  scriptCheckCost 7
        "continuous/missing/test.pcg"
        "continuous/missing/graph.bin"
  , scriptCheckCost 8
        "non-additive/missing/test.pcg"
        "non-additive/missing/graph.bin"
  , scriptCheckCost 1665
        "non-additive/single-block/arthropods.pcg"
        "non-additive/single-block/graph.bin"
  , scriptCheckCost 56
        "additive/missing/test.pcg"
        "additive/missing/graph.bin"
  , scriptCheckCost 2642
        "additive/single-block/arthropods.pcg"
        "additive/single-block/graph.bin"
  , scriptCheckCost 1
        "additive/case-1/case-1.pcg"
        "additive/case-1/graph.bin"
  , scriptCheckCost 3
        "additive/case-2a/case-2a.pcg"
        "additive/case-2a/graph.bin"
  , scriptCheckCost 5
        "additive/case-2b/case-2b.pcg"
        "additive/case-2b/graph.bin"
  , scriptCheckCost 4
        "additive/case-3/case-3.pcg"
        "additive/case-3/graph.bin"
  , scriptCheckCost 16
      "sankoff/multi-block/missing/missing.pcg"
      "sankoff/multi-block/missing/graph.bin"
  , scriptCheckCost 12
      "sankoff/single-block/missing/missing-values.pcg"
      "sankoff/single-block/missing/graph.bin"
  , scriptCheckCost 914
        "sankoff/single-block/dna/discrete/arthropods.pcg"
        "sankoff/single-block/dna/discrete/graph.bin"
  , scriptCheckCost 1713
        "sankoff/single-block/dna/L1-norm/arthropods.pcg"
        "sankoff/single-block/dna/L1-norm/graph.bin"
  , scriptCheckCost 914
        "sankoff/single-block/dna/1-2/arthropods.pcg"
        "sankoff/single-block/dna/1-2/graph.bin"
  , scriptCheckCost 1789
        "sankoff/single-block/dna/2-1/arthropods.pcg"
        "sankoff/single-block/dna/2-1/graph.bin"
  , scriptCheckCost 1143
        "sankoff/single-block/protein/discrete/invertebrates.pcg"
        "sankoff/single-block/protein/discrete/graph.bin"
  , scriptCheckCost 11813
        "sankoff/single-block/protein/L1-norm/invertebrates.pcg"
        "sankoff/single-block/protein/L1-norm/graph.bin"
  , scriptCheckCost 2012
        "sankoff/single-block/protein/1-2/invertebrates.pcg"
        "sankoff/single-block/protein/1-2/graph.bin"
  , scriptCheckCost 1304
        "sankoff/single-block/protein/2-1/invertebrates.pcg"
        "sankoff/single-block/protein/2-1/graph.bin"
  , scriptCheckCost 89
        "sankoff/single-block/slashes/discrete/test.pcg"
        "sankoff/single-block/slashes/discrete/graph.bin"
  , scriptCheckCost 2089
        "sankoff/single-block/slashes/L1-norm/test.pcg"
        "sankoff/single-block/slashes/L1-norm/graph.bin"
  , scriptCheckCost 89
        "sankoff/single-block/slashes/1-2/test.pcg"
        "sankoff/single-block/slashes/1-2/graph.bin"
  , scriptCheckCost 154
        "sankoff/single-block/slashes/2-1/test.pcg"
        "sankoff/single-block/slashes/2-1/graph.bin"
  , scriptCheckCost 240
        "sankoff/single-block/slashes/hamming/test.pcg"
        "sankoff/single-block/slashes/hamming/graph.bin"
  , scriptCheckCost 176
        "sankoff/single-block/slashes/levenshtein/test.pcg"
        "sankoff/single-block/slashes/levenshtein/graph.bin"
  , scriptCheckCost 158
        "sankoff/single-block/large-mix/discrete/test.pcg"
        "sankoff/single-block/large-mix/discrete/graph.bin"
  , scriptCheckCost 11004
        "sankoff/single-block/large-mix/L1-norm/test.pcg"
        "sankoff/single-block/large-mix/L1-norm/graph.bin"
  , scriptCheckCost 158
        "sankoff/single-block/large-mix/1-2/test.pcg"
        "sankoff/single-block/large-mix/1-2/graph.bin"
  , scriptCheckCost 251
        "sankoff/single-block/large-mix/2-1/test.pcg"
        "sankoff/single-block/large-mix/2-1/graph.bin"
  , scriptCheckCost 348
        "sankoff/single-block/large-mix/hamming/test.pcg"
        "sankoff/single-block/large-mix/hamming/graph.bin"
  , scriptCheckCost 200
        "sankoff/single-block/large-mix/levenshtein/test.pcg"
        "sankoff/single-block/large-mix/levenshtein/graph.bin"
  , scriptCheckCost 117
        "sankoff/single-block/huge-mix/discrete/test.pcg"
        "sankoff/single-block/huge-mix/discrete/graph.bin"
  , scriptCheckCost 12681
        "sankoff/single-block/huge-mix/L1-norm/test.pcg"
        "sankoff/single-block/huge-mix/L1-norm/graph.bin"
  , scriptCheckCost 117
        "sankoff/single-block/huge-mix/1-2/test.pcg"
        "sankoff/single-block/huge-mix/1-2/graph.bin"
  , scriptCheckCost 181
        "sankoff/single-block/huge-mix/2-1/test.pcg"
        "sankoff/single-block/huge-mix/2-1/graph.bin"
  , scriptCheckCost 275
        "sankoff/single-block/huge-mix/hamming/test.pcg"
        "sankoff/single-block/huge-mix/hamming/graph.bin"
  , scriptCheckCost 230
        "sankoff/single-block/huge-mix/levenshtein/test.pcg"
        "sankoff/single-block/huge-mix/levenshtein/graph.bin"
  , scriptCheckCost 45
        "dynamic/multi-block/missing/missing.pcg"
        "dynamic/multi-block/missing/graph.bin"
  , scriptCheckCost 2042
        "dynamic/multi-block/dna/arthropods.pcg"
        "dynamic/multi-block/dna/graph.bin"
  , scriptCheckCost 28
        "dynamic/single-block/missing/missing-values.pcg"
        "dynamic/single-block/missing/graph.bin"
  , scriptCheckCost 11036
        "dynamic/single-block/protein/L1-norm/invertebrates.pcg"
        "dynamic/single-block/protein/L1-norm/graph.bin"
  , scriptCheckCost 1132
        "dynamic/single-block/protein/discrete/invertebrates.pcg"
        "dynamic/single-block/protein/discrete/graph.bin"
  , scriptCheckCost 1948
        "dynamic/single-block/protein/1-2/invertebrates.pcg"
        "dynamic/single-block/protein/1-2/graph.bin"
  , scriptCheckCost 1241
        "dynamic/single-block/protein/2-1/invertebrates.pcg"
        "dynamic/single-block/protein/2-1/graph.bin"
  , scriptCheckCost 3413
        "dynamic/single-block/slashes/L1-norm/test.pcg"
        "dynamic/single-block/slashes/L1-norm/graph.bin"
  , scriptCheckCost 197
        "dynamic/single-block/slashes/discrete/test.pcg"
        "dynamic/single-block/slashes/discrete/graph.bin"
  , scriptCheckCost 254
        "dynamic/single-block/slashes/1-2/test.pcg"
        "dynamic/single-block/slashes/1-2/graph.bin"
  , scriptCheckCost 228
        "dynamic/single-block/slashes/2-1/test.pcg"
        "dynamic/single-block/slashes/2-1/graph.bin"
  , scriptCheckCost 671
        "dynamic/single-block/slashes/hamming/test.pcg"
        "dynamic/single-block/slashes/hamming/graph.bin"
  , scriptCheckCost 488
        "dynamic/single-block/slashes/levenshtein/test.pcg"
        "dynamic/single-block/slashes/levenshtein/graph.bin"
  , scriptCheckCost 133
        "dynamic/single-block/large-mix/discrete/test.pcg"
        "dynamic/single-block/large-mix/discrete/graph.bin"
  , scriptCheckCost 7185
        "dynamic/single-block/large-mix/L1-norm/test.pcg"
        "dynamic/single-block/large-mix/L1-norm/graph.bin"
  , scriptCheckCost 164
        "dynamic/single-block/large-mix/1-2/test.pcg"
        "dynamic/single-block/large-mix/1-2/graph.bin"
  , scriptCheckCost 172
        "dynamic/single-block/large-mix/2-1/test.pcg"
        "dynamic/single-block/large-mix/2-1/graph.bin"
  , scriptCheckCost 367
        "dynamic/single-block/large-mix/hamming/test.pcg"
        "dynamic/single-block/large-mix/hamming/graph.bin"
  , scriptCheckCost 213
        "dynamic/single-block/large-mix/levenshtein/test.pcg"
        "dynamic/single-block/large-mix/levenshtein/graph.bin"
  , scriptCheckCost 246
        "dynamic/single-block/huge-mix/discrete/test.pcg"
        "dynamic/single-block/huge-mix/discrete/graph.bin"
  , scriptCheckCost 21753
        "dynamic/single-block/huge-mix/L1-norm/test.pcg"
        "dynamic/single-block/huge-mix/L1-norm/graph.bin"
  , scriptCheckCost 325
        "dynamic/single-block/huge-mix/1-2/test.pcg"
        "dynamic/single-block/huge-mix/1-2/graph.bin"
  , scriptCheckCost 284
        "dynamic/single-block/huge-mix/2-1/test.pcg"
        "dynamic/single-block/huge-mix/2-1/graph.bin"
  , scriptCheckCost 872
        "dynamic/single-block/huge-mix/hamming/test.pcg"
        "dynamic/single-block/huge-mix/hamming/graph.bin"
  , scriptCheckCost 698
        "dynamic/single-block/huge-mix/levenshtein/test.pcg"
        "dynamic/single-block/huge-mix/levenshtein/graph.bin"
  ]


-- |
-- This test suite-exists to list input data sets that PCG should reject
-- as input due to inconsistencies.
failureTestSuite :: TestTree
failureTestSuite = testGroup "Expected Failures"
  [ scriptInputError "failure/file-not-found/test.pcg"
  , scriptParseError "failure/file-unparsable/test.pcg"
  , scriptUnifyError "failure/unmatched-leaf-taxon/test.pcg"
  , scriptUnifyError "failure/unmatched-tree-taxon/test.pcg"
--  , scriptUnifyError "failure/duplicate-leaf-taxon/test.pcg"
  , scriptUnifyError "failure/duplicate-leaf-taxon/test.pcg"
-- We omit this test because the DAG.unfoldr function in the ParsedForest call
-- will ensure that there is only one leaf in the graph. It may have multiple
-- parents however.
--  , scriptFailure "duplicate-tree-taxon/test.pcg"
  , scriptUnifyError "failure/no-data-in-graph/test.pcg"
  ]


-- |
-- Most general and primative script test-case builder.
--
-- Runs PCG with the specified script file. After execution completes, collects
-- the contents of the specified output files.
runScripts
  :: ( Foldable1 f
     , Traversable t
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
      Right _ -> Right <$> collectFileContents outputPaths
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
{-
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
-}

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
      Left  (path, exitCode) -> assertFailure $ fold
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
                          Left  (path, exitCode) -> assertFailure $ fold
                              ["Script '", path, "'failed with exit code: ", show exitCode]
                          Right  binStreams      -> assertBool "Not all outputs were the same!" $
                                                        equalityOf id binStreams


-- |
-- Expects the PCG script to return a non-zero exitcode.
scriptFailure :: String -> TestTree
scriptFailure scriptPath = testCase scriptPath $ do
    v <- runScripts (scriptPath:|[]) []
    assertBool "Expected script failure, but script was successful..." $ isLeft v


-- |
-- Expects the PCG script to return an input error exitcode.
scriptInputError :: String -> TestTree
scriptInputError = scriptWithExitCode (bit 2) "an input error"


-- |
-- Expects the PCG script to return a parse error exitcode.
scriptParseError :: String -> TestTree
scriptParseError = scriptWithExitCode (bit 3) "a parse error"


-- |
-- Expects the PCG script to return a unifcation error exitcode.
scriptUnifyError :: String -> TestTree
scriptUnifyError = scriptWithExitCode (bit 2 .|. bit 3) "a unification error"


-- |
-- Build for 'TestTree' that expects a specific exitcode.
scriptWithExitCode :: Int -> String -> String -> TestTree
scriptWithExitCode expVal description scriptPath = testCase scriptPath $ do
    v <- runScripts (scriptPath:|[]) []
    case v of
      Right {}     -> assertFailure "Expected script failure, but script was successful..."
      Left  (_,ec) -> let errMsg = fold [ "Expected exitcode (", show expVal, ") indicating "
                                        , description, ", but instead exitcode (", show ec,") was found"
                                        ]
                      in  assertBool errMsg $ ec == expVal
  where
    


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
          Right e -> assertBool "No files were to be collected" $ null e
          Left (failedScript, ec) -> assertFailure $ fold
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


