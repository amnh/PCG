{-# LANGUAGE OverloadedStrings #-}

module TestSuite.ScriptTests
  ( testSuite
  ) where

import Control.Arrow    ((&&&))
import Data.Either
import Data.Foldable
import Data.Text        (Text)
import Numeric.Extended.Real
import Test.Tasty
import Test.Tasty.HUnit
import Turtle


testSuite :: IO TestTree
testSuite = testGroup "Script Test Suite" <$> sequenceA
  [ scriptCheckCost 0
        "datasets/continuous/single-block/arthContin.pcg"
        "datasets/continuous/single-block/arthContin.data"
  , scriptCheckCost 0
        "datasets/non-additive/single-block/arthNonAdd.pcg"
        "datasets/non-additive/single-block/arthNonAdd.data"
  , scriptCheckCost 0
        "datasets/additive/single-block/arthAdd.pcg"
        "datasets/additive/single-block/arthAdd.data"
  , scriptFailure "datasets/unmatched-leaf-taxon/test.pcg"
  ]


scriptTest
  :: String                          -- ^ Script File
  -> [String]                        -- ^ Expected Output Files
  -> (Either Int [Text] -> TestTree) -- ^ Build a TestTree from the resulting
                                     -- output file contents or ExitStatus code
  -> IO TestTree
scriptTest scriptPath outputPaths testLogic = testLogic <$> runExecutable scriptPath outputPaths


scriptCheckCost
  :: ExtendedReal                    -- ^ Expected cost ∈ [0, ∞]
  -> String                          -- ^ Script File
  -> String                          -- ^ Expected output file containing the cost
  -> IO TestTree
scriptCheckCost expectedCost scriptPath outputPath = scriptTest scriptPath [outputPath] $ testCase scriptPath . checkResult
  where
    checkResult (Left     exitCode) = assertFailure $ "Script failed with exit code: " <> show exitCode
    checkResult (Right          []) = assertFailure $ "No files were returned despite supplying one path!"
    checkResult (Right (outFile:_)) =
        case parseCost outFile of
          Nothing   -> assertFailure $ "No cost found in the output file: " <> show outFile
          Just cost -> cost @?= expectedCost


parseCost :: Text -> Maybe ExtendedReal
parseCost = const $ Just 0


scriptFailure :: String -> IO TestTree
scriptFailure scriptPath = scriptTest scriptPath [] (testCase scriptPath . assertBool "Expected script failure" . isLeft)


-- |
-- Runs PCG with the specified script file. After execution completes, collects
-- the contents of the specified output files.
--
-- Useful for integration tests specified with 'Test.Tasty.withResource'.
runExecutable
  :: String                 -- ^ Path to the script file to run
  -> [String]               -- ^ Paths to the generated output files
  -> IO (Either Int [Text]) -- ^ Resulting file contents of the specified output
                            --   files, or the exit code if the script failed
runExecutable scriptStr outputPaths = do
    startingDirectory <- pwd
    cd scriptDirectory
    exitCode <- shell ("stack exec pcg -- --input " <> scriptText <> " --output test.log") mempty
    cd startingDirectory
    case exitCode of
      ExitFailure v -> pure $ Left v
      _             -> Right <$> traverse (readTextFile . decodeString) outputPaths
  where
    scriptText = either id id $ toText scriptFile

    (scriptDirectory, scriptFile) = breakScriptPath $ decodeString scriptStr

    breakScriptPath = (collapse . foldl' (</>) defaultDirectory . init &&& last) . splitDirectories
      where
        defaultDirectory = decodeString "."
