{-# LANGUAGE OverloadedStrings #-}

module TestSuite.ScriptTests
  ( testSuite
  ) where

import Control.Arrow    ((&&&))
import Data.Either
import Data.Foldable
import Data.Text        (Text)
import Test.Tasty
import Test.Tasty.HUnit
import Turtle


testSuite :: IO TestTree
testSuite = testGroup "Script Test Suite" <$> sequenceA
  [ scriptFailure "datasets/unmatched-leaf-taxon/test.pcg"
  ]


scriptTest
  :: String                          -- ^ Script File
  -> [String]                        -- ^ Expected Output Files
  -> (Either Int [Text] -> TestTree) -- ^ Build a TestTree from the resulting
                                     -- output file contents or ExitStatus code
  -> IO TestTree
scriptTest scriptPath outputPaths testLogic = testLogic <$> runExecutable scriptPath outputPaths


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
