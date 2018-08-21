{-# LANGUAGE OverloadedStrings #-}

module TestSuite.ScriptTests
  ( testSuite
  ) where

import Control.Arrow    ((&&&))
import Data.Either
import Data.Foldable
import Data.Functor     (($>))
import Data.Text        (Text)
import Test.Tasty
import Test.Tasty.HUnit
import Turtle
import Turtle.Prelude


testSuite :: TestTree
testSuite = testGroup "Integration Test Suite"
  [ scriptFailure "test-data/pcg-test-missing-leaf/test.pcg"
  ]


scriptTest
  :: String   -- ^ Script File
  -> [String] -- ^ Expected Output Files
  -> (Either Int [String] -> TestTree) -- ^ Build a TestTree from the resulting
                                       -- output file contents or ExitStatus code
  -> TestTree
scriptTest scriptPath outputPaths testLogic = withResource (runExecutable scriptPath outputPaths) (const (pure ())) (testLogic . pure)


scriptFailure :: String -> TestTree
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
    exitCode <- shell ("stack exec pcg < " <> scriptText) mempty
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
