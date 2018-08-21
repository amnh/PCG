{-# LANGUAGE OverloadedStrings #-}

module TestSuite.ScriptTests
  ( testSuite
  ) where

import Data.Functor   (($>))
import Data.List.NonEmpty
import Data.Text
import Test.Tasty
import Turtle
import Turtle.Prelude

testSuite :: TestTree
testSuite = testGroup "Integration Test Suite" []


exampleIntegrationTest = withResource (runExecutable "path/to/script/test.pcg" ["path/to/output.data"] (pure ()) $
    \fileContests -> undefined


-- |
-- Runs PCG with the specified script file. After execution completes, collects
-- the contents of the specified output files.
--
-- Useful for integration tests specified with 'Test.Tasty.withResource'.
runExecutable
  :: String                         -- ^ Path to the script file to run
  -> NonEmpty String                -- ^ Paths to the generated output files
  -> IO (Either Int (NonEmpty Text) -- ^ Resulting file contents of the specified output files, or the exit code if the script failed
runExecutable scriptStr outputPaths = do
    startingDirectory <- pwd 
    cd scriptDirectory
    exitCode <- shell ("stack exec pcg < " <> toText scriptFile) []
    cd startingDirectory
    case exitCode of
      ExitFailure v -> pure $ Left Int
      _             -> Right <$> traverse readTextFile outputPaths
  where
    (scriptDirectory, scriptFile) = breakScriptPath $ decodeString scriptStr

    breakScriptPath = (collapse . foldl' (</>) defaultDirectory . init &&& last) . splitDirectories
      where
        defaultDirectory = decodeString "."
