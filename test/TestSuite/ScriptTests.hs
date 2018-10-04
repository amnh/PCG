
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module TestSuite.ScriptTests
  ( testSuite
  ) where

import Control.Arrow              ((&&&))
import Data.Char                  (isSpace)
import Data.Either
import Data.Foldable
import Data.Functor               (($>))
import Data.Scientific            hiding (scientific)
import Data.Text                  (Text)
import Data.Void                  (Void)
import Numeric.Extended.Real
import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (scientific)
import Turtle                     hiding (char, many, satisfy, x)


testSuite :: IO TestTree
testSuite = testGroup "Script Test Suite" <$> sequenceA
  [ scriptCheckCost 50.46
        "datasets/continuous/single-block/arthropods.pcg"
        "datasets/continuous/single-block/cost.data"
  , scriptCheckCost 1665.0
        "datasets/non-additive/single-block/arthropods.pcg"
        "datasets/non-additive/single-block/cost.data"
  , scriptCheckCost 77252.0
        "datasets/additive/single-block/arthropods.pcg"
        "datasets/additive/single-block/cost.data"
  , scriptCheckCost 914.0
        "datasets/sankoff/single-block/dna/discrete/arthropods.pcg"
        "datasets/sankoff/single-block/dna/discrete/cost.data"
  , scriptCheckCost 1713.0
        "datasets/sankoff/single-block/dna/L1-norm/arthropods.pcg"
        "datasets/sankoff/single-block/dna/L1-norm/cost.data"
  , scriptCheckCost 914.0
        "datasets/sankoff/single-block/dna/1-2/arthropods.pcg"
        "datasets/sankoff/single-block/dna/1-2/cost.data"
  , scriptCheckCost 1789.0
        "datasets/sankoff/single-block/dna/2-1/arthropods.pcg"
        "datasets/sankoff/single-block/dna/2-1/cost.data"
  , scriptCheckCost 89
        "datasets/sankoff/single-block/slashes/discrete/test.pcg"
        "datasets/sankoff/single-block/slashes/discrete/cost.data"
  , scriptCheckCost 2089
        "datasets/sankoff/single-block/slashes/L1-norm/test.pcg"
        "datasets/sankoff/single-block/slashes/L1-norm/cost.data"
  , scriptCheckCost 89
        "datasets/sankoff/single-block/slashes/1-2/test.pcg"
        "datasets/sankoff/single-block/slashes/1-2/cost.data"
  , scriptCheckCost 154
        "datasets/sankoff/single-block/slashes/2-1/test.pcg"
        "datasets/sankoff/single-block/slashes/2-1/cost.data"
  , scriptCheckCost 240
        "datasets/sankoff/single-block/slashes/hamming/test.pcg"
        "datasets/sankoff/single-block/slashes/hamming/cost.data"
  , scriptCheckCost 176
        "datasets/sankoff/single-block/slashes/levenshtein/test.pcg"
        "datasets/sankoff/single-block/slashes/levenshtein/cost.data"
  , scriptCheckCost 2042.0
        "datasets/dynamic/multi-block/arthropods.pcg"
        "datasets/dynamic/multi-block/cost.data"
  , scriptCheckCost 197.0
        "datasets/dynamic/single-block/slashes/discrete/test.pcg"
        "datasets/dynamic/single-block/slashes/discrete/cost.data"
  , scriptCheckCost 2042.0
        "datasets/dynamic/single-block/slashes/L1-norm/test.pcg"
        "datasets/dynamic/single-block/slashes/L1-norm/cost.data"
  , scriptCheckCost 254.0
        "datasets/dynamic/single-block/slashes/1-2/test.pcg"
        "datasets/dynamic/single-block/slashes/1-2/cost.data"
  , scriptCheckCost 228.0
        "datasets/dynamic/single-block/slashes/2-1/test.pcg"
        "datasets/dynamic/single-block/slashes/2-1/cost.data"
  , scriptCheckCost 671.0
        "datasets/dynamic/single-block/slashes/hamming/test.pcg"
        "datasets/dynamic/single-block/slashes/hamming/cost.data"
  , scriptCheckCost 488.0
        "datasets/dynamic/single-block/slashes/levenshtein/test.pcg"
        "datasets/dynamic/single-block/slashes/levenshtein/cost.data"
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
    checkResult (Right          []) = assertFailure "No files were returned despite supplying one path!"
    checkResult (Right (outFile:_)) =
        case parseCost outFile of
          Nothing   -> assertFailure "No cost found in the output file!"
          Just cost -> cost @?= expectedCost


parseCost :: Text -> Maybe ExtendedReal
parseCost = parseMaybe fileSpec
  where
    fileSpec =  many (try (ignoredLine <* notFollowedBy costLine))
             *> ignoredLine
             *> costLine
             <* many ignoredLine

    costLine :: MonadParsec Void Text m => m ExtendedReal
    costLine =  many inlineSpace
             *> string' "DAG total cost:"
             *> many inlineSpace
             *> extendedReal
             <* ignoredLine


    extendedReal :: MonadParsec Void Text m => m ExtendedReal
    extendedReal = (fromFinite . toRealFloat <$> scientific) <|> (char '∞' $> infinity)

    ignoredLine  :: MonadParsec Void Text m => m ()
    ignoredLine  = many inlineChar *> newlineChar

    inlineSpace  :: MonadParsec Void Text m => m ()
    inlineSpace  = void $ satisfy (\x -> isSpace x && x /= '\n')

    inlineChar   :: MonadParsec Void Text m => m ()
    inlineChar   = void $ satisfy (/= '\n')

    newlineChar  :: (MonadParsec Void s m, Token s ~ Char) => m ()
    newlineChar  = void $ char '\n'



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
