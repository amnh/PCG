{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE TypeFamilies      #-}

module TestSuite.ScriptTests
  ( testSuite
  ) where

import Control.DeepSeq
import Data.Bifunctor             (first)
import Data.Char                  (isSpace)
import Data.Either
import Data.Functor               (void, ($>))
import Data.Scientific            hiding (scientific)
import Data.Text                  (Text)
import Data.Text.IO               (readFile)
import Data.Void                  (Void)
import Numeric.Extended.Real
import Prelude                    hiding (readFile, writeFile)
import System.Exit
import System.Process
import Test.Tasty
import Test.Tasty.HUnit
import TestSuite.SubProcess
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (scientific)


testSuite :: TestTree
testSuite = testGroup "Script Test Suite"
  [ scriptCheckCost 50.46
        "datasets/continuous/single-block/arthropods.pcg"
        "datasets/continuous/single-block/cost.data"
  ,  scriptCheckCost 7
        "datasets/continuous/missing/test.pcg"
        "datasets/continuous/missing/cost.data"
  , scriptCheckCost 8
        "datasets/non-additive/missing/test.pcg"
        "datasets/non-additive/missing/cost.data"
  , scriptCheckCost 1665
        "datasets/non-additive/single-block/arthropods.pcg"
        "datasets/non-additive/single-block/cost.data"
  , scriptCheckCost 56
        "datasets/additive/missing/test.pcg"
        "datasets/additive/missing/cost.data"
  , scriptCheckCost 2642
        "datasets/additive/single-block/arthropods.pcg"
        "datasets/additive/single-block/cost.data"
  , scriptCheckCost 1
        "datasets/additive/case-1/case-1.pcg"
        "datasets/additive/case-1/cost.data"
  , scriptCheckCost 3
        "datasets/additive/case-2a/case-2a.pcg"
        "datasets/additive/case-2a/cost.data"
  , scriptCheckCost 5
        "datasets/additive/case-2b/case-2b.pcg"
        "datasets/additive/case-2b/cost.data"
  , scriptCheckCost 4
        "datasets/additive/case-3/case-3.pcg"
        "datasets/additive/case-3/cost.data"
  , scriptCheckCost 16
      "datasets/sankoff/multi-block/missing/missing.pcg"
      "datasets/sankoff/multi-block/missing/cost.data"
  , scriptCheckCost 12
      "datasets/sankoff/single-block/missing/missing-values.pcg"
      "datasets/sankoff/single-block/missing/cost.data"
  , scriptCheckCost 914
        "datasets/sankoff/single-block/dna/discrete/arthropods.pcg"
        "datasets/sankoff/single-block/dna/discrete/cost.data"
  , scriptCheckCost 1713
        "datasets/sankoff/single-block/dna/L1-norm/arthropods.pcg"
        "datasets/sankoff/single-block/dna/L1-norm/cost.data"
  , scriptCheckCost 914
        "datasets/sankoff/single-block/dna/1-2/arthropods.pcg"
        "datasets/sankoff/single-block/dna/1-2/cost.data"
  , scriptCheckCost 1789
        "datasets/sankoff/single-block/dna/2-1/arthropods.pcg"
        "datasets/sankoff/single-block/dna/2-1/cost.data"
  , scriptCheckCost 1143
        "datasets/sankoff/single-block/protein/discrete/invertebrates.pcg"
        "datasets/sankoff/single-block/protein/discrete/cost.data"
  , scriptCheckCost 11813
        "datasets/sankoff/single-block/protein/L1-norm/invertebrates.pcg"
        "datasets/sankoff/single-block/protein/L1-norm/cost.data"
  , scriptCheckCost 2012
        "datasets/sankoff/single-block/protein/1-2/invertebrates.pcg"
        "datasets/sankoff/single-block/protein/1-2/cost.data"
  , scriptCheckCost 1304
        "datasets/sankoff/single-block/protein/2-1/invertebrates.pcg"
        "datasets/sankoff/single-block/protein/2-1/cost.data"
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
  , scriptCheckCost 158
        "datasets/sankoff/single-block/large-mix/discrete/test.pcg"
        "datasets/sankoff/single-block/large-mix/discrete/cost.data"
  , scriptCheckCost 11004
        "datasets/sankoff/single-block/large-mix/L1-norm/test.pcg"
        "datasets/sankoff/single-block/large-mix/L1-norm/cost.data"
  , scriptCheckCost 158
        "datasets/sankoff/single-block/large-mix/1-2/test.pcg"
        "datasets/sankoff/single-block/large-mix/1-2/cost.data"
  , scriptCheckCost 251
        "datasets/sankoff/single-block/large-mix/2-1/test.pcg"
        "datasets/sankoff/single-block/large-mix/2-1/cost.data"
  , scriptCheckCost 348
        "datasets/sankoff/single-block/large-mix/hamming/test.pcg"
        "datasets/sankoff/single-block/large-mix/hamming/cost.data"
  , scriptCheckCost 200
        "datasets/sankoff/single-block/large-mix/levenshtein/test.pcg"
        "datasets/sankoff/single-block/large-mix/levenshtein/cost.data"
  , scriptCheckCost 117
        "datasets/sankoff/single-block/huge-mix/discrete/test.pcg"
        "datasets/sankoff/single-block/huge-mix/discrete/cost.data"
  , scriptCheckCost 12681
        "datasets/sankoff/single-block/huge-mix/L1-norm/test.pcg"
        "datasets/sankoff/single-block/huge-mix/L1-norm/cost.data"
  , scriptCheckCost 117
        "datasets/sankoff/single-block/huge-mix/1-2/test.pcg"
        "datasets/sankoff/single-block/huge-mix/1-2/cost.data"
  , scriptCheckCost 181
        "datasets/sankoff/single-block/huge-mix/2-1/test.pcg"
        "datasets/sankoff/single-block/huge-mix/2-1/cost.data"
  , scriptCheckCost 275
        "datasets/sankoff/single-block/huge-mix/hamming/test.pcg"
        "datasets/sankoff/single-block/huge-mix/hamming/cost.data"
  , scriptCheckCost 230
        "datasets/sankoff/single-block/huge-mix/levenshtein/test.pcg"
        "datasets/sankoff/single-block/huge-mix/levenshtein/cost.data"
  , scriptCheckCost 45
      "datasets/dynamic/multi-block/missing/missing.pcg"
      "datasets/dynamic/multi-block/missing/cost.data"
  , scriptCheckCost 2042
      "datasets/dynamic/multi-block/dna/arthropods.pcg"
      "datasets/dynamic/multi-block/dna/cost.data"
  , scriptCheckCost 28
      "datasets/dynamic/single-block/missing/missing-values.pcg"
      "datasets/dynamic/single-block/missing/cost.data"
  , scriptCheckCost 11036
      "datasets/dynamic/single-block/protein/L1-norm/invertebrates.pcg"
      "datasets/dynamic/single-block/protein/L1-norm/cost.data"
  , scriptCheckCost 1132
      "datasets/dynamic/single-block/protein/discrete/invertebrates.pcg"
      "datasets/dynamic/single-block/protein/discrete/cost.data"
  , scriptCheckCost 1948
        "datasets/dynamic/single-block/protein/1-2/invertebrates.pcg"
        "datasets/dynamic/single-block/protein/1-2/cost.data"
  , scriptCheckCost 1241
        "datasets/dynamic/single-block/protein/2-1/invertebrates.pcg"
        "datasets/dynamic/single-block/protein/2-1/cost.data"
  , scriptCheckCost 3413
      "datasets/dynamic/single-block/slashes/L1-norm/test.pcg"
      "datasets/dynamic/single-block/slashes/L1-norm/cost.data"
  , scriptCheckCost 197
        "datasets/dynamic/single-block/slashes/discrete/test.pcg"
        "datasets/dynamic/single-block/slashes/discrete/cost.data"
  , scriptCheckCost 254
        "datasets/dynamic/single-block/slashes/1-2/test.pcg"
        "datasets/dynamic/single-block/slashes/1-2/cost.data"
  , scriptCheckCost 228
        "datasets/dynamic/single-block/slashes/2-1/test.pcg"
        "datasets/dynamic/single-block/slashes/2-1/cost.data"
  , scriptCheckCost 671
        "datasets/dynamic/single-block/slashes/hamming/test.pcg"
        "datasets/dynamic/single-block/slashes/hamming/cost.data"
  , scriptCheckCost 488
        "datasets/dynamic/single-block/slashes/levenshtein/test.pcg"
        "datasets/dynamic/single-block/slashes/levenshtein/cost.data"
  , scriptCheckCost 133
        "datasets/dynamic/single-block/large-mix/discrete/test.pcg"
        "datasets/dynamic/single-block/large-mix/discrete/cost.data"
  , scriptCheckCost 7185
        "datasets/dynamic/single-block/large-mix/L1-norm/test.pcg"
        "datasets/dynamic/single-block/large-mix/L1-norm/cost.data"
  , scriptCheckCost 164
        "datasets/dynamic/single-block/large-mix/1-2/test.pcg"
        "datasets/dynamic/single-block/large-mix/1-2/cost.data"
  , scriptCheckCost 172
        "datasets/dynamic/single-block/large-mix/2-1/test.pcg"
        "datasets/dynamic/single-block/large-mix/2-1/cost.data"
  , scriptCheckCost 367
        "datasets/dynamic/single-block/large-mix/hamming/test.pcg"
        "datasets/dynamic/single-block/large-mix/hamming/cost.data"
  , scriptCheckCost 213
        "datasets/dynamic/single-block/large-mix/levenshtein/test.pcg"
        "datasets/dynamic/single-block/large-mix/levenshtein/cost.data"
  , scriptCheckCost 246
        "datasets/dynamic/single-block/huge-mix/discrete/test.pcg"
        "datasets/dynamic/single-block/huge-mix/discrete/cost.data"
  , scriptCheckCost 21753
        "datasets/dynamic/single-block/huge-mix/L1-norm/test.pcg"
        "datasets/dynamic/single-block/huge-mix/L1-norm/cost.data"
  , scriptCheckCost 325
        "datasets/dynamic/single-block/huge-mix/1-2/test.pcg"
        "datasets/dynamic/single-block/huge-mix/1-2/cost.data"
  , scriptCheckCost 284
        "datasets/dynamic/single-block/huge-mix/2-1/test.pcg"
        "datasets/dynamic/single-block/huge-mix/2-1/cost.data"
  , scriptCheckCost 872
        "datasets/dynamic/single-block/huge-mix/hamming/test.pcg"
        "datasets/dynamic/single-block/huge-mix/hamming/cost.data"
  , scriptCheckCost 698
        "datasets/dynamic/single-block/huge-mix/levenshtein/test.pcg"
        "datasets/dynamic/single-block/huge-mix/levenshtein/cost.data"
  , scriptFailure "datasets/unmatched-leaf-taxon/test.pcg"
  , scriptFailure "datasets/unmatched-tree-taxon/test.pcg"
  , scriptFailure "datasets/duplicate-leaf-taxon/test.pcg"
-- We omit this test because the DAG.unfoldr function in the ParsedForest call
-- will ensure that there is only one leaf in the graph. It may have multiple
-- parents however.
--  , scriptFailure "datasets/duplicate-tree-taxon/test.pcg"
  , scriptFailure "datasets/no-data-in-graph/test.pcg"
  ]


scriptCheckCost
  :: ExtendedReal -- ^ Expected cost ∈ [0, ∞]
  -> String       -- ^ Script File
  -> String       -- ^ Expected output file containing the cost
  -> TestTree
scriptCheckCost expectedCost scriptPath outputPath = testCase scriptPath $ do
    v <- runExecutable scriptPath [outputPath]
    case v of
      Left     exitCode -> assertFailure $ "Script failed with exit code: " <> show exitCode
      Right          [] -> assertFailure "No files were returned despite supplying one path!"
      Right (outData:_) -> case force $ parseCost outputPath outData of
                             Left  pErr -> assertFailure $ "No cost found in the output file!\n" <> pErr <> show outData
                             Right cost -> cost @?= expectedCost


parseCost
  :: FilePath -- ^ The path of the input file
  -> Text     -- ^ The text stream to parse
  -> Either String ExtendedReal
parseCost path str = first errorBundlePretty parseResult
  where
    parseResult :: Either (ParseErrorBundle Text Void) ExtendedReal
    parseResult = parse fileSpec path $ force str

    fileSpec =  many (try (ignoredLine <* notFollowedBy costLine))
             *> (try costLine <|> (ignoredLine *> costLine))

    costLine :: MonadParsec Void Text m => m ExtendedReal
    costLine =  many inlineSpace
             *> string' "DAG total cost:"
             *> many inlineSpace
             *> extendedReal
             <* ignoredLine

    extendedReal :: MonadParsec Void Text m => m ExtendedReal
    extendedReal = (fromFinite . toRealFloat <$> scientific) <|> (char '∞' $> infinity)

    ignoredLine  :: MonadParsec Void Text m => m ()
    ignoredLine  = many inlineChar *> newlineChar -- (newlineChar <|> eof)

    inlineSpace  :: MonadParsec Void Text m => m ()
    inlineSpace  = void $ satisfy (\x -> isSpace x && x /= '\n')

    inlineChar   :: MonadParsec Void Text m => m ()
    inlineChar   = void $ satisfy (/= '\n')

    newlineChar  :: (MonadParsec Void s m, Token s ~ Char) => m ()
    newlineChar  = void $ char '\n'


scriptFailure :: String -> TestTree
scriptFailure scriptPath = testCase scriptPath $ do
    v <- runExecutable scriptPath []
    assertBool "Expected script failure" $ isLeft v


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
--
{-
    dbFile <- openFile "output.debug" WriteMode
    hSetBuffering dbFile NoBuffering
    hPutStrLn dbFile startingDirectory
    hPutStrLn dbFile scriptDirectory
    hPutStrLn dbFile absScriptDirectory
    _      <- hClose dbFile
-}
    ctx <- constructProcess scriptStr
    (exitCode, _, _) <- readCreateProcessWithExitCode (process ctx) mempty
--                            (defineProcess absScriptDirectory command) mempty
--                            (defineProcess scriptDirectory command) mempty
    _ <- destructProcess ctx
    case exitCode of
      ExitFailure v -> pure  $ Left v
      _             -> force . Right <$> traverse readFile outputPaths
--  where
--    command :: String
--    command = "stack exec pcg -- --input " <> scriptFile <> " --output test.log"


{-
outLogFileName :: String
outLogFileName = "log.out"


errLogFileName :: String
errLogFileName = "log.err"


data  ScriptContext
    = ScriptContext
    { process :: CreateProcess
    , outPath :: FilePath
    , errPath :: FilePath
    }


constructProcess
  :: FilePath -- ^ Relative path to the PCG script
  -> IO ScriptContext
constructProcess scriptStr = do
    prefix <- makeAbsolute scriptDirectory
    let outFilePath = prefix </> outLogFileName
    let errFilePath = prefix </> errLogFileName
    let commandStr  = unwords
                    [ "stack exec pcg -- --input"
                    , scriptFileName
                    , "--output"
                    , outFilePath
                    , "2>"
                    , errFilePath
                    ]

    let p = CreateProcess
            { cmdspec            = ShellCommand commandStr
            , cwd                = Just scriptDirectory
            , env                = Nothing
            -- Do not use the stream handles, they do not work with Tasty
            , std_in             = NoStream
            , std_out            = NoStream
            , std_err            = NoStream
            , close_fds          = True
            , create_group       = False
            , delegate_ctlc      = False
            , detach_console     = False
            , create_new_console = False
            , new_session        = False
            , child_group        = Nothing
            , child_user         = Nothing
            , use_process_jobs   = False
            }

    pure ScriptContext
        { process = p
        , outPath = outFilePath
        , errPath = errFilePath
        }
  where
    (scriptDirectory, scriptFileName) = breakScriptPath scriptStr

    breakScriptPath = (normalise . foldl' (</>) defaultDirectory . init &&& last) . splitDirectories
      where
        defaultDirectory = "."


destructProcess :: ScriptContext -> IO ()
destructProcess ctx = mapM_ cleanUpHandle $
    [ (std_out . process &&& outPath)
    , (std_err . process &&& errPath)
    ] <*> [ ctx ]
  where
    cleanUpHandle (s, p) = do
        -- If we have an opened handle, close it
        _ <- case s of
               UseHandle h -> hClose h
               _           -> pure ()
        n <- getFileSize p
        if n == 0 -- If no data was written, delete the file
        then removeFile p
        else pure ()
-}
