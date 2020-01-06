{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Text.Megaparsec.Custom.Test
  ( testSuite
  ) where
-- We only export the testSuite
-- This function can be combined with other testSuites definitions from other
-- Test modules to form a giant test suite to run

import Data.Either
import Data.List              (nub, sort)
import Data.Proxy
import Data.Void
import Safe                   (readMay)
import Test.Custom.Parse
import Test.SmallCheck.Series ()
import Test.Tasty             (TestTree, testGroup)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Custom


-- |
-- Concrete types in the parameterized functions.
testingParse :: Parsec Void s a -> String -> s -> Either (ParseErrorBundle s Void) a
testingParse = parse


-- |
-- Coalesce the many TestTrees to a single TestTree
testSuite :: TestTree
testSuite = testGroup "Custom Parsec Combinator Tests" tests


-- |
-- A list of all tests in represented as TestTrees
tests :: [TestTree]
tests = [ testGroup "Double Parsing"             [decimalProperties]
        , testGroup "Inline Space Parsing"       [inlinedSpaceCharAssertions, inlinedSpaceAssertions]
        , testGroup "Combinator 'anythingTill'"  [ anythingTillProperties]
        , testGroup "Combinator 'somethingTill'" [somethingTillProperties]
        , testGroup "Combinator 'endOfLine'"     [endOfLineAssertions]
        , testGroup "Combinator 'fails'"         [failsProperties]
        ]


decimalProperties :: TestTree
decimalProperties = testGroup "Arbitrary Double Tests"
    [ testProperty "Injectivity"  decimalInjection
    , testProperty "Surjectivity" decimalSurjection
    ]


-- |
-- Ensure that all Doubles represented as Strings are correctly parsed as Doubles.
-- The parser should never fail to testingParse a String representation of an Int.
-- NOTE: This doesn't work due to the strangeness of decimal's precision
decimalSurjection :: Double -> Bool
decimalSurjection x = Right x  == testingParse double "" (show x)


-- |
-- Ensure that all Strings which can be `read` as a Double are parsed as Double.
-- The parser should always fail to testingParse a String that cannot be read as
-- an Double.
decimalInjection :: String -> Bool
decimalInjection x =
    case readMay x :: Maybe Double of
      Nothing  -> True
      Just res -> testingParse (space *> double <* space <* eof) "" x == Right res


inlinedSpaceCharAssertions :: TestTree
inlinedSpaceCharAssertions = testGroup "Inline Space Char Assertions" [validInlineSpace, invalidInlineSpace]
  where
    validInlineSpace = testGroup "Valid inlinedSpaceChar"
        [ testCase "space" $ parseEquals (inlinedSpaceChar <* eof) " "  ' '
        , testCase "tab"   $ parseEquals (inlinedSpaceChar <* eof) "\t" '\t'
        , testCase "vtab"  $ parseEquals (inlinedSpaceChar <* eof) "\v" '\v'
        ]

    invalidInlineSpace = testGroup "Invalid inlinedSpaceChar"
        [ testCase "newline"        $ parseFailure (inlinedSpaceChar <* eof) "\n"
        , testCase "caraige return" $ parseFailure (inlinedSpaceChar <* eof) "\r"
        ]


inlinedSpaceAssertions :: TestTree
inlinedSpaceAssertions = testGroup "Inline Space Assertions" [validInlineSpace, invalidInlineSpace]
  where
    validInlineSpace = testGroup "Valid inlinedSpace"
        [ testCase "Consumes multiple spaces" $ parseSuccess (inlinedSpace <* eof) " \t\v"
        , testCase "Consumes spaces up to a newline" $ mapM_ parseIt exampleInputs
        ]

    invalidInlineSpace = testGroup "Invalid inlinedSpace"
        [ testCase "newline"        $ parseFailure (inlinedSpace <* eof) "\n"
        , testCase "caraige return" $ parseFailure (inlinedSpace <* eof) "\r"
        ]

    parseIt (inlines,line) = parseSuccess (inlinedSpace <* string line <* eof) (inlines <> line)
    exampleSpaces     = "\t\v "
    exampleNewlines   = "\n\r"
    exampleInputs = [ ([x,y],[z]) | x <- exampleSpaces, y <- exampleSpaces, z <- exampleNewlines ]


anythingTillProperties :: TestTree
anythingTillProperties = testGroup "Properties"
    [ emptySuccess
    , properConsumption
    ]
  where
    properConsumption = testProperty "Consumes up to 'stop mark'" f
      where
        f :: (NonEmptyList Char, Char, NonEmptyList Char) -> Bool
        f (prefix, delimiter, suffix) = testingParse (anythingTill stopMark <* stopMark <* remaining <* eof) "" stream == Right prefix'
          where
            (stopMark, prefix', stream, remaining) = getConsumtionComponents prefix delimiter suffix

    emptySuccess = testProperty "Succeed when presented with just the 'stop mark'" f
      where
        f :: NonEmptyList Char -> Bool
        f delimiter = testingParse (anythingTill stopMark <* stopMark <* eof) "" stream == Right ""
          where
            stream   = getNonEmpty delimiter
            stopMark = string stream


somethingTillProperties :: TestTree
somethingTillProperties = testGroup "Properties"
    [ emptyFailure
    , properConsumption
    , emptyCharFailure
    ]
  where
    properConsumption = testProperty "Consumes up to 'stop mark'" f
      where
        f :: (NonEmptyList Char, Char, NonEmptyList Char) -> Bool
        f (prefix, delimiter, suffix) = null prefix'
                                     || testingParse (somethingTill stopMark <* stopMark <* remaining <* eof) "" stream == Right prefix'
          where
            (stopMark, prefix', stream, remaining) = getConsumtionComponents prefix delimiter suffix

    emptyFailure = testProperty "Fail when presented with just the 'stop mark'" f
      where
        f :: NonEmptyList Char -> Bool
        f delimiter = isLeft $ testingParse (somethingTill stopMark <* stopMark <* eof) "" stream
          where
            stream   = getNonEmpty delimiter
            stopMark = string stream

    emptyCharFailure = testProperty "Fail on leading single Char 'stop mark'" f
      where
        f :: (NonEmptyList Char, Char) -> Bool
        f (buffer, delimiter) = isLeft $ testingParse (somethingTill stopMark) "" stream
          where
            stopMark = char delimiter
            buffer'  = getNonEmpty buffer
            stream   = [delimiter] <> buffer' <> [delimiter]


-- |
-- We abstract this construction code for testing proper consumption between 'anythingTill' and 'somethingTill' test-suites
getConsumtionComponents
  :: forall e s m.
  ( MonadParsec e s m
  , Token s ~ Char
  )
  => NonEmptyList Char
  -> Char
  -> NonEmptyList Char
  -> (m (Token s), Tokens s, Tokens s, m (Tokens s))
getConsumtionComponents prefix delimiter suffix =
    (stopMark, toTokens prefix', toTokens stream, remaining)
  where
    toTokens  = tokensToChunk (Proxy :: Proxy s)
    stopMark  = char delimiter
    prefix'   = filter (/= delimiter) $ getNonEmpty prefix
    suffix'   = getNonEmpty suffix
    stream    = prefix' <> [delimiter] <> suffix'
    remaining = string $ toTokens suffix'


endOfLineAssertions :: TestTree
endOfLineAssertions = testGroup "Assertions" [matchesUnix, matchesWindows, matchesOldMac]
  where
    matchesUnix    = testCase "Succeeds on Unix format '\\n'"         $ parseSuccess (endOfLine <* eof) "\n"
    matchesWindows = testCase "Succeeds on Windows format \"\\r\\n\"" $ parseSuccess (endOfLine <* eof) "\r\n"
    matchesOldMac  = testCase "Succeeds on Mac format '\\r'"          $ parseSuccess (endOfLine <* eof) "\r"


failsProperties :: TestTree
failsProperties = testGroup "Property" [failsProperty]
  where
    failsProperty = testProperty "Arbitrary strings are lifted to testingParse errors" f
      where
        f :: NonEmptyList (NonEmptyList Char) -> Bool
        f randomMessages =
            case testingParse (fails errors <* eof) "" "" of
              Left  _ -> True -- It better fail!
              Right _ -> False
          where
            errors = nub . sort . getNonEmpty $ getNonEmpty <$> randomMessages


{-
commentAssertions :: TestTree
commentAssertions = testGroup "" []
  where
    f :: (NonEmpty Char, NonEmpty Char, NonEmpty Char) ->

-}
