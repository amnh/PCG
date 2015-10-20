{-# LANGUAGE FlexibleContexts #-}

module File.Format.TransitionCostMatrix.Test
  ( testSuite
  ) where

import Data.List                               (intercalate)
import File.Format.TransitionCostMatrix.Parser
import Test.Custom                             (parseEquals,parseFailure,parseSuccess)
import Test.Tasty                              (TestTree,testGroup)
import Test.Tasty.HUnit
import Text.Parsec                             (eof)

testSuite :: TestTree
testSuite = testGroup "TCM Format"
  [ testGroup "TCM Combinators"
      [alphabetLine',matrixBlock']
  , testGroup "TCM Parser" 
      [tcmStreamParser']
  , testGroup "TCM Converter"
      []
  ]


validAlphabets :: [String]
validAlphabets = appendNewlines
  [ "1 2 3"
  , "a b c"
  , "do re mi"
  , "\\alpha \\beta \\gamma"
  , "Wiskey Tango Foxtrot"
  , intercalate " " $ pure <$> ['a'..'z']
  , intercalate " " $ pure <$> ['0'..'9']
  ]

invalidAlphabets :: [String]
invalidAlphabets = appendNewlines
  [ ""            -- empty line
  , "a a"         -- duplicate entries
  , "a b c a b a" -- many duplicate entries
  ]

alphabetLine' :: TestTree
alphabetLine' = testGroup "alphabetLine" [validLines,invalidLines]
  where
    validLines   = testGroup "Valid unquoted labels"   $ success <$> validAlphabets
    invalidLines = testGroup "Invalid unquoted labels" $ failure <$> invalidAlphabets
    success str  = testCase (show str) $ parseEquals   (alphabetLine <* eof) str (words str)
    failure str  = testCase (show str) $ parseFailure  (alphabetLine <* eof) str

matrixBlock' :: TestTree
matrixBlock' = testGroup "matrixBlock" [validBlocks,invalidBlocks]
  where
    validBlocks   = testGroup "Valid unquoted labels"   $ success <$> validMatricies
    invalidBlocks = testGroup "Invalid unquoted labels" $ failure <$> invalidMatricies
    success str   = testCase (show str) $ parseSuccess  (matrixBlock <* eof) str
    failure str   = testCase (show str) $ parseFailure  (matrixBlock <* eof) str

validMatricies :: [String]
validMatricies = appendNewlines
  [ "1 2 3 \n 4 5 6\n7 8 9\n"
  , "1.0 1.0\n1.0 0.0"
  ]

invalidMatricies :: [String]
invalidMatricies = appendNewlines
  [ ""              -- empty matrix
  , "1 2 3\n 4 5 6" -- not square matrix
  , "1 2 3\n4 5"    -- inconsistent column length
  ]

tcmStreamParser' :: TestTree
tcmStreamParser' = testGroup "tcmStreamParser" [valid,invalid]
  where
    valid          = testGroup "Valid TCM streams"          $ success <$> validStreams
    invalid        = testGroup "Invalid invalid TCM stream" $ failure <$> invalidStreams
    success str    = testCase (show str) $ parseSuccess  (tcmStreamParser <* eof) str
    failure str    = testCase (show str) $ parseFailure  (tcmStreamParser <* eof) str
    validStreams   = ["a b c\n1 2 3 4\n5 6 7 8\n9 0 9 8\n7 6 5 4\n\n"]
    invalidStreams = ["a b c\n1 2 3\n4 5 6\n7 8 9\n"]

appendNewlines :: [String] -> [String]
appendNewlines = fmap (\x -> x ++"\n")
