{-# LANGUAGE FlexibleContexts #-}

module File.Format.TransitionCostMatrix.Test
  ( testSuite
  ) where

import Data.Foldable                           (toList)
import Data.List.NonEmpty                      (fromList)
import File.Format.TransitionCostMatrix.Parser
import Test.Custom.Parse                       (parseEquals,parseFailure,parseSuccess)
import Test.Tasty                              (TestTree,testGroup)
import Test.Tasty.HUnit
import Text.Megaparsec                         (eof)

testSuite :: TestTree
testSuite = testGroup "TCM Format"
  [ testGroup "TCM Combinators"
      [tcmAlphabet', tcmMatrix']
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
  , unwords $ pure <$> ['a'..'z']
  , unwords $ pure <$> ['0'..'9']
  ]

invalidAlphabets :: [String]
invalidAlphabets = appendNewlines
  [ ""            -- empty line
  , "a a"         -- duplicate entries
  , "a b c a b a" -- many duplicate entries
  ]

tcmAlphabet' :: TestTree
tcmAlphabet' = testGroup "tcmAlphabet" [validLines, invalidLines]
  where
    validLines   = testGroup "Valid alphabet definition"   $ success <$> validAlphabets
    invalidLines = testGroup "Invalid alphabet definition" $ failure <$> invalidAlphabets
    success str  = testCase (show str) $ parseEquals   (tcmAlphabet <* eof) str (fromList . words $ toList str)
    failure str  = testCase (show str) $ parseFailure  (tcmAlphabet <* eof) str

tcmMatrix' :: TestTree
tcmMatrix' = testGroup "tcmMatrix" [validBlocks, invalidBlocks]
  where
    validBlocks   = testGroup "Valid matrix block"   $ success <$> validMatricies
    invalidBlocks = testGroup "Invalid matrix block" $ failure <$> invalidMatricies
    success str   = testCase (show str) $ parseSuccess  (tcmMatrix <* eof) str
    failure str   = testCase (show str) $ parseFailure  (tcmMatrix <* eof) str

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
tcmStreamParser' = testGroup "tcmStreamParser" [valid, invalid]
  where
    valid          = testGroup "Valid TCM streams"          $ success <$> validStreams
    invalid        = testGroup "Invalid invalid TCM stream" $ failure <$> invalidStreams
    success str    = testCase (show str) $ parseSuccess  (tcmStreamParser <* eof) str
    failure str    = testCase (show str) $ parseFailure  (tcmStreamParser <* eof) str
    validStreams   = ["a b c\n1 2 3 4\n5 6 7 8\n9 0 9 8\n7 6 5 4\n\n"]
    invalidStreams = ["a b c\n1 2 3\n4 5 6\n7 8 9\n"]

appendNewlines :: [String] -> [String]
appendNewlines = fmap (++"\n")
