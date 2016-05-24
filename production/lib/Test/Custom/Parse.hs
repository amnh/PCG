{-# LANGUAGE FlexibleContexts #-}

module Test.Custom.Parse
  ( parseEquals
  , parseFailure
  , parseSuccess
  , parserFalsifies
  , parserSatisfies
  , module Test.Custom.Types
  ) where

import Test.Custom.Types
import Test.Tasty.HUnit
import Text.Megaparsec       (Dec, Parsec, parse)
import Text.Megaparsec.Error (parseErrorPretty)

parseEquals :: (Eq a, Show a) => Parsec Dec String a -> String -> a -> Assertion
parseEquals parser input expected =
  case result of
    Left  x -> assertFailure $ parseErrorPretty x
    Right x -> assertEqual ("Parse result of :" ++ show input) expected x
  where
    result = parse parser "" input

parseFailure :: Parsec Dec String a -> String -> Assertion
parseFailure parser input =
  case result of
    Right _ -> assertFailure $ "Should have failed to parse input: " ++ show input
    Left  _ -> assert True
  where
    result = parse parser "" input

parseSuccess :: Parsec Dec String a -> String -> Assertion
parseSuccess parser input =
  case result of
    Left  x -> assertFailure $ parseErrorPretty x
    Right _ -> assert True
  where
    result = parse parser "" input

parserSatisfies :: Parsec Dec String a -> String -> (a -> Bool) -> Bool
parserSatisfies parser input property =
  case parse parser "" input of
    Left  _ -> False
    Right x -> property x

parserFalsifies :: Parsec Dec String a -> String -> (a -> Bool) -> Bool
parserFalsifies parser input property =
  case parse parser "" input of
    Left  _ -> True
    Right x -> property x
