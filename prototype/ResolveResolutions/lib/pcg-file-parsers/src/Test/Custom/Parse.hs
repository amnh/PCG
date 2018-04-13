{-# LANGUAGE FlexibleContexts #-}

module Test.Custom.Parse
  ( parseEquals
  , parseFailure
  , parseSuccess
  , parserFalsifies
  , parserSatisfies
  ) where

import Data.Void
import Test.Tasty.HUnit
import Text.Megaparsec       (Parsec, parse)
import Text.Megaparsec.Error (parseErrorPretty)


parseEquals :: (Eq a, Show a) => Parsec Void String a -> String -> a -> Assertion
parseEquals parser input expected =
  case result of
    Left  x -> assertFailure $ parseErrorPretty x
    Right x -> assertEqual ("Parse result of :" ++ show input) expected x
  where
    result = parse parser "" input


parseFailure :: Parsec Void String a -> String -> Assertion
parseFailure parser input =
  case result of
    Right _ -> assertFailure $ "Should have failed to parse input: " ++ show input
    Left  _ -> assert True
  where
    result = parse parser "" input


parseSuccess :: Parsec Void String a -> String -> Assertion
parseSuccess parser input =
  case result of
    Left  x -> assertFailure $ parseErrorPretty x
    Right _ -> assert True
  where
    result = parse parser "" input


parserSatisfies :: Parsec Void String a -> String -> (a -> Bool) -> Bool
parserSatisfies parser input property =
  case parse parser "" input of
    Left  _ -> False
    Right x -> property x


parserFalsifies :: Parsec Void String a -> String -> (a -> Bool) -> Bool
parserFalsifies parser input property =
  case parse parser "" input of
    Left  _ -> True
    Right x -> property x
