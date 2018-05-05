{-# LANGUAGE FlexibleContexts #-}

module Test.Custom.Parse
  ( parseEquals
  , parseFailure
  , parseSuccess
  , parserFalsifies
  , parserSatisfies
  ) where

import Data.Either
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
    assertBool ("Should have failed to parse input: " ++ show input) $ isLeft result
  where
    result = parse parser "" input


parseSuccess :: Parsec Void String a -> String -> Assertion
parseSuccess parser input =
  case result of
    Left  x -> assertFailure $ parseErrorPretty x
    Right _ -> () @=? ()
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
