module Test.Custom
  ( parseEquals
  , parseFailure
  , parseSuccess
  ) where

import Test.Tasty.HUnit
import Text.Parsec

parseSuccess :: Parsec String () a -> String -> Assertion
parseSuccess parser str =
  case result of
    Left  x -> assertFailure $ show x
    Right _ -> assert True
  where
    result = parse parser "" str

parseFailure :: Parsec String () a -> String -> Assertion
parseFailure parser str =
  case result of
    Right _ -> assertFailure $ "Should have failed to parse input: " ++ show str
    Left  _ -> assert True
  where
    result = parse parser "" str

parseEquals :: (Eq a, Show a) => Parsec String () a -> String -> a -> Assertion
parseEquals parser str expected =
  case result of
    Left  x -> assertFailure $ show x
    Right x -> assertEqual ("Parse result of :" ++ show str) expected x
  where
    result = parse parser "" str

