{-# LANGUAGE FlexibleContexts #-}

module Test.Custom.Parse
  ( parseEquals
  , parseFailure
  , parseSuccess
--  , parserFalsifies
  , parserSatisfies
  ) where

import           Data.Either
import qualified Data.Text             as T
import qualified Data.Text.Lazy        as LT
import           Data.Void
import           Test.Tasty.HUnit
import           Text.Megaparsec       (Parsec, TraversableStream, VisualStream, parse)
import           Text.Megaparsec.Error (errorBundlePretty)


{-# INLINEABLE parseEquals #-}
{-# SPECIALISE parseEquals :: (Eq a, Show a) => Parsec Void  T.Text a ->  T.Text -> a -> Assertion #-}
{-# SPECIALISE parseEquals :: (Eq a, Show a) => Parsec Void LT.Text a -> LT.Text -> a -> Assertion #-}
{-# SPECIALISE parseEquals :: (Eq a, Show a) => Parsec Void  String a ->  String -> a -> Assertion #-}
parseEquals :: (Eq a, Show a, Show s, TraversableStream s, VisualStream s) => Parsec Void s a -> s -> a -> Assertion
parseEquals parser input expected =
  case result of
    Left  x -> assertFailure $ errorBundlePretty x
    Right x -> assertEqual ("Parse result of :" <> show input) expected x
  where
    result = parse parser "" input


{-# INLINEABLE parseFailure #-}
{-# SPECIALISE parseFailure :: Parsec Void  T.Text a ->  T.Text -> Assertion #-}
{-# SPECIALISE parseFailure :: Parsec Void LT.Text a -> LT.Text -> Assertion #-}
{-# SPECIALISE parseFailure :: Parsec Void  String a ->  String -> Assertion #-}
parseFailure :: Show s => Parsec Void s a -> s -> Assertion
parseFailure parser input =
    assertBool ("Should have failed to parse input: " <> show input) $ isLeft result
  where
    result = parse parser "" input


{-# INLINEABLE parseSuccess #-}
{-# SPECIALISE parseSuccess :: Parsec Void  T.Text a ->  T.Text -> Assertion #-}
{-# SPECIALISE parseSuccess :: Parsec Void LT.Text a -> LT.Text -> Assertion #-}
{-# SPECIALISE parseSuccess :: Parsec Void  String a ->  String -> Assertion #-}
parseSuccess :: (TraversableStream s, VisualStream s) => Parsec Void s a -> s -> Assertion
parseSuccess parser input =
  case result of
    Left  x -> assertFailure $ errorBundlePretty x
    Right _ -> () @=? ()
  where
    result = parse parser "" input


{-# INLINEABLE parserSatisfies #-}
parserSatisfies :: Parsec Void s a -> s -> (a -> Bool) -> Bool
parserSatisfies parser input property =
  case parse parser "" input of
    Left  _ -> False
    Right x -> property x


{-
parserFalsifies :: (TraversableStream s, VisualStream s) => Parsec Void s a -> s -> (a -> Bool) -> Bool
parserFalsifies parser input property =
  case parse parser "" input of
    Left  _ -> True
    Right x -> property x
-}
