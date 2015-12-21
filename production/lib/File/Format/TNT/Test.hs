{-# LANGUAGE FlexibleContexts #-}

module File.Format.TNT.Test
  ( testSuite
  ) where

import Control.Monad              (join)
import Data.Char
import Data.Either.Combinators    (isLeft,isRight)
import qualified Data.Map as M
import Data.Set                   (toList)
import File.Format.TNT.Parser
import Test.Custom                (parseEquals,parseFailure,parseSuccess)
import Test.Tasty                 (TestTree,testGroup)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Text.Megaparsec            (char,eof,parse,string)

testSuite :: TestTree
testSuite = testGroup "TNT Format"
  [ testGroup "TNT Combinators" [ xreadHeader'
                                ] 
  ]

xreadHeader' = testGroup "XREAD header" [beginsWithXREAD, possibleComment]
  where
    beginsWithXREAD = testCase     "Begins with XREAD"           $ parseSuccess xreadHeader "XREAD\n;"
    possibleComment = testProperty "Possibly contains a comment" f
      where
        f :: NonEmptyList Char -> Bool
        f x = isRight $ parse xreadHeader "" input
          where
            input   = "XREAD '" ++ comment ++ "'"
            comment = filter (/= '\'') $ getNonEmpty x

flexiblePositiveInt' = testGroup "Positive Int parsed flexibly" [parsesInts, parsesIntegralDoubles]
  where
    parsesInts = testProperty "Parses positive, signed Integer literals" f
      where
        f :: Int -> Bool
        f x = (x > 0) == isRight (parse (flexiblePositiveInt "") "" $ show x)
    parsesIntegralDoubles = testProperty "Parses positive, signed integral valued Doubles" f
      where
        f :: Int -> Bool
        f x = (x > 0) == isRight (parse (flexiblePositiveInt "") "" $ show (fromIntegral x :: Double))
