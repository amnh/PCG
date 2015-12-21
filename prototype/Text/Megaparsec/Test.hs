{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
module Text.Megaparsec.Test
  ( testSuite
  ) where

import Data.Either.Combinators
import Data.List (permutations)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Text.Megaparsec
import Text.Megaparsec.Accum
import Text.Megaparsec.Prim (MonadParsec)

testSuite = testGroup "Accum Tests" []

testAssociativity = testProperty "associativity" f
  where
    f :: (Char,Char,Char,Char,Char) -> Bool
    f (a,b,c,d,e) = and $ parse' <$> dataSet
      where
        parse' (acc,inp) = isRight $ parse (makeAccumParser acc) "" inp
        dataSet = [ (x, y) | x <- parserPermutations, y <- streamPermutations ]
        streamPermutations = permutations [a,b,c,d,e]
        parserPermutations =
          [ makeAccumParser $ (,,,,) `v` (char a `w` char b `x` char c `y` char d `z` char e)
          | v <- fixCombinators
          , w <- addCombinators
          , x <- addCombinators
          , y <- addCombinators
          , z <- addCombinators
          ]

fixCombinators = [(<$?$>),(<$@$>),(<$*$>),(<$+$>)]
addCombinators = [(<|?|>),(<|@|>),(<|*|>),(<|+|>)]

instance MonadParsec s m Char => Arbitrary (m Char) where
  arbitrary = char <$> arbitrary

