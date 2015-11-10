{-# LANGUAGE FlexibleContexts #-}

module File.Format.Nexus.Test
  ( testSuite
  ) where

import Data.Char
import Data.Either.Custom         (isLeft,isRight,rightMay)
import Data.Set                   (toList)
import File.Format.Nexus.Parser
import Test.Custom                (parseEquals,parseFailure,parseSuccess)
import Test.Tasty                 (TestTree,testGroup)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Text.Megaparsec            (eof,parse)

testSuite :: TestTree
testSuite = testGroup "Nexus Format"
  [ testGroup "Nexus Combinators"
      [booleanDefinition', stringDefinition']
  ]

booleanDefinition' :: TestTree
booleanDefinition' = testGroup "booleanDefinition" [generalProperty]
  where
    generalProperty :: TestTree
    generalProperty = testProperty "General boolean flag structure" f
      where
        f x = null x
           || parse (booleanDefinition x <* eof) "" x == Right True

stringDefinition' :: TestTree
stringDefinition' = testGroup "stringDefinition" [generalProperty, rejectsKeywords]
  where
    generalProperty = testProperty "General string definition: key=value, capture value" f
      where
        f :: (NonEmptyList AsciiAlphaNum, NonEmptyList AsciiAlphaNum) -> Bool
        f (x,y) = parse (stringDefinition key <* eof) "" str == Right val
          where
            key = getNonSpace <$> getNonEmpty x
            val = getNonSpace <$> getNonEmpty y
            str = key ++ "=" ++ val
    rejectsKeywords = testProperty "" f
      where
        f :: (NonEmptyList AsciiAlphaNum, NexusKeyword) -> Bool
        f (x,y) = isLeft $ parse (stringDefinition key <* eof) "" str
          where
            key = getNonSpace <$> getNonEmpty x
            val = getNexusKeyword y
            str = key ++ "=" ++ val

y
        

newtype AsciiAlphaNum = AsciiAlphaNum Char deriving (Eq)

getNonSpace (AsciiAlphaNum c) = c
nonSpaceChars = fmap AsciiAlphaNum . filter isAlphaNum $ chr <$> [0..128]

instance Arbitrary AsciiAlphaNum where
  arbitrary = elements nonSpaceChars

instance Show AsciiAlphaNum where
  show (AsciiAlphaNum c) = show c

newtype NexusKeyword = NexusKeyword String deriving (Eq)

instance Arbitrary NexusKeyword where
  arbitrary = elements . fmap NexusKeyword $ toList nexusKeywords

instance Show NexusKeyword where
  show (NexusKeyword c) = show c

getNexusKeyword (NexusKeyword c) = c
