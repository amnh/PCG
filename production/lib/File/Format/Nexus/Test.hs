{-# LANGUAGE FlexibleContexts #-}

module File.Format.Nexus.Test
  ( testSuite
  ) where

import Control.Monad              (join)
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
      [booleanDefinition', stringDefinition', quotedStringDefinition', ignoredSubBlockDef']
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
            key = getAsciiAlphaNum <$> getNonEmpty x
            val = getAsciiAlphaNum <$> getNonEmpty y
            str = key ++ "=" ++ val
    rejectsKeywords = testProperty "Rejects Keywords" f
      where
        f :: (NonEmptyList AsciiAlphaNum, NexusKeyword) -> Bool
        f (x,y) = isLeft $ parse (stringDefinition key <* eof) "" str
          where
            key = getAsciiAlphaNum <$> getNonEmpty x
            val = getNexusKeyword y
            str = key ++ "=" ++ val

quotedStringDefinition' :: TestTree
quotedStringDefinition' = testGroup "quotedStringDefinition" [generalProperty, missingCloseQuote, rejectsKeywords]
  where
    generalProperty = testProperty "General quoted string definition: key=\"space delimited values\", capture values" f
      where
        f :: (NonEmptyList AsciiAlphaNum, NonEmptyList Char) -> Bool
        f (x,y) = null res || parse (quotedStringDefinition key <* eof) "" str == Right (Right res)
          where
            key = getAsciiAlphaNum <$> getNonEmpty x
            val = filter (`notElem` bad) $ getNonEmpty y
            res = words val
            str = key ++ "=\"" ++ val ++ "\""
            bad = "[;\""
    missingCloseQuote = testProperty "Missing close quote" f
      where
        f :: (NonEmptyList AsciiAlphaNum, NonEmptyList AsciiAlphaNum) -> Bool
        f (x,y) = (isLeft <$> parse (quotedStringDefinition key <* eof) "" str) == Right True
          where
            key = getAsciiAlphaNum <$> getNonEmpty x
            val = getAsciiAlphaNum <$> getNonEmpty y
            str = key ++ "=\"" ++ val
    rejectsKeywords = testProperty "Rejects keywords" f
      where
        f :: (NonEmptyList AsciiAlphaNum, NexusKeyword) -> Bool
        f (x,y) = isLeft $ parse (quotedStringDefinition key <* eof) "" str
          where
            key = getAsciiAlphaNum <$> getNonEmpty x
            val = getNexusKeyword y
            str = key ++ "=\"" ++ val ++ "\""

ignoredSubBlockDef' :: TestTree
ignoredSubBlockDef' = testGroup "ignoredSubBlockDef" [endTest {-, semicolonTest, argumentTest-}]
    where
        endTest = testProperty "Block ends with \"end;\"" f
            where
                f :: NonEmptyList AsciiAlphaNum -> Bool
                f x = parse (ignoredSubBlockDef ';' <* eof) "" inp == Right x'
                    where
                        x' = (getAsciiAlphaNum <$> getNonEmpty x)
                        inp = x' ++ " end;"

newtype AsciiAlphaNum = AsciiAlphaNum Char deriving (Eq)

getAsciiAlphaNum (AsciiAlphaNum c) = c
nonSpaceChars = fmap AsciiAlphaNum . filter isAlphaNum $ chr <$> [0..128]

--createStringList :: [AsciiAlphaNum] -> ([[AsciiAlphaNum]], [AsciiAlphaNum])
--createStringList ys = (vals, valLst)
--    where
--        vals = 
--        valLst = fmap AsciiAlphaNum . foldr (\x acc -> x ++ (sublistOf " \t\n\r")  acc) [] ys

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
