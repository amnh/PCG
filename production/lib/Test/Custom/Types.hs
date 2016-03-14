{-# LANGUAGE FlexibleContexts #-}

module Test.Custom.Types where

import Data.Char
import Test.QuickCheck

newtype AsciiAlphaNum    = AsciiAlphaNum    { getAsciiAlphaNum    :: Char   } deriving (Eq)
newtype AsciiNonAlphaNum = AsciiNonAlphaNum { getAsciiNonAlphaNum :: Char   } deriving (Eq)
newtype Whitespace       = Whitespace       { getWhitespaceChar   :: Char   } deriving (Eq)
newtype InlineSpace      = InlineSpace      { getInlineSpaceChar  :: Char   } deriving (Eq)
-- | A 'TextToken' consists of one or more printable non-space characters
newtype TextToken        = TextToken        { getTextToken        :: String } deriving (Eq)
-- | A 'WordToken' consists of one or more alphabetic characters
newtype WordToken        = WordToken        { getWordToken        :: String } deriving (Eq)

instance Arbitrary TextToken where
  arbitrary = fmap TextToken . listOf1 $ elements textChar
    where
      textChar = filter (\x -> not (isControl x) && not (isSpace x)) $ chr <$> [0..128]

instance Show TextToken where
  show (TextToken s) = s

instance Arbitrary WordToken where
  arbitrary = fmap WordToken . listOf1 $ elements wordChar
    where
      wordChar = filter isAlpha $ chr <$> [0..128]

instance Show WordToken where
  show (WordToken s) = s

instance Arbitrary AsciiAlphaNum where
  arbitrary = elements nonSpaceChars
    where
      nonSpaceChars = fmap AsciiAlphaNum . filter isAlphaNum  $ chr <$> [0..128]

instance Show AsciiAlphaNum where
  show (AsciiAlphaNum c) = show c

instance Arbitrary AsciiNonAlphaNum where
  arbitrary = elements nonAlphaNumChars
    where
      nonAlphaNumChars = fmap AsciiNonAlphaNum . filter (not . isAlphaNum) $ chr <$> [0..128]

instance Show AsciiNonAlphaNum where
  show (AsciiNonAlphaNum c) = show c

instance Arbitrary Whitespace where
  arbitrary = elements whitespaceChars
    where
      whitespaceChars = Whitespace <$> " \t\n\r\f\v"

instance Show Whitespace where
  show (Whitespace c) = show c

instance Arbitrary InlineSpace where
  arbitrary = elements inlineSpaceChars
    where
      inlineSpaceChars = InlineSpace <$> " \t"

instance Show InlineSpace where
    show (InlineSpace c) = show c
