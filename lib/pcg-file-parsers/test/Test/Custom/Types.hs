{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}

module Test.Custom.Types where

import Data.Char
import Test.Tasty.QuickCheck


newtype AsciiAlphaNum    = AsciiAlphaNum    { getAsciiAlphaNum    :: Char   } deriving (Eq)
newtype AsciiNonAlphaNum = AsciiNonAlphaNum { getAsciiNonAlphaNum :: Char   } deriving (Eq)
newtype InlineSpace      = InlineSpace      { getInlineSpaceChar  :: Char   } deriving (Eq)
newtype TextToken        = TextToken        { getTextToken        :: String } deriving (Eq)
newtype Whitespace       = Whitespace       { getWhitespaceChar   :: Char   } deriving (Eq)
newtype WordToken        = WordToken        { getWordToken        :: String } deriving (Eq)


instance Arbitrary AsciiAlphaNum where

    arbitrary = elements nonSpaceChars
      where
        nonSpaceChars = fmap AsciiAlphaNum . filter isAlphaNum  $ chr <$> [0..128]


instance Arbitrary AsciiNonAlphaNum where

    arbitrary = elements nonAlphaNumChars
      where
        nonAlphaNumChars = fmap AsciiNonAlphaNum . filter (not . isAlphaNum) $ chr <$> [0..128]


instance Arbitrary InlineSpace where

    arbitrary = elements inlineSpaceChars
      where
        inlineSpaceChars = InlineSpace <$> " \t"


instance Arbitrary TextToken where

    arbitrary = fmap TextToken . listOf1 $ elements textChar
      where
        textChar = filter (\x -> not (isControl x) && not (isSpace x)) $ chr <$> [0..128]


instance Arbitrary Whitespace where

    arbitrary = elements whitespaceChars
      where
        whitespaceChars = Whitespace <$> " \t\n\r\f\v"


instance Arbitrary WordToken where

    arbitrary = fmap WordToken . listOf1 $ elements wordChar
      where
        wordChar = filter isAlpha $ chr <$> [0..128]


instance Show AsciiAlphaNum where

    show (AsciiAlphaNum c) = show c


instance Show AsciiNonAlphaNum where

    show (AsciiNonAlphaNum c) = show c


instance Show InlineSpace where

    show (InlineSpace c) = show c


instance Show TextToken where

    show (TextToken s) = s


instance Show Whitespace where

    show (Whitespace c) = show c


instance Show WordToken where

    show (WordToken s) = s


