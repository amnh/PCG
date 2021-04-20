-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Megaparsec.Custom
-- Copyright   :  (c) 2015-2021 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Custom utility combinators for 'Text.Megaparsec' parser construction.
--
-----------------------------------------------------------------------------

{-# LANGUAGE ApplicativeDo       #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Text.Megaparsec.Custom
  ( anythingTill
  , double
  , endOfLine
  , fails
  , noneOfThese
  , someOfThese
  , somethingTill
  , string''
  ) where

import           Data.CaseInsensitive       (FoldCase)
import           Data.Foldable
import           Data.Functor               (($>))
import           Data.List                  (sort)
import           Data.List.NonEmpty         (nonEmpty)
import           Data.Maybe                 (mapMaybe)
import           Data.Proxy
import qualified Data.Set                   as S
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as LT
import           Data.Vector.Unboxed        (Unbox, Vector, (!))
import qualified Data.Vector.Unboxed        as V
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as LEX


-- |
-- Parse a string-like chunk.
{-# INLINEABLE string'' #-}
{-# SPECIALISE string'' :: String -> Parsec Void  T.Text  T.Text #-}
{-# SPECIALISE string'' :: String -> Parsec Void LT.Text LT.Text #-}
{-# SPECIALISE string'' :: String -> Parsec Void  String  String #-}
string'' :: forall e s m. (FoldCase (Tokens s), MonadParsec e s m, Token s ~ Char) => String -> m (Tokens s)
string'' = string' . tokensToChunk (Proxy :: Proxy s)


-- |
-- @anythingTill end@ consumes zero or more characters until @end@ is matched,
-- leaving @end@ in the stream.
{-# INLINEABLE anythingTill #-}
{-# SPECIALISE anythingTill :: Parsec Void  T.Text a -> Parsec Void  T.Text String #-}
{-# SPECIALISE anythingTill :: Parsec Void LT.Text a -> Parsec Void LT.Text String #-}
{-# SPECIALISE anythingTill :: Parsec Void  String a -> Parsec Void  String String #-}
anythingTill :: MonadParsec e s m => m a -> m [Token s]
anythingTill c = do
    ahead <- optional . try $ lookAhead c
    case ahead of
      Just _  -> pure []
      Nothing -> somethingTill c


-- |
-- @somethingTill end@ consumes one or more characters until @end@ is matched,
-- leaving @end@ in the stream.
{-# INLINEABLE somethingTill #-}
{-# SPECIALISE somethingTill :: Parsec Void  T.Text a -> Parsec Void  T.Text String #-}
{-# SPECIALISE somethingTill :: Parsec Void LT.Text a -> Parsec Void LT.Text String #-}
{-# SPECIALISE somethingTill :: Parsec Void  String a -> Parsec Void  String String #-}
somethingTill :: MonadParsec e s m => m a -> m [Token s]
somethingTill c = do
    _ <- notFollowedBy c
    (:) <$> anySingle <*> anythingTill c


-- |
-- Matches one or more elements from the supplied collection.
--
-- Coerces the collection to a sorted, unboxed vector and performs a binary
-- search on the elements to determine if a 'Token s' is part of the collection.
--
-- Preferable to 'Text.Megaparsec.someOf'.
{-# INLINE someOfThese #-}
{-# SPECIALISE someOfThese :: Foldable f => f Char -> Parsec Void  T.Text  T.Text #-}
{-# SPECIALISE someOfThese :: Foldable f => f Char -> Parsec Void LT.Text LT.Text #-}
{-# SPECIALISE someOfThese :: Foldable f => f Char -> Parsec Void  String  String #-}
{-# SPECIALISE someOfThese :: String -> Parsec Void  T.Text  T.Text #-}
{-# SPECIALISE someOfThese :: String -> Parsec Void LT.Text LT.Text #-}
{-# SPECIALISE someOfThese :: String -> Parsec Void  String  String #-}
someOfThese :: (Foldable f, MonadParsec e s m, Token s ~ a, Unbox a) => f a -> m (Tokens s)
someOfThese xs =
    let !uvec = V.fromList . sort $ toList xs
        !cond = withinVec uvec
    in  takeWhile1P Nothing cond


-- |
-- Matches one or more elements /not/ from the supplied collection.
--
-- Coerces the collection to a sorted, unboxed vector and performs a binary
-- search on the elements to determine if a 'Token s' is part of the collection.
--
-- Preferable to 'noneOf'.
{-# INLINE noneOfThese #-}
{-# SPECIALISE noneOfThese :: Foldable f => f Char -> Parsec Void  T.Text  T.Text #-}
{-# SPECIALISE noneOfThese :: Foldable f => f Char -> Parsec Void LT.Text LT.Text #-}
{-# SPECIALISE noneOfThese :: Foldable f => f Char -> Parsec Void  String  String #-}
{-# SPECIALISE noneOfThese :: String -> Parsec Void  T.Text  T.Text #-}
{-# SPECIALISE noneOfThese :: String -> Parsec Void LT.Text LT.Text #-}
{-# SPECIALISE noneOfThese :: String -> Parsec Void  String  String #-}
noneOfThese :: (Foldable f, MonadParsec e s m, Token s ~ a, Unbox a) => f a -> m (Tokens s)
noneOfThese xs =
    let !uvec = V.fromList . sort $ toList xs
        !cond = not . withinVec uvec
    in  takeWhile1P Nothing cond


-- |
-- Flexibly parses a 'Double' value represented in a variety of forms.
{-# INLINEABLE double #-}
{-# SPECIALISE double :: Parsec Void  T.Text Double #-}
{-# SPECIALISE double :: Parsec Void LT.Text Double #-}
{-# SPECIALISE double :: Parsec Void  String Double #-}
double :: (MonadParsec e s m, Token s ~ Char) => m Double
double = try real <|> fromIntegral <$> int
  where
     int  :: (MonadParsec e s m, Token s ~ Char) => m Integer
     int  = LEX.signed space LEX.decimal
     real = LEX.signed space LEX.float


-- |
-- Custom 'eol' combinator to account for /very/ old Mac file formats ending
-- lines in a single @\'\\r\'@.
{-# INLINE endOfLine #-}
{-# SPECIALISE endOfLine :: Parsec Void  T.Text () #-}
{-# SPECIALISE endOfLine :: Parsec Void LT.Text () #-}
{-# SPECIALISE endOfLine :: Parsec Void  String () #-}
endOfLine :: (Enum (Token s), MonadParsec e s m) => m ()
endOfLine = choice [ nl, try (cr *> nl), cr ] $> ()
  where
    newLineChar  = enumCoerce '\n'
    carriageChar = enumCoerce '\r'
    nl = single newLineChar  $> ()
    cr = single carriageChar $> ()


-- |
-- Accepts zero or more Failure messages.
{-# INLINEABLE fails #-}
{-# SPECIALISE fails :: [String] -> Parsec Void  T.Text a #-}
{-# SPECIALISE fails :: [String] -> Parsec Void LT.Text a #-}
{-# SPECIALISE fails :: [String] -> Parsec Void  String a #-}
fails :: MonadParsec e s m => [String] -> m a
fails = failure Nothing . S.fromList . fmap Label . mapMaybe nonEmpty


-- |
-- Convert one Enum to another through the Int value.
enumCoerce :: (Enum a, Enum b) => a -> b
enumCoerce = toEnum . fromEnum


{-# INLINE withinVec #-}
{-# SPECIALISE withinVec :: Vector Char -> Char -> Bool #-}
withinVec :: (Ord a, Unbox a) => Vector a -> a -> Bool
withinVec v e = go 0 (V.length v - 1)
  where
    -- Perform a binary search on the unboxed vector
    -- to determine if a character is valid.
    --
    -- Equally fast, and uses less memory than a Set.
    {-# INLINE go #-}
    go !lo !hi
      | lo > hi   = False
      | otherwise = let !md = (hi + lo) `div` 2
                        !z  = v ! md
                    in  case z `compare` e of
                          EQ -> True
                          LT -> go    (md + 1) hi
                          GT -> go lo (md - 1)
