-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Megaparsec.Custom
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Custom utility combinators for 'Megaparsec' parser construction
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}

module Text.Megaparsec.Custom
 ( (<:>)
 , (<++>)
 , anythingTill
 , comment
 , double
 , endOfLine
 , fails
 , inlineSpace 
 , inlineSpaces
 , somethingTill
 ) where

import Data.Char             (isSpace)
import Text.Megaparsec
import Text.Megaparsec.Prim  (MonadParsec)
import Text.Megaparsec.Lexer (float,integer,signed)

-- | Prepend a single combinator result element to the combinator result of a list of elements
(<:>)  :: Applicative f => f a -> f [a] -> f [a]
(<:>)  a b = (:)  <$> a <*> b

-- | Concatenate the result of two list producing combinators
(<++>) :: Applicative f => f [a] -> f [a] -> f [a]
(<++>) a b = (++) <$> a <*> b

-- | @anythingTill end@ consumes zero or more characters until @end@ is matched, leaving @end@ in the stream
anythingTill :: MonadParsec s m Char => m a -> m String
anythingTill c = do 
    ahead <- optional $ lookAhead c
    case ahead of
      Just _  -> pure []
      Nothing -> somethingTill c

-- | @somethingTill end@ consumes one or more characters until @end@ is matched, leaving @end@ in the stream
somethingTill :: MonadParsec s m Char => m a -> m String
somethingTill c = anyChar <:> anythingTill c

-- | Flexibly parses a 'Double' value represented in a variety of forms.
double :: MonadParsec s m Char => m Double
double = try (signed space float)
     <|> fromIntegral <$> (signed space integer)

-- | Custom 'eol' combinator to account for /very/ old MAc file formats ending lines in a single @\'\r\'@
endOfLine :: MonadParsec s m Char => m Char
endOfLine = (try eol <|> string "\r") *> pure '\n'

-- | Accepts zero or more Failure messages
fails :: MonadParsec s m Char => [String] -> m a
fails = failure . fmap Message

-- | Consumes a whitespace character that is not a newline character
inlineSpace :: MonadParsec s m Char => m Char
inlineSpace = satisfy $ \x -> isSpace x 
                           && '\n' /= x
                           && '\r' /= x

-- | Consumes zero or more whitespace characters that are not newline characters
inlineSpaces :: MonadParsec s m Char => m ()
inlineSpaces = skipMany inlineSpace

-- | @comment start end@ will parse a /nested/ comment structure which begins with
-- the delimiter @start@ and ends with the delimiter @end@.
--
-- Each opening @start@ must be matched with an @end@ in a proper nested structure.
comment :: MonadParsec s m Char => m String -> m String -> m String
comment start end = commentDefinition' False
  where
    commentChar    = (notFollowedBy $ start <|> end) *> anyChar
    commentContent = many commentChar
    commentDefinition' enquote = do
        prefix   <- start
        before   <- commentContent
        comments <- concat <$> many (commentDefinition' True <++> commentContent)
        suffix   <- end
        after    <- if enquote
                    then many spaceChar
                    else pure ""
        pure . concat $
          if enquote
          then [prefix,before,comments,suffix,after]
          else [before,comments,after]
