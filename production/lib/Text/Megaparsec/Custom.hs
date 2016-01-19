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
 , inlineSpaceChar 
 , inlineSpace
 , somethingTill
 ) where

import Data.Char             (isSpace)
import Data.Maybe            (maybe)
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
    ahead <- optional $ try $ lookAhead c
    case ahead of
      Just _  -> pure []
      Nothing -> somethingTill c

-- | @somethingTill end@ consumes one or more characters until @end@ is matched, leaving @end@ in the stream
somethingTill :: MonadParsec s m Char => m a -> m String
somethingTill c = 
    do
    _ <- notFollowedBy c
    anyChar <:> anythingTill c

-- | Flexibly parses a 'Double' value represented in a variety of forms.
double :: MonadParsec s m Char => m Double
double = try (signed space float)
     <|> fromIntegral <$> signed space integer

-- | Custom 'eol' combinator to account for /very/ old Mac file formats ending lines in a single @\'\\r\'@
endOfLine :: MonadParsec s m Char => m Char
endOfLine = (try eol <|> string "\r") *> pure '\n'

-- | Accepts zero or more Failure messages
fails :: MonadParsec s m Char => [String] -> m a
fails = failure . fmap Message

-- | Consumes a whitespace character that is not a newline character
inlineSpaceChar :: MonadParsec s m Char => m Char
inlineSpaceChar = satisfy $ \x -> isSpace x 
                           && '\n' /= x
                           && '\r' /= x

-- | Consumes zero or more whitespace characters that are not newline characters
inlineSpace :: MonadParsec s m Char => m ()
inlineSpace = skipMany inlineSpaceChar

-- | @comment start end@ will parse a /nested/ comment structure which begins with
-- the delimiter @start@ and ends with the delimiter @end@.
--
-- Each opening @start@ must be matched with an @end@ in a proper nested structure.
--
-- *NOTE:* if @start@ and @end@ are not completely disjoint combinators, that is if
-- there exists any string which both @start@ and @end@ can match, the @comment@
-- combinator will not behave as expected. The parser is unable to handle the ambiguity
-- of whether strings matched in the intersection of @start@ and @end@ should represent
-- the beginning of a nested comment structure or the closing of an open comment structure.
--
-- Ensure that the following holds for all `x :: String`:
--
-- > isRight (parse start "" x) /= isRight (parse end "" x)
comment :: MonadParsec s m Char => m String -> m String -> m String
comment start end = commentDefinition' False
  where
    commentChar    = notFollowedBy (start <|> end) *> anyChar
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

-- | Takes a 'Stream' of 'Char's and returns a String
-- with EOL sequences standardized to the Unix EOL sequence.
--
--  * @"\\r\\n"@ -> @\'\\n\'@ (Windows EOL sequence)
--
--  * @"\\r"@ -> @\'\\n\'@ (Old Mac EOL sequence
--
--  * @"\\n"@ -> @\'\\n\'@ (Unix EOL sequence)
unifyNewlines :: Stream s Char => s -> String
unifyNewlines xs =
  case (curr, next) of
    (Nothing  , _        ) -> []
    (Just '\r', Just '\n') -> '\n' : maybe [] unifyNewlines less
    (Just '\r', _        ) -> '\n' : maybe [] unifyNewlines more
    (Just tok , Just _   ) -> tok  : maybe [] unifyNewlines more
    (Just tok , Nothing  ) -> [tok]
  where
    (curr, more) = seqTuple $ uncons xs
    (next, less) = seqTuple $ uncons =<< more
    seqTuple may = case may of
                     Just (x,y) -> (Just x, Just y)
                     Nothing    -> (Nothing,Nothing)
                     
