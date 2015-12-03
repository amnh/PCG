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

(<:>) :: Applicative f => f a -> f [a] -> f [a]
(<:>) a b = (:)  <$> a <*> b

(<++>) :: Applicative f => f [a] -> f [a] -> f [a]
(<++>) a b = (++) <$> a <*> b

anythingTill :: MonadParsec s m Char => m a -> m String
anythingTill c = do 
    ahead <- optional $ try $ lookAhead c
    case ahead of
      Just _  -> pure []
      Nothing -> somethingTill c

somethingTill :: MonadParsec s m Char => m a -> m String
somethingTill c = 
    do
    _ <- notFollowedBy c
    anyChar <:> anythingTill c

double :: MonadParsec s m Char => m Double
double = try (signed space float)
     <|> fromIntegral <$> (signed space integer)

endOfLine :: MonadParsec s m Char => m Char
endOfLine = (try eol <|> string "\r") *> pure '\n'

fails :: MonadParsec s m Char => [String] -> m a
fails = failure . fmap Message

inlineSpace :: MonadParsec s m Char => m Char
inlineSpace = satisfy $ \x -> isSpace x 
                           && '\n' /= x
                           && '\r' /= x

inlineSpaces :: MonadParsec s m Char => m ()
inlineSpaces = skipMany inlineSpace

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
