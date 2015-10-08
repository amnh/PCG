{-# LANGUAGE FlexibleContexts #-}

module Text.Parsec.Custom
 ( (<:>)
 , (<++>)
 , anyTill
 , caseInsensitiveChar
 , caseInsensitiveString
 , eol
 , fails
 , inlineSpace 
 , inlineSpaces
 , integer
 , decimal
 ) where

import Control.Applicative (empty)
import Data.Char           (isSpace,toLower,toUpper)
import Text.Parsec


(<:>) :: Applicative f => f a -> f [a] -> f [a]
(<:>) a b = (:)  <$> a <*> b

(<++>) :: Applicative f => f [a] -> f [a] -> f [a]
(<++>) a b = (++) <$> a <*> b

eol :: Stream s m Char => ParsecT s u m Char
eol = try endOfLine
  <|> char '\r' -- Silly old Mac format

decimal :: Stream s m Char => ParsecT s u m Double
decimal = read <$> (try decimal' <|> integer')

integer :: Stream s m Char => ParsecT s u m Int
integer = read <$> integer'

plus, minus, number, integer', decimal' :: Stream s m Char => ParsecT s u m String
plus     = char '+' *> number
minus    = char '-' <:> number
number   = many1 digit
integer' = plus <|> minus <|> number
decimal' = integer' <++> (char '.' <:> number)

inlineSpace :: Stream s m Char => ParsecT s u m Char
inlineSpace = satisfy $ \x -> isSpace x 
                           && '\n' /= x
                           && '\r' /= x

inlineSpaces :: Stream s m Char => ParsecT s u m ()
inlineSpaces = skipMany inlineSpace

-- Match the lowercase or uppercase form of 'c'
caseInsensitiveChar :: Stream s m Char => Char -> ParsecT s u m Char
caseInsensitiveChar c = char (toLower c) <|> char (toUpper c)

-- Match the string 's', accepting either lowercase or uppercase form of each character 
caseInsensitiveString :: Stream s m Char => String -> ParsecT s u m String
caseInsensitiveString s = try (mapM caseInsensitiveChar s) <?> "\"" ++ s ++ "\""

fails :: [String] -> ParsecT s u m a
fails [x] = fail x
fails xs  = labels empty xs

anyTill :: Stream s m Char => ParsecT s u m a -> ParsecT s u m String
anyTill c = do 
    ahead <- optionMaybe $ lookAhead c
    case ahead of
      Just _  -> pure []
      Nothing -> anyChar <:> anyTill c

