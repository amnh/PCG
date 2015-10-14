module PGC.Command.Parser 
    ( parseScript
    , parseCommand
    ) where

import Data.Char             (toUpper)
import Data.Int              (Int64)
import Data.Maybe            (fromJust)
import Data.Time.Clock       (DiffTime,secondsToDiffTime)
import Text.Parsec           (Parsec)
import Text.Parsec.Char
import Text.ParserCombinators.Parsec

data Script    = Script [Command]
               deriving (Show)
data Command   = Command Lident [Argument]
               deriving (Show)
data Lident    = Lident String
               deriving (Show)
data Argument  = PrimativeArg Primative
               | LidentArg Lident
               | LidentNamedArg Lident Argument
               | CommandArg Command
               | ArgumentList [Argument]
               deriving (Show)
data Primative = WholeNum  Int64
               | RealNum   Double
               | BitValue  Bool
               | TextValue String
               | TimeSpan  DiffTime
               deriving (Show)

parseScript :: String -> Either ParseError Script
parseScript = parse scriptDefinition "POY Script"

parseCommand :: String -> Either ParseError Command
parseCommand = parse commandDefinition "!"

scriptDefinition :: Parsec String u Script
scriptDefinition = Script <$> (trimmed (many1 commandDefinition) <* eof)

commandDefinition :: Parsec String u Command
commandDefinition = do
  _         <- whitespace
  lident    <- lidentDefinition
  arguments <- argumentListDefinition
  return $ Command lident arguments

lidentDefinition :: Parsec String u Lident
lidentDefinition = try $ Lident <$> symbol lident
  where
    lident         = leadingChar <:> many followingChars
    leadingChar    = char '_' <|> lower
    followingChars = char '_' <|> alphaNum

argumentDefinition :: Parsec String u Argument
argumentDefinition =
      try (PrimativeArg <$> primativeDefinition)
  <|> try (CommandArg   <$> commandDefinition)
  <|> try lidentNamedArg'
  <|> try (LidentArg    <$> lidentDefinition)
  <|> try (ArgumentList <$> argumentListDefinition)
  where
    lidentNamedArg' = do 
      lident   <- lidentDefinition
      _        <- trimmed $ char ':' 
      argument <- argumentDefinition
      return $ LidentNamedArg lident argument

argumentListDefinition :: Parsec String u [Argument]
argumentListDefinition = 
     symbol (char '(') 
  *> argumentDefinition `sepBy` trimmed (char ',')
  <* symbol (char ')')

primativeDefinition :: Parsec String u Primative
primativeDefinition = symbol $
      try  timeValue'
  <|> try (RealNum   . read <$> (try exponential <|> decimal))
  <|> try (WholeNum  . read <$> integer)
  <|> try (bitValue'        <$> bitValue)
  <|> try (TextValue        <$> textValue)
  where 
    plus        = char '+' *> number
    minus       = char '-' <:> number
    number      = many1 digit
    integer     = plus <|> minus <|> number
    exponential = (decimal <|> number) <++> (oneOf "eE" <:> integer)
    decimal     = integer <++> (char '.' <:> number)
    bitValue    = try (oneOf "tT" <:> string "rue") <|> try (oneOf "fF" <:> string "alse")
    bitValue'   = BitValue . read . format
      where
        format     [] = []
        format (x:xs) = toUpper x : xs
    timeValue'  = do
        days    <- number 
        _       <- char ':'
        hours   <- number 
        _       <- char ':'
        minutes <- number
        let totalSeconds = read days    * 60 * 60 * 24
                         + read hours   * 60 * 60
                         + read minutes * 60 
        pure . TimeSpan $ secondsToDiffTime totalSeconds
    textValue   = char '"' *> many (escaped <|> nonEscaped) <* char '"'
    nonEscaped  = noneOf "\\\""
    escaped = do
      _ <- char '\\'
      c <- oneOf $ fst <$> mapping 
      pure . fromJust $ c `lookup` mapping 
      where 
        -- all the characters which can be escaped after '\'
        -- and thier unescaped literal character value
        mapping = [('\\', '\\')
                  ,( '"',  '"')
                  ,( '0', '\0')
                  ,( 'n', '\n')
                  ,( 'r', '\r')
                  ,( 'v', '\v')
                  ,( 't', '\t')
                  ,( 'b', '\b')
                  ,( 'f', '\f')
                  ]

-- Other combinators
(<++>) :: Applicative f => f [a] -> f [a] -> f [a]
(<++>) a b = (++) <$> a <*> b

(<:>) :: Applicative f => f a -> f [a] -> f [a]
(<:>) a b = (:)  <$> a <*> b

trimmed :: Parsec String u a -> Parsec String u a
trimmed x = whitespace *> x <* whitespace

symbol  :: Parsec String u a -> Parsec String u a
symbol  x = x <* whitespace

--whitespace = spaces
whitespace = try commentDefinition <|> spaces
  where
    commentDefinition :: Parsec String u ()
    --commentDefinition = string "(*" *> manyTill anyChar (try $ string "*)")
    --commentDefinition = between (string "(*") (try $ string "*)") $ skipMany anyChar
    commentDefinition = spaces *> string "(*" *> manyTill (noneOf "*") (char '*') <* char ')' <* spaces >>= \_ -> pure ()
        

-- Currently unused
--liftReadS :: ReadS a -> String -> Parser a
--liftReadS reader = maybe (unexpected "no parse") (return . fst) .
--                 listToMaybe . filter (null . snd) . reader
