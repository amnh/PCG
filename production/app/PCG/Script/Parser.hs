{-# LANGUAGE FlexibleContexts #-}
module PCG.Script.Parser where

import Data.Char              (toLower)
import Data.Maybe             (fromJust)
import Data.Time.Clock        (secondsToDiffTime)
import Text.Megaparsec
import Text.Megaparsec.Custom
import Text.Megaparsec.Prim   (MonadParsec)
import Text.Megaparsec.Lexer  (float,integer,signed)

import PCG.Script.Types

scriptStreamParser :: MonadParsec s m Char => m Script
scriptStreamParser = scriptDefinition

scriptDefinition :: MonadParsec s m Char => m Script
scriptDefinition = Script <$> (trimmed (some commandDefinition) <* eof)

commandDefinition :: MonadParsec s m Char => m DubiousCommand
commandDefinition = do
  _         <- whitespace
  lident    <- lidentDefinition
  arguments <- argumentListDefinition
  pure $ DubiousCommand lident arguments

lidentDefinition :: MonadParsec s m Char => m Lident
lidentDefinition = try $ Lident <$> symbol lident
  where
    lident         = leadingChar <:> many followingChars
    leadingChar    = char '_' <|> lowerChar
    followingChars = char '_' <|> alphaNumChar

argumentDefinition :: MonadParsec s m Char => m Argument
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
      pure $ LidentNamedArg lident argument

argumentListDefinition :: MonadParsec s m Char => m [Argument]
argumentListDefinition = 
     symbol (char '(') 
  *> argumentDefinition `sepBy` trimmed (char ',')
  <* symbol (char ')')

primativeDefinition :: MonadParsec s m Char => m Primative
primativeDefinition = symbol $
      try  timeValue'
  <|> try (RealNum   <$> signed space float)
  <|> try (WholeNum . fromIntegral <$> signed space integer)
  <|> try (BitValue  <$> bitValue)
  <|> try (TextValue <$> textValue)
  where 
    bitValue    = ((=="true") . fmap toLower) <$> (try (string' "true") <|> try (string' "false"))
    timeValue'  = do
        days    <- integer
        _       <- char ':'
        hours   <- integer
        _       <- char ':'
        minutes <- integer
        let totalSeconds = days    * 60 * 60 * 24
                         + hours   * 60 * 60
                         + minutes * 60 
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
trimmed :: MonadParsec s m Char => m a -> m a
trimmed x = whitespace *> x <* whitespace

symbol  :: MonadParsec s m Char => m a -> m a
symbol  x = x <* whitespace

whitespace :: MonadParsec s m Char => m ()
whitespace = try (comment (string "(*") (string "*)") >> pure ()) <|> space
