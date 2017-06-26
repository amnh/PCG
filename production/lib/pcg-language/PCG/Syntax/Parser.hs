{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module PCG.Syntax.Parser where

import Data.Functor           (($>))
import Data.Char              (toLower)
import Data.List.NonEmpty     (NonEmpty, some1)
import qualified Data.List.NonEmpty as NE
import Data.Maybe             (fromJust)
import Data.Semigroup
import Data.Time.Clock        (secondsToDiffTime)
import PCG.Syntax.Types
import Text.Megaparsec
--import Text.Megaparsec.Custom
import Text.Megaparsec.Prim   (MonadParsec)
import Text.Megaparsec.Lexer  (float,integer,signed)



syntacticStreamParser :: (MonadParsec e s m, Token s ~ Char) => m Syntax
syntacticStreamParser = syntaxDefinition


syntaxDefinition :: (MonadParsec e s m, Token s ~ Char) => m Syntax
syntaxDefinition = Syntax <$> (trimmed (some1 commandDefinition) <* eof)


commandDefinition :: (MonadParsec e s m, Token s ~ Char) => m SyntacticCommand
commandDefinition = do
  _         <- whitespace
  lident    <- listIdDefinition
  arguments <- argumentListDefinition
  pure $ SyntacticCommand lident arguments


listIdDefinition :: (MonadParsec e s m, Token s ~ Char) => m ListIdentifier
listIdDefinition = try $ ListId <$> symbol lident
  where
    lident         = (:) <$> leadingChar <*> many followingChars
    leadingChar    = char '_' <|> lowerChar
    followingChars = char '_' <|> alphaNumChar


argumentDefinition :: (MonadParsec e s m, Token s ~ Char) => m Argument
argumentDefinition = choice
    [ try (PrimativeArg <$> primativeDefinition)
    , try (CommandArg   <$> commandDefinition)
    , try lidentNamedArg'
    , try (ListIdArg    <$> listIdDefinition)
    , try (ArgumentList <$> argumentListDefinition)
    ]
  where
    lidentNamedArg' = do 
      listId   <- listIdDefinition
      _        <- trimmed $ char ':' 
      argument <- argumentDefinition
      pure $ ListIdNamedArg listId argument


argumentListDefinition :: (MonadParsec e s m, Token s ~ Char) => m (NonEmpty Argument)
argumentListDefinition = do
    _ <- symbol $ char '('
    x <- NE.fromList <$> argumentDefinition `sepBy1` trimmed (char ',')
    _ <- symbol $ char ')'
    pure x


primativeDefinition :: (MonadParsec e s m, Token s ~ Char) => m Primative
primativeDefinition = symbol $ choice
    [ try  timeValue'
    , try (RealNum   <$> signed space float)
    , try (WholeNum . fromIntegral <$> signed space integer)
    , try (BitValue  <$> bitValue)
    , try (TextValue <$> textValue)
    ]
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

anyToken :: MonadParsec e s m => m (Token s)
anyToken = token Right Nothing


comment :: MonadParsec e s m => m [Token s] -> m [Token s] -> m [Token s]
comment start end = commentDefinition' False
  where
    commentChar    = notFollowedBy (start <|> end) *> anyToken
    commentContent = many commentChar
    commentDefinition' enquote = do
        prefix   <- start
        before   <- commentContent
        comments <- concat <$> many ((<>) <$> commentDefinition' True <*> commentContent)
        suffix   <- end
{-
        after    <- if   enquote
                    then many spaceChar
                    else pure ""
-}
        pure . concat $
            if enquote
            then [ prefix, before, comments, suffix {- , after -} ]
            else [         before, comments         {- , after -} ] 


trimmed :: (MonadParsec e s m, Token s ~ Char) => m a -> m a
trimmed x = whitespace *> x <* whitespace


symbol  :: (MonadParsec e s m, Token s ~ Char) => m a -> m a
symbol  x = x <* whitespace


whitespace :: (MonadParsec e s m, Token s ~ Char) => m ()
whitespace = try commentBlock <|> space
  where
    commentBlock = comment (string "(*") (string "*)") $> ()
