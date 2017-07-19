{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module PCG.Syntax.Parser where

import Data.List.NonEmpty     (NonEmpty)
import Data.Time.Clock        (DiffTime)
import PCG.Syntax.Types
  

{-
import Data.Functor           (($>), void)
import Data.Char              (toLower)
import qualified Data.List.NonEmpty as NE
import Data.Maybe             (fromJust)
import Data.Semigroup
import PCG.Syntax.Types
import Text.Megaparsec hiding (space)
--import Text.Megaparsec.Custom
--import Text.Megaparsec.Prim   (MonadParsec)
import Text.Megaparsec.Char.Lexer
-}


-- |
-- 'SyntacticCommand' is "Stringly-Typed" and therefore inherently unsafe.
-- We will later consume a list of SyntacticCommand as a Script type and
-- convert these into thier less dubious, well-type counterpart of type Command,
-- or report an error explaing why the SyntacticCommand is not valid.
data  SyntacticCommand
    = SyntacticCommand ListIdentifier (NonEmpty Argument)
    deriving (Show)


data  Syntax
    = Syntax (NonEmpty SyntacticCommand)
    deriving (Show)


data  Argument
    = PrimativeArg   Primative
    | ListIdArg      ListIdentifier
    | ListIdNamedArg ListIdentifier Argument
    | CommandArg     SyntacticCommand
    | ArgumentList  (NonEmpty Argument)
    deriving (Show)


data  Primative
    = WholeNum  Int
    | RealNum   Double
    | BitValue  Bool
    | TextValue String
    | TimeSpan  DiffTime
    deriving (Show)


-- newtype ListIdentifier = ListId String deriving (Show)


{-
syntacticStreamParser :: (MonadParsec e s m, Token s ~ Char) => m Syntax
syntacticStreamParser = syntaxDefinition


syntaxDefinition :: (MonadParsec e s m, Token s ~ Char) => m Syntax
syntaxDefinition = Syntax <$> (trim (some1 commandDefinition) <* eof)


commandDefinition :: (MonadParsec e s m, Token s ~ Char) => m SyntacticCommand
commandDefinition = do
  _         <- whitespace
  lident    <- listIdDefinition
  arguments <- argumentListDefinition
  pure $ SyntacticCommand lident arguments


listIdDefinition :: (MonadParsec e s m, Token s ~ Char) => m ListIdentifier
listIdDefinition = ListId <$> clip lident
  where
    lident         = (:) <$> leadingChar <*> many followingChars
    leadingChar    = char '_' <|> lowerChar    <?> "An identifier must start with a lower-case letter or an underscore."
    followingChars = char '_' <|> alphaNumChar <?> "An identifier may only contain alpha-numeric values and underscores."


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
      _        <- trim $ char ':' 
      argument <- argumentDefinition
      pure $ ListIdNamedArg listId argument


argumentListDefinition :: (MonadParsec e s m, Token s ~ Char) => m (NonEmpty Argument)
argumentListDefinition = do
    _ <- clip $ char '('
    x <- NE.fromList <$> argumentDefinition `sepBy1` trim (char ',')
    _ <- clip $ char ')'
    pure x


primativeDefinition :: (MonadParsec e s m, Token s ~ Char) => m Primative
primativeDefinition = clip $ choice
    [ try  timeValue'
    , try (RealNum   <$> signed whitespace float)
    , try (WholeNum . fromIntegral <$> signed whitespace integer)
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


listId :: (MonadParsec e s m, Token s ~ Char) => String -> m ListIdentifier
listId label = do
    lid@(ListId found) <- listIdDefinition
    if found ==^ label
    then pure lid
    else fail $ mconcat ["'", found, "' did not match '", label, "'"]  
  where
    -- Case-insensitive matching
    (==^) :: String -> String -> Bool
    (==^) lhs rhs = (toLower <$> lhs) == (toLower <$> rhs)


-- Other combinators

trim :: (MonadParsec e s m, Token s ~ Char) => m a -> m a
trim x = whitespace *> x <* whitespace


clip :: (MonadParsec e s m, Token s ~ Char) => m a -> m a
clip x = lexeme whitespace x

{-
whitespace :: (MonadParsec e s m, Token s ~ Char) => m () 
whitespace = space token line block
  where
    token = void spaceChar
    line  = skipLineComment "**"
    block = skipBlockCommentNested "(*" "*)"
-}

{-
command :: (MonadParsec e s m, Token s ~ Char) => String -> m a
command name args = do
  _ <- listId name
  args
-}

--argumentList :: [m a] -> m a


namedArg :: (MonadParsec e s m, Token s ~ Char) => String -> m a -> m a
namedArg name arg = do
    ListId x <- listId name
    _        <- trim (char ':') <?> ("':' between the identifier '" <> x <> "' and it's agrument")
    arg
-}
