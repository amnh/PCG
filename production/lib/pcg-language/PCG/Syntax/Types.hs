{-# LANGUAGE ConstraintKinds, DeriveFunctor, FlexibleContexts, TemplateHaskell, TypeFamilies #-}

module PCG.Syntax.Types where

import           Control.Monad.Free
import           Control.Monad.Free.TH        (makeFree)
import           Data.CaseInsensitive         (FoldCase)
--import           Data.Foldable
import           Data.Functor                 (($>), void)
import           Data.Key
--import           Data.List                    (intercalate)
import           Data.List.NonEmpty           (NonEmpty(..))
import qualified Data.List.NonEmpty    as NE
import qualified Data.Map              as M
import           Data.Maybe                   (fromMaybe)
--import           Data.Ord
import           Data.Scientific       hiding (scientific)
import           Data.Semigroup        hiding (option)
--import           Data.Set                     (Set)
import qualified Data.Set              as S
import           Data.String                  (IsString(..))
import           Data.Time.Clock              (DiffTime, secondsToDiffTime)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Lexer        (integer, number, signed)
import qualified Text.Megaparsec.Lexer as Lex

--import Debug.Trace


type ParserConstraint e s m = (MonadParsec e s m, FoldCase (Tokens s), IsString (Tokens s), Token s ~ Char)


{-
data  PrefixedErrorMessage e
    = Prefixed
    { unexpectedPrefix  :: String
    ,   expectedPrefix  :: String
    , unexpectedValue   :: Maybe (ErrorItem e)
    ,   expectedValues  :: Set   (ErrorItem e)
    } deriving (Eq, Show)


instance Ord e => Ord (PrefixedErrorMessage e) where

    lhs `compare` rhs =
      case comparing unexpectedValue lhs rhs of
        EQ -> comparing expectedValues lhs rhs
        x  -> x


instance (Ord e, ShowToken e) => ShowErrorComponent (PrefixedErrorMessage e) where

    showErrorComponent e = unlines
        [ messageItemsPretty ("unexpected " <> unexpectedPrefix e <> " ") (toList $ unexpectedValue  e)
        , messageItemsPretty ("expecting "  <>   expectedPrefix e <> " ") (toList $   expectedValues e)
        ]
-}


data  PrimativeParseResult
    = PPR_Bool  Bool
    | PPR_Int   Int
    | PPR_Real  Double
    | PPR_Text  String
    | PPR_Time  DiffTime
    | PPR_Value String
    deriving (Show)


data  PrimativeType
    = PT_Bool
    | PT_Int
    | PT_Real
    | PT_Text
    | PT_Time
    | PT_Value String
    deriving (Eq)


data PrimativeValue a
   = PInt           (Int      -> a)
   | PReal          (Double   -> a)
   | PBool          (Bool     -> a)
   | PText          (String   -> a)
   | PTime          (DiffTime -> a)
   | PValue String  (()       -> a)
   deriving (Functor)


{--}
data  ArgumentValue a
    = APrimativeArg   (PrimativeValue a)
    | AListIdArg      (ListIdentifier -> a)
    | AListIdNamedArg ListIdentifier (ArgumentValue a)
--    | CommandArg     SyntacticCommand
    | AArgumentList  (NonEmpty (ArgumentValue a))
    deriving (Functor)
{--}


--primative :: (MonadFree PrimativeValue m, MonadFree ArgumentValue m') => m a -> m' a
--primative x = x >>= (\v -> liftF (PrimativeArg v))


newtype ListIdentifier = ListId String deriving (Show)


class HasPrimativeType a where

    getPrimativeType :: a -> PrimativeType

    getPrimativeName :: a -> String
    getPrimativeName x = 
        case getPrimativeType x of
          PT_Bool  {} -> "boolean value"
          PT_Int   {} -> "integer value"
          PT_Real  {} -> "real value"
          PT_Text  {} -> "text value"
          PT_Time  {} -> "time value"
          PT_Value v  -> "the literal '" <> v <> "'"

 
instance HasPrimativeType PrimativeType where
  
    getPrimativeType = id


instance HasPrimativeType (PrimativeValue a) where
  
    getPrimativeType x =
       case x of
          PBool  {}  -> PT_Bool
          PInt   {}  -> PT_Int
          PReal  {}  -> PT_Real
          PText  {}  -> PT_Text
          PTime  {}  -> PT_Time
          PValue v _ -> PT_Value v
  

instance HasPrimativeType PrimativeParseResult where
  
    getPrimativeType x =
        case x of
          PPR_Bool  {} -> PT_Bool
          PPR_Int   {} -> PT_Int
          PPR_Real  {} -> PT_Real
          PPR_Text  {} -> PT_Text
          PPR_Time  {} -> PT_Time
          PPR_Value v  -> PT_Value v

{-
-- |
-- Transforms a list of error messages into their textual representation.
messageItemsPretty :: ShowErrorComponent a
  => String  -- ^ Prefix to prepend
  -> [a]     -- ^ Collection of messages
  -> String  -- ^ Result of rendering
messageItemsPretty prefix ts
  | null ts   = ""
  | otherwise = prefix <> f ts
  where
    f = orList . NE.fromList . S.toAscList . S.fromList . fmap showErrorComponent


-- |
-- Print a pretty list where items are separated with commas and the word “or”
-- according to the rules of English punctuation.
orList :: NonEmpty String -> String
orList ne@(x:|xs)  =
  case xs of
    []   -> x
    y:ys ->
      case ys of
        []   -> x <> " or " <> y
        z:zs -> intercalate ", " (NE.init ne) <> ", or " <> NE.last (z:|zs)
-}


--makeFree ''ArgumentValue

bool :: MonadFree PrimativeValue m => m Bool
bool = liftF $ PBool id


int :: MonadFree PrimativeValue m => m Int
int = liftF $ PInt id


real :: MonadFree PrimativeValue m => m Double
real = liftF $ PReal id


text :: MonadFree PrimativeValue m => m String
text = liftF $ PText id


time :: MonadFree PrimativeValue m => m DiffTime
time = liftF $ PTime id


value :: MonadFree PrimativeValue m => String -> m ()
value str = liftF $ PValue str id


{--}
data TestStruct = TS Int String deriving (Show)

tester :: Free PrimativeValue TestStruct
tester = do
  x <- int
  y <- text
  pure $ TS x y
{--}


{--}
parsePrimative :: (ParserConstraint e s m, Show (Tokens s)) => Free PrimativeValue a -> m a
parsePrimative = iterM run
  where
--    run :: Monad m => PrimativeValue (m a) -> (m a)
    run (PBool      x) = boolParser >>= x
    run (PInt       x) =  intParser >>= x
    run (PReal      x) = realParser >>= x
    run (PText      x) = textParser >>= x
    run (PTime      x) = timeParser >>= x
    run (PValue str x) =
      let valueParser = typeMismatchContext (valueValue str) (PT_Value str)
      in  valueParser >>= x

    boolParser  = typeMismatchContext boolValue PT_Bool
    intParser   = typeMismatchContext  intValue PT_Int 
    realParser  = typeMismatchContext realValue PT_Real
    textParser  = typeMismatchContext textValue PT_Text
    timeParser  = typeMismatchContext timeValue PT_Time


boolValue :: ParserConstraint e s m => m Bool
boolValue = ((string' (fromString "true") $> True) <|> (string' (fromString "false") $> False)) <?> "boolean value"


intValue :: ParserConstraint e s m => m Int
intValue  = label intLabel $ numValue >>= convertToInt
  where
    convertToInt  s
      | isInteger s = pure . fromMaybe boundedValue $ toBoundedInteger s
      | otherwise   = failure unexpMsg expctMsg
      where
        boundedValue
          | signum s > 0 = maxBound
          | otherwise    = minBound

    unexpMsg  = Just . Label $ NE.fromList realLabel
    expctMsg  = S.singleton . Label $ NE.fromList  intLabel
    intLabel  = getPrimativeName PT_Int
    realLabel = getPrimativeName PT_Real


numValue :: ParserConstraint e s m => m Scientific
numValue = hidden signedNum <?> "number"
  where
    signedNum = signed whitespace number


realValue :: ParserConstraint e s m => m Double
realValue = label (getPrimativeName PT_Real)
          $ either id id . toBoundedRealFloat <$> numValue


textValue  :: ParserConstraint e s m => m String -- (Tokens s)
textValue = openQuote *> many (escaped <|> nonEscaped) <* closeQuote
  where
    openQuote   = char '"' <?> ("'\"' opening quote for " <> getPrimativeName PT_Text)
    closeQuote  = char '"' <?> ("'\"' closing quote for " <> getPrimativeName PT_Text)
    nonEscaped  = satisfy $ \x -> x /= '\\' && x /= '"'
    escaped = do
        _ <- char '\\' <?> "'\\' beginning of character escape sequence"
        c <- region characterEscaping $ oneOf escapeChars
        pure $ mapping ! c
      where
        -- all the characters which can be escaped after '\'
        -- and thier unescaped literal character value
        escapeChars = M.keysSet mapping
        mapping = M.fromList
            [ ('\\', '\\')
            , ( '"',  '"')
            , ( '0', '\0')
            , ( 'n', '\n')
            , ( 'r', '\r')
            , ( 'v', '\v')
            , ( 't', '\t')
            , ( 'b', '\b')
            , ( 'f', '\f')
            ]
        
        characterEscaping e@(FancyError {}) = e
        characterEscaping   (TrivialError pos uxpItems expItems) = TrivialError pos uxpItems' expItems'
          where
            uxpItems' = f <$> uxpItems
            expItems' = S.map (Tokens . pure) escapeChars <> expItems
            f  EndOfInput     = EndOfInput
            f (Tokens    ts ) = Label . NE.fromList $ "invalid escape sequence character: " <> showTokens ts
            f (Label (x:|xs)) = Label $ x :| xs <> " (not a valid escape sequence character)"


timeValue  :: ParserConstraint e s m => m DiffTime
timeValue = do
    days    <- integer
    _       <- char ':'
    hours   <- integer
    _       <- char ':'
    minutes <- integer
    let totalSeconds = days    * 60 * 60 * 24
                     + hours   * 60 * 60
                     + minutes * 60
    pure $ secondsToDiffTime totalSeconds


valueValue :: ParserConstraint e s m => String -> m ()
valueValue = void . string' . fromString


typeMismatchContext :: (ParserConstraint e s m, Show (Tokens s)) => m a -> PrimativeType -> m a
typeMismatchContext p targetType = do
    parsedPrimative <- primatives
    case parsedPrimative of
      Nothing -> p
      Just (str, parseResult) ->
        let resultType = getPrimativeType parseResult
        in
          if   targetType == resultType || targetType == PT_Real && resultType == PT_Int
          then p
          else let uxpMsg = Just . Label . NE.fromList $ mconcat [ getPrimativeName parseResult, " ", show str ]
                   expMsg = S.singleton . Label . NE.fromList $ getPrimativeName targetType
               in  failure uxpMsg expMsg
  where
    primatives = optional . choice $ hidden . try . lookAhead . match <$>
        [ PPR_Bool <$> boolValue
        , PPR_Text <$> textValue
        , PPR_Time <$> timeValue
        , (either PPR_Real PPR_Int . floatingOrInteger) <$> numValue
        ]


whitespace :: ParserConstraint e s m => m ()
whitespace = Lex.space single line block
  where
    single = void spaceChar
    line   = Lex.skipLineComment (fromString "**")
    block  = Lex.skipBlockCommentNested (fromString "(*") (fromString "*)")


{--}

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
{--}
