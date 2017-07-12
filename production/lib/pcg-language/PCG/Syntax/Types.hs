{-# LANGUAGE ConstraintKinds, DeriveFunctor, ExistentialQuantification, FlexibleContexts, ScopedTypeVariables, TypeFamilies #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}

module PCG.Syntax.Types where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Free
import           Control.Monad.FreeAlt
import           Data.CaseInsensitive              (FoldCase)
import           Data.Foldable
import           Data.Functor                      (($>), void)
import qualified Data.Functor.Alt           as Alt
import           Data.Key
--import           Data.List                         (intercalate)
import           Data.List.NonEmpty                (NonEmpty(..))
import qualified Data.List.NonEmpty         as NE
import qualified Data.Map                   as M
import           Data.Maybe                        (fromMaybe)
--import           Data.Ord
import           Data.Proxy
import           Data.Scientific            hiding (scientific)
import           Data.Semigroup             hiding (option)
--import           Data.Set                          (Set)
import qualified Data.Set                   as S
import           Data.String                       (IsString(..))
import           Data.Time.Clock                   (DiffTime, secondsToDiffTime)
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer        (decimal, scientific, signed)
import qualified Text.Megaparsec.Char.Lexer as Lex

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


{--
data  ArgumentValue a
    = APrimativeArg   (PrimativeValue a)
--    | AListIdArg      (ListIdentifier -> a)
    | AListIdNamedArg ListIdentifier (ArgumentValue a)
--    | CommandArg     SyntacticCommand
    | AArgumentList  (NonEmpty (ArgumentValue a))
    deriving (Functor)
--}


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


pbool :: MonadFree PrimativeValue m => m Bool
pbool = liftF $ PBool id


pint :: MonadFree PrimativeValue m => m Int
pint = liftF $ PInt id


preal :: MonadFree PrimativeValue m => m Double
preal = liftF $ PReal id


ptext :: MonadFree PrimativeValue m => m String
ptext = liftF $ PText id


ptime :: MonadFree PrimativeValue m => m DiffTime
ptime = liftF $ PTime id


pvalue :: MonadFree PrimativeValue m => String -> m ()
pvalue str = liftF $ PValue str id


primative :: (MonadFree ArgumentValue m) => FreeAlt3 PrimativeValue a -> m a
primative = liftF . APrimativeArg Nothing


bool :: MonadFree ArgumentValue m => m Bool
bool = primative . liftF $ PBool id


int :: MonadFree ArgumentValue m => m Int
int = primative . liftF $ PInt id


real :: MonadFree ArgumentValue m => m Double
real = primative . liftF $ PReal id


text :: MonadFree ArgumentValue m => m String
text = primative . liftF $ PText id


time :: MonadFree ArgumentValue m => m DiffTime
time = primative . liftF $ PTime id


value :: MonadFree ArgumentValue m => String -> m ()
value str = primative . liftF $ PValue str id


listId :: (MonadFree ArgumentValue m) => String -> FreeAlt3 ArgumentValue a -> m a
listId str x = liftF $ AListIdNamedArg Nothing x (ListId str)


argList :: (MonadFree ArgumentValue m) => FreeAlt3 ArgumentValue a -> m a
argList = liftF . AArgumentList Nothing


pickOne :: (MonadFree ArgumentValue m) => [FreeAlt3 ArgumentValue a] -> m a
pickOne    []  = error "You cannot construct an empty set of choices!"
pickOne (x:xs) = liftF . ExactlyOneOf Nothing $ x:xs
  

--someOf :: (MonadFree ArgumentValue m) => FreeAlt3 ArgumentValue a -> m (NonEmpty a)
--someOf = liftF . SomeOf Nothing . pure


data  ArgumentValue a
    =     
      APrimativeArg   (Maybe (ArgumentValue a)) (FreeAlt3 PrimativeValue a)
    | AListIdNamedArg (Maybe (ArgumentValue a)) (FreeAlt3 ArgumentValue  a) ListIdentifier
    | AArgumentList   (Maybe (ArgumentValue a)) (FreeAlt3 ArgumentValue  a)
--    | SomeOf          (Maybe (ArgumentValue a)) (NonEmpty (FreeAlt3 ArgumentValue  a))
    | ExactlyOneOf    (Maybe (ArgumentValue a)) [FreeAlt3 ArgumentValue  a]
    | None
    deriving (Functor)


-- |
-- Intercalates a monadic effect between actions.
iterM' :: (Monad m, Functor f) => m () -> (f (m a) -> m a) -> Free f a -> m a
iterM' _   _   (Pure x) = return x
iterM' eff phi (Free f) = phi (iterM ((eff *>) . phi) <$> f)


parseArgument :: (ParserConstraint e s m, Show (Tokens s)) => FreeAlt3 ArgumentValue a -> m a
parseArgument x =
    case unFA x of
      Pure x  -> return x
      context -> (char '(' <* whitespace)
              *> iterM' comma run context
              <* (char ')' <* whitespace)


run :: (ParserConstraint e s m, Show (Tokens s)) => ArgumentValue (m a) -> m a
run (APrimativeArg   next x) = join $ iterM (tokenize . parsePrimative) (unFA x)
run (AListIdNamedArg next y (ListId x)) = do
      _ <- void $ (string' (fromString x) <?> ("identifier '" <> x <> "'"))
      _ <- whitespace <* char ':' <* whitespace
      join $ iterM run (unFA y)   <* whitespace
run (ExactlyOneOf   next xs) = choice $ join . iterM run . unFA <$> xs
--run (SomeOf         next x) = fmap NE.fromList . some $ join . iterM run (unFA x)
run (AArgumentList       next tup) = join $ parseArgument tup  --openParen *> iterM run (unFA tup) <* closeParen
--  where
--    openParen  = char '(' <* whitespace
--    closeParen = char ')' <* whitespace


comma :: (ParserConstraint e s m) => m ()
comma = char ',' *> whitespace

commas :: (ParserConstraint e s m) => m a -> m a
commas   x = comma *> x <* whitespace

tokenize x = x <* whitespace -- <* char ',' <* whitespace



{--}
data TestStruct = TS Int String Double deriving (Show)

tester :: FreeAlt3 ArgumentValue TestStruct
tester = do
    age     <- listId "age" int
    (str,r) <- argList $ (,) <$> text <*> real
    pure $ TS age str r
{--}


parsePrimatives :: (FoldCase (Tokens s), MonadParsec e s m, Token s ~ Char) => Free PrimativeValue a -> m a
parsePrimatives = iterM parsePrimative


parsePrimative :: (FoldCase (Tokens s), MonadParsec e s m,  Token s ~ Char) => PrimativeValue (m a) -> m a
parsePrimative (PBool      x) = typeMismatchContext boolValue PT_Bool >>= x
parsePrimative (PInt       x) = typeMismatchContext  intValue PT_Int  >>= x
parsePrimative (PReal      x) = typeMismatchContext realValue PT_Real >>= x
parsePrimative (PText      x) = typeMismatchContext textValue PT_Text >>= x
parsePrimative (PTime      x) = typeMismatchContext timeValue PT_Time >>= x
parsePrimative (PValue str x) = valueParser >>= x
  where
    valueParser = typeMismatchContext (valueValue str) (PT_Value str)


boolValue :: forall e s m. (FoldCase (Tokens s), MonadParsec e s m, Token s ~ Char) => m Bool
boolValue = (truthhood <|> falsehood) <?> "boolean value"
  where
    truthhood = string' (tokensToChunk proxy "true" ) $> True
    falsehood = string' (tokensToChunk proxy "false") $> False
    proxy     = Proxy :: Proxy s


intValue :: (FoldCase (Tokens s), MonadParsec e s m, Token s ~ Char) => m Int
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


numValue :: (MonadParsec e s m,  Token s ~ Char) => m Scientific
numValue = hidden signedNum <?> "number"
  where
    signedNum = signed whitespace scientific


realValue :: (MonadParsec e s m, Token s ~ Char) => m Double
realValue = label (getPrimativeName PT_Real)
          $ either id id . toBoundedRealFloat <$> numValue


textValue  :: (MonadParsec e s m, Token s ~ Char) => m String -- (Tokens s)
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


timeValue  :: (MonadParsec e s m, Token s ~ Char) => m DiffTime
timeValue = do
    days    <- decimal
    _       <- char ':'
    hours   <- decimal
    _       <- char ':'
    minutes <- decimal
    let totalSeconds = days    * 60 * 60 * 24
                     + hours   * 60 * 60
                     + minutes * 60
    pure $ secondsToDiffTime totalSeconds


valueValue :: forall e s m. (FoldCase (Tokens s), MonadParsec e s m, Token s ~ Char) => String -> m ()
valueValue = void . string' . tokensToChunk (Proxy :: Proxy s)


typeMismatchContext :: forall e s m a. (FoldCase (Tokens s), MonadParsec e s m, Token s ~ Char) => m a -> PrimativeType -> m a
typeMismatchContext p targetType = do
    parsedPrimative <- primatives
    case parsedPrimative of
      Nothing -> p
      Just (str, parseResult) ->
        let resultType = getPrimativeType parseResult
        in
          if   targetType == resultType || targetType == PT_Real && resultType == PT_Int
          then p
          else let uxpMsg = Just . Label . NE.fromList $ mconcat [ getPrimativeName parseResult, " ", chunkToTokens (Proxy :: Proxy s) str ]
                   expMsg = S.singleton . Label . NE.fromList $ getPrimativeName targetType
               in  failure uxpMsg expMsg
  where
    primatives = optional . choice $ hidden . try . lookAhead . match <$>
        [ PPR_Bool <$> boolValue
        , PPR_Text <$> textValue
        , PPR_Time <$> timeValue
        , (either PPR_Real PPR_Int . floatingOrInteger) <$> numValue
        ]


whitespace :: forall e s m. (MonadParsec e s m, Token s ~ Char) => m ()
whitespace = Lex.space single line block
  where
    pxy    = Proxy :: Proxy s
    single = void spaceChar
    line   = Lex.skipLineComment (tokensToChunk pxy "**")
    block  = Lex.skipBlockCommentNested open close
    open   = tokensToChunk pxy "(*"
    close  = tokensToChunk pxy "*)"


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





{-
data ArgumentList a x = Perm (Maybe a) [Branch a x] x
  deriving (Functor)


data Branch a x = forall b. Branch (ArgumentList (b -> a) x) b x


instance Functor (Branch a) where

    fmap f (Branch args b x) = Branch (f <$> args) b $ f x


newperm :: (a -> b) -> x -> ArgumentList (a -> b) x
newperm f = Perm (Just f) [] 


add :: ArgumentList (a -> b) x -> a -> ArgumentList b x
add perm@(Perm _mf fs x) p = Perm Nothing (first : fmap insert fs) x
  where
    first = Branch perm  p  x
    insert (Branch perm' p' x') = Branch (add (mapPerms flip perm') p) p' x'


addopt
  :: ArgumentList (a -> b) x
  -> a
  -> Free ArgumentValue a
  -> ArgumentList b x
addopt perm@(Perm mf fs x) v p = Perm (fmap ($ v) mf) (first : fmap insert fs) x
  where
    first = Branch perm  p  x
    insert (Branch perm' p' x') = Branch (addopt (mapPerms flip -perm') x p) p' x'


mapPerms
  :: (a -> b)
  -> ArgumentList a x
  -> ArgumentList b x
mapPerms f (Perm a as x) = Perm (fmap f a) (fmap mapBranch as) x
  where
    mapBranch (Branch perm p x') = Branch (mapPerms (f .) perm) p x'



(<||>) :: (a -> b) -> Free ArgumentValue a -> ArgumentList a x
(<||>) f p = newperm f <::> p


(<::>) :: ArgumentList (a -> b) x -> Free ArgumentValue a -> ArgumentList b x
(<::>) f p = add


-}


data ReadCommandz = Read [FileSpec]

data FileSpec
   = Unspecified    String
   | Nucleotide     String
   | Chromosome     String
   | CustomAlphabet String
   deriving (Show)


unspecified :: Free ArgumentValue FileSpec
unspecified = Unspecified <$> text

nucleotide :: Free ArgumentValue FileSpec
nucleotide = listId "nucleotide" $ Nucleotide <$> text

{-
customAlphabet :: Free ArgumentValue FileSpec
customAlphabet = listId "custom_alphabet"
  where
    parameters = argList $ (,) </|)
-}


{--}
newtype SyntaxParser a = SP { unSyntax :: Parsec Void String a }


instance Functor SyntaxParser where

    fmap f = SP . fmap f . (<* whitespace) . unSyntax


instance Applicative SyntaxParser where

    pure = SP . pure

    (<*>) f = SP . ((unSyntax f) <*>) . (char ',' *> whitespace *>) . unSyntax


instance Monad SyntaxParser where

    (>>=) x f = SP $ unSyntax x >>= unSyntax . f

    (>>)  f = SP . (unSyntax f >>) . unSyntax

    return = pure

    fail = SP . fail


instance MonadPlus SyntaxParser where

    mzero = SP mzero

    mplus x y = SP $ unSyntax x `mplus` unSyntax y


instance Alternative SyntaxParser where

    empty = SP empty

    (<|>) x y = SP $ unSyntax x <|> unSyntax y


instance MonadParsec Void String SyntaxParser where

    failure e es        = SP $ failure e es
    fancyFailure es     = SP $ fancyFailure es
    label str           = SP . label str . unSyntax
    try                 = SP . try . unSyntax
    lookAhead           = SP . lookAhead . unSyntax
    notFollowedBy       = SP . notFollowedBy . unSyntax
    withRecovery f      = SP . withRecovery (unSyntax . f) . unSyntax
    observing           = SP . observing . unSyntax
    eof                 = SP eof
    token  f m          = SP $ token  f m
    tokens f m          = SP $ tokens f m
    takeWhileP  m f     = SP $ takeWhileP  m f
    takeWhile1P m f     = SP $ takeWhile1P m f
    takeP m i           = SP $ takeP m i
    getParserState      = SP getParserState
    updateParserState f = SP $ updateParserState f
{--}


syntaxParse syn file str = parse (unSyntax syn) file str
