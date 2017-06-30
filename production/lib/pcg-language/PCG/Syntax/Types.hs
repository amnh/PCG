{-# Language DeriveFunctor, FlexibleContexts, TemplateHaskell, TypeFamilies #-}

module PCG.Syntax.Types where

import Control.Monad.Free     (liftF, Free, iterM, MonadFree)
import Control.Monad.Free.TH  (makeFree)
--import Data.Char              (toLower)
import Data.Functor           (($>), void)
import Data.Key
import Data.List.NonEmpty     (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Scientific hiding (scientific)
import Data.Semigroup  hiding (option)
import qualified Data.Set as S
import Data.Time.Clock        (DiffTime, secondsToDiffTime)
import Text.Megaparsec hiding (space)
import Text.Megaparsec.Prim   (MonadParsec)
import Text.Megaparsec.Lexer

-- import Debug.Trace


data PrimativeValue a
   = PInt           (Int      -> a)
   | PReal          (Double   -> a)
   | PBool          (Bool     -> a)
   | PText          (String   -> a)
   | PTime          (DiffTime -> a)
   | PValue String  (()       -> a)
   deriving (Functor)


data  PrimativeParseResult
    = PPR_Bool  Bool
    | PPR_Int   Int
    | PPR_Real  Double
    | PPR_Text  String
    | PPR_Time  DiffTime
    | PPR_Value String
    deriving (Show)


primativeTypeMatch :: PrimativeParseResult -> PrimativeParseResult -> Bool
primativeTypeMatch lhs rhs =
  case (lhs, rhs) of
    (PPR_Bool  {}, PPR_Bool  {}) -> True
    (PPR_Int   {}, PPR_Int   {}) -> True
    (PPR_Real  {}, PPR_Real  {}) -> True
    (PPR_Text  {}, PPR_Text  {}) -> True
    (PPR_Time  {}, PPR_Time  {}) -> True
    (PPR_Value {}, PPR_Value {}) -> True
    _                            -> False
                     

primativeTypeName :: PrimativeParseResult -> String
primativeTypeName ppr =
    case ppr of
      PPR_Bool  {} -> "boolean value"
      PPR_Int   {} -> "integer value"
      PPR_Real  {} -> "real value"
      PPR_Text  {} -> "text value"
      PPR_Time  {} -> "time value"
      PPR_Value x  -> "the literal '" <> x <> "'"


makeFree ''PrimativeValue


{-
bool  :: PrimativeValue
bool = BitValue

int   :: PrimativeValue
int = WholeNum

real  :: PrimativeValue
real = RealNum

text  :: PrimativeValue
text = TextValue

time  :: PrimativeValue
time = TimeSpan

value :: String -> PrimativeValue
value str = TextExact str
-}


scientific2 :: (MonadParsec e s m, Token s ~ Char) => m Scientific
scientific2 = label "floating point number" $ do
    integralPrefix    <- some digitChar
    fractionalPrefix  <- option "" $ (:) <$> char '.' <*> some digitChar
    exponentialSuffix <- option "" fExp
    return . read $ integralPrefix ++ fractionalPrefix ++ exponentialSuffix


fraction :: (MonadParsec e s m, Token s ~ Char) => m String
fraction = do
    void (char '.')
    d <- some digitChar
    e <- option "" fExp
    return ('.' : d ++ e)


fExp :: (MonadParsec e s m, Token s ~ Char) => m String
fExp = do
    expChar <- char' 'e'
    signStr <- option "" (pure <$> choice (char <$> "+-"))
    d       <- some digitChar
    return (expChar : signStr ++ d)
                     

data TestStruct = TS Int String deriving (Show)

tester :: Free PrimativeValue TestStruct
tester = do
  x <- pInt
  y <- pText
  pure $ TS x y


{--}
parsePrimative :: (MonadParsec Dec s m, Token s ~ Char) => Free PrimativeValue a -> m a
parsePrimative = iterM run
  where
--    run :: Monad m => PrimativeValue (m a) -> (m a)
    run (PBool      x) = boolParser >>= x
    run (PInt       x) =  intParser >>= x
    run (PReal      x) = realParser >>= x
    run (PText      x) = textParser >>= x
    run (PTime      x) = timeParser >>= x
    run (PValue str x) =
      let valueParser = typeMismatchContext (valueValue str) (PPR_Value str)
      in  valueParser >>= x

    boolParser  = typeMismatchContext boolValue $ PPR_Bool undefined
    intParser   = typeMismatchContext intValue  $ PPR_Int  undefined
    realParser  = typeMismatchContext realValue $ PPR_Real undefined
    textParser  = typeMismatchContext textValue $ PPR_Text undefined
    timeParser  = typeMismatchContext timeValue $ PPR_Time undefined



boolValue :: (MonadParsec e s m, Token s ~ Char) => m Bool
boolValue = ((string' "true" $> True) <|> (string' "false" $> False)) <?> "boolean value"


intValue :: (MonadParsec e s m, Token s ~ Char) => m Int
intValue  = fancyInt <?> "integer value"
  where
    fancyInt = do
        result <- signed whitespace $ hidden number
        if not $ isInteger result
        then let unpMsg = S.singleton . Label . NE.fromList . primativeTypeName $ PPR_Real undefined
                 expMsg = S.singleton . Label . NE.fromList . primativeTypeName $ PPR_Int  undefined
             in  failure unpMsg expMsg mempty
        else pure $ case toBoundedInteger result of
                      Just  v -> v
                      Nothing ->
                        if signum result > 0
                        then maxBound
                        else minBound

realValue :: (MonadParsec e s m, Token s ~ Char) => m Double
realValue = ((signed whitespace fancyReal) {- <|> cleanInt -} ) <?> "real value"
  where
    fancyReal = do
        result <- floatingOrInteger <$> hidden number 
        pure $ case result of
                 Left  r -> r
                 Right i -> fromIntegral (i :: Integer)


textValue  :: (MonadParsec e s m, Token s ~ Char) => m String
textValue = openQuote *> many (escaped <|> nonEscaped) <* closeQuote
  where
    openQuote   = char '"' <?> "'\"' opening quote for textual value"
    closeQuote  = char '"' <?> "'\"' closing quote for textual value"
    nonEscaped  = noneOf "\\\""
    escaped = def <?> ""
      where
        def = do
            _ <- char '\\' <?> "'\\' beginning of character escape sequence"
            c <- oneOf (M.keysSet mapping) <?> "escape sequence character"
            pure $ mapping ! c
        -- all the characters which can be escaped after '\'
        -- and thier unescaped literal character value
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
                                                                          

timeValue  :: (MonadParsec e s m, Token s ~ Char) => m DiffTime
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


valueValue :: (MonadParsec e s m, Token s ~ Char) => String -> m ()
valueValue = void . string' 


typeMismatchContext :: (MonadParsec e s m, Token s ~ Char) => m a -> PrimativeParseResult -> m a
typeMismatchContext p target = do
    primResult <- primatives
    case primResult of
      Nothing -> p
      Just (str,actual) ->
        if   target `primativeTypeMatch` actual
        then p
        else let uxpMsg = S.singleton . Label . NE.fromList $ mconcat [ primativeTypeName actual, " '", str, "'" ]
                 expMsg = S.singleton . Label . NE.fromList $ primativeTypeName target
             in  failure uxpMsg expMsg mempty
  where
    primatives = optional . choice $ hidden . try . lookAhead . match <$>
        [ PPR_Bool <$> boolValue
        , PPR_Text <$> textValue
        , PPR_Time <$> timeValue
        , PPR_Int  <$>  intValue
        , PPR_Real <$> realValue
        ]


whitespace :: (MonadParsec e s m, Token s ~ Char) => m ()
whitespace = space single line block
  where
    single = void spaceChar
    line   = skipLineComment "**"
    block  = skipBlockCommentNested "(*" "*)"


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


newtype ListIdentifier = ListId String deriving (Show)


