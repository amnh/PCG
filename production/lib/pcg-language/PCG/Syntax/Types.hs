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

--import Debug.Trace


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

          
makeFree ''PrimativeValue


bool :: MonadFree PrimativeValue m => m Bool
bool = pBool

int :: MonadFree PrimativeValue m => m Int
int = pInt

real :: MonadFree PrimativeValue m => m Double
real = pReal

text :: MonadFree PrimativeValue m => m String
text = pText

time :: MonadFree PrimativeValue m => m DiffTime
time = pTime

value :: MonadFree PrimativeValue m => String -> m ()
value = pValue


{-
data TestStruct = TS Int String deriving (Show)

tester :: Free PrimativeValue TestStruct
tester = do
  x <- pInt
  y <- pText
  pure $ TS x y
-}


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
      let valueParser = typeMismatchContext (valueValue str) (PT_Value str)
      in  valueParser >>= x

    boolParser  = typeMismatchContext boolValue PT_Bool
    intParser   = typeMismatchContext  intValue PT_Int 
    realParser  = typeMismatchContext realValue PT_Real
    textParser  = typeMismatchContext textValue PT_Text
    timeParser  = typeMismatchContext timeValue PT_Time


boolValue :: (MonadParsec e s m, Token s ~ Char) => m Bool
boolValue = ((string' "true" $> True) <|> (string' "false" $> False)) <?> "boolean value"


intValue :: (MonadParsec e s m, Token s ~ Char) => m Int
intValue  = label intLabel $ numValue >>= (either realFail intCast)
  where
    intCast   = pure . fromEnum
    realFail  = const (failure unexpMsg expctMsg mempty)
    unexpMsg  = S.singleton . Label $ NE.fromList realLabel
    expctMsg  = S.singleton . Label $ NE.fromList  intLabel
    intLabel  = getPrimativeName PT_Int
    realLabel = getPrimativeName PT_Real


numValue :: (MonadParsec e s m, Token s ~ Char) => m (Either Double Integer)
numValue = (floatingOrInteger <$> hidden signedNum) <?> "number"
  where
    signedNum = signed whitespace number


realValue :: (MonadParsec e s m, Token s ~ Char) => m Double
realValue = label (getPrimativeName PT_Real)
          $ either id fromIntegral <$> numValue


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


typeMismatchContext :: (MonadParsec e s m, Token s ~ Char) => m a -> PrimativeType -> m a
typeMismatchContext p targetType = do
    parsedPrimative <- primatives
    case parsedPrimative of
      Nothing -> p
      Just (str, parseResult) ->
        let resultType = getPrimativeType parseResult
        in
          if   targetType == resultType || targetType == PT_Real && resultType == PT_Int
          then p
          else let uxpMsg = S.singleton . Label . NE.fromList $ mconcat [ getPrimativeName parseResult, " '", str, "'" ]
                   expMsg = S.singleton . Label . NE.fromList $ getPrimativeName targetType
               in  failure uxpMsg expMsg mempty
  where
    primatives = optional . choice $ hidden . try . lookAhead . match <$>
        [ PPR_Bool <$> boolValue
        , PPR_Text <$> textValue
        , PPR_Time <$> timeValue
        , (either PPR_Real (PPR_Int . fromEnum)) <$> numValue
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


