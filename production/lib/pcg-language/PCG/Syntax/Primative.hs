{-# LANGUAGE DeriveFunctor, FlexibleContexts, ScopedTypeVariables, TypeFamilies #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}

module PCG.Syntax.Primative
  ( PrimativeValue()
  -- ** Primative Free Monad constructors
  , bool
  , int
  , real
  , text
  , time
  , value
  -- ** MonadParsec based Free Monad interpreter
  , parsePrimative
  , whitespace
  ) where

import           Control.Applicative
import           Control.Monad.Free
import           Data.CaseInsensitive              (FoldCase)
import           Data.Functor                      (($>), void)
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
import           Data.Time.Clock                   (DiffTime, secondsToDiffTime)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer        (decimal, scientific, signed)
import qualified Text.Megaparsec.Char.Lexer as Lex

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


-- |
-- Defines the whitespace for the syntax.
--
-- Exported for reuse by other MonadParsec based Free Monad interpreters.
whitespace :: forall e s m. (MonadParsec e s m, Token s ~ Char) => m ()
whitespace = Lex.space single line block
  where
    pxy    = Proxy :: Proxy s
    single = void spaceChar
    line   = Lex.skipLineComment $ tokensToChunk pxy "**"
    block  = Lex.skipBlockCommentNested open close
    open   = tokensToChunk pxy "(*"
    close  = tokensToChunk pxy "*)"


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


intValue :: (MonadParsec e s m, Token s ~ Char) => m Int
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
