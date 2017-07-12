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
    = ResultBool  Bool
    | ResultInt   Int
    | ResultReal  Double
    | ResultText  String
    | ResultTime  DiffTime
    | ResultValue String
    deriving (Show)


data  PrimativeType
    = TypeOfBool
    | TypeOfInt
    | TypeOfReal
    | TypeOfText
    | TypeOfTime
    | TypeOfValue String
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
          TypeOfBool  {} -> "boolean value"
          TypeOfInt   {} -> "integer value"
          TypeOfReal  {} -> "real value"
          TypeOfText  {} -> "text value"
          TypeOfTime  {} -> "time value"
          TypeOfValue v  -> "the literal '" <> v <> "'"

 
instance HasPrimativeType PrimativeType where
  
    getPrimativeType = id


instance HasPrimativeType (PrimativeValue a) where
  
    getPrimativeType x =
       case x of
          PBool  {}  -> TypeOfBool
          PInt   {}  -> TypeOfInt
          PReal  {}  -> TypeOfReal
          PText  {}  -> TypeOfText
          PTime  {}  -> TypeOfTime
          PValue v _ -> TypeOfValue v
  

instance HasPrimativeType PrimativeParseResult where
  
    getPrimativeType x =
        case x of
          ResultBool  {} -> TypeOfBool
          ResultInt   {} -> TypeOfInt
          ResultReal  {} -> TypeOfReal
          ResultText  {} -> TypeOfText
          ResultTime  {} -> TypeOfTime
          ResultValue v  -> TypeOfValue v


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
parsePrimative (PBool      x) = typeMismatchContext boolValue TypeOfBool >>= x
parsePrimative (PInt       x) = typeMismatchContext  intValue TypeOfInt  >>= x
parsePrimative (PReal      x) = typeMismatchContext realValue TypeOfReal >>= x
parsePrimative (PText      x) = typeMismatchContext textValue TypeOfText >>= x
parsePrimative (PTime      x) = typeMismatchContext timeValue TypeOfTime >>= x
parsePrimative (PValue str x) = valueParser >>= x
  where
    valueParser = typeMismatchContext (valueValue str) (TypeOfValue str)


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
    intLabel  = getPrimativeName TypeOfInt
    realLabel = getPrimativeName TypeOfReal


numValue :: (MonadParsec e s m,  Token s ~ Char) => m Scientific
numValue = hidden signedNum <?> "number"
  where
    signedNum = signed whitespace scientific


realValue :: (MonadParsec e s m, Token s ~ Char) => m Double
realValue = label (getPrimativeName TypeOfReal)
          $ either id id . toBoundedRealFloat <$> numValue


textValue  :: (MonadParsec e s m, Token s ~ Char) => m String -- (Tokens s)
textValue = openQuote *> many (escaped <|> nonEscaped) <* closeQuote
  where
    openQuote   = char '"' <?> ("'\"' opening quote for " <> getPrimativeName TypeOfText)
    closeQuote  = char '"' <?> ("'\"' closing quote for " <> getPrimativeName TypeOfText)
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
        
        characterEscaping e@FancyError {} = e
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
          if   targetType == resultType || targetType == TypeOfReal && resultType == TypeOfInt
          then p
          else let uxpMsg = Just . Label . NE.fromList $ mconcat [ getPrimativeName parseResult, " '", chunkToTokens (Proxy :: Proxy s) str, "'" ]
                   expMsg = S.singleton . Label . NE.fromList $ getPrimativeName targetType
               in  failure uxpMsg expMsg
  where
    primatives = optional . choice $ hidden . try . lookAhead . match <$>
        [ ResultBool <$> boolValue
        , ResultText <$> textValue
        , ResultTime <$> timeValue
        , (either ResultReal ResultInt . floatingOrInteger) <$> numValue
        ]
