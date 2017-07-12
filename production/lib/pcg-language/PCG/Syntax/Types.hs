{-# LANGUAGE ConstraintKinds, DeriveFunctor, ExistentialQuantification, FlexibleContexts, ScopedTypeVariables, TypeFamilies #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}

module PCG.Syntax.Types
  ( ArgumentValue()
  -- ** Primative Free Monad constructors
  , bool
  , int
  , real
  , text
  , time
  , value
  -- ** Syntactic Free Monad constructors
  , argList
  , listId
  , pickOne
  -- ** MonadParsec based Free Monad interpreter
  , parseArgument
  -- ** Testing
  , TestStruct
  , tester
  ) where

--import           Control.Applicative
import           Control.Monad
import           Control.Monad.Free
import           Control.Monad.FreeAlt
import           Data.CaseInsensitive              (FoldCase)
import           Data.Functor                      (void)
--import qualified Data.Functor.Alt           as Alt
--import           Data.List                         (intercalate)
import           Data.List.NonEmpty                (NonEmpty(..))
--import qualified Data.List.NonEmpty         as NE
import           Data.Proxy
import           Data.Semigroup             hiding (option)
import           Data.Time.Clock                   (DiffTime)
--import           Data.Void
import           PCG.Syntax.Primative              (PrimativeValue, parsePrimative, whitespace)
import qualified PCG.Syntax.Primative       as P
import           Text.Megaparsec
import           Text.Megaparsec.Char

--import Debug.Trace


newtype ListIdentifier = ListId String deriving (Show)


primative :: (MonadFree ArgumentValue m) => FreeAlt3 PrimativeValue a -> m a
primative = liftF . APrimativeArg


bool :: MonadFree ArgumentValue m => m Bool
bool = primative P.bool


int :: MonadFree ArgumentValue m => m Int
int = primative P.int


real :: MonadFree ArgumentValue m => m Double
real = primative P.real


text :: MonadFree ArgumentValue m => m String
text = primative P.text


time :: MonadFree ArgumentValue m => m DiffTime
time = primative P.time


value :: MonadFree ArgumentValue m => String -> m ()
value str = primative $ P.value str


listId :: (MonadFree ArgumentValue m) => String -> FreeAlt3 ArgumentValue a -> m a
listId str x = liftF $ AListIdNamedArg x (ListId str)


argList :: (MonadFree ArgumentValue m) => FreeAlt3 ArgumentValue a -> m a
argList = liftF . AArgumentList


pickOne :: (MonadFree ArgumentValue m) => [FreeAlt3 ArgumentValue a] -> m a
pickOne    []  = error "You cannot construct an empty set of choices!"
pickOne (x:xs) = liftF . ExactlyOneOf $ x:xs


--someOf :: (MonadFree ArgumentValue m) => FreeAlt3 ArgumentValue a -> m (NonEmpty a)
--someOf = liftF . SomeOf Nothing . pure


data  ArgumentValue a
    =     
      APrimativeArg   (FreeAlt3 PrimativeValue a)
    | AListIdNamedArg (FreeAlt3 ArgumentValue  a) ListIdentifier
    | AArgumentList   (FreeAlt3 ArgumentValue  a)
--    | SomeOf        (NonEmpty (FreeAlt3 ArgumentValue  a))
    | ExactlyOneOf    [FreeAlt3 ArgumentValue  a]
    deriving (Functor)


-- |
-- Intercalates a monadic effect between actions.
iterM' :: (Monad m, Functor f) => m () -> (f (m a) -> m a) -> Free f a -> m a
iterM' _   _   (Pure x) = pure x
iterM' eff phi (Free f) = phi (iterM ((eff *>) . phi) <$> f)


parseArgument :: (FoldCase (Tokens s), MonadParsec e s m,  Token s ~ Char) => FreeAlt3 ArgumentValue a -> m a
parseArgument arg =
    case unFA arg of
      Pure x  -> pure x
      context -> (char '(' <* whitespace)
              *> iterM' comma run context
              <* (char ')' <* whitespace)


run :: forall a e s m. (FoldCase (Tokens s), MonadParsec e s m,  Token s ~ Char) => ArgumentValue (m a) -> m a
run (APrimativeArg   x) = join $ iterM (tokenize . parsePrimative) (unFA x)
run (AListIdNamedArg y (ListId x)) = do
      _ <- (void . string' . tokensToChunk (Proxy :: Proxy s)) x <?> ("identifier '" <> x <> "'")
      _ <- whitespace <* char ':' <* whitespace
      join $ iterM run (unFA y)   <* whitespace
run (ExactlyOneOf   xs) = choice $ join . iterM run . unFA <$> xs
--run (SomeOf         next x) = fmap NE.fromList . some $ join . iterM run (unFA x)
run (AArgumentList  tup) = join $ parseArgument tup



comma :: (MonadParsec e s m,  Token s ~ Char) => m ()
comma = char ',' *> whitespace

commas :: (MonadParsec e s m,  Token s ~ Char) => m a -> m a
commas   x = comma *> x <* whitespace

tokenize :: (Token s ~ Char, MonadParsec e s m) => m a -> m a
tokenize x = x <* whitespace -- <* char ',' <* whitespace


{--}
data TestStruct = TS Int String Double deriving (Show)

tester :: FreeAlt3 ArgumentValue TestStruct
tester = do
    age     <- listId "age" int
    (str,r) <- argList $ (,) <$> text <*> real
    pure $ TS age str r
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


{--
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


syntaxParse syn file str = parse (unSyntax syn) file str
--}

