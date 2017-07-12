{-# LANGUAGE ApplicativeDo, ConstraintKinds, DeriveFunctor, ExistentialQuantification, FlexibleContexts, ScopedTypeVariables, TypeFamilies #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, RankNTypes, TypeSynonymInstances #-}

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


import           Control.Applicative
import           Control.Alternative.Free   hiding (Pure)
import           Control.Monad
--import           Control.Monad.Free                (Free)
--import           Control.Monad.FreeAlt
import           Control.Monad.Trans
import           Control.Monad.Trans.Free
import           Data.CaseInsensitive              (FoldCase)
import           Data.Functor                      (void)
import qualified Data.Functor.Alt           as Alt
--import           Data.List                         (intercalate)
import           Data.List.NonEmpty                (NonEmpty(..))
--import qualified Data.List.NonEmpty         as NE
import           Data.Maybe                        (fromMaybe)
import           Data.Proxy
import           Data.Semigroup             hiding (option)
import           Data.Time.Clock                   (DiffTime)
--import           Data.Void
import           PCG.Syntax.Primative              (PrimativeValue, parsePrimative, whitespace)
import qualified PCG.Syntax.Primative       as P
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Perm

--import Debug.Trace


data  ArgumentValue p a
    = APrimativeArg    (SyntaxParser PrimativeValue    p a)
    | ADefault         (SyntaxParser (ArgumentValue p) p a) a
    | AListIdNamedArg  (SyntaxParser (ArgumentValue p) p a) ListIdentifier
    | AArgumentList    (SyntaxParser (ArgumentValue p) p a)
    | SomeOf           (SyntaxParser (ArgumentValue p) p a)
    | ExactlyOneOf    [(SyntaxParser (ArgumentValue p) p a)]
    deriving (Functor)


newtype ListIdentifier = ListId String deriving (Show)


bool :: (Monad p, MonadFree (ArgumentValue p) m) => m Bool
bool = primative P.bool


int :: (Monad p, MonadFree (ArgumentValue p) m) => m Int
int = primative P.int


real :: (Monad p, MonadFree (ArgumentValue p) m) => m Double
real = primative P.real


text :: (Monad p, MonadFree (ArgumentValue p) m) => m String
text = primative P.text


time :: (Monad p, MonadFree (ArgumentValue p) m) => m DiffTime
time = primative P.time


value :: (Monad p, MonadFree (ArgumentValue p) m) => String -> m ()
value str = primative $ P.value str


listId :: (Monad p, MonadFree (ArgumentValue p) m) => String -> SyntaxParser (ArgumentValue p) p a -> m a
listId str x = liftF $ AListIdNamedArg x (ListId str)


argList :: (Monad p, MonadFree (ArgumentValue p) m) => SyntaxParser (ArgumentValue p) p a -> m a
argList = liftF . AArgumentList


pickOne :: (Monad p, MonadFree (ArgumentValue p) m) => [SyntaxParser (ArgumentValue p) p a] -> m a
pickOne    []  = error "You cannot construct an empty set of choices!"
pickOne (x:xs) = liftF . ExactlyOneOf $ x:xs


--someOf :: (Monad p, MonadFree (ArgumentValue p) m) => (a -> m [a]) -> FreeAlt3 ArgumentValue a -> m a
--someOf = Alt.some . liftF . SomeOf


withDefault :: (Monad p, MonadFree (ArgumentValue p) m) => SyntaxParser (ArgumentValue p) p a -> a -> m a
withDefault x = liftF . ADefault x


parseArgument :: (FoldCase (Tokens s), MonadParsec e s m,  Token s ~ Char) => SyntaxParser (ArgumentValue m) m a -> m a
parseArgument arg = do
    val <- runFreeT arg
    case val of
      Pure x  -> pure x
      Free x -> begin *> whitespace
              *> iterT' comma run arg
              <* close <* whitespace
  where
    begin = char '(' <?> "'(' starting a new argument list"
    close = char ')' <?> "')' ending the argument list"


run :: (FoldCase (Tokens s), MonadParsec e s m,  Token s ~ Char) => ArgumentValue m (m a) -> m a
run (APrimativeArg    x) = join $ iterT parsePrimative x
--run (ADefault       x v) = join $ iterT (fmap (withDef2 v) . run) x
run e@(ADefault       x v) = join . iterT run $ x >>= (lift . pure . withDef v)
run (ExactlyOneOf    xs) = choice $ join . iterT run <$> xs
--run (SomeOf           x) = fmap head . some . join . iterM run $ unFA x
run (AArgumentList  tup) = join $ parseArgument tup
run (AListIdNamedArg y (ListId x)) = do
      _ <- try (string'' x <?> ("identifier '" <> x <> "'"))
      _ <- whitespace <* char ':' <* whitespace
      join $ iterT run y


string'' :: forall e s m. (FoldCase (Tokens s), MonadParsec e s m,  Token s ~ Char) => String -> m ()
string'' = void . string' . tokensToChunk (Proxy :: Proxy s)


withDef :: MonadParsec e s m => m a -> m a -> m a
withDef v p = optional (try p) >>= maybe v pure


withDef2 :: MonadParsec e s m => m a -> m a -> m a
withDef2 v p = v >>= \x -> makePermParser $ id <$?> (x, try p)


-- |
-- The monadic effect to be intercalated between components of the syntax.
--
-- Consumes a comma character with leading and training whitespace.
comma :: (MonadParsec e s m,  Token s ~ Char) => m ()
comma = whitespace *> seperator *> whitespace
  where
    seperator = char ',' <?> "',' seperating arguments"


{-
-- |
-- Intercalates a monadic effect between actions.
iterM' :: (Monad m, Functor f) => m () -> (f (m a) -> m a) -> Free f a -> m a
iterM' _   _   (Pure x) = pure x
iterM' eff phi (Free f) = phi (iterM ((eff *>) . phi) <$> f)
-}


-- |
-- Intercalates a monadic effect between actions.
iterT' :: (Monad m, Functor f) => m () -> (f (m a) -> m a) -> FreeT f m a -> m a
iterT' eff f (FreeT m) = do
    val <- m
    case fmap (iterT ((eff *>) . f)) val of
      Pure x -> pure x
      Free y -> f y


primative :: (Monad p, MonadFree (ArgumentValue p) m) => SyntaxParser PrimativeValue p a -> m a
primative = liftF . APrimativeArg


{--}
data TestStruct = TS Int String [Double] deriving (Show)

tester :: MonadPlus p => SyntaxParser (ArgumentValue p) p TestStruct
tester = do
    age <- listId "age" int `withDefault` 42
    str <- text
    r   <- real
    pure $ TS age str [r]
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


type SyntaxParser f p a = FreeT f p a


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

{-

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
-}


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

