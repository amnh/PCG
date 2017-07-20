{-# LANGUAGE ApplicativeDo, ConstraintKinds, DeriveFunctor, ExistentialQuantification, FlexibleContexts, ScopedTypeVariables, TypeFamilies #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, RankNTypes, TypeSynonymInstances #-}

module PCG.Syntax.Combinators
  ( SyntacticArgument()
  , ListIdentifier(..)
  -- ** Primative Free Monad constructors
  , bool
  , int
  , real
  , text
  , time
  , value
  -- ** Syntactic Free Monad constructors
  , argList
  , choiceFrom
  , listId
  , manyOf
  , someOf
  , withDefault
  -- ** MonadParsec based Free Monad interpreter
  , runExpr
  )
where


import           Control.Applicative
import           Control.Applicative.Free   hiding (Pure)
import qualified Control.Applicative.Free   as Ap
import           Control.Alternative.Free   hiding (Pure,Ap)
import qualified Control.Alternative.Free   as A
import           Control.Alternative.Permutation
import qualified Control.Monad.Free         as F
import           Data.CaseInsensitive              (FoldCase)
import           Data.Functor                      (void)
import           Data.List.NonEmpty                (NonEmpty(..))
import           Data.Proxy
import           Data.Semigroup             hiding (option)
import           Data.Time.Clock                   (DiffTime)
import           PCG.Syntax.Primative              (PrimativeValue, parsePrimative, whitespace)
import qualified PCG.Syntax.Primative       as P
import           Text.Megaparsec
import           Text.Megaparsec.Char

--import Debug.Trace


--type SyntaxParser f p a = FreeT f p a


data ArgList z
    = Exact (       Ap.Ap SyntacticArgument  z)
    | SomeZ (Alt   (Ap.Ap SyntacticArgument) z)
    | ManyZ (Alt   (Ap.Ap SyntacticArgument) z)
    deriving (Functor)


data  SyntacticArgument z
    = PrimativeArg   (F.Free PrimativeValue z)
    | DefaultValue   (Ap.Ap SyntacticArgument z) z
    | ExactlyOneOf   (NonEmpty (Ap.Ap SyntacticArgument z))
--    | ListIdArg      ListIdentifier
    | ListIdNamedArg ListIdentifier (Ap.Ap SyntacticArgument z)
--    | CommandArg     SyntacticCommand
    | ArgumentList   (ArgList z)
    deriving (Functor)


newtype ListIdentifier = ListId String deriving (Show)


primative :: F.Free PrimativeValue a -> Ap.Ap SyntacticArgument a
primative = liftAp . PrimativeArg


bool :: Ap.Ap SyntacticArgument Bool
bool = primative P.bool


int :: Ap.Ap SyntacticArgument Int
int = primative P.int


real :: Ap.Ap SyntacticArgument Double
real = primative P.real


text :: Ap.Ap SyntacticArgument String
text = primative P.text


time :: Ap.Ap SyntacticArgument DiffTime
time = primative P.time


value :: String -> Ap.Ap SyntacticArgument ()
value str = primative $ P.value str


withDefault :: Ap.Ap SyntacticArgument a -> a -> Ap.Ap SyntacticArgument a
withDefault arg def = liftAp $ DefaultValue arg def


choiceFrom :: [Ap.Ap SyntacticArgument a] -> Ap.Ap SyntacticArgument a
choiceFrom    []  = error "You cannot construct an empty set of choices!"
choiceFrom (x:xs) = liftAp . ExactlyOneOf $ x:|xs


listId :: String -> Ap.Ap SyntacticArgument a -> Ap.Ap SyntacticArgument a
listId str x = liftAp $ ListIdNamedArg (ListId str) x


someOf :: Ap.Ap SyntacticArgument z -> Ap.Ap SyntacticArgument [z]
someOf = liftAp . ArgumentList . SomeZ . some . liftAlt


manyOf :: Ap.Ap SyntacticArgument z -> Ap.Ap SyntacticArgument [z]
manyOf = liftAp . ArgumentList . ManyZ . many . liftAlt


argList :: Ap.Ap SyntacticArgument a -> Ap.Ap SyntacticArgument a
argList = liftAp . ArgumentList . Exact


runExpr :: (FoldCase (Tokens s), MonadParsec e s m, Token s ~ Char) => Ap.Ap SyntacticArgument a -> m a
runExpr = runPermParser . f
  where
    f :: (FoldCase (Tokens s), MonadParsec e s m, Token s ~ Char) => Ap.Ap SyntacticArgument a -> Perm m a
    f = runAp' commaP apRunner

commaP :: (MonadParsec e s m, Token s ~ Char) => Perm m ()
commaP = toPerm $ whitespace *> seperator *> whitespace
      where
        seperator = char ',' <?> "',' seperating arguments"


apRunner :: forall a e m s. (FoldCase (Tokens s), MonadParsec e s m, Token s ~ Char) => Perm m () -> SyntacticArgument a -> Perm m a
apRunner effect (PrimativeArg p  ) = toPerm . try $ runPermParser effect *> F.iterM parsePrimative p
apRunner effect (ExactlyOneOf  xs) = toPerm . try $ runPermParser effect *> choice (runPermParser . runAp (apRunner voidEffect) <$> xs)
apRunner effect (ArgumentList p  ) = toPerm . try $ runPermParser effect *> runPermParser (parseArgumentList p)
apRunner effect (DefaultValue p v) = toPermWithDefault v . try
                                   $ runPermParser effect *> runPermParser (runAp (apRunner (toPerm voidEffect)) p)
apRunner effect (ListIdNamedArg (ListId x) y) = toPerm . try $ do
    _ <- string'' x <?> ("identifier '" <> x <> "'")
    _ <- whitespace <* char ':' <* whitespace
    runPermParser $ runAp (apRunner effect) y


parseArgumentList :: (FoldCase (Tokens s), MonadParsec e s m, Token s ~ Char) => ArgList a -> Perm m a
parseArgumentList argListVal = toPerm $ begin *> datum <* close
  where
    bookend p = void $ whitespace *> p <* whitespace
    begin = bookend . label "'(' starting a new argument list" $ char '('
    close = bookend . label "')' ending the argument list"     $ char ')'
    datum = 
      case argListVal of
        Exact e -> runExpr e
        SomeZ s -> runAlt' comma (runPermParser . runAp (apRunner voidEffect)) s
        ManyZ m -> runAlt' comma (runPermParser . runAp (apRunner voidEffect)) m



voidEffect :: Applicative f => f ()
voidEffect = pure ()


-- |
-- 
runAp' :: forall f g a. Applicative g => g () -> (forall x. g () -> f x -> g x) -> Ap.Ap f a -> g a
runAp' eff phi val =
    case val of
      Ap.Pure x -> pure x
      Ap.Ap f x -> flip id <$> phi voidEffect f <*> runAp'' eff phi x


runAp'' :: forall f g a. Applicative g => g () -> (forall x. g () -> f x -> g x) -> Ap.Ap f a -> g a
runAp'' eff phi val =
    case val of
      Ap.Pure x -> pure x
      Ap.Ap {}  -> runAp (phi eff) val


runAlt' :: forall f g a. Alternative g => g () -> (forall x. f x -> g x) -> Alt f a -> g a
runAlt' eff phi = go
  where
    go   :: Alt f b -> g b
    go   (A.Alt xs) = foldr (\r a -> flo r <|> a) empty xs

    flo  :: AltF f b -> g b
    flo  (A.Pure a) = pure a
    flo  (A.Ap x f) = flip id <$> phi x <*> go' f

    go'  :: Alt f b -> g b
    go'  (A.Alt xs) = foldr (\r a -> flo' r <|> a) empty xs

    flo' :: AltF f b -> g b
    flo' (A.Pure a) = pure a
    flo' (A.Ap x f) = flip id <$> ((eff *>) . phi) x <*> go' f


string'' :: forall e s m. (FoldCase (Tokens s), MonadParsec e s m,  Token s ~ Char) => String -> m ()
string'' = void . string' . tokensToChunk (Proxy :: Proxy s)


-- |
-- The monadic effect to be intercalated between components of the syntax.
--
-- Consumes a comma character with leading and training whitespace.
comma :: (MonadParsec e s m,  Token s ~ Char) => m ()
comma = whitespace *> seperator *> whitespace
  where
    seperator = char ',' <?> "',' seperating arguments"


{--
-- |
-- Intercalates a monadic effect between actions.
iterM' :: (Monad m, Functor f) => m () -> (f (m a) -> m a) -> F.Free f a -> m a
iterM' _   _   (F.Pure x) = pure x
iterM' eff phi (F.Free f) = phi (F.iterM ((eff *>) . phi) <$> f)


-- |
-- Intercalates a monadic effect between actions.
iterT' :: (Monad m, Functor f) => m () -> (f (m a) -> m a) -> FreeT f m a -> m a
iterT' eff f (FreeT m) = do
    val <- m
    case fmap (iterT ((eff *>) . f)) val of
      Pure x -> pure x
      Free y -> f y
--}



{--
data TestStruct = TS Int String [Double] deriving (Show)

tester :: MonadPlus p => SyntaxParser (ArgumentValue p) p TestStruct
tester = do
    age <- listId "age" int `withDefault` 42
    str <- text
    r   <- real
    pure $ TS age str [r]
-}


{--

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
--}


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

