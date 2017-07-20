{-# LANGUAGE ApplicativeDo, ConstraintKinds, DeriveFunctor, ExistentialQuantification, FlexibleContexts, ScopedTypeVariables, TypeFamilies #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, RankNTypes, TypeSynonymInstances #-}

module PCG.Syntax.Combinators
  ( SyntacticArgument()
  , ListIdentifier(..)
  -- ** Primative Free Applicaitve constructors
  , bool
  , int
  , real
  , text
  , time
  , value
  -- ** Syntactic Free Applicative constructors
  , argList
  , choiceFrom
  , listId
  , manyOf
  , someOf
  , withDefault
  -- ** MonadParsec based syntactic interpreter
  , runSyntax
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


data ArgList z
    = Exact (       Ap.Ap SyntacticArgument  z)
    | SomeZ (Alt   (Ap.Ap SyntacticArgument) z)
    | ManyZ (Alt   (Ap.Ap SyntacticArgument) z)
    deriving (Functor)


data  SyntacticArgument z
    = PrimativeArg   (F.Free PrimativeValue z)
    | DefaultValue   (Ap.Ap SyntacticArgument z) z
    | ExactlyOneOf   (NonEmpty (Ap.Ap SyntacticArgument z))
    | ListIdNamedArg ListIdentifier (Ap.Ap SyntacticArgument z)
    | ArgumentList   (ArgList z)
    deriving (Functor)


newtype ListIdentifier = ListId String deriving (Show)


-- |
-- Define a boolean value as part of a command specification.
bool :: Ap.Ap SyntacticArgument Bool
bool = primative P.bool


-- |
-- Define a integer value as part of a command specification.
int :: Ap.Ap SyntacticArgument Int
int = primative P.int


-- |
-- Define a real value as part of a command specification.
--
-- Things that look like an integer can be captured as a real value by this
-- combinator. This can lead to ambiguity between `int` and 'real'. To avoid
-- ambiguity, use 'listId' to require a disambiguating prefix on one or more of
-- the command components.
real :: Ap.Ap SyntacticArgument Double
real = primative P.real

-- |
-- Define a textual value as part of a command specification.
text :: Ap.Ap SyntacticArgument String
text = primative P.text


-- |
-- Define a temporal value in minutes as part of a command specification.
time :: Ap.Ap SyntacticArgument DiffTime
time = primative P.time


-- |
-- Define a unique literal value as part of a command specification.
value :: String -> Ap.Ap SyntacticArgument ()
value str = primative $ P.value str


-- |
-- A list of arguments as part of a command.
argList :: Ap.Ap SyntacticArgument a -> Ap.Ap SyntacticArgument a
argList = liftAp . ArgumentList . Exact


-- |
-- Matches exactly one of the provided arguments to be used in the command.
choiceFrom :: [Ap.Ap SyntacticArgument a] -> Ap.Ap SyntacticArgument a
choiceFrom    []  = error "You cannot construct an empty set of choices!"
choiceFrom (x:xs) = liftAp . ExactlyOneOf $ x:|xs


-- |
-- Require a prefix on an agrument value to disambiguate it from other argument
-- values.
listId :: String -> Ap.Ap SyntacticArgument a -> Ap.Ap SyntacticArgument a
listId str x = liftAp $ ListIdNamedArg (ListId str) x


-- |
-- Produce zero or more of the provided argument for the command.
manyOf :: Ap.Ap SyntacticArgument z -> Ap.Ap SyntacticArgument [z]
manyOf = liftAp . ArgumentList . ManyZ . many . liftAlt


-- |
-- Produce one or more of the provided argument for the command.
someOf :: Ap.Ap SyntacticArgument z -> Ap.Ap SyntacticArgument [z]
someOf = liftAp . ArgumentList . SomeZ . some . liftAlt


-- |
-- Provide a default value for the argument if it is missing from the user input.
withDefault :: Ap.Ap SyntacticArgument a -> a -> Ap.Ap SyntacticArgument a
withDefault arg def = liftAp $ DefaultValue arg def


-- |
-- Create a 'MonadParsec' parser matching the specified semantics of the command.
runSyntax :: (FoldCase (Tokens s), MonadParsec e s m, Token s ~ Char) => Ap.Ap SyntacticArgument a -> m a
runSyntax = runPermParser . f
  where
    f :: (FoldCase (Tokens s), MonadParsec e s m, Token s ~ Char) => Ap.Ap SyntacticArgument a -> Perm m a
    f = runAp' commaP apRunner


-- == Internal functions == --


-- |
-- The \"natural transformation\" used to convert the Free Alternative to the
-- 'MonadParsec' parser result.
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


-- |
-- Part of the recursively defined evaluation where the argument permutations
-- are enumerated and evaluated.
parseArgumentList :: (FoldCase (Tokens s), MonadParsec e s m, Token s ~ Char) => ArgList a -> Perm m a
parseArgumentList argListVal = toPerm $ begin *> datum <* close
  where
    bookend p = void $ whitespace *> p <* whitespace
    begin = bookend . label "'(' starting a new argument list" $ char '('
    close = bookend . label "')' ending the argument list"     $ char ')'
    datum = 
      case argListVal of
        Exact e -> runSyntax e
        SomeZ s -> runAlt' comma (runPermParser . runAp (apRunner voidEffect)) s
        ManyZ m -> runAlt' comma (runPermParser . runAp (apRunner voidEffect)) m


-- |
-- The Applicative effect to be intercalated between components of the syntax.
--
-- Consumes a comma character with leading and training whitespace.
comma :: (MonadParsec e s m,  Token s ~ Char) => m ()
comma = whitespace *> seperator *> whitespace
  where
    seperator = char ',' <?> "',' seperating arguments"


-- |
-- The Applicative effect 'comma' nested within a permutation context.
commaP :: (MonadParsec e s m, Token s ~ Char) => Perm m ()
commaP = toPerm comma


-- |
-- Lifts a primative value Free Monad into a 'SyntacticArgument' context.
primative :: F.Free PrimativeValue a -> Ap.Ap SyntacticArgument a
primative = liftAp . PrimativeArg


-- |
-- Intercalates the effect when running the Free Alternative.
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


-- |
-- Intercalates the effect when running the Free Applicative. Is carefult to not
-- apply the effect before the first Applicative value, but before every
-- Applicative value after the first.
runAp' :: forall f g a. Applicative g => g () -> (forall x. g () -> f x -> g x) -> Ap.Ap f a -> g a
runAp' eff phi val =
    case val of
      Ap.Pure x -> pure x
      Ap.Ap f x -> flip id <$> phi voidEffect f <*> runAp'' eff phi x


-- |
-- Applies the effect before every Applicative value.
runAp'' :: forall f g a. Applicative g => g () -> (forall x. g () -> f x -> g x) -> Ap.Ap f a -> g a
runAp'' eff phi val =
    case val of
      Ap.Pure x -> pure x
      Ap.Ap {}  -> runAp (phi eff) val


-- |
-- Takes a 'String' and consumes a case-insensitive match in from the stream.
-- This is polymorphic over all streams where @Token ~ Char@.
string'' :: forall e s m. (FoldCase (Tokens s), MonadParsec e s m,  Token s ~ Char) => String -> m ()
string'' = void . string' . tokensToChunk (Proxy :: Proxy s)


-- |
-- The side effect of *no* side effects.
--
-- Useful for when a side effect is expected, but none should be performed.
voidEffect :: Applicative f => f ()
voidEffect = pure ()


