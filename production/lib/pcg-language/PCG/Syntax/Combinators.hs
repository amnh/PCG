-----------------------------------------------------------------------------
-- |
-- Module      :  PCG.Syntax.Combinators
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Provides the syntactic combinators for specifying commands conforming to the
-- PCG scripting language syntax. Allows for specifying commands which will be
-- interpreted by the PCG scripting language syntax in a a context-free manner.
-- Being context-free allows for permutations of command component as long as no
-- two command components are ambiguously specified. Such ambiguity may lead to
-- unexpected results when using the provided parser 'runSyntax'.
--
-- Commands must be specified using an 'Applicative' style so that a Free
-- Applicative can be used as the syntactic & semantic parser.
--
-----------------------------------------------------------------------------


{-# LANGUAGE DeriveFunctor, FlexibleContexts, RankNTypes, ScopedTypeVariables, TypeFamilies #-}


module PCG.Syntax.Combinators
  ( CommandSpecification()
  , SyntacticArgument()
  , ArgumentIdentifier(..)
  -- ** Primative Free Applicaitve constructors
  , bool
  , int
  , real
  , text
  , time
  , value
  -- ** Syntactic Free Applicative constructors
  , argList
  , command
  , choiceFrom
  , argId
  , argIds
  , manyOf
  , someOf
  , withDefault
  -- ** MonadParsec based syntactic interpreter
  , parseCommand
  , runSyntax
  -- ** Whitespace definition of the syntax 
  , P.whitespace
  ) where


import           Control.Applicative
import           Control.Applicative.Free
import           Control.Alternative.Free   hiding (Pure,Ap)
import qualified Control.Alternative.Free   as Alt
import           Control.Alternative.Permutation
import qualified Control.Monad.Free         as F
import           Data.CaseInsensitive              (FoldCase)
import           Data.Foldable
import           Data.Functor                      (void)
import           Data.List.NonEmpty                (NonEmpty(..))
import           Data.Proxy
import           Data.Semigroup             hiding (option)
import           Data.String                       (IsString(..))
import           Data.Time.Clock                   (DiffTime)
import           PCG.Syntax.Primative              (PrimativeValue, parsePrimative, whitespace)
import qualified PCG.Syntax.Primative       as P
import           Text.Megaparsec
import           Text.Megaparsec.Char


data  ArgList z
    = Exact (       Ap SyntacticArgument  z)
    | SomeZ (Alt   (Ap SyntacticArgument) z)
    | ManyZ (Alt   (Ap SyntacticArgument) z)
    deriving (Functor)


-- |
-- Specifcation for a command in the PCG scripting language syntax.
data  CommandSpecification z
    = CommandSpec String (Ap SyntacticArgument z)


-- |
-- Component of a semantic command embedded in the PCG scripting language syntax.
data  SyntacticArgument z
    = PrimativeArg   (F.Free PrimativeValue z)
    | ArgIdNamedArg (Ap SyntacticArgument z) ArgumentIdentifier
    | DefaultValue   (Ap SyntacticArgument z) z
    | ExactlyOneOf   (NonEmpty (Ap SyntacticArgument z))
    | ArgumentList   (ArgList z)
    deriving (Functor)


-- |
-- A identifier to disambiguate values of the same type.
newtype ArgumentIdentifier = ArgId String


instance IsString ArgumentIdentifier where

    fromString = ArgId


instance Show ArgumentIdentifier where

    show (ArgId x) = show x


-- |
-- Define a boolean value as part of a command specification.
bool :: Ap SyntacticArgument Bool
bool = primative P.bool


-- |
-- Define a integer value as part of a command specification.
int :: Ap SyntacticArgument Int
int = primative P.int


-- |
-- Define a real value as part of a command specification.
--
-- Things that look like an integer can be captured as a real value by this
-- combinator. This can lead to ambiguity between `int` and 'real'. To avoid
-- ambiguity, use 'argId' to require a disambiguating prefix on one or more of
-- the command components.
real :: Ap SyntacticArgument Double
real = primative P.real


-- |
-- Define a textual value as part of a command specification.
text :: Ap SyntacticArgument String
text = primative P.text


-- |
-- Define a temporal value in minutes as part of a command specification.
time :: Ap SyntacticArgument DiffTime
time = primative P.time


-- |
-- Define a unique literal value as part of a command specification.
value :: String -> Ap SyntacticArgument ()
value str = primative $ P.value str


-- |
-- A list of arguments as part of a command.
argList :: Ap SyntacticArgument a -> Ap SyntacticArgument a
argList = liftAp . ArgumentList . Exact


-- |
-- Specifies a command in the PCG scripting language defined by the command name
-- and the argument structure.
command :: String -> Ap SyntacticArgument a -> CommandSpecification a
command = CommandSpec


-- |
-- Matches exactly one of the provided arguments to be used in the command.
choiceFrom :: Foldable f => f (Ap SyntacticArgument a) -> Ap SyntacticArgument a
choiceFrom opts =
    case toList opts of
      []   -> error "You cannot construct an empty set of choices!"
      x:xs -> liftAp . ExactlyOneOf $ x:|xs


-- |
-- Require a prefix on an agrument value to disambiguate it from other argument
-- values.
argId :: String -> Ap SyntacticArgument a -> Ap SyntacticArgument a
argId str x = liftAp $ ArgIdNamedArg x (ArgId str)


-- |
-- Require a prefix on an agrument value to disambiguate it from other argument
-- values. Accepts multiple aliases for the prefix used to disambiuate the
-- argument.
argIds :: Foldable f => f String -> Ap SyntacticArgument a -> Ap SyntacticArgument a
argIds strs x = choiceFrom $ liftAp . ArgIdNamedArg x . ArgId <$> toList strs


-- |
-- Produce zero or more of the provided argument for the command.
manyOf :: Ap SyntacticArgument z -> Ap SyntacticArgument [z]
manyOf = liftAp . ArgumentList . ManyZ . many . liftAlt


-- |
-- Produce one or more of the provided argument for the command.
someOf :: Ap SyntacticArgument z -> Ap SyntacticArgument [z]
someOf = liftAp . ArgumentList . SomeZ . some . liftAlt


-- |
-- Provide a default value for the argument if it is missing from the user input.
withDefault :: Ap SyntacticArgument a -> a -> Ap SyntacticArgument a
withDefault arg def = liftAp $ DefaultValue arg def


-- |
-- Parse a command specification.
parseCommand :: (FoldCase (Tokens s), MonadParsec e s m, Token s ~ Char) => CommandSpecification a -> m a
parseCommand (CommandSpec commandName defintion) = string'' commandName *> runSyntax defintion


-- |
-- Create a 'MonadParsec' parser matching the specified semantics of the command.
runSyntax :: (FoldCase (Tokens s), MonadParsec e s m, Token s ~ Char) => Ap SyntacticArgument a -> m a
runSyntax = runPermParserWithSeperator comma . runAp' noEffect apRunner
  where
    noEffect = toPerm voidEffect


-- == Internal functions == --


-- |
-- The \"natural transformation\" used to convert the Free Alternative to the
-- 'MonadParsec' parser result.
apRunner :: forall a e m s. (FoldCase (Tokens s), MonadParsec e s m, Token s ~ Char) => Perm m () -> SyntacticArgument a -> Perm m a
apRunner effect (PrimativeArg p  ) = toPerm . try $ runPermParser effect *> F.iterM parsePrimative p
apRunner effect (ExactlyOneOf ps ) = toPerm . try $ runPermParser effect *> choice (runPermParser . runAp (apRunner voidEffect) <$> ps)
apRunner effect (ArgumentList p  ) = toPerm . try $ runPermParser effect *> runPermParser (parseArgumentList p)
apRunner effect (DefaultValue p v) = toPermWithDefault v . try
                                   $ runPermParser effect *> runPermParser (runAp (apRunner (toPerm voidEffect)) p)
apRunner effect (ArgIdNamedArg p (ArgId x)) = toPerm . try $ do
    _ <- string'' x <?> ("identifier '" <> x <> "'")
    _ <- whitespace <* char ':' <* whitespace
    runPermParser $ runAp (apRunner effect) p


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
-- Lifts a primative value Free Monad into a 'SyntacticArgument' context.
primative :: F.Free PrimativeValue a -> Ap SyntacticArgument a
primative = liftAp . PrimativeArg


-- |
-- Intercalates the effect when running the Free Alternative.
runAlt' :: forall f g a. Alternative g => g () -> (forall x. f x -> g x) -> Alt f a -> g a
runAlt' eff phi = go
  where
    go   :: Alt f b -> g b
    go   (Alt.Alt xs) = foldr (\r a -> flo r <|> a) empty xs

    flo  :: AltF f b -> g b
    flo  (Alt.Pure a) = pure a
    flo  (Alt.Ap x f) = flip id <$> phi x <*> go' f

    go'  :: Alt f b -> g b
    go'  (Alt.Alt xs) = foldr (\r a -> flo' r <|> a) empty xs

    flo' :: AltF f b -> g b
    flo' (Alt.Pure a) = pure a
    flo' (Alt.Ap x f) = flip id <$> ((eff *>) . phi) x <*> go' f


-- |
-- Intercalates the effect when running the Free Applicative. Is carefult to not
-- apply the effect before the first Applicative value, but before every
-- Applicative value after the first.
runAp' :: forall f g a. Applicative g => g () -> (forall x. g () -> f x -> g x) -> Ap f a -> g a
runAp' eff phi val =
    case val of
      Pure x -> pure x
      Ap f x -> flip id <$> phi voidEffect f <*> runAp'' eff phi x


-- |
-- Applies the effect before every Applicative value.
runAp'' :: forall f g a. Applicative g => g () -> (forall x. g () -> f x -> g x) -> Ap f a -> g a
runAp'' eff phi val =
    case val of
      Pure x -> pure x
      Ap {}  -> runAp (phi eff) val


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


