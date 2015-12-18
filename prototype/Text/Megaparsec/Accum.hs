-- |
-- Module      :  Text.Megaparsec.Accum
-- Copyright   :  Â© 2015 Ward Wheeler
-- License     :  FreeBSD
--
-- Maintainer  :  Ward Wheeler <wheeler@amnh.org>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module implements a variation of permutation parsers designed
-- to accumulate values in a less rigid format. The algorithm is a
-- variation of the method described in: /Parsing Permutation Phrases/,
-- by Arthur Baars, Andres Loh and Doaitse Swierstra. Published as a
-- functional pearl at the Haskell Workshop 2001.

{-# LANGUAGE ExistentialQuantification #-}

module Text.Megaparsec.Accum
  ( AccumParser
  , makeAccumParser
  , (<$?$>)
  , (<$@$>)
  , (<$*$>)
  , (<$+$>)
  , (<|?|>)
  , (<|@|>)
  , (<|*|>)
  , (<|+|>)
  )
where

import Data.List.NonEmpty
import Text.Megaparsec.Combinator (choice)
import Text.Megaparsec.Prim

-- #if !MIN_VERSION_base(4,8,0)
-- import Control.Applicative ((<$>), (<*>))
-- #endif

infixl 1 <|?|>, <|@|>, <|*|>, <|+|>
infixl 2 <$?$>, <$@$>, <$*$>, <$+$>

-- | The type @AccumParser s m a@ denotes a permutation parser that,
-- when converted by the 'makePermParser' function, produces instance of
-- 'MonadParsec' @m@ that parses @s@ stream and returns a value of type @a@
-- on success.
--
-- Normally, a permutation parser is first build with special operators like
-- ('<||>') and than transformed into a normal parser using
-- 'makePermParser'.

data AccumParser s m a = Accum (Maybe a) [Branch s m a]

data Branch s m a = forall b. Branch (AccumParser s m (b -> a)) (m b)

-- | The parser @makeAccumParser perm@ parses a permutation of parser described
-- by @perm@. For example, suppose we want to parse a permutation of: an
-- optional string of @a@'s, the character @b@ and an optional @c@. This can
-- be described by:
--
-- > test = makeAccumParser $
-- >          (,,) <$?> ("", some (char 'a'))
-- >               <||> char 'b'
-- >               <|?> ('_', char 'c')

makeAccumParser :: MonadParsec s m t => AccumParser s m a -> m a
makeAccumParser (Accum def xs) = choice (fmap branch xs ++ empty)
  where empty = case def of
                  Nothing -> []
                  Just x  -> [return x]
        branch (Branch perm p) = flip ($) <$> p <*> makeAccumParser perm

-- | The expression @f \<$$> p@ creates a fresh permutation parser
-- consisting of parser @p@. The the final result of the permutation parser
-- is the function @f@ applied to the return value of @p@. The parser @p@ is
-- not allowed to accept empty input â€” use the optional combinator ('<$?>')
-- instead.
--
-- If the function @f@ takes more than one parameter, the type variable @b@
-- is instantiated to a functional type which combines nicely with the adds
-- parser @p@ to the ('<||>') combinator. This results in stylized code
-- where a permutation parser starts with a combining function @f@ followed
-- by the parsers. The function @f@ gets its parameters in the order in
-- which the parsers are specified, but actual input can be in any order.

(<$$>) :: MonadParsec s m t => (a -> b) -> m a -> AccumParser s m b
f <$$> p = newperm f <||> p

(<$?$>) :: MonadParsec s m t => (a -> b) -> m a -> AccumParser s m (Maybe b)
f <$?$> p = newperm f <|?|> p

(<$@$>) :: MonadParsec s m t => (a -> b) -> m a -> AccumParser s m b
f <$@$> p = newperm f <|@|> p

(<$*$>) :: MonadParsec s m t => (a -> b) -> m a -> AccumParser s m [b]
f <$*$> p = newperm f <|*|> p

(<$+$>) :: MonadParsec s m t => (a -> b) -> m a -> AccumParser s m (NonEmpty b)
f <$+$> p = newperm f <|+|> p
            
-- | The expression @perm \<||> p@ adds parser @p@ to the permutation
-- parser @perm@. The parser @p@ is not allowed to accept empty input â€” use
-- the optional combinator ('<|?>') instead. Returns a new permutation
-- parser that includes @p@.

(<||>) :: MonadParsec s m t
       => AccumParser s m (a -> b) -> m a -> AccumParser s m b
(<||>) = add

(<|?|>) :: MonadParsec s m t => AccumParser s m (a -> b) -> m a -> AccumParser s m (Maybe b)
(<|?|>) = undefined

(<|@|>) :: MonadParsec s m t => AccumParser s m (a -> b) -> m a -> AccumParser s m b
(<|@|>) = undefined

(<|*|>) :: MonadParsec s m t => AccumParser s m (a -> b) -> m a -> AccumParser s m [b]
(<|*|>) = undefined

(<|+|>) :: MonadParsec s m t => AccumParser s m (a -> b) -> m a -> AccumParser s m (NonEmpty b)
(<|+|>) = undefined

newperm :: MonadParsec s m t => (a -> b) -> AccumParser s m (a -> b)
newperm f = Accum (Just f) []

add :: MonadParsec s m t => AccumParser s m (a -> b) -> m a -> AccumParser s m b
add perm@(Accum _mf fs) p = Accum Nothing (first : fmap insert fs)
  where first = Branch perm p
        insert (Branch perm' p') = Branch (add (mapAccums flip perm') p) p'

addopt :: MonadParsec s m t => AccumParser s m (a -> b) -> a -> m a -> AccumParser s m b
addopt perm@(Accum mf fs) x p = Accum (fmap ($ x) mf) (first : fmap insert fs)
  where first   = Branch perm p
        insert (Branch perm' p') = Branch (addopt (mapAccums flip perm') x p) p'

mapAccums :: MonadParsec s m t => (a -> b) -> AccumParser s m a -> AccumParser s m b
mapAccums f (Accum x xs) = Accum (fmap f x) (fmap mapBranch xs)
  where mapBranch (Branch perm p) = Branch (mapAccums (f .) perm) p


