
-- |
-- Module      :  Text.Megaparsec.Partition
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

module Text.Megaparsec.Partition
  ( PartitionParser
  , makePartitionParser
  , (<$:>)
  , (<|:>)
  ) where

import Text.Megaparsec.Combinator (choice)
import Text.Megaparsec.Prim

-- #if !MIN_VERSION_base(4,8,0)
-- import Control.Applicative ((<$>), (<*>))
-- #endif

infixl 1 <|:>
infixl 2 <$:>

-- | The type @PartitionParser s m a@ denotes a permutation parser that,
-- when converted by the 'makePartitionParser' function, produces instance of
-- 'MonadParsec' @m@ that parses @s@ stream and returns a value of type @a@
-- on success.
--
-- Normally, a permutation parser is first build with special operators like
-- ('<|:>') and than transformed into a normal parser using
-- 'makePermParser'.

-- | The parser @makePartitionParser perm@ parses a permutation of parser described
-- by @perm@. For example, suppose we want to parse a permutation of: an
-- optional string of @a@'s, the character @b@ and an optional @c@. This can
-- be described by:
--
-- > test = makePartitioningParser $
-- >          (,,) <$:> char 'a'
-- >               <|:> char 'b'
-- >               <|:> char 'c'


-- | The expression @f \<$$> p@ creates a fresh permutation parser
-- consisting of parser @p@. The the final result of the permutation parser
-- is the function @f@ applied to the return value of @p@. The parser @p@ is
-- not allowed to accept empty input â€” use the optional combinator ('<$?>')
-- instead.
--
-- If the function @f@ takes more than one parameter, the type variable @b@
-- is instantiated to a functional type which combines nicely with the adds
-- parser @p@ to the ('<|:>') combinator. This results in stylized code
-- where a permutation parser starts with a combining function @f@ followed
-- by the parsers. The function @f@ gets its parameters in the order in
-- which the parsers are specified, but actual input can be in any order.

(<$:>) :: MonadParsec s m t => (a -> b) -> m a -> PartitionParser s m b
f <$:> p = newperm f <|:> p

-- | The expression @perm \<|:> p@ adds parser @p@ to the permutation
-- parser @perm@. The parser @p@ is not allowed to accept empty input â€” use
-- the optional combinator ('<|:>') instead. Returns a new permutation
-- parser that includes @p@.

(<|:>) :: MonadParsec s m t => PartitionParser s m (a -> b) -> m a -> PartitionParser s m b
(<|:>) = undefined

data PartitionParser s m a = Partitioning (Maybe a) [Branch s m a]

data Branch s m a = forall b. Branch (PartitionParser s m (b -> a)) (m b)

makePartitionParser :: MonadParsec s m t => PartitionParser s m a -> m a
makePartitionParser (Partitioning def xs) = choice (fmap branch xs ++ empty)
  where empty = case def of
                  Nothing -> []
                  Just x  -> [return x]
        branch (Branch perm p) = flip ($) <$> p <*> makePartitionParser perm
                  
newperm :: MonadParsec s m t => (a -> b) -> PartitionParser s m (a -> b)
newperm f = Partitioning (Just f) []

-- | partitions build up a list of
-- (,,) char 'a' <|:> char 'b' <|:> char 'c'
-- [ a -> ([a], [b], [c]) -> ([a], [b], [c])
-- , b -> ([a], [b], [c]) -> ([a], [b], [c])
-- , c -> ([a], [b], [c]) -> ([a], [b], [c])
-- ]
            
add :: MonadParsec s m t => PartitionParser s m (a -> b) -> m a -> PartitionParser s m b
add perm@(Partitioning _mf fs) p = Partitioning Nothing (first : fmap insert fs)
  where first = Branch perm  p
        insert (Branch perm' p') = Branch (add (mapPartitions flip perm') p) p'

mapPartitions :: MonadParsec s m t => (a -> b) -> PartitionParser s m a -> PartitionParser s m b
mapPartitions f (Partitioning x xs) = Partitioning (fmap f x) (fmap mapBranch xs)
  where mapBranch (Branch perm p) = Branch (mapPartitions (f .) perm) p


