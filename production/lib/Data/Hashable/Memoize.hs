{-# LANGUAGE BangPatterns #-}

module Data.Hashable.Memoize
  ( memoize
  ) where

import Data.Functor
import Data.Hashable
import Data.HashTable.IO
import Prelude hiding (lookup)
import System.IO.Unsafe

-- |
-- /O(1)/
--
-- Takes a function with a hashable and equatable first argument and returns a
-- memoized function with constant time access for already computed values.
--
-- __Note:__ This does /not/ memoize recursively defined functions.
--
-- To memoize a recursively defined function, you must redefine the function's
-- recursive calls to internally call a memoized definition in a mutually recursive
-- manner.
--
-- === Example:
--
-- > fib 0 = 0
-- > fib 1 = 1
-- > fib x = fib (x-1) + fib (x-2)
--
-- >>> let memo = memoize fib in memo 10000 -- does not memoize the recursive definitions
--
-- > fibM = f
-- >   where
-- >     f 0 = 0
-- >     f 1 = 1
-- >     f x = g (x-1) + g (x-2)
-- >     g = memoize f
--
-- >>> fibM 10000 -- will memoize the recursive definitions
--
{-# NOINLINE memoize #-}
memoize :: (Eq a, Hashable a) => (a -> b) -> a -> b
memoize f = unsafePerformIO $ do
    ht <- new :: IO (BasicHashTable a b)
    -- This is the returned closure of a memozized f
    -- The closure captures the mutable reference to the hashtable above
    -- Once the mutable hashtable reference is escaped from the IO monad,
    -- this creates a new memoized reference to f.
    -- The technique should be safe for all pure functions, probably, I think.
    pure $ \k -> unsafePerformIO $ do
        result <- ht `lookup` k
        case result of
          Just v  -> pure v
          Nothing ->
            let !v = f k
            in insert ht k v $> v

{-
-- These are included for haddock generation
fib 0 = 0
fib 1 = 1
fib x = fib (x-1) + fib (x-2)

fibM :: Integer -> Integer
fibM = f
  where
    f 0 = 0
    f 1 = 1
    f x = g (x-1) + g (x-2)
    g = memoize f
-}
