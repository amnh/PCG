-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Hashable.Memoize
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Exposes memoization combinators. Assumes that the supplied functions are
-- side effect free. If this assumtion is violated, undefined and unexpected
-- behavior may result.
-----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Hashable.Memoize
  ( memoize
  , memoize2
  , memoize3
  ) where


import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.DeepSeq
import Control.Monad.ST
import Data.Functor
import Data.Hashable
import Data.HashTable.IO
--import Data.HashTable.ST.Basic
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
-- === Example: Does /not/ memoize the recursive definitions
--
-- > fib 0 = 0
-- > fib 1 = 1
-- > fib x = fib (x-1) + fib (x-2)
--
-- >>> let memo = memoize fib in memo 10000
--
--
-- === Example: Does memoize the recursive definitions
--
-- > fibM = f
-- >   where
-- >     f 0 = 0
-- >     f 1 = 1
-- >     f x = g (x-1) + g (x-2)
-- >     g = memoize f
--
-- >>> fibM 10000
--
{-# NOINLINE memoize #-}
memoize :: forall a b. (Eq a, Hashable a, NFData b) => (a -> b) -> a -> b
memoize f = unsafePerformIO $ do
    -- Create a TVar which holds the ST state and the HashTable
    !htRef <- newTVarIO (new :: IO (BasicHashTable a b))
    -- This is the returned closure of a memozized f
    -- The closure captures the "mutable" reference to the hashtable above
    -- through the TVar.
    --
    -- Once the mutable hashtable reference is escaped from the IO monad,
    -- this creates a new memoized reference to f.
    -- The technique should be safe for all pure functions, probably, I think.
    pure $ \k -> unsafePerformIO $ do
        -- Read the TVar, we use IO since it is the outer monad
        -- and the documentation says that this doesn't perform a complete transaction,
        -- it just reads the current value from the TVar
        st <- readTVarIO htRef
        -- We use the HashTable to try and lookup the memoized value
--        let result = runST $ (ht `lookup` k :: forall s. ST s (Maybe b))
        result <- (st >>= (\ht -> ht `lookup` k))
        -- Here we check if the memoized value exists
        case result of
          -- If the value exists return it
          Just v  -> pure v
          -- If the value doesn't exist:
          Nothing ->
            -- Perform the expensive calculation to determine the value
            -- associated with the key, fully evaluated.
            let v = force $ f k
            -- we want to perform the following modification atomically.
            in  atomically $
                  -- Don't use writeTVar or use a reference to the HashTable from above.
                  -- It may have been concurrently modified before reaching this point!
                  -- We *atomically* insert the new key-value pair into the exisiting
                  -- HashTable behind the TVar, modifying the results of the TVar.
                  modifyTVar' htRef
                    (\st -> st                -- Get the ST state from the TVar
                        >>= (\ht ->           -- Bind the hashtable in the state to x
                                insert ht k v -- Insert the key-value pair into the HashTable
                                $> ht         -- Return the HashTable as the value in ST state
                            )
                    )
                  -- After performing the update side effects,
                  -- we return the value associated with the key
                  $> v


-- |
-- A memoizing combinator similar to 'memoize' except that it that acts on a
-- function of two inputs rather than one.
{-# NOINLINE memoize2 #-}
memoize2 :: (Eq a, Eq b, Hashable a, Hashable b, NFData c) => (a -> b -> c) -> a -> b -> c
memoize2 f = let f' = memoize (uncurry f)
             in curry f'


-- |
-- A memoizing combinator similar to 'memoize' except that it that acts on a
-- function of two inputs rather than one.
{-# NOINLINE memoize3 #-}
memoize3
  :: ( Eq a
     , Eq b
     , Eq c
     , Hashable a
     , Hashable b
     , Hashable c
     , NFData d
     )
  => (a -> b -> c -> d)
  -> a
  -> b
  -> c
  -> d
memoize3 f = let f' = memoize (uncurry3 f)
             in curry3 f'
  where
    curry3   f  x y z  = f (x,y,z)
    uncurry3 f (x,y,z) = f x y z



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
