{-# LANGUAGE ForeignFunctionInterface, BangPatterns #-}

import System.IO.Unsafe
import Foreign
import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types

#include "yuAlign.h"

-- Includes a struct (actually, a pointer thereto), and that struct, in turn, has a string
-- in it, so Ptr CChar
-- Modified from code samples here: https://en.wikibooks.org/wiki/Haskell/FFI#Working_with_C_Structures
data AlignResult = AlignResult { val      :: CInt
                               , seqFinal :: CString 
                               }

-- This is the declaration of the Haskell wrapper for the C function we're calling.
foreign import ccall unsafe "yuAlign.h f"
    callExtAlignFn_c :: CInt

-- Because we're using a struct we need to make a Storable instance
instance Storable AlignResult where
    sizeOf    _ = (#size struct retType)
    alignment _ = alignment (undefined :: CDouble)
    peek ptr = do
        value    <- (#peek struct retType, weight) ptr
        sequence <- (#peek struct retType, alignment) ptr
        return  AlignResult { val = value, seqFinal = sequence }
------------- Don't need this part, but left in for completion ---------------
----- Will get compiler warning if left out, because of missing instances ----
    poke ptr (AlignResult val seqFinal) = do
        (#poke struct retType, weight) ptr val
        (#poke struct retType, alignment) ptr seqFinal

testAlign :: Either String (Int, String) 
testAlign = unsafePerformIO $ 
    -- have to allocate memory. Note that we're allocating to a lambda fn. I 
    -- don't yet understand what exactly is going on here.
    alloca $ \alignPtr -> do 
        -- This first part is similar to simple example.
        arg1   <- newCAString "hello"
        arg2   <- newCAString "goodbye"
        -- Using strict here because of problems we had with simple example.
        let !status = callExtAlignFn_c --arg1 arg2 1 1 alignPtr 
        free arg1
        free arg2
        if (fromIntegral status) == 0 
            then do
                AlignResult val seq <- peek alignPtr
                seqStr <- peekCAString seq
                free seq
                pure $ Right (fromIntegral val, seqStr)
            else do
                pure $ Left "Out of memory"

-- Just for testing from CLI outside of ghci.
main :: IO ()
main = putStrLn $ show testAlign

