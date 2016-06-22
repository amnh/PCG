-----------------------------------------------------------------------------
-- |
-- a more complex example of an FFI interface, for learning
--
-- This example uses pointers, both to structs and to fields within the 
-- structs. This is much easier to accomplish via .hsc rather than doing 
-- straight FFI. A .hsc file are read by hsc2hs, which then creates a .c
-- file, which is compiled and run to create an .hs file, which is then 
-- compiled for use in outside modules.
--
-----------------------------------------------------------------------------

{-# LANGUAGE ForeignFunctionInterface, BangPatterns #-}

import Foreign
import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types
import System.IO.Unsafe

#include "myComplexTestC.h"

-- Includes a struct (actually, a pointer thereto), and that struct, in turn, has a string
-- in it, so Ptr CChar.
-- Modified from code samples here: https://en.wikibooks.org/wiki/Haskell/FFI#Working_with_C_Structures
data AlignResult = AlignResult { val      :: CInt
                               , seqFinal :: CString 
                               }

-- Because we're using a struct we need to make a Storable instance
instance Storable AlignResult where
    sizeOf    _ = (#size struct align) -- #size is a built-in that works with arrays, as are #peek and #poke, below
    alignment _ = alignment (undefined :: CChar)
    peek ptr    = do -- to get values from the C app
        value    <- (#peek struct align, finalWt) ptr
        sequence <- (#peek struct align, finalStr) ptr
        return  AlignResult { val = value, seqFinal = sequence }

------------- Don't need this part, but left in for completion ---------------
----- Will get compiler warning if left out, because of missing instances ----
    poke ptr (AlignResult val seqFinal) = do -- to modify values in the C app
        (#poke struct align, finalWt) ptr val
        (#poke struct align, finalStr) ptr seqFinal

-- This is the declaration of the Haskell wrapper for the C function we're calling.
-- Note that this fn is called from testFn.
foreign import ccall unsafe "myComplexTestC.h testFn"
    callExtFn_c :: CString -> CString -> CInt -> CInt -> Ptr AlignResult -> CInt

-- testFn can be called from within Haskell code.
testFn :: Either String (Int, String) 
testFn = unsafePerformIO $ 
    -- have to allocate memory. Note that we're allocating via a lambda fn. I 
    -- don't yet understand what exactly is going on here.
    alloca $ \alignPtr -> do 
        -- This first part is similar to simple example.
        arg1   <- newCAString "hello"
        arg2   <- newCAString "goodbye"

        -- Using strict here because the values need to be read before freeing, 
        -- so lazy is dangerous.
        let !status = callExtFn_c arg1 arg2 3 3 alignPtr
        free arg1
        free arg2

        -- Now checking return status. If 0, then all is well, otherwise throw an error.
        if (fromIntegral status) == 0 
            then do
                AlignResult val seq <- peek alignPtr
                seqStr              <- peekCAString seq
                free seq
                pure $ Right (fromIntegral val, seqStr)
            else do
                pure $ Left "Out of memory"
        
-- Just for testing from CLI outside of ghci.
main :: IO ()
main = putStrLn $ show testFn