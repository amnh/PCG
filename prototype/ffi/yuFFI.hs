{-# LANGUAGE ForeignFunctionInterface, BangPatterns #-}

import System.IO.Unsafe
import Foreign
import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types

#include "yuAlign.h"

-- FFI to work with Yu's C code. Based on myComplexFfi.hs, which is modified from a wiki (noted therein)
data AlignResult = AlignResult { val      :: CInt
                               , seqFinal :: CString 
                               }

-- Because we're using a struct we need to make a Storable instance
instance Storable AlignResult where
    sizeOf    _ = (#size struct align)
    alignment _ = alignment (undefined :: CDouble)
    peek ptr = do
        value    <- (#peek struct align, finalWt) ptr
        sequence <- (#peek struct align, finalStr) ptr
        return  AlignResult { val = value, seqFinal = sequence }
------------- Don't need this part, but left in for completion ---------------
----- Will get compiler warning if left out, because of missing instances ----
    poke ptr (AlignResult val seqFinal) = do
        (#poke struct align, finalWt) ptr val
        (#poke struct align, finalStr) ptr seqFinal

-- This is the declaration of the Haskell wrapper for the C function we're calling.
foreign import ccall unsafe "yuAlign.h aligner"
    callExtFn_c :: CString -> CString -> CInt -> CInt -> Ptr AlignResult -> CInt

-- Just for testing from CLI outside of ghci.
main :: IO ()
main = putStrLn $ show callYuCode


callYuCode :: Either String (Int, String) 
callYuCode = unsafePerformIO $ 
    -- have to allocate memory. Note that we're allocating to a lambda fn. I 
    -- don't yet understand what exactly is going on here.
    alloca $ \alignPtr -> do 
        -- This first part is similar to simple example.
        arg1   <- newCAString "hello"
        arg2   <- newCAString "goodbye"
        -- Using strict here because of problems we had with simple example.
        let !status = callExtFn_c arg1 arg2 3 3 alignPtr
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
        
        
