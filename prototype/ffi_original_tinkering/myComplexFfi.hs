{-# LINE 1 "myComplexFFI.hsc" #-}
{-# LANGUAGE ForeignFunctionInterface, BangPatterns #-}
{-# LINE 2 "myComplexFFI.hsc" #-}

import Foreign
import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types
import System.IO.Unsafe


{-# LINE 10 "myComplexFFI.hsc" #-}

-- Includes a struct (actually, a pointer thereto), and that struct, in turn, has a string
-- in it, so Ptr CChar
-- Modified from code samples here: https://en.wikibooks.org/wiki/Haskell/FFI#Working_with_C_Structures
data AlignResult = AlignResult { val      :: CInt
                               , seqFinal :: CString 
                               }

-- Because we're using a struct we need to make a Storable instance
instance Storable AlignResult where
    sizeOf    _ = ((16))
{-# LINE 21 "myComplexFFI.hsc" #-}
    alignment _ = alignment (undefined :: CChar)
    peek ptr = do
        value    <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) ptr
{-# LINE 24 "myComplexFFI.hsc" #-}
        sequence <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) ptr
{-# LINE 25 "myComplexFFI.hsc" #-}
        return  AlignResult { val = value, seqFinal = sequence }
------------- Don't need this part, but left in for completion ---------------
----- Will get compiler warning if left out, because of missing instances ----
    poke ptr (AlignResult val seqFinal) = do
        ((\hsc_ptr -> pokeByteOff hsc_ptr 0)) ptr val
{-# LINE 30 "myComplexFFI.hsc" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 8)) ptr seqFinal
{-# LINE 31 "myComplexFFI.hsc" #-}

-- This is the declaration of the Haskell wrapper for the C function we're calling.
foreign import ccall unsafe "myComplexTestC.h testFn"
    callExtFn_c :: CString -> CString -> CInt -> CInt -> Ptr AlignResult -> CInt

testFn :: Either String (Int, String) 
testFn = unsafePerformIO $ 
    -- have to allocate memory. Note that we're allocating via a lambda fn. I 
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
        
-- Just for testing from CLI outside of ghci.
main :: IO ()
main = putStrLn $ show testFn