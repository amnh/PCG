{-# LINE 1 "yuAlign.hsc" #-}
{-# LANGUAGE ForeignFunctionInterface, BangPatterns #-}
{-# LINE 2 "yuAlign.hsc" #-}

import System.IO.Unsafe
import Foreign
import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types


{-# LINE 10 "yuAlign.hsc" #-}

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
    sizeOf    _ = ((16))
{-# LINE 25 "yuAlign.hsc" #-}
    alignment _ = alignment (undefined :: CDouble)
    peek ptr = do
        value    <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) ptr
{-# LINE 28 "yuAlign.hsc" #-}
        sequence <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) ptr
{-# LINE 29 "yuAlign.hsc" #-}
        return  AlignResult { val = value, seqFinal = sequence }
------------- Don't need this part, but left in for completion ---------------
----- Will get compiler warning if left out, because of missing instances ----
    poke ptr (AlignResult val seqFinal) = do
        ((\hsc_ptr -> pokeByteOff hsc_ptr 0)) ptr val
{-# LINE 34 "yuAlign.hsc" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 8)) ptr seqFinal
{-# LINE 35 "yuAlign.hsc" #-}

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

