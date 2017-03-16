-----------------------------------------------------------------------------
-- |
-- a simple example of an FFI interface, for learning
--
-- Note that the simple example does not use .hsc, and interfaces, therefore 
-- directly with the .c.
--
-----------------------------------------------------------------------------

{-# LANGUAGE ForeignFunctionInterface, BangPatterns #-}

import System.IO.Unsafe
import Foreign
import Foreign.C.String
import Foreign.C.Types

foreign import ccall unsafe "mySimpleTestC.h testFN"
     callExtFn_c :: CString -> CString -> CInt -> CInt -> CString

main :: IO ()
main = putStrLn testFn

testFn :: String
testFn = unsafePerformIO $ do -- This means that I have to guarantee the C code is (externally only?) pure.
    
    arg1   <- newCAString "hello"
    arg2   <- newCAString "goodbye"

    -- Strictness to prevent preemptive free calls
    -- FFI function call allocates memory that needs to be freed after marshaling
    let !buffer = callExtFn_c arg1 arg2 1 3
    free arg1
    free arg2

    -- Copies CString from FFI to Haskell String
    result <- peekCAString buffer
    
    -- Free the FFI allocated memory
    free buffer
    pure result

