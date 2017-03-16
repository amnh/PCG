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
import Data.Word
import Foreign
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Ptr           (Ptr)

foreign import ccall unsafe "mySimpleBitArrayTestC.h testFN"
     callExtFn_c :: Ptr CULong -> CInt -> CInt -> CInt

{-
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
-}

type CArrayUnit  = CULong -- This will be compatible with uint64_t
-- so I can use this multiple times below. Allows to quickly change to 32 bits, for instance. 
-- Note that changing this to Int32 will probably require changes in the C code.
data IntArr = IntArr { val :: [CArrayUnit] } deriving (Show)

-- Because we're using a struct we need to make a Storable instance
instance Storable IntArr where
    sizeOf  _ = 8 -- hard-coding (for now), as size, which is just a pointer.
    alignment _ = alignment (undefined :: CArrayUnit)
    
    peek ptr    = do 
        value <- (\hsc_ptr -> peek hsc_ptr) ptr
        return  value

------------- Don't need this part, but left in for completion ---------------
----- Will get compiler warning if left out, because of missing instances ----
------------------- Also, it's currently heavily broken ----------------------
    poke ptr val = do 
        ((\hsc_ptr -> pokeElemOff hsc_ptr 0 val)) ptr

testFn :: Either String IntArr
testFn = unsafePerformIO $ 
    -- have to allocate memory. Note that we're allocating via a lambda fn. In use, the lambda will take whatever is the 
    -- argument of testFn, but here there is no argument, as all allocation is hard-coded.

    -- length of the array is hard-coded
    let 
        thisElem :: CArrayUnit
        thisElem = 0
        input  = [thisElem, thisElem]
    in
        do 
            my_arr <- (newArray input)
                    
            -- Using strict here because the values need to be read before freeing, 
            -- so lazy is dangerous.
            let arr_len = (length input)
                !status = callExtFn_c my_arr (fromIntegral arr_len) (fromIntegral $ 8 * sizeOf (undefined :: CArrayUnit)) -- sizeOf returns bytes

            
            -- Now checking return status. If 0, then all is well, otherwise throw an error.
            if status == 0 
                then (Right . (\vals -> IntArr { val = vals })) <$> peekArray arr_len my_arr
                else pure $ Left "Out of memory"

-- Just for testing from CLI outside of ghci.
main :: IO ()
main = putStrLn $ show testFn