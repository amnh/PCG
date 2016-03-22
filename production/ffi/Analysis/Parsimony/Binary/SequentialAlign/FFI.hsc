{-# LANGUAGE ForeignFunctionInterface, BangPatterns #-}

module Analysis.Parsimony.Binary.SequentialAlign.FFI where

import System.IO.Unsafe
import Foreign
import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types

#include "seqAlign_ffi.h"

-- Includes a struct (actually, a pointer thereto), and that struct, in turn, has a string
-- in it, so Ptr CChar
-- Modified from code samples here: https://en.wikibooks.org/wiki/Haskell/FFI#Working_with_C_Structures
data AlignResult = AlignResult { val    :: CInt
                               , seq1   :: CString 
                               , seq2   :: CString
                               , seqLen :: CLong
                               }

-- This is the declaration of the Haskell wrapper for the C function we're calling.
foreign import ccall unsafe "seqAlign_ffi.h aligner"
    callExtAlignFn_c :: CString -> CString -> CInt -> CInt -> Ptr AlignResult -> CInt

-- Because we're using a struct we need to make a Storable instance
instance Storable AlignResult where
    sizeOf    _ = (#size struct retType)
    alignment _ = alignment (undefined :: CDouble)
    peek ptr = do
        value   <- (#peek struct retType, weight) ptr
        seq1Fin <- (#peek struct retType, seq1) ptr
        seq2Fin <- (#peek struct retType, seq2) ptr
        algnLen <- (#peek struct retType, alignmentLength) ptr
        return  AlignResult { val = value, seq1 = seq1Fin, seq2 = seq2Fin, seqLen = algnLen }
------------- Don't need this part, but left in for completion ---------------
----- Will get compiler warning if left out, because of missing instances ----
    poke ptr (AlignResult val seq1Fin seq2Fin alignLen) = do
        (#poke struct retType, weight) ptr val
        (#poke struct retType, seq1) ptr seq1Fin
        (#poke struct retType, seq2) ptr seq2Fin
        (#poke struct retType, alignmentLength) ptr alignLen -- need to be able to pass in length of alignemnt string

sequentialAlign :: Int -> Int -> String -> String -> Either String (Int, String, String) 
sequentialAlign indelCst subCst inpStr1 inpStr2 = unsafePerformIO $ 
    -- have to allocate memory. Note that we're allocating to a lambda fn. I 
    -- don't yet understand what exactly is going on here.
    alloca $ \alignPtr -> do 
        -- This first part is similar to simple example, but now I need to pre-allocate the length of the String *inside* the retAlign struct
        let len  = length inpStr1 + length inpStr2 + 5 -- This is the total length of the alignment. 
                                                       -- I padded each half by 2, and there's one more for NULL term. 
                                                       -- This math will have to be rectified in C.
        arg1     <- newCAString $ map removeAmbDNA inpStr1
        arg2     <- newCAString $ map removeAmbDNA inpStr2
        algnSeq1 <- newCAString $ replicate len ' ' -- to force allocation of space inside struct; will add NULL terminator in C.
        algnSeq2 <- newCAString $ replicate len ' '

        -- putting allocated aligned sequences and length of that string into return struct
        poke alignPtr $ AlignResult 0 algnSeq1 algnSeq2 (CLong (fromIntegral (succ len) :: Int64)) 
        
        -- Using strict here because of problems we had with simple example.
        let !status = callExtAlignFn_c arg1 arg2 (CInt (fromIntegral indelCst :: Int32)) (CInt (fromIntegral subCst :: Int32)) alignPtr 
        free arg1
        free arg2
        if (fromIntegral status) == 0 
            then do
                AlignResult val outSeq1 outSeq2 _ <- peek alignPtr
                seq1Fin <- peekCAString algnSeq1
                seq2Fin <- peekCAString outSeq2
                free algnSeq1
                free algnSeq2
                pure $ Right (fromIntegral val, seq1Fin, seq2Fin)
            else do
                free algnSeq1
                free algnSeq2
                pure $ Left "Out of memory"

-- TODO: note I'm assuming all caps here.
removeAmbDNA :: Char -> Char
removeAmbDNA x = case x of
    'B' -> 'C'
    'D' -> 'A'
    'H' -> 'A'
    'K' -> 'G'
    'M' -> 'A'
    'N' -> 'A'
    'R' -> 'A'
    'S' -> 'C'
    'V' -> 'A'
    'W' -> 'A'
    'X' -> 'A'
    'Y' -> 'C'
    _   ->  x

-- Just for testing from CLI outside of ghci.
main :: IO ()
main = 
    putStrLn $ show $ sequentialAlign 1 1 "CE" "GCT"

