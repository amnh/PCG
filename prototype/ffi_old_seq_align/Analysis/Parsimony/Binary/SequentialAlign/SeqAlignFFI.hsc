-----------------------------------------------------------------------------
-- |
-- Module      :  Analysis.Parsimony.Binary.SequentialAlign.SeqAlignFFI
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- An FFI export module of the sequential alignment heuristic developed by
-- Yu Xiang from Harvard.
--
-----------------------------------------------------------------------------
{-# LANGUAGE ForeignFunctionInterface, BangPatterns #-}

module Analysis.Parsimony.Binary.SequentialAlign.SeqAlignFFI
  ( sequentialAlign
  ) where

import Data.Char (toUpper)
import System.IO.Unsafe
import Foreign
-- import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types

#include "seqAlignForHaskell.h"

-- | Includes a struct (actually, a pointer thereto), and that struct, in turn, has a string
--   in it, so Ptr CChar.
--   Modified from code samples here: https://en.wikibooks.org/wiki/Haskell/FFI#Working_with_C_Structures
data AlignResult = AlignResult { val    :: CInt
                               , seq1   :: CString 
                               , seq2   :: CString
                               , seqLen :: CLong
                               }

-- This is the declaration of the Haskell wrapper for the C function we're calling.
foreign import ccall unsafe "seqAlignForHaskell.h aligner"
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
    poke ptr (AlignResult value seq1Fin seq2Fin alignLen) = do
        (#poke struct retType, weight) ptr value
        (#poke struct retType, seq1) ptr seq1Fin
        (#poke struct retType, seq2) ptr seq2Fin
        (#poke struct retType, alignmentLength) ptr alignLen -- need to be able to pass in length of alignemnt string

-- | A pure FFI call to YU Xiang's sequential alignment algorithm.
sequentialAlign :: Int -> Int -> String -> String -> Either String (Int, String, String) 
sequentialAlign indelCst subCst inpStr1 inpStr2 = unsafePerformIO $ 
    -- have to allocate memory. Note that we're allocating to a lambda fn. I 
    -- don't yet understand what exactly is going on here.
    alloca $ \alignStructPtr -> do 
        -- This first part is similar to simple example, but now I need to pre-allocate the length of the String *inside* the retAlign struct
        let len  = length inpStr1 + length inpStr2 + 5 -- This is the total length of the alignment. 
                                                       -- I padded each half by 2, and there's one more for NULL term. 
                                                       -- This math will have to be rectified in C.
        arg1     <- newCAString $ map (removeAmbDNA . toUpper) inpStr1
        arg2     <- newCAString $ map (removeAmbDNA . toUpper) inpStr2
        algnSeq1 <- newCAString $ replicate len ' ' -- to force allocation of space inside struct; will add NULL terminator in C.
        algnSeq2 <- newCAString $ replicate len ' '

        -- putting allocated aligned sequences and length of that string into return struct
        poke alignStructPtr $ AlignResult 0 algnSeq1 algnSeq2 (CLong (fromIntegral (succ len) :: Int64)) 
        
        -- Using strict here because of problems we had with simple example.
        let !status = callExtAlignFn_c arg1 arg2 (CInt (fromIntegral indelCst :: Int32)) (CInt (fromIntegral subCst :: Int32)) alignStructPtr 
        free arg1
        free arg2
        if (fromIntegral status :: Int) == 0 
            then do
                AlignResult value outSeq1 outSeq2 _ <- peek alignStructPtr
                seq1Fin <- peekCAString outSeq1
                seq2Fin <- peekCAString outSeq2
                free algnSeq1
                free algnSeq2
                pure $ Right (fromIntegral value, seq1Fin, seq2Fin)
            else do
                free algnSeq1
                free algnSeq2
                pure $ Left "Out of memory"

-- TODO: note I'm assuming all caps here.
-- TODO: order these by popularity?
removeAmbDNA :: Char -> Char
removeAmbDNA x = case x of
    'A' -> 'A'
    'C' -> 'C'
    'G' -> 'G'
    'T' -> 'T'
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
    '?' -> 'T'
    _   ->  x

-- Just for testing from CLI outside of ghci.
--main :: IO ()
--main = 
--    putStrLn $ show $ sequentialAlign 1 1 "ce" "GCT"

