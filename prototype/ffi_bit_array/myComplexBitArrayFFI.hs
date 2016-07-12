{-# LINE 1 "myComplexBitArrayFFI.hsc" #-}
-----------------------------------------------------------------------------
{-# LINE 2 "myComplexBitArrayFFI.hsc" #-}
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

{-# LANGUAGE ForeignFunctionInterface, BangPatterns, TypeSynonymInstances, FlexibleInstances #-}

import Data.Bits
import Data.Foldable
import Data.Monoid
import Foreign
import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types
import System.IO.Unsafe
import Test.QuickCheck hiding ((.&.))


{-# LINE 26 "myComplexBitArrayFFI.hsc" #-}

{-# LINE 27 "myComplexBitArrayFFI.hsc" #-}

-- Includes a struct (actually, a pointer thereto), and that struct, in turn, has a string
-- in it, so Ptr CChar.
-- Modified from code samples here: https://en.wikibooks.org/wiki/Haskell/FFI#Working_with_C_Structures
data AlignResult = AlignResult { alignmentCost :: CInt
                               , lengthFinal   :: CInt
                               , seqFinal      :: Ptr CArrayUnit
                               }

data CDynamicChar = CDynamicChar { alphabetSize :: CInt
                                 , dynCharLen   :: CInt
                                 , dynChar      :: Ptr CArrayUnit
                                 }

instance Show CDynamicChar where
    show (CDynamicChar alphSize dcLen dChar) = mconcat ["alphabetSize:  "
                                                       , show intAlphSize
                                                       , "\ndynCharLen: "
                                                       , show intLen
                                                       , "\nbuffer length: "
                                                       , show bufferLength
                                                       , "\ndynChar:    "
                                                       , show $ unsafePerformIO printedArr
                                                       ]
        where
            (quot, rem)  = (intAlphSize * intLen) `divMod` 64
            bufferLength = quot + if rem == 0 then 0 else 1
            intAlphSize  = fromIntegral alphSize
            intLen       = fromIntegral dcLen
            printedArr   = show <$> peekArray bufferLength dChar

instance Arbitrary CDynamicChar where
    arbitrary = do
        alphSize <- (arbitrary :: Gen Int) `suchThat` (\x -> 0 < x && x <= 64)
        charSize <- (arbitrary :: Gen Int) `suchThat` (\x -> 0 < x && x <= 16)
        let (full,rem) = (alphSize * charSize) `divMod` 64
        fullBitVals <- vectorOf full (arbitrary :: Gen CArrayUnit)
        -- Note there is a faster way to do this loop in 2 steps by utilizing 2s compliment subtraction and setbit.
        let mask    = foldl' (\val i -> val `setBit` i) (zeroBits :: CArrayUnit) [0..rem]
        remBitVals  <- if   rem == 0
                       then pure []
                       else (pure . (mask .&.)) <$> (arbitrary :: Gen CArrayUnit)
        pure CDynamicChar
           { alphabetSize = fromIntegral alphSize
           , dynCharLen   = fromIntegral charSize
           , dynChar      = unsafePerformIO . newArray $ fullBitVals <> remBitVals
           }

type CArrayUnit  = CULong -- This will be compatible with uint64_t

instance Arbitrary CArrayUnit where
    arbitrary = do
        num <- arbitrary :: Gen Integer
        pure $ fromIntegral num

instance Storable CDynamicChar where
    sizeOf    _ = ((16)) -- #size is a built-in that works with arrays, as are #peek and #poke, below
{-# LINE 84 "myComplexBitArrayFFI.hsc" #-}
    alignment _ = alignment (undefined :: CArrayUnit)
    peek ptr    = do -- to get values from the C app
        alphLen  <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) ptr
{-# LINE 87 "myComplexBitArrayFFI.hsc" #-}
        seqLen   <- ((\hsc_ptr -> peekByteOff hsc_ptr 4)) ptr
{-# LINE 88 "myComplexBitArrayFFI.hsc" #-}
        sequence <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) ptr
{-# LINE 89 "myComplexBitArrayFFI.hsc" #-}
        return  CDynamicChar { alphabetSize = alphLen
                             , dynCharLen   = seqLen
                             , dynChar      = sequence  
                             }
    poke ptr (CDynamicChar alphabetSize dynCharLen dynChar) = do -- to modify values in the C app
        ((\hsc_ptr -> pokeByteOff hsc_ptr 0)) ptr alphabetSize
{-# LINE 95 "myComplexBitArrayFFI.hsc" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 4)) ptr dynCharLen
{-# LINE 96 "myComplexBitArrayFFI.hsc" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 8)) ptr dynChar
{-# LINE 97 "myComplexBitArrayFFI.hsc" #-}

-- Because we're using a struct we need to make a Storable instance
instance Storable AlignResult where
    sizeOf    _ = ((16)) -- #size is a built-in that works with arrays, as are #peek and #poke, below
{-# LINE 101 "myComplexBitArrayFFI.hsc" #-}
    alignment _ = alignment (undefined :: CArrayUnit)
    peek ptr    = do -- to get values from the C app
        value    <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) ptr
{-# LINE 104 "myComplexBitArrayFFI.hsc" #-}
        len      <- ((\hsc_ptr -> peekByteOff hsc_ptr 4)) ptr
{-# LINE 105 "myComplexBitArrayFFI.hsc" #-}
        sequence <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) ptr
{-# LINE 106 "myComplexBitArrayFFI.hsc" #-}
        return  AlignResult { alignmentCost = value, lengthFinal = len, seqFinal = sequence }

------------- Don't need this part, but left in for completion ---------------
----- Will get compiler warning if left out, because of missing instances ----
    poke ptr (AlignResult cost len seqFinal) = do -- to modify values in the C app
        ((\hsc_ptr -> pokeByteOff hsc_ptr 0)) ptr cost
{-# LINE 112 "myComplexBitArrayFFI.hsc" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 4)) ptr len
{-# LINE 113 "myComplexBitArrayFFI.hsc" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 8)) ptr seqFinal
{-# LINE 114 "myComplexBitArrayFFI.hsc" #-}

-- This is the declaration of the Haskell wrapper for the C function we're calling.
-- Note that this fn is called from testFn.
foreign import ccall unsafe "myComplexBitArrayTestC.h testFn"
    callExtFn_c :: Ptr CDynamicChar -> Ptr CDynamicChar -> Ptr AlignResult -> CInt

-- testFn can be called from within Haskell code.
testFn :: CDynamicChar -> CDynamicChar -> Either String (Int, String) 
testFn char1 char2 = unsafePerformIO $ 
    -- have to allocate memory. Note that we're allocating via a lambda fn. In use, the lambda will take whatever is the 
    -- argument of testFn, but here there is no argument, so all allocation is hard-coded.
    alloca $ \alignPtr -> do 
        marshalledChar1 <- new char1
        marshalledChar2 <- new char2
        print marshalledChar1
        -- Using strict here because the values need to be read before freeing, 
        -- so lazy is dangerous.
        let !status = callExtFn_c marshalledChar1 marshalledChar2 alignPtr

        -- Now checking return status. If 0, then all is well, otherwise throw an error.
        if (fromIntegral status) == 0 
            then do
                AlignResult cost len seq <- peek alignPtr
                --seqFinalPtr              <- peek seq
                seqFinal                 <- peekArray (fromIntegral len) seq
                free seq
                pure $ Right (fromIntegral cost, show seqFinal)
            else do
                pure $ Left "Out of memory"
        
-- Just for testing from CLI outside of ghci.
main :: IO ()
main = do 
    char1 <- generate (arbitrary :: Gen CDynamicChar)
    --char2 <- generate (arbitrary :: Gen CDynamicChar)
    print char1
    --print char2
    print $ testFn char1 char1


data ExportableDynamicCharacter
   = ExportableDynamicCharacter
   { characterLength :: Int
   , alphabetLength  :: Int
   , bufferChunks    :: [CArrayUnit]
   } deriving (Eq, Show)

toExportable :: EncodableDynamicCharacter c => c -> ExportableDynamicCharacter
toExportable (DC (BitMatrix alphabetSize bv)) =
     ExportableDynamicCharacter
     { characterLength = 0
     , alphabetLength  = 0
     , bufferChunks    = []
     }
