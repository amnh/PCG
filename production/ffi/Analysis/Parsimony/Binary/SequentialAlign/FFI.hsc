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

{-# LANGUAGE ForeignFunctionInterface, BangPatterns, TypeSynonymInstances, FlexibleInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Analysis.Parsimony.Binary.SequentialAlign.FFI
  ( sequentialAlign
  , testFn
  , main
  ) where

--import Bio.Character.Encodable
--import Bio.Character.Exportable.Class
import Data.Bits
import Data.Foldable
import Data.Monoid
import Foreign         hiding (alignPtr)
--import Foreign.Ptr
--import Foreign.C.String
import Foreign.C.Types
import System.IO.Unsafe
import Test.QuickCheck hiding ((.&.))

#include "dynamicCharacterOperations.h"
#include "seqAlignForHaskell.c"
#include <stdint.h>

-- TODO: replace when Yu Xiang updated his code for bit arrays.
-- | STUB, DO NOT USE
sequentialAlign :: Int -> Int -> s -> s -> Either String (Int, s, s)
sequentialAlign x y a b = Right (x + y, a, b)


-- Includes a struct (actually, a pointer thereto), and that struct, in turn, has a string
-- in it, so Ptr CChar.
-- Modified from code samples here: https://en.wikibooks.org/wiki/Haskell/FFI#Working_with_C_Structures
-- |
-- The result of the alignment from the C side of the FFI
data AlignResult
   = AlignResult
   { alignmentCost :: CInt
   , lengthFinal   :: CInt
   , seqFinal      :: Ptr CArrayUnit
   }


-- |
-- Type of a dynamic character to pass back and forth across the FFI interface.
data CDynamicChar
   = CDynamicChar
   { alphabetSize :: CInt
   , dynCharLen   :: CInt
   , dynChar      :: Ptr CArrayUnit
   }


-- | (✔)
instance Show CDynamicChar where
    show (CDynamicChar alphSize dcLen dChar) =
       mconcat
         ["alphabetSize:  "
         , show intAlphSize
         , "\ndynCharLen: "
         , show intLen
         , "\nbuffer length: "
         , show bufferLength
         , "\ndynChar:    "
         , show $ unsafePerformIO printedArr
         ]
        where
            (q, r)       = (intAlphSize * intLen) `divMod` 64
            bufferLength = q + if r == 0 then 0 else 1
            intAlphSize  = fromIntegral alphSize
            intLen       = fromIntegral dcLen
            printedArr   = show <$> peekArray bufferLength dChar


-- | (✔)
instance Arbitrary CDynamicChar where
    arbitrary = do
        alphSize <- (arbitrary :: Gen Int) `suchThat` (\x -> 0 < x && x <= 64)
        charSize <- (arbitrary :: Gen Int) `suchThat` (\x -> 0 < x && x <= 16)
        let (q,r)   = (alphSize * charSize) `divMod` 64
        fullBitVals <- vectorOf q (arbitrary :: Gen CArrayUnit)
        -- Note there is a faster way to do this loop in 2 steps by utilizing 2s compliment subtraction and setbit.
        let mask    = foldl' (\val i -> val `setBit` i) (zeroBits :: CArrayUnit) [0..r]
        remBitVals  <- if   r == 0
                       then pure []
                       else (pure . (mask .&.)) <$> (arbitrary :: Gen CArrayUnit)
        pure CDynamicChar
           { alphabetSize = fromIntegral alphSize
           , dynCharLen   = fromIntegral charSize
           , dynChar      = unsafePerformIO . newArray $ fullBitVals <> remBitVals
           }


-- |
-- A convient type alias for improved clairity of use.
type CArrayUnit  = CULong -- This will be compatible with uint64_t


-- | (✔)
instance Arbitrary CArrayUnit where
    arbitrary = do
        num <- arbitrary :: Gen Integer
        pure $ fromIntegral num


-- | (✔)
instance Storable CDynamicChar where
    sizeOf    _ = (#size struct dynChar_t) -- #size is a built-in that works with arrays, as are #peek and #poke, below
    alignment _ = alignment (undefined :: CArrayUnit)
    peek ptr    = do -- to get values from the C app
        alphLen <- (#peek struct dynChar_t, alphSize  ) ptr
        seqLen  <- (#peek struct dynChar_t, dynCharLen) ptr
        seqVal  <- (#peek struct dynChar_t, dynChar   ) ptr
        pure CDynamicChar
             { alphabetSize = alphLen
             , dynCharLen   = seqLen
             , dynChar      = seqVal
             }
    poke ptr (CDynamicChar alphLen seqLen seqVal) = do -- to modify values in the C app
        (#poke struct dynChar_t, alphSize  ) ptr alphLen
        (#poke struct dynChar_t, dynCharLen) ptr seqLen
        (#poke struct dynChar_t, dynChar   ) ptr seqVal


-- Because we're using a struct we need to make a Storable instance
-- | (✔)
instance Storable AlignResult where
    sizeOf    _ = (#size struct alignResult_t) -- #size is a built-in that works with arrays, as are #peek and #poke, below
    alignment _ = alignment (undefined :: CArrayUnit)
    peek ptr    = do -- to get values from the C app
        value  <- (#peek struct alignResult_t, finalWt ) ptr
        seqLen <- (#peek struct alignResult_t, finalLength) ptr
        seqVal <- (#peek struct alignResult_t, finalStr) ptr
        pure AlignResult
             { alignmentCost = value
             , lengthFinal   = seqLen
             , seqFinal      = seqVal
             }


------------- Don't need this part, but left in for completion ---------------
----- Will get compiler warning if left out, because of missing instances ----
    poke ptr (AlignResult cost seqLen seqVal) = do -- to modify values in the C app
        (#poke struct alignResult_t, finalWt    ) ptr cost
        (#poke struct alignResult_t, finalLength) ptr seqLen
        (#poke struct alignResult_t, finalStr   ) ptr seqVal


-- This is the declaration of the Haskell wrapper for the C function we're calling.
-- Note that this fn is called from testFn.f


-- |
-- FFI call to the C pairwise alignment algorithm with /defaulted/ sub & indel cost parameters
foreign import ccall unsafe "exportCharacter testFn"
    callExtFn_c  :: Ptr CDynamicChar -> Ptr CDynamicChar -> Ptr AlignResult -> CInt


-- |
-- FFI call to the C pairwise alignment algorithm with /explicit/ sub & indel cost parameters.
foreign import ccall unsafe "seqAlignForHaskell aligner"
    call_aligner :: Ptr CDynamicChar -> Ptr CDynamicChar -> CInt -> CInt -> Ptr AlignResult -> CInt


-- |
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
        if status == (0 :: CInt)
            then do
                AlignResult cost seqLen seqVal <- peek alignPtr
                seqFinalVal                    <- peekArray (fromIntegral seqLen) seqVal
                free seqVal
                pure $ Right (fromIntegral cost, show seqFinalVal)
            else do
                pure $ Left "Out of memory"


-- Just for testing from CLI outside of ghci.
-- | A test driver for the FFI functionality
main :: IO ()
main = do
    char1 <- generate (arbitrary :: Gen CDynamicChar)
    --char2 <- generate (arbitrary :: Gen CDynamicChar)
    print char1
    --print char2
    print $ testFn char1 char1


