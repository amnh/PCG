-----------------------------------------------------------------------------
-- |
-- TODO: Document module.
--
-- Exports C types for dynamic characters and their constructors allong with
-- an FFI binding for the memoizing TCM structure.
-----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeSynonymInstances     #-}

module Data.TCM.Memoized.Types
  ( CBufferUnit
  , CDynamicChar(..)
  , DCElement(..)
  , ForeignVoid()
  , MemoizedCostMatrix(..)
  -- * Utility functions
  , calculateBufferLength
  , coerceEnum
  -- * Constructors
  , constructCharacter
  , constructElement
  , constructEmptyElement
  -- * Destructors 
  , destructCharacter
  , destructElement
  ) where

import Bio.Character.Exportable
import Control.DeepSeq
import Data.Bits
import Data.Foldable
import Foreign         hiding (alignPtr)
import Foreign.C.Types
import GHC.Generics           (Generic)
import System.IO.Unsafe
import Test.QuickCheck hiding ((.&.), output)

#include "costMatrixWrapper.h"
#include "dynamicCharacterOperations.h"


-- |
-- A convient type alias for improved clairity of use.
type CBufferUnit  = CULong -- This will be compatible with uint64_t


-- |
-- Type of a dynamic character to pass back and forth across the FFI interface.
data  CDynamicChar
    = CDynamicChar
    { alphabetSizeChar :: CSize
    , numElements      :: CSize
    , dynCharLen       :: CSize
    , dynChar          :: Ptr CBufferUnit
    }


-- |
-- Represents a single element in a dynamic character in an exportable form.
data  DCElement
    = DCElement
    { alphabetSizeElem :: CSize
    , characterElement :: Ptr CBufferUnit
    } deriving (Show)


-- |
-- A closed type wrapping a void pointer in C to the C++ memoized TCM.
data ForeignVoid deriving (Generic)


-- |
-- A type-safe wrapper for the mutable, memoized TCm.
data  MemoizedCostMatrix
    = MemoizedCostMatrix
    { costMatrix :: StablePtr ForeignVoid
    } deriving (Eq, Generic)


instance Show MemoizedCostMatrix where

    show = const "(MemoizedCostMatrix ?)"


{--
-- | (✔)
instance Arbitrary CULong where

    arbitrary = do
        num <- arbitrary :: Gen Integer
        pure $ fromIntegral num
--}


-- | (✔)
instance Arbitrary CDynamicChar where

    arbitrary = do
        alphSize    <- (arbitrary :: Gen Int) `suchThat` (\x -> 0 < x && x <= 64)
        charSize    <- (arbitrary :: Gen Int) `suchThat` (\x -> 0 < x && x <= 16)
        numElems    <- (arbitrary :: Gen Int) `suchThat` (\x -> 0 < x && x <= 100)
        fullBitVals <- vectorOf numElems (arbitrary :: Gen CBufferUnit)
        -- Note there is a faster way to do this loop in 2 steps by utilizing 2s compliment subtraction and setbit.
        let mask    = foldl' (\val i -> val `setBit` i) (zeroBits :: CBufferUnit) [0..numElems]
        remBitVals  <- if   numElems == 0
                       then pure []
                       else (pure . (mask .&.)) <$> (arbitrary :: Gen CBufferUnit)
        pure CDynamicChar
           { alphabetSizeChar = toEnum alphSize
           , dynCharLen       = toEnum charSize
           , numElements      = toEnum numElems
           , dynChar          = unsafePerformIO . newArray $ fullBitVals <> remBitVals
           }


instance NFData ForeignVoid


instance NFData MemoizedCostMatrix where

    rnf (MemoizedCostMatrix !_) = ()


{-
-- | (✔)
instance Show CDynamicChar where
    show (CDynamicChar alphSize dcLen numElems dChar) =
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
            bufferLength = fromEnum numElems
            intAlphSize  = fromEnum alphSize
            intLen       = fromEnum dcLen
            printedArr   = show <$> peekArray bufferLength dChar

-}


instance Storable CDynamicChar where

    sizeOf    _ = (#size struct dynChar_t) -- #size is a built-in that works with arrays, as are #peek and #poke, below

    alignment _ = alignment (undefined :: CBufferUnit)

    peek ptr    = do -- to get values from the C app
        alphLen <- (#peek struct dynChar_t, alphSize  ) ptr
        nElems  <- (#peek struct dynChar_t, numElems  ) ptr
        seqLen  <- (#peek struct dynChar_t, dynCharLen) ptr
        seqVal  <- (#peek struct dynChar_t, dynChar   ) ptr
        pure CDynamicChar
             { alphabetSizeChar = alphLen
             , numElements      = nElems
             , dynCharLen       = seqLen
             , dynChar          = seqVal
             }

    poke ptr (CDynamicChar alphLen nElems seqLen seqVal) = do -- to modify values in the C app
        (#poke struct dynChar_t, alphSize  ) ptr alphLen
        (#poke struct dynChar_t, numElems  ) ptr nElems
        (#poke struct dynChar_t, dynCharLen) ptr seqLen
        (#poke struct dynChar_t, dynChar   ) ptr seqVal


-- | (✔)
instance Storable DCElement where

    sizeOf    _ = (#size struct dcElement_t)

    alignment _ = alignment (undefined :: CBufferUnit)

    peek ptr    = do
        alphLen <- (#peek struct dcElement_t, alphSize) ptr
        element <- (#peek struct dcElement_t, element ) ptr
        pure DCElement
            { alphabetSizeElem = alphLen
            , characterElement = element
            }

    poke ptr (DCElement alphLen element) = do
        (#poke struct dcElement_t, alphSize) ptr alphLen
        (#poke struct dcElement_t, element ) ptr element


{-
instance Storable MemoizedCostMatrix where

    sizeOf    _ = (#size void*) -- #size is a built-in that works with arrays, as are #peek and #poke, below

    alignment _ = alignment (undefined :: CBufferUnit)

    peek _ptr   = undefined

    poke _ptr   = undefined
-}



{-
-- TODO: For now we only allocate 2d matrices. 3d will come later.
-- |
-- Create and allocate cost matrix.
-- The first argument, TCM, is only for non-ambiguous nucleotides, and it used to
-- generate the entire cost matrix, which includes ambiguous elements. TCM is
-- row-major, with each row being the left character element. It is therefore
-- indexed not by powers of two, but by cardinal integer.
foreign import ccall unsafe "costMatrixWrapper matrixInit"
    initializeMemoizedCMfn_c :: CSize
                             -> Ptr CUInt
                             -> IO (StablePtr ForeignVoid)


foreign import ccall unsafe "costMatrix getCostAndMedian2D"
    getCostAndMedian2D_c :: Ptr DCElement
                         -> Ptr DCElement
                         -> Ptr DCElement
                         -> StablePtr ForeignVoid
                         -> IO CUInt


foreign import ccall unsafe "costMatrix getCostAndMedian3D"
    getCostAndMedian3D_c :: Ptr DCElement
                         -> Ptr DCElement
                         -> Ptr DCElement
                         -> Ptr DCElement
                         -> StablePtr ForeignVoid
                         -> IO CUInt
-}


-- |
-- /O(1)/
--
-- Calculate the buffer length based on the element count and element bit width.
calculateBufferLength :: Enum b
                      => Word -- ^ Element count
                      -> Word -- ^ Element bit width
                      -> b
calculateBufferLength count width = coerceEnum $ q + if r == 0 then 0 else 1
   where
    (q,r)  = (count * width) `divMod` toEnum (finiteBitSize (undefined :: CULong))


-- |
-- Coerce one 'Enum' value to another through the type's corresponding 'Int'
-- values.
coerceEnum :: (Enum a, Enum b) => a -> b
coerceEnum = toEnum . fromEnum


-- |
-- /O(n)/ where @n@ is the length of the dynamic character.
--
-- Malloc and populate a pointer to an exportable representation of the
-- 'Exportable' value. The supplied value is assumed to be a dynamic character
-- and the result is a pointer to a C representation of a dynamic character.
--
-- Call 'destructCharacter' to free the resulting pointer.
constructCharacter :: Exportable s => s -> IO (Ptr CDynamicChar)
constructCharacter exChar = do
    valueBuffer <- newArray $ exportedBufferChunks exportableBuffer
    charPointer <- malloc :: IO (Ptr CDynamicChar)
    let charValue = CDynamicChar (coerceEnum width) (coerceEnum count) bufLen valueBuffer
    !_ <- poke charPointer charValue
    pure charPointer
  where
    count  = exportedElementCountSequence exportableBuffer
    width  = exportedElementWidthSequence exportableBuffer
    bufLen = calculateBufferLength count width
    exportableBuffer = toExportableBuffer exChar


-- |
-- /O(1)/
--
-- Malloc and populate a pointer to an exportable representation of the
-- 'Exportable' value. The supplied value is assumed to be a dynamic character
-- element and the result is a pointer to a C representation of a dynamic
-- character element.
--
-- Call 'destructElement' to free the resulting pointer.
constructElement :: Exportable s => s -> IO (Ptr DCElement)
constructElement exChar = do
    valueBuffer    <- newArray $ exportedBufferChunks exportableBuffer
    elementPointer <- malloc :: IO (Ptr DCElement)
    let elementValue = DCElement (coerceEnum width) valueBuffer
    !_ <- poke elementPointer elementValue
    pure elementPointer
  where
    width  = exportedElementWidthSequence exportableBuffer
    exportableBuffer = toExportableBuffer exChar


-- |
-- /O(1)/
--
-- Malloc and populate a pointer to a C representation of a dynamic character.
-- The buffer of the resulting value is intentially zeroed out.
--
-- Call 'destructElement' to free the resulting pointer.
constructEmptyElement :: Word -- ^ Bit width of a dynamic character element.
                      -> IO (Ptr DCElement)
constructEmptyElement alphabetSize = do
    elementPointer <- malloc :: IO (Ptr DCElement)
    valueBuffer    <- mallocArray bufferLength
    let elementValue = DCElement (coerceEnum alphabetSize) valueBuffer
    !_ <- poke elementPointer elementValue
    pure elementPointer
  where
    bufferLength = calculateBufferLength alphabetSize 1


-- |
-- /O(1)/
--
-- Free's a character allocated by a call to 'constructCharacter'.
destructCharacter :: Ptr CDynamicChar -> IO ()
destructCharacter p = do
    !e <- peek p
    !_ <- free (dynChar e)
    free p


-- |
-- /O(1)/
--
-- Free's a character allocated by a call to 'constructElement' or 'constructEmptyElement'.
destructElement :: Ptr DCElement -> IO ()
destructElement p = do
    !e <- peek p
    !_ <- free (characterElement e)
    free p
