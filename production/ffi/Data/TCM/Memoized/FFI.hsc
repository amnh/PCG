-----------------------------------------------------------------------------
-- |
-- TODO: Document module.
--
-- Exports C types for dynamic characters and their constructors allong with
-- an FFI binding for the memoizing TCM structure.
-----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns, DeriveGeneric, FlexibleInstances, ForeignFunctionInterface, TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.TCM.Memoized.FFI
  ( CBufferUnit
  , CDynamicChar(..)
  , DCElement(..)
  , ForeignVoid()
  , MemoizedCostMatrix(costMatrix)
  , getMemoizedCostMatrix
  , getMedianAndCost
  -- * Utility functions
  , calculateBufferLength
  , coerceEnum
  , constructCharacterFromExportable
  , constructElementFromExportable
  , constructEmptyElement
  ) where

import Bio.Character.Exportable.Class
import Control.DeepSeq
import Data.Bits
import Data.Foldable
import Data.Monoid
import Foreign         hiding (alignPtr)
import Foreign.C.Types
import GHC.Generics           (Generic)
import System.IO.Unsafe
import Test.QuickCheck hiding ((.&.), output)

import Debug.Trace

#include "costMatrixWrapper.h"
#include "dynamicCharacterOperations.h"


-- |
-- A convient type alias for improved clairity of use.
type CBufferUnit  = CULong -- This will be compatible with uint64_t


-- |
-- Type of a dynamic character to pass back and forth across the FFI interface.
data CDynamicChar
   = CDynamicChar
   { alphabetSizeChar :: CSize
   , numElements      :: CSize
   , dynCharLen       :: CSize
   , dynChar          :: Ptr CBufferUnit
   }


data DCElement = DCElement
    { alphabetSizeElem :: CSize
    , characterElement :: Ptr CBufferUnit
    } deriving (Show)


data ForeignVoid deriving (Generic)


data MemoizedCostMatrix
   = MemoizedCostMatrix
   { costMatrix :: StablePtr ForeignVoid
   } deriving (Eq, Generic)


-- | (✔)
instance Arbitrary CBufferUnit where

    arbitrary = do
        num <- arbitrary :: Gen Integer
        pure $ fromIntegral num


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


-- TODO: For now we only allocate 2d matrices. 3d will come later.
-- |
-- Create and allocate cost matrix.
-- The first argument, TCM, is only for non-ambiguous nucleotides, and it used to
-- generate the entire cost matrix, which includes ambiguous elements. TCM is
-- row-major, with each row being the left character element. It is therefore
-- indexed not by powers of two, but by cardinal integer.
foreign import ccall unsafe "costMatrixWrapper matrixInit"
    initializeMemoizedCMfn_c :: CSize
                             -> Ptr CInt
                             -> IO (StablePtr ForeignVoid)


foreign import ccall unsafe "costMatrix getCostAndMedian"
    getCostAndMedianFn_c :: Ptr DCElement
                         -> Ptr DCElement
                         -> Ptr DCElement
--                         -> CSize
                         -> StablePtr ForeignVoid
                         -> IO CInt


-- |
-- Set up and return a cost matrix.
--
-- The cost matrix is allocated strictly.
getMemoizedCostMatrix :: Word
                      -> (Word -> Word -> Word)
                      -> MemoizedCostMatrix
getMemoizedCostMatrix alphabetSize costFn = unsafePerformIO . withArray rowMajorList $ \allocedTCM -> do
    !resultPtr <- initializeMemoizedCMfn_c (coerceEnum alphabetSize) allocedTCM
    pure $ MemoizedCostMatrix resultPtr
  where
    rowMajorList = [ coerceEnum $ costFn i j | i <- range,  j <- range ]
    range = [0 .. alphabetSize - 1]


getMedianAndCost :: Exportable s => MemoizedCostMatrix -> s -> s -> (s, Word)
getMedianAndCost memo lhs rhs = unsafePerformIO $ do
    medianPtr     <- constructEmptyElement alphabetSize
    lhs'          <- constructElementFromExportable lhs
    rhs'          <- constructElementFromExportable rhs

    lhs''  <- peek lhs'
    lhs''' <- peekArray 1 $ characterElement lhs'' 
    !_ <- trace (show lhs'  ) $ pure ()
    !_ <- trace (show lhs'' ) $ pure ()
    !_ <- trace (show lhs''') $ pure ()

    !_ <- trace "Before FFI call" $ pure ()
    !cost         <- getCostAndMedianFn_c lhs' rhs' medianPtr (costMatrix memo)
    !_ <- trace "After  FFI call" $ pure ()

    medianElement <- peek medianPtr
    medianValue   <- fmap buildExportable . peekArray bufferLength $ characterElement medianElement
    pure (medianValue, coerceEnum cost)
  where
--    width           = toEnum alphabetSize
    alphabetSize    = exportedElementWidthSequence $ toExportableBuffer lhs
    buildExportable = fromExportableBuffer . ExportableCharacterSequence 1 alphabetSize
    bufferLength    = calculateBufferLength alphabetSize 1


calculateBufferLength :: Enum b => Int -> Int -> b
calculateBufferLength count width = coerceEnum $ q + if r == 0 then 0 else 1
   where
    (q,r)  = (count * width) `divMod` finiteBitSize (undefined :: CULong)


coerceEnum :: (Enum a, Enum b) => a -> b
coerceEnum = toEnum . fromEnum


constructCharacterFromExportable :: Exportable s => s -> IO (Ptr CDynamicChar)
constructCharacterFromExportable exChar = do
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


constructElementFromExportable :: Exportable s => s -> IO (Ptr DCElement)
constructElementFromExportable exChar = do
    valueBuffer    <- newArray $ exportedBufferChunks exportableBuffer
    elementPointer <- malloc :: IO (Ptr DCElement)
    let elementValue = DCElement (coerceEnum width) valueBuffer
    !_ <- poke elementPointer elementValue
    pure elementPointer
  where
    width  = exportedElementWidthSequence exportableBuffer
    exportableBuffer = toExportableBuffer exChar


constructEmptyElement :: Int -> IO (Ptr DCElement)
constructEmptyElement alphabetSize = do
    elementPointer <- malloc :: IO (Ptr DCElement)
    valueBuffer    <- mallocArray bufferLength
    let elementValue = DCElement (coerceEnum alphabetSize) valueBuffer
    !_ <- poke elementPointer elementValue
    pure elementPointer
  where
    bufferLength = calculateBufferLength alphabetSize 1
