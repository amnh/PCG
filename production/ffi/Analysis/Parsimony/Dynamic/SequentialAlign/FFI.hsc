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

{-# LANGUAGE BangPatterns, DeriveGeneric, FlexibleInstances, ForeignFunctionInterface, TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Analysis.Parsimony.Dynamic.SequentialAlign.FFI
  ( ForeignVoid()
  , MemoizedCostMatrix(costMatrix)
  , getMemoizedCostMatrix
  , pairwiseSequentialAlignment
--  , sequentialAlign
--  , testFn
--  , main
  ) where

import Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise (filterGaps)
import Bio.Character.Encodable
import Bio.Character.Exportable.Class
import Control.DeepSeq
import Control.Lens    hiding (element)
import Data.Bits
import Data.Foldable
import Data.Monoid
--import Data.Void
import Foreign         hiding (alignPtr)
--import Foreign.Ptr
--import Foreign.C.String
import Foreign.C.Types
import GHC.Generics           (Generic)
import System.IO.Unsafe
import Test.QuickCheck hiding ((.&.), output)

import Debug.Trace

#include "costMatrixWrapper.h"
#include "dynamicCharacterOperations.h"
#include "seqAlignInterface.h"
-- #include "seqAlignForHaskell.c"
#include <stdint.h>

-- |
-- A convient type alias for improved clairity of use.
type CArrayUnit  = CULong -- This will be compatible with uint64_t

-- | (✔)
instance Arbitrary CArrayUnit where
    arbitrary = do
        num <- arbitrary :: Gen Integer
        pure $ fromIntegral num


data ForeignVoid deriving (Generic)


instance NFData ForeignVoid


data MemoizedCostMatrix
   = MemoizedCostMatrix
   { costMatrix :: StablePtr ForeignVoid
   } deriving (Eq, Generic)


instance NFData MemoizedCostMatrix where

    rnf (MemoizedCostMatrix !x) = ()


instance Storable MemoizedCostMatrix where
    sizeOf    _ = (#size void*) -- #size is a built-in that works with arrays, as are #peek and #poke, below
    alignment _ = alignment (undefined :: CArrayUnit)
    peek _ptr   = undefined
    poke _ptr   = undefined



data DCElement = DCElement
    { alphabetSizeElem :: CSize
    , characterElement :: Ptr CArrayUnit
    }


-- | (✔)
instance Storable DCElement where
    sizeOf    _ = (#size struct dcElement_t)
    alignment _ = alignment (undefined :: CULong)
    peek ptr    = do
        alphLen <- (#peek struct dcElement_t, alphSize) ptr
        element <- (#peek struct dcElement_t, element)  ptr
        pure DCElement
            { alphabetSizeElem = alphLen
            , characterElement = element
            }
    poke ptr (DCElement alphLen element) = do
        (#poke struct dcElement_t, alphSize) ptr alphLen
        (#poke struct dcElement_t, element ) ptr element

-- | Create and allocate cost matrix
-- first argument, TCM, is only for non-ambiguous nucleotides, and it used to generate
-- the entire cost matrix, which includes ambiguous elements.
-- TCM is row-major, with each row being the left character element.
-- It is therefore indexed not by powers of two, but by cardinal integer.
-- TODO: For now we only allocate 2d matrices. 3d will come later.
foreign import ccall unsafe "costMatrixWrapper matrixInit"
    initializeMemoizedCMfn_c :: CSize
                             -> Ptr CInt
                             -> IO (StablePtr ForeignVoid) -- MemoizedCostMatrix


-- | Set up and return a cost matrix
--
-- The cost matrix is allocated strictly.
getMemoizedCostMatrix :: Word
                      -> (Word -> Word -> Word)
                      -> MemoizedCostMatrix
getMemoizedCostMatrix alphabetSize costFn = unsafePerformIO . withArray rowMajorList $ \allocedTCM -> do
--        output <- malloc :: IO (Ptr MemoizedCostMatrix)
        -- Hopefully the strictness annotation forces the allocation of the CostMatrix2d to happen immediately.
        ! resultPtr <- initializeMemoizedCMfn_c (coerceEnum alphabetSize) allocedTCM
        !_ <- putStrLn "Initialized Sparse Memoized TCM through FFI binding!"
        pure $ MemoizedCostMatrix resultPtr
    where
        -- This *should* be in row major order due to the manner in which list comprehensions are performed.
        rowMajorList = [ coerceEnum $ costFn i j | i <- range,  j <- range ]
        range = [0 .. alphabetSize - 1]


coerceEnum :: (Enum a, Enum b) => a -> b
coerceEnum = toEnum . fromEnum



-- |
-- FFI call to the C pairwise alignment algorithm with /explicit/ sub & indel cost parameters.
{- foreign import ccall unsafe "seqAlignForHaskell aligner"
    call_aligner :: Ptr CDynamicChar -> Ptr CDynamicChar -> CInt -> CInt -> Ptr AlignResult -> CInt
-}
pairwiseSequentialAlignment :: (EncodableDynamicCharacter s, Exportable s, Show s) => MemoizedCostMatrix -> s -> s -> (s, Double, s, s, s)
pairwiseSequentialAlignment memo char1 char2 = unsafePerformIO $ do
--        !_ <- trace "Before constructing char1" $ pure ()
        char1'        <- constructCDynamicCharacterFromExportableCharacter char1
--        !_ <- trace "After  constructing char1" $ pure ()

--        !_ <- trace "Before constructing char2" $ pure ()
        char2'        <- constructCDynamicCharacterFromExportableCharacter char2
--        !_ <- trace "After  constructing char1" $ pure ()

--        !_ <- trace "Before mallocing result " $ pure ()
        resultPointer <- malloc :: IO (Ptr AlignResult)
--        !_ <- trace "After  mallocing result " $ pure ()

--        !_ <- trace ("Shown character 1: " <> show char1) $ pure ()
--        !_ <- trace ("Shown character 2: " <> show char2) $ pure ()
        
        !_ <- trace "Before FFI call" $ pure ()
        !success      <- performSeqAlignfn_c char1' char2' (costMatrix memo) resultPointer
        !_ <- trace "After  FFI call" $ pure ()

--        _ <- free char1'
--        _ <- free char2'
        resultStruct  <- peek resultPointer
        let alignmentCost   = fromIntegral $ cost resultStruct
            resultElemCount = coerceEnum $ finalLength resultStruct
            bufferLength    = calculateBufferLength width resultElemCount
            buildExportable = ExportableCharacterSequence resultElemCount width
            generalizeFromBuffer = fromExportableBuffer . buildExportable
        !alignedChar1    <- fmap generalizeFromBuffer . peekArray bufferLength $ character1 resultStruct
        !alignedChar2    <- fmap generalizeFromBuffer . peekArray bufferLength $ character2 resultStruct
        !medianAlignment <- fmap generalizeFromBuffer . peekArray bufferLength $ medianChar resultStruct
        let !ungapped = filterGaps medianAlignment
--        _ <- free resultPointer
{--
        !_ <- trace ("Shown   gapped           : " <> show medianAlignment) $ pure ()
        !_ <- trace ("Shown ungapped           : " <> show ungapped       ) $ pure ()
        !_ <- trace ("Shown character 1 aligned: " <> show alignedChar1   ) $ pure ()
        !_ <- trace ("Shown character 2 aligned: " <> show alignedChar2   ) $ pure ()
--}
        !_ <- trace "Right Before Return" $ pure ()
        pure (ungapped, alignmentCost, medianAlignment, alignedChar1, alignedChar2)
    where
        width = exportedElementWidthSequence $ toExportableBuffer char1


constructCDynamicCharacterFromExportableCharacter :: Exportable s => s -> IO (Ptr CDynamicChar)
constructCDynamicCharacterFromExportableCharacter exChar = do
--        !_ <- trace (show exportableBuffer) $ pure ()
        valueBuffer <- newArray $ exportedBufferChunks exportableBuffer
        charPointer <- malloc :: IO (Ptr CDynamicChar)
        let charValue = CDynamicChar (coerceEnum width) (coerceEnum count) bufLen valueBuffer
        {-
            CDynamicChar
            { alphabetSizeChar = width
            , dynCharLen       = count
            , numElements      = bufLen
            , dynChar          = valueBuffer
            }
            -}
        !_ <- poke charPointer charValue
        pure charPointer
    where
        count  = exportedElementCountSequence exportableBuffer
        width  = exportedElementWidthSequence exportableBuffer
        bufLen = calculateBufferLength count width
        exportableBuffer = toExportableBuffer exChar

calculateBufferLength count width = coerceEnum $ q + if r == 0 then 0 else 1
    where
        (q,r)  = (count * width) `divMod` finiteBitSize (undefined :: CULong)



{-}
foreign import ccall unsafe "costMatrix getCostAndMedian"
    getCostMedianfn_c :: Ptr MemoizedCostMatrix
                      -> Ptr CArrayUnit
                      -> Ptr CArrayUnit
                      -> Ptr CArrayUnit
                      -> CSize
                      -> CInt

lookupCostAndMedian :: Exportable s => Ptr MemoizedCostMatrix -> s -> s -> (s, Word)
lookupCostAndMedian tcm lhs rhs = unsafePerformIO $ do
    firstElem  <- malloc :: IO (Ptr leftInput)
    secondElem <- malloc :: IO (Ptr rightInput)
    outputPtr  <- malloc :: IO (Ptr CArrayUnit)

    !cost <- getCostMedianfn_c tcm firstElem secondElem outputPtr alphSize

    pure (exportableOutput, outCost)
    where
        alphSize              = lhs ^. exportedElementWidth ^. alphabetSize
        lhsExportableSequence = toExportable lhs
        leftInput             = DCElement alphSize (lhsExportableSequence ^. discreteCharacter)
        rhsExportableSequence = toExportable rhs
        rightInput            = DCElement alphSize (rhsExportableSequence ^. discreteCharacter)
        retVal                = DCElement alphSize (lhsExportableSequence ^. discreteCharacter)
        exportableOutput      = Exportable 1 alphSize peek retVal
        outCost               = toEnum (fromEnum cost) :: Word
-}


{-
foreign import ccall unsafe "costMatrix lookUpCost"
    getCostfn_c :: Ptr MemoizedCostMatrix
                -> Ptr CDynamicChar
-}

foreign import ccall unsafe "seqAlignInteface performSequentialAlignment"
    performSeqAlignfn_c :: Ptr CDynamicChar
                        -> Ptr CDynamicChar
                        -> StablePtr ForeignVoid
                        -> Ptr AlignResult
                        -> IO CInt


-- TODO: replace when Yu Xiang updates his code for bit arrays.
-- | STUB, DO NOT USE
sequentialAlign :: Int -> Int -> s -> s -> Either String (Int, s, s)
sequentialAlign x y a b = Right (x + y, a, b)


-- |
-- The result of the alignment from the C side of the FFI
-- Includes a struct (actually, a pointer thereto), and that struct, in turn, has a string
-- in it, so Ptr CChar.
-- Modified from code samples here: https://en.wikibooks.org/wiki/Haskell/FFI#Working_with_C_Structures
data AlignResult
   = AlignResult
   { cost        :: CSize
   , finalLength :: CSize
   , character1  :: Ptr CArrayUnit
   , character2  :: Ptr CArrayUnit
   , medianChar  :: Ptr CArrayUnit
   }


-- | Because we're using a struct we need to make a Storable instance
instance Storable AlignResult where
    sizeOf    _ = (#size struct alignResult_t) -- #size is a built-in that works with arrays, as are #peek and #poke, below
    alignment _ = alignment (undefined :: CArrayUnit)
    peek ptr    = do -- to get values from the C app
        newCost <- (#peek struct alignResult_t, finalWt )    ptr
        len     <- (#peek struct alignResult_t, finalLength) ptr
        char1   <- (#peek struct alignResult_t, finalChar1)  ptr
        char2   <- (#peek struct alignResult_t, finalChar2)  ptr
        med     <- (#peek struct alignResult_t, medianChar)  ptr
        pure AlignResult
             { cost        = newCost
             , finalLength = len
             , character1  = char1
             , character2  = char2
             , medianChar  = med
             }


------------- Don't need this part, but left in for completion ---------------
----- Will get compiler warning if left out, because of missing instances ----
    poke ptr (AlignResult cost charLen char1Val char2Val medVal) = do -- to modify values in the C app
        (#poke struct alignResult_t, finalWt    ) ptr cost
        (#poke struct alignResult_t, finalLength) ptr charLen
        (#poke struct alignResult_t, finalChar1 ) ptr char1Val
        (#poke struct alignResult_t, finalChar2 ) ptr char2Val
        (#poke struct alignResult_t, medianChar ) ptr medVal



-- |
-- Type of a dynamic character to pass back and forth across the FFI interface.
data CDynamicChar
   = CDynamicChar
   { alphabetSizeChar :: CSize
   , numElements      :: CSize
   , dynCharLen       :: CSize
   , dynChar          :: Ptr CArrayUnit
   }

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
-- | (✔)
instance Arbitrary CDynamicChar where
    arbitrary = do
        alphSize    <- (arbitrary :: Gen Int) `suchThat` (\x -> 0 < x && x <= 64)
        charSize    <- (arbitrary :: Gen Int) `suchThat` (\x -> 0 < x && x <= 16)
        numElems    <- (arbitrary :: Gen Int) `suchThat` (\x -> 0 < x && x <= 100)
        fullBitVals <- vectorOf numElems (arbitrary :: Gen CArrayUnit)
        -- Note there is a faster way to do this loop in 2 steps by utilizing 2s compliment subtraction and setbit.
        let mask    = foldl' (\val i -> val `setBit` i) (zeroBits :: CArrayUnit) [0..numElems]
        remBitVals  <- if   numElems == 0
                       then pure []
                       else (pure . (mask .&.)) <$> (arbitrary :: Gen CArrayUnit)
        pure CDynamicChar
           { alphabetSizeChar = toEnum alphSize
           , dynCharLen       = toEnum charSize
           , numElements      = toEnum numElems
           , dynChar          = unsafePerformIO . newArray $ fullBitVals <> remBitVals
           }


-- | (✔)
instance Storable CDynamicChar where
    sizeOf    _ = (#size struct dynChar_t) -- #size is a built-in that works with arrays, as are #peek and #poke, below
    alignment _ = alignment (undefined :: CArrayUnit)
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




-- |
-- FFI call to the C pairwise alignment algorithm with /defaulted/ sub & indel cost parameters
foreign import ccall unsafe "exportCharacter testFn"
    callExtFn_c  :: Ptr CDynamicChar -> Ptr CDynamicChar -> Ptr AlignResult -> CInt





{-
-- |
-- testFn can be called from within Haskell code.
testFn :: CDynamicChar -> CDynamicChar -> Either String (Int, String)
testFn char1 char2 char3 = unsafePerformIO $
    -- have to allocate memory. Note that we're allocating via a lambda fn. In use, the lambda will take whatever is the
    -- argument of testFn, but here there is no argument, so all allocation is hard-coded.
    alloca $ \alignPtr -> do
        marshalledChar1 <- new char1
        marshalledChar2 <- new char2
        marshalledChar3 <- new char3
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

-}

{-
-- Just for testing from CLI outside of ghci.
-- | A test driver for the FFI functionality
main :: IO ()
main = do
    char1 <- generate (arbitrary :: Gen CDynamicChar)
    --char2 <- generate (arbitrary :: Gen CDynamicChar)
    print char1
    --print char2
    print $ testFn char1 char1
-}
