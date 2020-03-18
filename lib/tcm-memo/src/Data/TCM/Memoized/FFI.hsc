-----------------------------------------------------------------------------
-- |
-- TODO: Document module.
--
-- Exports C types for dynamic characters and their constructors allong with
-- an FFI binding for the memoizing TCM structure.
-----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns, DeriveGeneric, FlexibleInstances, ForeignFunctionInterface, TypeSynonymInstances #-}

module Data.TCM.Memoized.FFI
  ( CBufferUnit
  , CDynamicChar(..)
  , DCElement(..)
  , ForeignVoid()
  , MemoizedCostMatrix(costMatrix)
  , getMemoizedCostMatrix
  , getMedianAndCost2D
  , getMedianAndCost3D
  -- * Utility functions
  , calculateBufferLength
  , coerceEnum
  -- * Constructors
  , constructElement
  , constructEmptyElement
  -- * Destructors 
  , destructElement
  ) where

import Bio.Character.Exportable
import Data.TCM.Memoized.Types
import Foreign         hiding (alignPtr)
import Foreign.C.Types
import System.IO.Unsafe

-- import Debug.Trace

#include "costMatrixWrapper.h"
#include "dynamicCharacterOperations.h"


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



-- |
-- Set up and return a cost matrix.
--
-- The cost matrix is allocated strictly.
getMemoizedCostMatrix :: Word
                      -> (Word -> Word -> Word)
                      -> MemoizedCostMatrix
getMemoizedCostMatrix alphabetSize costFn = unsafePerformIO . withArray rowMajorList $
    -- The array 'allocedTCM' is free'd at the end of the IO code in the do block's scope.
    \allocedTCM -> do
        !resultPtr <- initializeMemoizedCMfn_c (coerceEnum alphabetSize) allocedTCM
        pure $ MemoizedCostMatrix resultPtr
  where
    rowMajorList = [ coerceEnum $ costFn i j | i <- range,  j <- range ]
    range = [0 .. alphabetSize - 1]


-- |
-- /O(1)/ amortized.
--
-- Calculate the median symbol set and transition cost between the two input
-- symbol sets.
--
-- *Note:* This operation is lazily evaluated and memoized for future calls.
getMedianAndCost2D :: Exportable s => MemoizedCostMatrix -> s -> s -> (s, Word)
getMedianAndCost2D memo e1 e2 = unsafePerformIO $ do
    medianPtr     <- constructEmptyElement alphabetSize
    e1'           <- constructElement e1
    e2'           <- constructElement e2
    !cost         <- getCostAndMedian2D_c e1' e2' medianPtr (costMatrix memo)
    _             <- destructElement e1'
    _             <- destructElement e2'
    medianElement <- peek medianPtr
    medianValue   <- fmap buildExportable . peekArray bufferLength $ characterElement medianElement
    _             <- destructElement medianPtr
    pure (medianValue, coerceEnum cost)
  where
    alphabetSize    = exportedElementWidthSequence $ toExportableBuffer e1
    buildExportable = fromExportableBuffer . ExportableCharacterSequence 1 alphabetSize
    bufferLength    = calculateBufferLength alphabetSize 1


-- |
-- /O(1)/ amortized.
--
-- Calculate the median symbol set and transition cost between the two input
-- symbol sets.
--
-- *Note:* This operation is lazily evaluated and memoized for future calls.
getMedianAndCost3D :: Exportable s => MemoizedCostMatrix -> s -> s -> s -> (s, Word)
getMedianAndCost3D memo e1 e2 e3 = unsafePerformIO $ do
    medianPtr     <- constructEmptyElement alphabetSize
    e1'           <- constructElement e1
    e2'           <- constructElement e2
    e3'           <- constructElement e3
    !cost         <- getCostAndMedian3D_c e1' e2' e3' medianPtr (costMatrix memo)
    _             <- destructElement e1'
    _             <- destructElement e2'
    _             <- destructElement e3'
    medianElement <- peek medianPtr
    medianValue   <- fmap buildExportable . peekArray bufferLength $ characterElement medianElement
    _             <- destructElement medianPtr
    pure (medianValue, coerceEnum cost)
  where
    alphabetSize    = exportedElementWidthSequence $ toExportableBuffer e1
    buildExportable = fromExportableBuffer . ExportableCharacterSequence 1 alphabetSize
    bufferLength    = calculateBufferLength alphabetSize 1
