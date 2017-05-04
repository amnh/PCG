-----------------------------------------------------------------------------
-- |
-- TODO: Document module.
--
-----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns, DeriveGeneric, FlexibleInstances, ForeignFunctionInterface, TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Analysis.Parsimony.Dynamic.SequentialAlign.FFI
  ( pairwiseSequentialAlignment
  ) where

import Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise (filterGaps)
import Bio.Character.Encodable
import Bio.Character.Exportable.Class
import Data.Bits
--import Data.Monoid
import Data.TCM.Memoized.FFI
import Foreign         hiding (alignPtr)
import Foreign.C.Types
import System.IO.Unsafe

import Debug.Trace

#include "sequentialAlignInterface.h"
#include "sequentialAlignOutputTypes.h"
-- #include <stdint.h>


-- |
-- The result of the alignment from the C side of the FFI
-- Includes a struct (actually, a pointer thereto), and that struct, in turn, has a string
-- in it, so Ptr CChar.
-- Modified from code samples here: https://en.wikibooks.org/wiki/Haskell/FFI#Working_with_C_Structures
data AlignResult
   = AlignResult
   { cost        :: CSize
   , finalLength :: CSize
   , character1  :: Ptr CBufferUnit
   , character2  :: Ptr CBufferUnit
   , medianChar  :: Ptr CBufferUnit
   }


-- |
-- Because we're using a struct we need to make a Storable instance
instance Storable AlignResult where

    sizeOf    _ = (#size struct alignResult_t) -- #size is a built-in that works with arrays, as are #peek and #poke, below

    alignment _ = alignment (undefined :: CBufferUnit)

    peek ptr    = do -- to get values from the C app
        newCost <- (#peek struct alignResult_t, finalCost  ) ptr
        len     <- (#peek struct alignResult_t, finalLength) ptr
        char1   <- (#peek struct alignResult_t, finalChar1 ) ptr
        char2   <- (#peek struct alignResult_t, finalChar2 ) ptr
        med     <- (#peek struct alignResult_t, medianChar ) ptr
        pure AlignResult
             { cost        = newCost
             , finalLength = len
             , character1  = char1
             , character2  = char2
             , medianChar  = med
             }

    poke ptr (AlignResult costVal charLen char1Val char2Val medVal) = do
        (#poke struct alignResult_t, finalCost  ) ptr costVal
        (#poke struct alignResult_t, finalLength) ptr charLen
        (#poke struct alignResult_t, finalChar1 ) ptr char1Val
        (#poke struct alignResult_t, finalChar2 ) ptr char2Val
        (#poke struct alignResult_t, medianChar ) ptr medVal


foreign import ccall unsafe "sequentialAlignInterface performSequentialAlignment"
    performSeqAlignfn_c :: Ptr CDynamicChar
                        -> Ptr CDynamicChar
                        -> StablePtr ForeignVoid
                        -> Ptr AlignResult
                        -> IO CInt


-- |
-- FFI call to the C pairwise alignment algorithm with /explicit/ sub & indel cost parameters.
pairwiseSequentialAlignment :: (EncodableDynamicCharacter s, Exportable s, Show s) => MemoizedCostMatrix -> s -> s -> (Word, s, s, s, s)
pairwiseSequentialAlignment memo char1 char2 = unsafePerformIO $ do
--        !_ <- trace "Before constructing char1" $ pure ()
        char1'        <- constructCharacterFromExportable char1
--        !_ <- trace "After  constructing char1" $ pure ()

--        !_ <- trace "Before constructing char2" $ pure ()
        char2'        <- constructCharacterFromExportable char2
--        !_ <- trace "After  constructing char1" $ pure ()

--        !_ <- trace "Before mallocing result " $ pure ()
        resultPointer <- malloc :: IO (Ptr AlignResult)
--        !_ <- trace "After  mallocing result " $ pure ()

--        !_ <- trace ("Shown character 1: " <> show char1) $ pure ()
--        !_ <- trace ("Shown character 2: " <> show char2) $ pure ()

        !_ <- trace "Before FFI call" $ pure ()
        !_success     <- performSeqAlignfn_c char1' char2' (costMatrix memo) resultPointer
        !_ <- trace "After  FFI call" $ pure ()

--        _ <- free char1'
--        _ <- free char2'
        resultStruct  <- peek resultPointer
        let alignmentCost   = toEnum . fromEnum $ cost resultStruct
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
        pure (alignmentCost, ungapped, medianAlignment, alignedChar1, alignedChar2)
    where
        width = exportedElementWidthSequence $ toExportableBuffer char1
