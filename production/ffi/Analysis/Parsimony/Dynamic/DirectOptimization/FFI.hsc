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
-- For notes on usage, data construction and external see referenced C
-- compilation units, and also driver.c, which is not imported, but is
-- included indirectory for reference.
--
-----------------------------------------------------------------------------

-- TODO: do I need this: https://hackage.haskell.org/package/base-4.9.0.0/docs/Foreign-StablePtr.html

{-# LANGUAGE ForeignFunctionInterface, BangPatterns #-}

module Analysis.Parsimony.Dynamic.DirectOptimization.FFI where

import Foreign
--import Foreign.Ptr
--import Foreign.C.String
import Foreign.C.Types
--import Foreign.Marshal.Array
--import Foreign.StablePtr
import Prelude hiding (lcm, sequence, tail)
--import System.IO.Unsafe

#include "costMatrix.h"
#include "c_code_alloc_setup.h"
#include "c_alignment_interface.h"
#include "nwMatrices.h"
#include "seqAlign.h"

type CArrayUnit  = CULong -- This will be compatible with uint64_t
type CDirMtxUnit = CShort -- C short type

{- ******************************************* Sequence declaration and Storable instance ******************************************* -}

data Alignment2d = Alignment2d { alignedSequence1 :: AlignIO
                               , alignedSequence2 :: AlignIO
                               , medianSequence   :: AlignIO
                               , cost             :: CInt
                               }

{-
data Alignment3d = Alignment3d { sequence3d1      :: AlignIO
                               , sequence3d2      :: AlignIO
                               , alignedSequence3 :: AlignIO
                               , alignment3d      :: AlignIO
                               , cost3d           :: CInt
                               }
-}

data AlignIO = AlignIO { -- magic_number :: CInt     -- TODO: No idea what this is for; figure it out?
                         sequence :: StablePtr CInt     --
                       , seqLen   :: CSize        -- Total length of the sequence stored
                       , arrCap   :: CSize        -- Total capacity of allocated array
                       }

-- Because we're using a struct we need to make a Storable instance
instance Storable AlignIO where
    sizeOf    _  = (#size struct alignIO) -- #size is a built-in that works with arrays, as are #peek and #poke, below
    alignment _   = alignment (undefined :: CSize)
    peek ptr     = do                 -- to get values from the C app
        len  <- (#peek struct alignIO, length)   ptr
        arr  <- (#peek struct alignIO, sequence) ptr
        cap  <- (#peek struct alignIO, capacity) ptr

        return  AlignIO { seqLen   = len
                        , sequence = arr
                        , arrCap   = cap
                        }
    poke ptr (AlignIO len arr cap) = do -- to modify values in the C app
        (#poke struct alignIO, sequence) ptr len
        (#poke struct alignIO, length)   ptr arr
        (#poke struct alignIO, capacity) ptr cap


{- ******************************************* CostMatrix declarations and Storable instances ******************************************* -}
-- | Holds single cost matrix, which contains costs and medians for all
-- possible sequence elements. It is completely filled using a TCM. See note below at 'setupCostMatrixFn_c'.
data CostMatrix2d = CostMatrix2d { alphSize      :: CInt      -- alphabet size including gap, and including ambiguities if
                                                              --     combinations == True
                                 , lcm           :: CInt      -- ceiling of log_2 (alphSize)
                                 , gapChar       :: CInt      -- gap value (1 << (alphSize - 1))
                                 , costModelType :: CInt      {- The type of cost model to be used in the alignment,
                                                               - i.e. affine or not.
                                                               - Based on cost_matrix.ml, values are:
                                                               - • linear == 0
                                                               - • affine == 1
                                                               - • no_alignment == 2
                                                               -}
                                 , combinations    :: CInt    {- This is a flag set to true if we are going to accept
                                                               - all possible combinations of the elements in the alphabet
                                                               - in the alignments. This is not true for protein sequences
                                                               - for example, where the number of elements of the alphabet
                                                               - is already too big to build all the possible combinations.
                                                               -}
                                 , gapOpenCost   :: CInt      {- The cost of opening a gap. This is only useful in
                                                               - certain cost_model_types (type 2: affine, based on my reading of ML code).
                                                               -}
                                 , isMetric      :: CInt      {- if tcm is metric
                                                               - Not present in 3d. -}
                                 , allElems      :: CInt      -- total number of elements TODO: figure out how this is different from alphSize
                                 , bestCost      :: Ptr CInt  {- The transformation cost matrix, including ambiguities,
                                                               - storing the **best** cost for each ambiguity pair
                                                               -}
                                 , medians       :: StablePtr CUInt {- The matrix of possible medians between elements in the
                                                               - alphabet. The best possible medians according to the cost
                                                               - matrix.
                                                               -}
                                 , worstCost     :: StablePtr CInt  {- The transformation cost matrix, including ambiguities,
                                                               - storing the **worst** cost for each ambiguity pair
                                                               - Missing in 3d
                                                               -}
                                 , prependCost   :: StablePtr CInt  {- The cost of going from gap -> each base. For ambiguities, use best cost.
                                                               - Set up as all_elements x all_elements
                                                               - matrix, but seemingly only first row is used.
                                                               - Missing in 3d because current version of 3d sets gap cost
                                                               - as constant.
                                                               -}
                                 , tailCost      :: StablePtr CInt  {- As prepend_cost, but with reverse directionality,
                                                               - so base -> gap.
                                                               - As with prepend_cost, seems to be allocated as too large.
                                                               - Missing in 3d because current version of 3d sets gap cost
                                                               - as constant.
                                                               -}
                                 }



-- Because we're using a struct we need to make a Storable instance
instance Storable CostMatrix2d where
    sizeOf    _   = (#size struct seq) -- #size is a built-in that works with arrays, as are #peek and #poke, below
    alignment _   = alignment (undefined :: StablePtr CUInt)
    peek ptr      = do -- to get values from the C app
        aSizeVal     <- (#peek struct cost_matrices_2d, alphSize)        ptr
        lcmVal'      <- (#peek struct cost_matrices_2d, lcm)             ptr
        gapcharVal   <- (#peek struct cost_matrices_2d, gap_char)        ptr
        costModelVal <- (#peek struct cost_matrices_2d, cost_model_type) ptr
        combosVal    <- (#peek struct cost_matrices_2d, combinations)    ptr
        gapOpenVal   <- (#peek struct cost_matrices_2d, gap_open)        ptr
        metricVal    <- (#peek struct cost_matrices_2d, is_metric)       ptr
        elemsVal     <- (#peek struct cost_matrices_2d, all_elements)    ptr
        bestVal      <- (#peek struct cost_matrices_2d, cost)            ptr
        medsVal      <- (#peek struct cost_matrices_2d, median)          ptr
        worstVal     <- (#peek struct cost_matrices_2d, worst)           ptr
        prependVal   <- (#peek struct cost_matrices_2d, prepend_cost)    ptr
        tailVal <- (#peek struct cost_matrices_2d, tail_cost)       ptr
        return  CostMatrix2d { alphSize      = aSizeVal
                             , lcm           = lcmVal'
                             , gapChar       = gapcharVal
                             , costModelType = costModelVal
                             , combinations  = combosVal
                             , gapOpenCost   = gapOpenVal
                             , isMetric      = metricVal
                             , allElems      = elemsVal
                             , bestCost      = bestVal
                             , medians       = medsVal
                             , worstCost     = worstVal
                             , prependCost   = prependVal
                             , tailCost      = tailVal
                       }

    poke ptr (CostMatrix2d
                  alphSizeVal
                  lcmVal
                  gapCharVal
                  costModelTypeVal
                  combinationsVal
                  gapOpenVal
                  isMetricVal
                  elemsVal
                  bestCostVal
                  mediansVal
                  worstCostVal
                  prependCostVal
                  tailCostVal
              ) = do -- to modify values in the C app
        (#poke struct cost_matrices_2d, alphSize)        ptr alphSizeVal
        (#poke struct cost_matrices_2d, lcm)             ptr lcmVal
        (#poke struct cost_matrices_2d, gap_char)        ptr gapCharVal
        (#poke struct cost_matrices_2d, cost_model_type) ptr costModelTypeVal
        (#poke struct cost_matrices_2d, combinations)    ptr combinationsVal
        (#poke struct cost_matrices_2d, gap_open)        ptr gapOpenVal
        (#poke struct cost_matrices_2d, is_metric)       ptr isMetricVal
        (#poke struct cost_matrices_2d, all_elements)    ptr elemsVal
        (#poke struct cost_matrices_2d, cost)            ptr bestCostVal
        (#poke struct cost_matrices_2d, median)          ptr mediansVal
        (#poke struct cost_matrices_2d, worst)           ptr worstCostVal
        (#poke struct cost_matrices_2d, prepend_cost)    ptr prependCostVal
        (#poke struct cost_matrices_2d, tail_cost)       ptr tailCostVal

{-
{- ******************************************* CostMatrix declarations and Storable instances ******************************************* -}
-- | Holds single cost matrix, which contains costs and medians for all
-- possible sequence elements. It is completely filled using a TCM. See note below at 'setupCostMatrixFn_c'.
data CostMatrix3d = CostMatrix3d { sSize      :: CInt      -- alphabet size including gap, and including ambiguities if
                                                              --     combinations == True
                                 , lcm           :: CInt      -- ceiling of log_2 (alphSize)
                                 , gapChar       :: CInt      -- gap value (1 << (alphSize - 1))
                                 , costModelType :: CInt      {- The type of cost model to be used in the alignment,
                                                               - i.e. affine or not.
                                                               - Based on cost_matrix.ml, values are:
                                                               - • linear == 0
                                                               - • affine == 1
                                                               - • no_alignment == 2
                                                               -}
                                 , combinations    :: CInt     {- This is a flag set to true if we are going to accept
                                                                - all possible combinations of the elements in the alphabet
                                                                - in the alignments. This is not true for protein sequences
                                                                - for example, where the number of elements of the alphabet
                                                                - is already too big to build all the possible combinations.
                                                                -}
                                 , gapOpenCost   :: CInt        {- The cost of opening a gap. This is only useful in
                                                                 - certain cost_model_types (type 2: affine, based on my reading of ML code).
                                                                 -}
                                 , isMetric      :: CInt        {- if tcm is symmetric
                                                                 - Not present in 3d. -}
                                 , allElems      :: CInt        -- total number of elements TODO: figure out how this is different from alphSize
                                 , bestCost      :: Ptr CInt    {- The transformation cost matrix, including ambiguities,
                                                                 - storing the **best** cost for each ambiguity pair
                                                                 -}
                                 , medians       :: Ptr CUInt   {- The matrix of possible medians between elements in the
                                                                 - alphabet. The best possible medians according to the cost
                                                                 - matrix.
                                                                 -}
                                 }


-}



-- | Create and allocate cost matrix
-- first argument, TCM, is only for non-ambiguous nucleotides, and it used to generate
-- the entire cost matrix, which includes ambiguous elements.
-- TCM is row-major, with each row being the left sequence element.
-- It is therefore indexed not by powers of two, but by cardinal integer.
foreign import ccall unsafe "c_code_alloc_setup.h setupCostMtx"
    setupCostMatrixFn_c :: Ptr CInt  -- tcm
                        -> CInt      -- alphSize
                        -> CInt      -- gap_open
                        -> CInt      -- is_2d
                        -> Ptr CostMatrix2d

foreign import ccall unsafe "c_code_alloc_setup.h initializeSeq"
    allocateSequenceFn_c :: CSize -> CSize -> Ptr AlignIO


{-

-- | Get union of two sequences.
-- Will only work if alignment and backtrace have already been called.
-- First sequence must be shortest
foreign import ccall unsafe "algn.h algn_union"
    getUnionFn_c :: Ptr Sequence -> Ptr Sequence -> Ptr Sequence

-- | Will only work if alignment and backtrace have already been called.
-- First sequence must be shortest
foreign import ccall unsafe "algn.h algn_get_median_2d_no_gaps"
    getUngappedMedianFn_c :: Ptr Sequence -> Ptr Sequence -> Ptr CostMatrix -> Ptr Sequence

-- | Will only work if alignment and backtrace have already been called.
-- First sequence must be shortest
foreign import ccall unsafe "algn.h algn_get_median_2d_with_gaps"
    getGappedMedianFn_c :: Ptr Sequence -> Ptr Sequence -> Ptr CostMatrix -> Ptr Sequence



-- testFn can be called from within Haskell code.
call2dSeqAlignFn_c :: Sequence -> Sequence -> CostMatrix -> NWMatrices -> Alignment
call2dSeqAlignFn_c shortSeq longSeq costMatrix nwMatrices = unsafePerformIO $
    -- have to allocate memory. Note that we're allocating via a lambda fn. I
    -- don't yet understand what exactly is going on here.
    alloca $ \alignPtr -> do

        -- Using strict here because the values need to be read before freeing,
        -- so lazy is dangerous.
        let !status = callExtFn_c arg1 arg2 3 3 alignPtr
        free arg1
        free arg2

        -- Now checking return status. If 0, then all is well, otherwise throw an error.
        if (fromIntegral status) == 0
            then do
                Sequence val seq <- peek alignPtr
                seqStr           <- peekCAString seq
                free seq
                pure $ Right (fromIntegral val, seqStr)
            else do
                pure $ Left "Out of memory"

-- Just for testing from CLI outside of ghci.
main :: IO ()
main = putStrLn $ show callSeqAlignFn_c


-}