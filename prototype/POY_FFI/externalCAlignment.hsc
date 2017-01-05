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

import Foreign
import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshall.Array
import System.IO.Unsafe

#include "seqAlign.h"
#include "nwMatrices.h"

type CArrayUnit  = CULong -- This will be compatible with uint64_t
type CDirMtxUnit = CShort -- C short type

{- ******************************************* Sequence declaration and Storable instance ******************************************* -}

data Alignment2d = Alignment2d { alignedSequence1 :: AlignIO
                               , alignedSequence2 :: AlignIO
                               , alignment        :: AlignIO
                               , cost             :: CInt
                               }

data Alignment3d = Alignment3d { alignedSequence1 :: AlignIO
                               , alignedSequence2 :: AlignIO
                               , alignedSequence3 :: AlignIO
                               , alignment        :: AlignIO
                               , cost             :: CInt
                               }


data AlignIO = AlignIO { -- magic_number :: CInt     -- TODO: No idea what this is for; figure it out?
                         sequence :: Ptr CInt     -- Capacity of the sequence memory structure.
                       , seqLen   :: CSize        -- Total length of the sequence stored.
                       }

-- Because we're using a struct we need to make a Storable instance
instance Storable AlignIO where
    sizeOf    _  = (#size struct alignIO) -- #size is a built-in that works with arrays, as are #peek and #poke, below
    alignment _  = alignment (undefined :: CChar)
    peek ptr     = do                 -- to get values from the C app
        len             <- (#peek struct alignIO, len)      ptr
        arr :: Ptr CInt <- (#peek struct alignIO, sequence) ptr
        seq             <- (#peekArray len arr)             ptr

        return  AlignIO { seqLen   = len
                        , sequence = seq
                        }
    poke ptr (AlignIO len seq ) = do -- to modify values in the C app
        (#pokeArray len (struct alignIO, sequence)) ptr seq
        (#poke struct alignIO, seqLen)              ptr len


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
                                 , isMetric      :: CInt      {- if tcm is symmetric
                                                               - Not present in 3d. -}
                                 , allElems      :: CInt      -- total number of elements TODO: figure out how this is different from alphSize
                                 , bestCost      :: Ptr CInt  {- The transformation cost matrix, including ambiguities,
                                                               - storing the **best** cost for each ambiguity pair
                                                               -}
                                 , medians       :: Ptr CUInt {- The matrix of possible medians between elements in the
                                                               - alphabet. The best possible medians according to the cost
                                                               - matrix.
                                                               -}
                                 , worstCost     :: Ptr CInt  {- The transformation cost matrix, including ambiguities,
                                                               - storing the **worst** cost for each ambiguity pair
                                                               - Missing in 3d
                                                               -}
                                 , prependCost   :: Ptr CInt  {- The cost of going from gap -> each base. For ambiguities, use best cost.
                                                               - Set up as all_elements x all_elements
                                                               - matrix, but seemingly only first row is used.
                                                               - Missing in 3d because current version of 3d sets gap cost
                                                               - as constant.
                                                               -}
                                 , tailCost      :: Ptr CInt  {- As prepend_cost, but with reverse directionality,
                                                               - so base -> gap.
                                                               - As with prepend_cost, seems to be allocated as too large.
                                                               - Missing in 3d because current version of 3d sets gap cost
                                                               - as constant.
                                                               -}
                                 }



-- Because we're using a struct we need to make a Storable instance
instance Storable CostMatrix3d where
    sizeOf    _   = (#size struct seq) -- #size is a built-in that works with arrays, as are #peek and #poke, below
    alignment _   = alignment (undefined :: CChar)
    peek ptr      = do -- to get values from the C app
        aSize     <- (#peek struct costMatrix, alphSize)        ptr
        lcm'      <- (#peek struct costMatrix, lcm)             ptr
        gapchar   <- (#peek struct costMatrix, gap_char)        ptr
        costModel <- (#peek struct costMatrix, cost_model_type) ptr
        combos    <- (#peek struct costMatrix, combinations)    ptr
        gapOpen   <- (#peek struct costMatrix, gap_open)        ptr
        metric    <- (#peek struct costMatrix, is_metric)       ptr
        elems     <- (#peek struct costMatrix, all_elements)    ptr
        best      <- (#peek struct costMatrix, cost)            ptr
        meds      <- (#peek struct costMatrix, median)          ptr
        worst     <- (#peek struct costMatrix, worst)           ptr
        prepend   <- (#peek struct costMatrix, prepend_cost)    ptr
        tail      <- (#peek struct costMatrix, tail_cost)       ptr
        return  CostMatrix { alphSize      = aSize
                           , lcm           = lcm'
                           , gapChar       = gapchar
                           , costModelType = costmodel
                           , combinations  = combos
                           , gapOpenCost   = gapOpen
                           , allElems      = elems
                           , bestCost      = best
                           , medians       = meds
                           , worstCost     = worst
                           , prependCost   = prepend
                           , tailCost      = tail
                       }

    poke ptr (CostMatrix3d val seqFinal) = do -- to modify values in the C app
        (#peek struct costMatrix, alphSize)        ptr alphSize
        (#peek struct costMatrix, lcm)             ptr lcm
        (#peek struct costMatrix, gap_char)        ptr gapChar
        (#peek struct costMatrix, cost_model_type) ptr costModelType
        (#peek struct costMatrix, combinations)    ptr combinations
        (#peek struct costMatrix, gap_open)        ptr gapOpen
        (#peek struct costMatrix, is_metric)       ptr isMetric
        (#peek struct costMatrix, all_elements)    ptr elems
        (#peek struct costMatrix, cost)            ptr bestCost
        (#peek struct costMatrix, median)          ptr medians
        (#peek struct costMatrix, worst)           ptr worstCost
        (#peek struct costMatrix, prepend_cost)    ptr prependCost
        (#peek struct costMatrix, tail_cost)       ptr tailCost


{- ******************************************* CostMatrix declarations and Storable instances ******************************************* -}
-- | Holds single cost matrix, which contains costs and medians for all
-- possible sequence elements. It is completely filled using a TCM. See note below at 'setupCostMatrixFn_c'.
data CostMatrix3d = CostMatrix3d { alphSize      :: CInt      -- alphabet size including gap, and including ambiguities if
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



-- Because we're using a struct we need to make a Storable instance
instance Storable CostMatrix where
    sizeOf    _   = (#size struct seq) -- #size is a built-in that works with arrays, as are #peek and #poke, below
    alignment _   = alignment (undefined :: CChar)
    peek ptr      = do -- to get values from the C app
        aSize     <- (#peek struct costMatrix, alphSize)        ptr
        lcm'      <- (#peek struct costMatrix, lcm)             ptr
        gapchar   <- (#peek struct costMatrix, gap_char)        ptr
        costModel <- (#peek struct costMatrix, cost_model_type) ptr
        combos    <- (#peek struct costMatrix, combinations)    ptr
        gapOpen   <- (#peek struct costMatrix, gap_open)        ptr
        metric    <- (#peek struct costMatrix, is_metric)       ptr
        elems     <- (#peek struct costMatrix, all_elements)    ptr
        best      <- (#peek struct costMatrix, cost)            ptr
        meds      <- (#peek struct costMatrix, median)          ptr
        return  CostMatrix { alphSize      = aSize
                           , lcm           = lcm'
                           , gapChar       = gapchar
                           , costModelType = costmodel
                           , combinations  = combos
                           , gapOpenCost   = gapOpen
                           , allElems      = elems
                           , bestCost      = best
                           , medians       = meds
                           }

    poke ptr (CostMatrix val seqFinal) = do -- to modify values in the C app
        (#peek struct costMatrix, alphSize)        ptr alphSize
        (#peek struct costMatrix, lcm)             ptr lcm
        (#peek struct costMatrix, gap_char)        ptr gapChar
        (#peek struct costMatrix, cost_model_type) ptr costModelType
        (#peek struct costMatrix, combinations)    ptr combinations
        (#peek struct costMatrix, gap_open)        ptr gapOpen
        (#peek struct costMatrix, is_metric)       ptr isMetric
        (#peek struct costMatrix, all_elements)    ptr elems
        (#peek struct costMatrix, cost)            ptr bestCost
        (#peek struct costMatrix, median)          ptr medians


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
                        -> Ptr CostMatrix

foreign import ccall unsafe "c_code_alloc_setup.h initializeSeq"
    allocateSequenceFn_c :: CSize -> CSize -> Ptr Sequence




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


