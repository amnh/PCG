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

-- TODO: do I need this: https://hackage.haskell.org/package/base-4.9.0.0/docs/Foreign-StablePtr.html

{-# LANGUAGE ForeignFunctionInterface, BangPatterns #-}

import Foreign
import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshall.Array
import System.IO.Unsafe

#include "algn.h"
#include "nwMatrices.h"

type CArrayUnit  = CULong -- This will be compatible with uint64_t
type CDirMtxUnit = CShort -- C short type

{- ******************************************* Sequence declaration and instance ******************************************* -}

data Sequence = Sequence { magic_number :: CInt     -- No idea what this is for
                         , capacity     :: CInt     -- Capacity of the sequence memory structure.
                         , seqLen       :: CInt     -- Total length of the sequence stored.
                         , arrayHead    :: Ptr CInt -- Pointer to beginning of the allocated array
                         , seqHead      :: Ptr CInt -- Position where the first element of the sequence is actually stored.
                     {- ********  Sequence as it comes in must be in last x spaces in array!  ******** -}
                         , seq_end      :: Ptr CInt -- Pointer to end of allocated array (and thus sequence)
                         }

-- Because we're using a struct we need to make a Storable instance
instance Storable Sequence where
    sizeOf    _  = (#size struct seq) -- #size is a built-in that works with arrays, as are #peek and #poke, below
    alignment _  = alignment (undefined :: CChar)
    peek ptr     = do                 -- to get values from the C app
        cap      <- (#peek struct align, cap)        ptr
        len      <- (#peek struct align, len)        ptr
        seqStart <- (#peek struct align, seq_begin)  ptr
        arrHead  <- (#peek struct align, array_head) ptr
        end      <- (#peek struct align, end)        ptr

        return  Sequence { capacity  = cap
                         , seqLen    = len
                         , arrayHead = arrHead
                         , seqHead   = seqStart
                         , seqEnd    = end
                         }
    poke ptr (Sequence val capacity ) = do -- to modify values in the C app
        (#poke struct align, array_head) ptr arrayHead
        (#poke struct align, cap)        ptr capacity
        (#poke struct align, len)        ptr seqLen
        (#poke struct align, seq_begin)  ptr seqHead
        (#poke struct align, end)        ptr seqEnd


{- ******************************************* NWMatrices declaration and instance ******************************************* -}
{- /****** In each of the following calculations, seq length includes opening gap *******/ -}
data NWMatrices = NWMatrices { capNW      :: CInt              {- Total length of available memory allocated to matrix or 3d matrix ==
                                                                   | for 2d: 12 * max(len_s1, len_s2)
                                                                   | for 3d: len_s1 * len_s2 * len_s3
                                                               -}
                             , capEff     :: CInt              -- Length of the efficiency matrix; at least as large as len
                                                               --   TODO: figure out what len_eff actually is
                             , capPre     :: CInt              -- Length of the precalculated matrix == max(len_s1, len_s2) * (alphSize + 1);
                                                               --   extra 1 is for gap
                             , nwMatrix   :: Ptr CInt          -- NW cost matrix
                             , dirMtx2d   :: Ptr CDirMtxUnit   -- Matrix for backtrace directions in a 2d alignment
                             , pointers3d :: Ptr Ptr CInt      -- Matrix of pointers to each row in a 3d align
                             , matrix3d   :: Ptr CInt          -- Matrix for 3d alignment, just a set of pointers into nw_costMtx -- alloced internally.
                             , dirMtx3d   :: Ptr CDirMtxUnit   {- Matrix for backtrack directions in a 3d alignment, just a set of pointers
                                                                  into nw_costMtx -- alloced internally
                                                               -}
                             , precalc    :: Ptr CInt          {- a three-dimensional matrix that holds
                                                                  the transition costs for the entire alphabet (of all three sequences)
                                                                  with the sequence seq3. The columns are the bases of seq3, and the rows are
                                                                  each of the alphabet characters (possibly including ambiguities). See
                                                                  cm_precalc_4algn_3d for more information).
                                                               -}
                             }

-- Because we're using a struct we need to make a Storable instance
instance Storable NWMatrices where
    sizeOf    _  = (#size struct seq) -- #size is a built-in that works with arrays, as are #peek and #poke, below
    alignment _  = alignment (undefined :: CChar)
    peek ptr     = do -- to get values from the C app
        cap_nw         <- (#peek struct nwMatrices, cap_nw)       ptr
        cap_eff        <- (#peek struct nwMatrices, cap_eff)      ptr
        cap_pre        <- (#peek struct nwMatrices, cap_pre) ptr
        nw_costMtx     <- (#peek struct nwMatrices, nw_costMtx)   ptr
        dir_mtx_2d     <- (#peek struct nwMatrices, dir_mtx_2d)   ptr
        pointers_3d    <- (#peek struct nwMatrices, pointers_3d)  ptr
        nw_costMtx3d   <- (#peek struct nwMatrices, nw_costMtx3d)         ptr
        nw_costMtx3d_d <- (#peek struct nwMatrices, nw_costMtx3d_d)       ptr
        precalc        <- (#peek struct nwMatrices, precalc)      ptr

        return  NWMatrices { capNW      = cap_nw
                           , capEff     = cap_eff
                           , capPre     = cap_pre
                           , nwMatrix   = nw_costMtx
                           , dirMtx2d   = dir_mtx_2d
                           , pointers3d = pointers_3d
                           , matrix3d   = nw_costMtx3d
                           , dirMtx3d   = nw_costMtx3d_d
                           , precalc    = precalc
                       }

    poke ptr (NWMatrices val seqFinal) = do -- to modify values in the C app
        (#poke struct nwMatrices, cap_nw)         ptr capNW
        (#poke struct nwMatrices, cap_eff)        ptr capEff
        (#poke struct nwMatrices, cap_pre)        ptr capPre
        (#poke struct nwMatrices, nw_costMtx)     ptr nwMatrix
        (#poke struct nwMatrices, dir_mtx_2d)     ptr dirMtx2d
        (#poke struct nwMatrices, pointers_3d)    ptr pointers3d
        (#poke struct nwMatrices, nw_costMtx3d)   ptr matrix3d
        (#poke struct nwMatrices, nw_costMtx3d_d) ptr dirMtx3d
        (#poke struct nwMatrices, precalc)        ptr precalc




{- ******************************************* CostMatrix declaration and instance ******************************************* -}
data CostMatrix = CostMatrix { alphSize      :: CInt      -- alphabet size including gap, and including ambiguities if
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
                             , worstCost     :: Ptr CInt    {- The transformation cost matrix, including ambiguities,
                                                             - storing the **worst** cost for each ambiguity pair
                                                             - Missing in 3d
                                                             -}
                             , prependCost   :: Ptr CInt     {- The cost of going from gap -> each base. For ambiguities, use best cost.
                                                              - Set up as all_elements x all_elements
                                                              - matrix, but seemingly only first row is used.
                                                              - Missing in 3d because current version of 3d sets gap cost
                                                              - as constant.
                                                              -}
                             , tailCost      :: Ptr CInt     {- As prepend_cost, but with reverse directionality,
                                                              - so base -> gap.
                                                              - As with prepend_cost, seems to be allocated as too large.
                                                              - Missing in 3d because current version of 3d sets gap cost
                                                              - as constant.
                                                              -}
                             }



-- Because we're using a struct we need to make a Storable instance
instance Storable CostMatrix where
    sizeOf    _   = (#size struct seq) -- #size is a built-in that works with arrays, as are #peek and #poke, below
    alignment _   = alignment (undefined :: CChar)
    peek ptr      = do -- to get values from the C app
        aSize     <- (#peek struct nwMatrices, alphSize)        ptr
        lcm'      <- (#peek struct nwMatrices, lcm)             ptr
        gapchar   <- (#peek struct nwMatrices, gap_char)        ptr
        costModel <- (#peek struct nwMatrices, cost_model_type) ptr
        combos    <- (#peek struct nwMatrices, combinations)    ptr
        gapOpen   <- (#peek struct nwMatrices, gap_open)        ptr
        metric    <- (#peek struct nwMatrices, is_metric)       ptr
        elems     <- (#peek struct nwMatrices, all_elements)    ptr
        best      <- (#peek struct nwMatrices, cost)            ptr
        meds      <- (#peek struct nwMatrices, median)          ptr
        worst     <- (#peek struct nwMatrices, worst)           ptr
        prepend   <- (#peek struct nwMatrices, prepend_cost)    ptr
        tail      <- (#peek struct nwMatrices, tail_cost)       ptr
        return  CostMatrix { alphSize      = cap_nw
                           , lcm           = cap_eff
                           , gapChar       = cap_pre
                           , costModelType = nw_costMtx
                           , combinations  = dir_mtx_2d
                           , gapOpenCost   = gapOpen
                           , allElems      = elems
                           , bestCost      = best
                           , medians       = meds
                           , worstCost     = worst
                           , prependCost   = prepend
                           , tailCost      = tail
                       }

    poke ptr (CostMatrix val seqFinal) = do -- to modify values in the C app
        (#peek struct nwMatrices, alphSize)        ptr alphSize
        (#peek struct nwMatrices, lcm)             ptr lcm
        (#peek struct nwMatrices, gap_char)        ptr gapChar
        (#peek struct nwMatrices, cost_model_type) ptr costModelType
        (#peek struct nwMatrices, combinations)    ptr combinations
        (#peek struct nwMatrices, gap_open)        ptr gapOpen
        (#peek struct nwMatrices, is_metric)       ptr isMetric
        (#peek struct nwMatrices, all_elements)    ptr elems
        (#peek struct nwMatrices, cost)            ptr bestCost
        (#peek struct nwMatrices, median)          ptr medians
        (#peek struct nwMatrices, worst)           ptr worstCost
        (#peek struct nwMatrices, prepend_cost)    ptr prependCost
        (#peek struct nwMatrices, tail_cost)       ptr tailCost


-- create and allocate cost matrix
foreign import ccall unsafe "c_code_alloc_setup.h initializeNWMtx"
    setup_NW_MatrxFn_c :: Ptr NWMatrices

-- create and allocate cost matrix
foreign import ccall unsafe "c_code_alloc_setup.h setupCostMtx"
    setupCostMatrixFn_c :: Ptr CInt -> CInt -> CInt -> CInt -> Ptr CostMatrix -> Ptr CostMatrix

foreign import ccall unsafe "c_code_alloc_setup.h initializeSeq"
    allocateSequenceFn_c :: CSize -> Ptr Int -> CSize -> Ptr Sequence


-- | Take in two sequences and align them. First two arguments are 'Sequence's. Second
-- two are 'CostMatrix's. 'CInt' in last argument is treated as bool in C code. 1 requests
-- Ukkonen, 0 says not. Returns cost.
foreign import ccall unsafe "algn.h algn_nw_2d"
    callSeqAlign2dFn_c :: Ptr Sequence -> Ptr Sequence -> Ptr CostMatrix -> Ptr NWMatrices -> CInt -> CInt

-- | Take in two sequences and align them. First two arguments are 'Sequence's. Second
-- two are 'CostMatrix's. 'CInt' in last argument is treated as bool in C code. 1 requests
-- Ukkonen, 0 says not. Returns cost.
foreign import ccall unsafe "algn.h algn_nw_2d"
    callSeqAlign2dFn_c :: Ptr Sequence -> Ptr Sequence -> Ptr CostMatrix -> Ptr NWMatrices -> CInt -> CInt
algn_backtrace_2d (longSeq, shortSeq, retLongSeq, retShortSeq, algn_mtxs2d, costMtx2d, 0, 0, 1);

-- | Will only work if alignment and backtrace have already been called.
foreign import ccall unsafe "algn.h algn_union"
    getUnionFn_c :: Ptr Sequence -> Ptr Sequence -> Ptr Sequence

-- | Will only work if alignment and backtrace have already been called.
foreign import ccall unsafe "algn.h algn_get_median_2d_no_gaps"
    getUngappedMedianFn_c :: Ptr Sequence -> Ptr Sequence -> Ptr CostMatrix -> Ptr Sequence

-- | Will only work if alignment and backtrace have already been called.
foreign import ccall unsafe "algn.h algn_get_median_2d_with_gaps"
    getGappedMedianFn_c :: Ptr Sequence -> Ptr Sequence -> Ptr CostMatrix -> Ptr Sequence


-- testFn can be called from within Haskell code.
callSeqAlignFn_c :: Either String (Int, String)
callSeqAlignFn_c = unsafePerformIO $
    -- have to allocate memory. Note that we're allocating via a lambda fn. I
    -- don't yet understand what exactly is going on here.
    alloca $ \alignPtr -> do
        -- This first part is similar to simple example.
        arg1   <- newCAString "hello"
        arg2   <- newCAString "goodbye"

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