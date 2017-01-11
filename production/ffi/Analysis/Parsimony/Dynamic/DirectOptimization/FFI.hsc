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

import Bio.Character.Exportable.Class
import Foreign
--import Foreign.Ptr
--import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Array
--import Foreign.StablePtr
import Prelude hiding (lcm, sequence, tail)
--import System.IO.Unsafe

#include "costMatrix.h"
#include "c_code_alloc_setup.h"
#include "c_alignment_interface.h"
#include "nwMatrices.h"
-- #include "seqAlign.h"

-- | This will be compatible with uint64_t, which is type of external bit arrays.
type CArrayUnit  = CULong
-- | C short type, but unused right now.
type CDirMtxUnit = CShort

{- ******************************************* Sequence declaration and Storable instance ******************************************* -}
{-
-- | Input/output type for C. 'AlignIO' is used both to pass in unaligned sequences, and to receive aligned ones.
--  Input is
data Alignment2d = Alignment2d { alignedSequence1       :: AlignIO
                               , alignedSequence2       :: AlignIO
                               , ungappedMedianSequence :: AlignIO
                               , gappedMedianSequence   :: AlignIO
                               , unionSequence          :: AlignIO
                               , cost                   :: CInt
                               }

-- Because we're using a struct we need to make a Storable instance
instance Storable Alignment2d where
    sizeOf    _  = (#size struct alignment) -- #size is a built-in that works with arrays, as are #peek and #poke, below
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
-}

data AlignIO = AlignIO { -- magic_number :: CInt     -- TODO: No idea what this is for; figure it out?
                         sequence :: StablePtr CInt     --
                       , seqLen   :: CSize        -- Total length of the sequence stored
                       , arrCap   :: CSize        -- Total capacity of allocated array
                       }

-- Because we're using a struct we need to make a Storable instance
instance Storable AlignIO where
    sizeOf    _  = (#size struct alignIO) -- #size is a built-in that works with arrays, as are #peek and #poke, below
    alignment _  = alignment (undefined :: CSize)
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
                                 , combinations  :: CInt    {- This is a flag set to true if we are going to accept
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




-- | Create and allocate cost matrix
-- first argument, TCM, is only for non-ambiguous nucleotides, and it used to generate
-- the entire cost matrix, which includes ambiguous elements.
-- TCM is row-major, with each row being the left sequence element.
-- It is therefore indexed not by powers of two, but by cardinal integer.
-- TODO: For now we only allocate 2d matrices. 3d will come later.
foreign import ccall unsafe "c_code_alloc_setup.h setupCostMtx"
    setupCostMatrix2dFn_c :: Ptr CInt  -- tcm
                        -> CInt      -- alphSize
                        -> CInt      -- gap_open
                        -> CInt      -- is_2d
                        -> Ptr CostMatrix2d


foreign import ccall unsafe "c_code_alloc_setup.h initializeSeq"
    allocateSequenceFn_c :: CSize -> CSize -> Ptr AlignIO

--allocateCostMatrix ::


-- | Performs a naive direct optimization
-- Takes in two characters to run DO on and a metadata object
-- Returns an assignment character, the cost of that assignment, the assignment character with gaps included,
-- the aligned version of the first input character, and the aligned version of the second input character
-- The process for this algorithm is to generate a traversal matrix, then perform a traceback.
naiveDO :: Exportable s
        => s                    -- ^ First  dynamic character
        -> s                    -- ^ Second dynamic character
        -> (Int -> Int -> Int)  -- ^ Structure defining the transition costs between character states
        -> (s, Double, s, s, s) -- ^ The /ungapped/ character derived from the the input characters' N-W-esque matrix traceback
                                --
                                --   The cost of the alignment
                                --
                                --   The /gapped/ character derived from the the input characters' N-W-esque matrix traceback
                                --
                                --   The gapped alignment of the /first/ input character when aligned with the second character
                                --
                                --   The gapped alignment of the /second/ input character when aligned with the first character
naiveDO char1 char2 costStruct = undefined
  where
    exChar1 = toExportableElements char1
    exChar2 = toExportableElements char2


{-


-- |
foreign import ccall unsafe "c_alignment_interface.h align2d"
    do2dAlign :: AlignIO -> AlignIO -> AlignIO -> AlignIO -> AlignIO -> CostMatrix2d -> CInt -> CInt -> CInt



call2dSeqAlignFn_c :: Exportable c
                   => c
                   -> c
                   -> CostMatrix
                   -> (c, Int, c, c, c, c, c)
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
-}

{-
-- | First sequence must be shortest
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


{-
data Alignment3d = Alignment3d { sequence3d1      :: AlignIO
                               , sequence3d2      :: AlignIO
                               , alignedSequence3 :: AlignIO
                               , alignment3d      :: AlignIO
                               , cost3d           :: CInt
                               }
-}
{-
data NWMatrices = NWMatrices { nwCapacity        :: CSize      {- Total length of available memory allocated to matrix or cube ==
                                                                -    | for 2d: 12 * max(len_s1,
                                                                -    | for 3d: len_s1 * len_s2 * len_s3
                                                                -}
                             , effCapacity     :: CSize        -- Length of the efficiency matrix; at least as large as cap_nw
                             , precalcCapacity :: CSize        -- Length of the precalculated matrix == max(len_s1, len_s2) * (alphSize + 1)
                                                               -- extra 1 is for gap
                             , nwCostMtx       :: Ptr CInt     -- NW cost matrix for both 2d and 3d alignment
                             , dirMtx          :: Ptr CUShort  -- Matrix for backtrace directions in a 2d alignment
                             , nwCostMtx3dPtrs :: Ptr CInt     -- Matrix for 3d alignment, just a set of pointers into nw_costMtx
                                                               -- alloced on C side
                             , nwDirMtx3dPtrs  :: Ptr CUShort  {- Matrix for backtrace directions in a 3d alignment, just a set of pointers
                                                                - into nw_costMtx --- alloced on C side -}
                             , precalc         :: Ptr CInt     {- a three-dimensional matrix that holds
                                                                - the transition costs for the entire alphabet (of all three sequences)
                                                                - with the sequence seq3. The columns are the bases of seq3, and the rows are
                                                                - each of the alphabet characters (possibly including ambiguities). See
                                                                - cm_precalc_4algn_3d for more information).
                                                                -}
                             }


instance Storable NWMatrices where
    sizeOf    _   = (#size struct nwMatrices) -- #size is a built-in that works with arrays, as are #peek and #poke, below
    alignment _   = alignment (undefined :: CSize)
    peek ptr      = do                 -- to get values from the C app
        nw_cap    <- (#peek struct nwMatrices, cap_nw)       ptr
        eff_cap   <- (#peek struct nwMatrices, cap_eff)      ptr
        pre_cap   <- (#peek struct nwMatrices, cap_pre)      ptr
        cst_mtx   <- (#peek struct nwMatrices, nw_costMtx)   ptr
        dir_mtx   <- (#peek struct nwMatrices, dir_mtx_2d)   ptr
        cst_mtx3d <- (#peek struct nwMatrices, nw_costMtx3d) ptr
        dir_mtx3d <- (#peek struct nwMatrices, nw_costMtx3d) ptr
        pre       <- (#peek struct nwMatrices, precalcMtx)   ptr

        return  NWMatrices { nwCapacity      = nw_cap
                             effCapacity     = eff_cap
                             precalcCapacity = pre_cap
                             nwCostMtx       = cst_mtx
                             dirMtx          = dir_mtx
                             nwCostMtx3dPtrs = cst_mtx3d
                             nwDirMtx3dPtrs  = dir_mtx3d
                             precalc         = pre
                           }
    poke ptr (NWMatrices nw_cap eff_cap pre_cap cst_mtx dir_mtx cst_mtx3d dir_mtx3d pre) = do -- to modify values in the C app
        (#poke struct nwMatrices, cap_nw)         ptr nw_cap
        (#poke struct nwMatrices, cap_eff)        ptr eff_cap
        (#poke struct nwMatrices, cap_pre)        ptr pre_cap
        (#poke struct nwMatrices, nw_costMtx)     ptr cst_mtx
        (#poke struct nwMatrices, nw_dirMtx)      ptr dir_mtx
        (#poke struct nwMatrices, nw_costMtx3d_d) ptr cst_mtx3d
        (#poke struct nwMatrices, nw_dirMtx3d_d)  ptr dir_mtx3d
        (#poke struct nwMatrices, precalcMtx)     ptr pre
-}
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

