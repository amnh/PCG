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

{-# LANGUAGE ForeignFunctionInterface, BangPatterns #-}

import Foreign
import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types
import System.IO.Unsafe

#include "algn.h"

type CArrayUnit  = CULong -- This will be compatible with uint64_t



data Sequence = Sequence { magic_number :: CInt     -- No idea what this is for
                         , cap          :: CInt     -- Capacity of the sequence memory structure.
                         , len          :: CInt     -- Total length of the sequence stored.
                         , array_head   :: Ptr CInt -- Pointer to eginning of the allocated array
                         , seq_begin    :: Ptr CInt -- Position where the first element of the sequence is actually stored.
                            {- ***Sequence as it comes in must be in last x spaces in array!*** -}
                         , seq_end      :: Ptr CInt -- Pointer to end of allocated array (and thus sequence)
                         }



{- /****** In each of the following calculations, seq length includes opening gap *******/ -}
data NWMatrices = NWMatrices { len        :: CInt          {- Total length of available memory allocated to matrix or cube ==
                                                               | for 2d: 12 * max(len_s1, len_s2)
                                                               | for 3d: len_s1 * len_s2 * len_s3
                                                           -}
                             , len_eff    :: CInt          -- Length of the efficiency matrix; at least as large as len
                                                           --   TODO: figure out what len_eff actually is
                             , len_pre    :: CInt          -- Length of the precalculated matrix == max(len_s1, len_s2) * (alphSize + 1);
                                                           --   extra 1 is for gap
                             , nw_matrix  :: Ptr CInt      -- NW cost matrix
                             , dirMtx2d   :: Ptr CUShort   -- Matrix for backtrace directions in a 2d alignment
                             , pointers3d :: Ptr Ptr CInt  -- Matrix of pointers to each row in a 3d align
                             , matrix3d   :: Ptr CInt      -- Matrix for 3d alignment, just a set of pointers into nw_costMtx -- alloced internally.
                             , dirMtx3d   :: Ptr CUShort   {- Matrix for backtrack directions in a 3d alignment, just a set of pointers
                                                              into nw_costMtx -- alloced internally
                                                            -}
                             , precalc    :: Ptr CInt      {- a three-dimensional matrix that holds
                                                              the transition costs for the entire alphabet (of all three sequences)
                                                              with the sequence seq3. The columns are the bases of seq3, and the rows are
                                                              each of the alphabet characters (possibly including ambiguities). See
                                                              cm_precalc_4algn_3d for more information).
                                                           -}
                             }


data CostMatrix = CostMatrix { alphSize      :: CInt
                             , lcm           :: CInt
                             , gap           :: CInt
                             , costModelType :: CInt
                             , cominatins    :: CInt
                             , gapOpenCost   :: CInt
                             , isMetric      :: CInt
                             , allElems      :: CInt
                             , tcm           :: Ptr CInt
                             , median        :: Ptr CUInt
                             , worst         :: Ptr CInt
                             , prependCost   :: Ptr CInt
                             , tailCost      :: Ptr CInt
                             }


-- Because we're using a struct we need to make a Storable instance
instance Storable Sequence where
    sizeOf    _ = (#size struct seq) -- #size is a built-in that works with arrays, as are #peek and #poke, below
    alignment _ = alignment (undefined :: CChar)
    peek ptr    = do -- to get values from the C app
        value    <- (#peek struct align, finalWt) ptr
        sequence <- (#peek struct align, finalStr) ptr
        return  Sequence { val = value, seqFinal = sequence }

------------- Don't need this part, but left in for completion ---------------
----- Will get compiler warning if left out, because of missing instances ----
    poke ptr (Sequence val seqFinal) = do -- to modify values in the C app
        (#poke struct align, finalWt) ptr val
        (#poke struct align, finalStr) ptr seqFinal

-- This is the declaration of the Haskell wrapper for the C function we're calling.
-- Note that this fn is called from testFn.
foreign import ccall unsafe "myComplexTestC.h testFn"
    callExtFn_c :: CString -> CString -> CInt -> CInt -> Ptr Sequence -> CInt

-- testFn can be called from within Haskell code.
testFn :: Either String (Int, String)
testFn = unsafePerformIO $
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
                seqStr              <- peekCAString seq
                free seq
                pure $ Right (fromIntegral val, seqStr)
            else do
                pure $ Left "Out of memory"

-- Just for testing from CLI outside of ghci.
main :: IO ()
main = putStrLn $ show testFn