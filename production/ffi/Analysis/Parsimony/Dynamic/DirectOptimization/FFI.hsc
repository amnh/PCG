-----------------------------------------------------------------------------
-- |
-- an FFI interface for C code that efficiently aligns either two or three
-- sequences, using Ukkonen when appropriate, in both affine and non-affine
-- cases.
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
{-# LANGUAGE DeriveGeneric #-}

module Analysis.Parsimony.Dynamic.DirectOptimization.FFI
  ( CostMatrix2d
  , DenseTransitionCostMatrix
  , foreignPairwiseDO
  , generateDenseTransitionCostMatrix
  ) where

import Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.Internal (filterGaps)
import Bio.Character.Encodable
import Bio.Character.Exportable.Class
import Control.DeepSeq
import Control.Lens
import Data.Semigroup
import Debug.Trace
import Foreign
--import Foreign.Ptr
--import Foreign.C.String
import Foreign.C.Types
--import Foreign.ForeignPtr
--import Foreign.Marshal.Array
--import Foreign.StablePtr
import GHC.Generics (Generic)
import Prelude hiding (lcm, sequence, tail)
import System.IO.Unsafe (unsafePerformIO)

import Debug.Trace


#include "c_alignment_interface.h"
#include "c_code_alloc_setup.h"
#include "costMatrix.h"
#include "nwMatrices.h"
-- #include "seqAlign.h"


type DenseTransitionCostMatrix = Ptr CostMatrix2d


data UnionContext  = ComputeUnions  | DoNotComputeUnions


instance Enum UnionContext where

    fromEnum      ComputeUnions = 1
    fromEnum DoNotComputeUnions = 0

    toEnum 0 = DoNotComputeUnions
    toEnum _ =      ComputeUnions


data MedianContext = ComputeMedians | DoNotComputeMedians


instance Enum MedianContext where

    fromEnum      ComputeMedians = 1
    fromEnum DoNotComputeMedians = 0

    toEnum 0 = DoNotComputeMedians
    toEnum _ =      ComputeMedians


generateDenseTransitionCostMatrix :: Word -> (Word -> Word -> Word) -> DenseTransitionCostMatrix
generateDenseTransitionCostMatrix alphabetSize costFunction = getCostMatrix2dNonAffine alphabetSize costFunction


foreignPairwiseDO :: ( EncodableDynamicCharacter s
                     , Exportable s
                     )
                  => s                         -- ^ First  dynamic character
                  -> s                         -- ^ Second dynamic character
                  -> DenseTransitionCostMatrix -- ^ Structure defining the transition costs between character states
                  -> (s, Double, s, s, s)      -- ^ The /ungapped/ character derived from the the input characters' N-W-esque matrix traceback
foreignPairwiseDO lhs rhs costMatrix = algn2d lhs rhs costMatrix DoNotComputeUnions ComputeMedians



{- ******************************************* Sequence declaration and Storable instance ******************************************* -}
{-
-- | Input/output type for C. 'AlignIO' is used both to pass in unaligned characters, and to receive aligned ones.
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
        arr  <- (#peek struct alignIO, character) ptr
        cap  <- (#peek struct alignIO, capacity) ptr

        return  AlignIO { charLen   = len
                        , character = arr
                        , arrCap   = cap
                        }
    poke ptr (AlignIO len arr cap) = do -- to modify values in the C app
        (#poke struct alignIO, character) ptr len
        (#poke struct alignIO, length)   ptr arr
        (#poke struct alignIO, capacity) ptr cap
-}

-- | Input/output data type for C alignment code, to avoid having to write the whole seq type.
data AlignIO = AlignIO { -- magic_number :: CInt     -- TODO: No idea what this is for; figure it out?
                         character :: Ptr CUInt
                       , charLen   :: CSize        -- Total length of the character stored
                       , arrCap    :: CSize        -- Total capacity of allocated array
                       }


-- | Because we're using a struct we need to make a Storable instance
instance Storable AlignIO where
    sizeOf    _  = (#size struct alignIO) -- #size is a built-in that works with arrays, as are #peek and #poke, below
    alignment _  = alignment (undefined :: CSize)
    peek ptr     = do                 -- to get values from the C app
        arr  <- (#peek struct alignIO, character) ptr
        len  <- (#peek struct alignIO, length)    ptr
        cap  <- (#peek struct alignIO, capacity)  ptr

        return  AlignIO { character = arr
                        , charLen   = len
                        , arrCap    = cap
                        }

    poke ptr (AlignIO arr len cap) = do -- to modify values used in the C app
        (#poke struct alignIO, character) ptr arr
        (#poke struct alignIO, length)    ptr len
        (#poke struct alignIO, capacity)  ptr cap




{- ******************************************* CostMatrix declarations and Storable instances ******************************************* -}
-- | Holds single cost matrix, which contains costs and medians for all
-- possible character elements. It is completely filled using a TCM. See note below at 'setupCostMatrixFn_c'.
data CostMatrix2d = CostMatrix2d { alphSize      :: CInt      -- alphabet size including gap, and including ambiguities if
                                                              --     combinations == True
                                 , lcm           :: CInt      -- ceiling of log_2 (alphSize)
                                 , gapChar       :: CInt      -- gap value (1 << (alphSize - 1))
                                 , costModelType :: CInt      {- The type of cost model to be used in the alignment,
        System.IO                                                       - i.e. affine or not.
                                                               - Based on cost_matrix.ml, values are:
                                                               - • linear == 0
                                                               - • affine == 1
                                                               - • no_alignment == 2,
                                                               - but I updated it. See costMatrix.h.
                                                               -}
                                 , combinations  :: CInt      {- This is a flag set to true if we are going to accept
                                                               - all possible combinations of the elements in the alphabet
                                                               - in the alignments. This is not true for protein characters
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
                                 } deriving (Eq, Generic)

instance NFData CostMatrix2d

instance Show CostMatrix2d where

    show = unlines .
           ([ show . alphSize
            , show . lcm
            , show . gapChar
            , show . costModelType
            , show . combinations
            , show . gapOpenCost
            , show . isMetric
            , show . allElems
            , show . bestCost
            , show . medians
            , show . worstCost
            , show . prependCost
            , show . tailCost
            ] <*>) . pure


{--}
-- | Because we're using a struct we need to make a Storable instance
instance Storable CostMatrix2d where
    sizeOf _  = (#size struct cost_matrices_2d) -- #size is a built-in that works with arrays, as are #peek and #poke, below
    alignment = sizeOf -- alignment (undefined :: StablePtr CostMatrix2d)
    peek ptr  = do -- to get values from the C app
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
        tailVal      <- (#peek struct cost_matrices_2d, tail_cost)       ptr
        pure  CostMatrix2d { alphSize      = aSizeVal
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
{--}



-- | Create and allocate cost matrix
-- first argument, TCM, is only for non-ambiguous nucleotides, and it used to generate
-- the entire cost matrix, which includes ambiguous elements.
-- TCM is row-major, with each row being the left character element.
-- It is therefore indexed not by powers of two, but by cardinal integer.
-- TODO: For now we only allocate 2d matrices. 3d will come later.
foreign import ccall unsafe "c_code_alloc_setup.h setup2dCostMtx"
    setupCostMatrix2dFn_c :: Ptr CUInt         -- ^ tcm
                          -> CSize             -- ^ alphSize
                          -> CInt              -- ^ gap_open
--                          -> CInt              -- ^ is_2d
                          -> Ptr CostMatrix2d
                          -> IO ()


-- | Set up and return a non-affine cost matrix
--
-- The cost matrix is allocated strictly.
getCostMatrix2dNonAffine :: Word
                         -> (Word -> Word -> Word)
                         -> DenseTransitionCostMatrix
getCostMatrix2dNonAffine = performMatrixAllocation 1 0


-- | Set up and return a non-affine cost matrix
--
-- The cost matrix is allocated strictly.
getCostMatrix2dAffine :: CInt -- gap open cost
                      -> Word
                      -> (Word -> Word -> Word)
                      -> DenseTransitionCostMatrix
getCostMatrix2dAffine = performMatrixAllocation 1


performMatrixAllocation :: CInt -- Is 2d
                        -> CInt -- gap open cost
                        -> Word
                        -> (Word -> Word -> Word)
                        -> DenseTransitionCostMatrix
performMatrixAllocation is2D gapOpen alphabetSize costFn = unsafePerformIO . withArray rowMajorList $ \allocedTCM -> do
        !_ <- trace "Allocation a Dense Matrix" $ pure ()
        !output <- malloc :: IO (Ptr CostMatrix2d)
        !_ <- setupCostMatrix2dFn_c allocedTCM matrixDimension gapOpen {- is2D -} output
        pure output
    where
        matrixDimension = toEnum $ fromEnum alphabetSize
        -- This *should* be in row major order due to the manner in which list comprehensions are performed.
        rowMajorList = [ toEnum . fromEnum $ costFn i j | i <- range,  j <- range ]
        range = [0 .. alphabetSize - 1]


-- | Create and allocate cost matrix
-- first argument, TCM, is only for non-ambiguous nucleotides, and it used to generate
-- the entire cost matrix, which includes ambiguous elements.
-- TCM is row-major, with each row being the left character element.
-- It is therefore indexed not by powers of two, but by cardinal integer.
-- TODO: For now we only allocate 2d matrices. 3d will come later.
foreign import ccall unsafe "c_alignment_interface.h align2d"
    align2dFn_c :: Ptr AlignIO          -- ^ character1, input & output
                -> Ptr AlignIO          -- ^ character2, input & output
                -> Ptr AlignIO          -- ^ gapped median output
                -> Ptr AlignIO          -- ^ ungapped median output
                -- -> Ptr AlignIO          -- ^ unioned median output
                -> DenseTransitionCostMatrix
                -> CInt                  -- ^ compute ungapped & not   gapped medians
                -> CInt                  -- ^ compute   gapped & not ungapped medians
                -> CInt                  -- ^ compute union
                -> CInt                  -- ^ cost


-- | Performs a naive direct optimization
-- Takes in two characters to run DO on and a metadata object
-- Returns an assignment character, the cost of that assignment, the assignment character with gaps included,
-- the aligned version of the first input character, and the aligned version of the second input character
-- The process for this algorithm is to generate a traversal matrix, then perform a traceback.
algn2d :: ( EncodableDynamicCharacter s
          , Exportable s
          )
       => s                         -- ^ First  dynamic character
       -> s                         -- ^ Second dynamic character
       -> DenseTransitionCostMatrix -- ^ Structure defining the transition costs between character states
       -> UnionContext
       -> MedianContext
       -> (s, Double, s, s, s)      -- ^ The /ungapped/ character derived from the the input characters' N-W-esque matrix traceback
                                    --
                                    --   The cost of the alignment
                                    --
                                    --   The /gapped/ character derived from the the input characters' N-W-esque matrix traceback
                                    --
                                    --   The gapped alignment of the /first/ input character when aligned with the second character
                                    --
                                    --   The gapped alignment of the /second/ input character when aligned with the first character
algn2d char1 char2 costStruct computeUnion computeMedians =
    case (toExportableElements char1, toExportableElements char2) of
        (Just x, Just y) -> f x y
        (     _,      _) -> error "Sadness, such sadness"
    where
        f exportedChar1 exportedChar2 = unsafePerformIO $
            do
                char1ToSend <- allocInitALignIO exportedChar1Len . fmap toCInt $ exportedCharacterElements exportedChar1
                char2ToSend <- allocInitALignIO exportedChar2Len . fmap toCInt $ exportedCharacterElements exportedChar2
                retGapped   <- allocInitALignIO 0 []
                retUngapped <- allocInitALignIO 0 []
                -- retUnion    <- allocInitALignIO 0 []

{--}
                AlignIO char1Ptr char1Len buffer1Len <- peek char1ToSend
                AlignIO char2Ptr char2Len buffer2Len <- peek char2ToSend

                input1CharArr <- peekArray (fromEnum buffer1Len) char1Ptr
                input2CharArr <- peekArray (fromEnum buffer2Len) char2Ptr

                !_ <- trace (mconcat [" Input LHS : { ", show char1Len, " / ", show buffer1Len, " } ", show input1CharArr]) $ pure ()
                !_ <- trace (mconcat [" Input RHS : { ", show char2Len, " / ", show buffer2Len, " } ", show input2CharArr]) $ pure ()
{--}

                let !cost = align2dFn_c char1ToSend char2ToSend retGapped retUngapped costStruct neverComputeOnlyGapped (toCInt computeMedians) (toCInt computeUnion)

                AlignIO ungappedCharArr ungappedLen _ <- peek retUngapped
                AlignIO gappedCharArr   gappedLen   _ <- peek retGapped
                AlignIO retChar1CharArr char1Len    _ <- peek char1ToSend
                AlignIO retChar2CharArr char2Len    _ <- peek char2ToSend
                -- AlignIO unionCharArr    unionLen    _ <- peek retUnion

--                ungappedChar <- peekArray (fromEnum ungappedLen) ungappedCharArr
                gappedChar   <- peekArray (fromEnum gappedLen)   gappedCharArr
                char1Aligned <- peekArray (fromEnum char1Len)    retChar1CharArr
                char2Aligned <- peekArray (fromEnum char2Len)    retChar2CharArr
                -- unionChar    <- peekArray (fromEnum unionLen)    unionCharArr

                let resultingAlignedChar1 = coerceToOutputType char1Len char1Aligned
                let resultingAlignedChar2 = coerceToOutputType char2Len char2Aligned
                let resultingGapped       = coerceToOutputType gappedLen gappedChar
                let resultingUngapped     = filterGaps resultingGapped

                !_ <- trace (" Gapped Char : " <> show   gappedChar) $ pure ()
                !_ <- trace (" Aligned LHS : " <> show char1Aligned) $ pure ()
                !_ <- trace (" Aligned RHS : " <> show char2Aligned) $ pure ()

                output1Buffer <- peekArray (fromEnum buffer1Len) char1Ptr
                output2Buffer <- peekArray (fromEnum buffer2Len) char2Ptr

                !_ <- trace (mconcat [" Output LHS : { ", show char1Len, " / ", show buffer1Len, " } ", show output1Buffer]) $ pure ()
                !_ <- trace (mconcat [" Output RHS : { ", show char2Len, " / ", show buffer2Len, " } ", show output2Buffer]) $ pure ()
{--
                !_ <- trace ("Ungapped Char: " <> show     resultingUngapped) $ pure ()
                !_ <- trace ("  Gapped Char: " <> show       resultingGapped) $ pure ()
                !_ <- trace (" Aligned LHS : " <> show resultingAlignedChar1) $ pure ()
                !_ <- trace (" Aligned RHS : " <> show resultingAlignedChar2) $ pure ()
--}
                !_ <- trace  " > Done with FFI Alignment\n" $ pure ()

                pure (filterGaps resultingGapped, fromIntegral cost, resultingGapped, resultingAlignedChar1, resultingAlignedChar2)
            where
                neverComputeOnlyGapped = 0

                toCInt :: (Enum a, Enum b) => a -> b
                toCInt = toEnum . fromEnum

                elemWidth        = exportedChar1 ^. exportedElementWidth

                exportedChar1Len = toEnum $ exportedChar1 ^. exportedElementCount
                exportedChar2Len = toEnum $ exportedChar2 ^. exportedElementCount
                maxAllocLen      = exportedChar1Len + exportedChar2Len

                allocInitALignIO :: CSize -> [CUInt] -> IO (Ptr AlignIO)
                allocInitALignIO elemCount elemArr  =
                    do
                        output <- malloc :: IO (Ptr AlignIO)
                        outArray <- newArray paddedArr
                        poke output $ AlignIO outArray elemCount maxAllocLen
                        pure output
                    where
                        paddedArr = replicate (max 0 (fromEnum (maxAllocLen - elemCount))) 0 <> elemArr

                coerceToOutputType len charElements =
                    fromExportableElements . ExportableCharacterElements (fromEnum len) elemWidth $ fmap fromIntegral charElements






-- | A C binding that computes only the cost of a 2d alignment
align2dCostOnly
  :: ( EncodableDynamicCharacter s
     , Exportable s
     )
  => s
  -> s
  -> DenseTransitionCostMatrix
  -> (s, Double, s, s, s)
align2dCostOnly c1 c2 cm = trace "cost only" $ algn2d c1 c2 cm DoNotComputeUnions DoNotComputeMedians


-- | A C binding that aligns two DO characters and returns the cost and the ungapped median sequence
align2dGetUngapped
  :: ( EncodableDynamicCharacter s
     , Exportable s
     )
  => s
  -> s
  -> DenseTransitionCostMatrix
  -> (s, Double, s, s, s)
align2dGetUngapped c1 c2 cm = algn2d c1 c2 cm DoNotComputeUnions ComputeMedians


-- | A C binding that aligns two DO characters and returns the cost and the union median
align2dGetUnion
  :: ( EncodableDynamicCharacter s
     , Exportable s
     )
  => s
  -> s
  -> DenseTransitionCostMatrix
  -> (s, Double, s, s, s)
align2dGetUnion c1 c2 cm = algn2d c1 c2 cm ComputeUnions DoNotComputeMedians


-- | A C binding that aligns two DO characters and returns the cost and the gapped and ungapped median sequences
align2dGappedUngapped
  :: ( EncodableDynamicCharacter s
     , Exportable s
     )
  => s
  -> s
  -> DenseTransitionCostMatrix
  -> (s, Double, s, s, s)
align2dGappedUngapped c1 c2 cm = algn2d c1 c2 cm ComputeUnions ComputeMedians


{- Example code with peekArray

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP                      #-}

module RGB where

import Foreign
import Foreign.C
import Control.Monad (ap)

#include "rgb.h"

data RGB = RGB {
      r :: CFloat, g :: CFloat, b :: CFloat
} deriving Show

instance Storable RGB where
    sizeOf    _ = #{size rgb_t}
    alignment _ = alignment (undefined :: CInt)

    poke p rgb_t = do
      #{poke rgb_t, r} p $ r rgb_t
      #{poke rgb_t, g} p $ g rgb_t
      #{poke rgb_t, b} p $ b rgb_t

    peek p = return RGB
             `ap` (#{peek rgb_t, r} p)
             `ap` (#{peek rgb_t, g} p)
             `ap` (#{peek rgb_t, b} p)

foreign import ccall "rgb.h rgb_test" crgbTest :: Ptr RGB -> CSize -> IO ();

rgbTest :: [RGB] -> IO [RGB]
rgbTest rgbs = withArray rgbs $ \ptr ->
               do
                 crgbTest ptr (fromIntegral (length rgbs))
                 peekArray (length rgbs) ptr

rgbAlloc :: [RGB] -> IO (Ptr RGB)
rgbAlloc rgbs = newArray rgbs

rgbPeek :: Ptr RGB -> Int -> IO [RGB]
rgbPeek rgbs l = peekArray l rgbs

rgbTest2 :: Ptr RGB -> Int -> IO ()
rgbTest2 ptr l =
    do
      crgbTest ptr (fromIntegral l)
      return ()

-}

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
                Sequence val char <- peek alignPtr
                charStr           <- peekCAString char
                free char
                pure $ Right (fromIntegral val, charStr)
            else do
                pure $ Left "Out of memory"
-}
{-
-- | First character must be shortest
foreign import ccall unsafe "algn.h algn_get_median_2d_no_gaps"
    getUngappedMedianFn_c :: Ptr Sequence -> Ptr Sequence -> Ptr CostMatrix -> Ptr Sequence

-- | Will only work if alignment and backtrace have already been called.
-- First character must be shortest
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
                Sequence val char <- peek alignPtr
                charStr           <- peekCAString char
                free char
                pure $ Right (fromIntegral val, charStr)
            else do
                pure $ Left "Out of memory"

-- Just for testing from CLI outside of ghci.
main :: IO ()
main = putStrLn $ show callSeqAlignFn_c


-}


{-
data Alignment3d = Alignment3d { character3d1      :: AlignIO
                               , character3d2      :: AlignIO
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
                                                                - the transition costs for the entire alphabet (of all three characters)
                                                                - with the character char3. The columns are the bases of char3, and the rows are
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
-- possible character elements. It is completely filled using a TCM. See note below at 'setupCostMatrixFn_c'.
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
                                                                - in the alignments. This is not true for protein characters
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
