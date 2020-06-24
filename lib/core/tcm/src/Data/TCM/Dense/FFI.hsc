-----------------------------------------------------------------------------
-- |
-- An FFI interface for C code that efficiently aligns either two or three
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

{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE DeriveAnyClass           #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE DerivingStrategies       #-}
-- TODO: Do I need this: https://hackage.haskell.org/package/base-4.9.0.0/docs/Foreign-StablePtr.html
{-# LANGUAGE ForeignFunctionInterface #-}

module Data.TCM.Dense.FFI
  ( CostMatrix2d(..)
  , CostMatrix3d(..)
  , DenseTransitionCostMatrix(..)
  , AlignmentStrategy(..)
  -- * Construction
  , generateDenseTransitionCostMatrix
  -- * Accessor functions
  , lookupPairwise
  , lookupThreeway
  -- * Querries
  , getAlignmentStrategy
  ) where

import Control.DeepSeq
import Foreign
--import Foreign.Ptr
--import Foreign.C.String
import Foreign.C.Types
--import Foreign.ForeignPtr
--import Foreign.Marshal.Array
--import Foreign.StablePtr
import GHC.Generics     (Generic)
import Prelude   hiding (sequence, tail)
import System.IO.Unsafe (unsafePerformIO)

-- import Debug.Trace


#include "c_alignment_interface.h"
#include "c_code_alloc_setup.h"
#include "costMatrix.h"
#include "alignmentMatrices.h"
-- #include "seqAlign.h"


-- |
-- Specify which alignment to perform
data  AlignmentStrategy = Linear | Affine | Other
    deriving stock (Eq, Show)


-- |
-- Holds single cost matrix, which contains costs and medians for all possible
-- character elements. It is completely filled using a TCM.
--
-- See note below at 'setupCostMatrixFn_c'.
data  CostMatrix2d
    = CostMatrix2d
    { alphSize            :: CInt      -- alphabet size including gap, and including ambiguities if
    , costMatrixDimension :: CInt      -- ceiling of log_2 (alphSize)
    , gapChar             :: CInt      -- gap value (1 << (alphSize - 1))
    , costModelType       :: CInt      {- The type of cost model to be used in the alignment,
                                        - i.e. affine or not.
                                        - Based on cost_matrix.ml, values are:
                                        - • linear == 0
                                        - • affine == 3
                                        - • no_alignment == 2,
                                        - but I updated it. See costMatrix.h.
                                        -}
    , include_ambiguities :: CInt      {- This is a flag set to true if we are going to accept
                                          all possible combinations of the elements in the alphabet
                                          in the alignments. This is not true for protein characters
                                          for example, where the number of elements of the alphabet
                                          is already too big to build all the possible combinations.
                                       -}
    , gapOpenCost         :: CInt      {- The cost of opening a gap. This is only useful in
                                          certain cost_model_types (type 3: affine, based on my reading of ML code).
                                       -}
    , isMetric            :: CInt      -- if tcm is metric
    , allElems            :: CInt      -- total number of elements
    , bestCost            :: Ptr CInt  {- The transformation cost matrix, including ambiguities,
                                          storing the **best** cost for each ambiguity pair
                                       -}
    , medians             :: Ptr CUInt {- The matrix of possible medians between elements in the
                                          alphabet. The best possible medians according to the cost
                                           matrix.
                                       -}
    , worstCost           :: Ptr CInt  {- The transformation cost matrix, including ambiguities,
                                          storing the **worst** cost for each ambiguity pair
                                       -}
    , prependCost         :: Ptr CInt  {- The cost of going from gap -> each base. For ambiguities, use best cost.
                                          Set up as num_elements x num_elements matrix, but seemingly only first row is used.
                                       -}
    , tailCost            :: Ptr CInt  {- As prepend_cost, but with reverse directionality,
                                          so base -> gap.
                                          As with prepend_cost, seems to be allocated as too large.
                                       -}
    }
    deriving stock    (Eq, Generic)
    deriving anyclass (NFData)


-- |
-- A representation of the 3D cost matrix structure used on the C side.
data  CostMatrix3d
    = CostMatrix3d          -- See CostMatrix2d datatype for field description
    { alphSize3D            :: CInt
    , costMatrixDimension3D :: CInt
    , gapChar3D             :: CInt
    , costModelType3D       :: CInt
    , include_ambiguities3D :: CInt
    , gapOpenCost3D         :: CInt
    , allElems3D            :: CInt
    , bestCost3D            :: Ptr CInt
    , medians3D             :: Ptr CInt
    }
    deriving stock    (Eq, Generic)
    deriving anyclass (NFData)


-- TODO: StablePtr here maybe?
-- |
-- Exposed wrapper for C allocated cost matrix structs.
data  DenseTransitionCostMatrix
    = DenseTransitionCostMatrix
    { costMatrix2D :: Ptr CostMatrix2d
    , costMatrix3D :: Ptr CostMatrix3d
    }
    deriving stock    (Eq, Generic)
    deriving anyclass (NFData)


instance Show CostMatrix2d where

    show = unlines . (fieldRendering <*>)  . pure
      where
        fieldRendering =
            [ show . alphSize
            , show . costMatrixDimension
            , show . gapChar
            , show . costModelType
            , show . include_ambiguities
            , show . gapOpenCost
            , show . isMetric
            , show . allElems
            , show . bestCost
            , show . medians
            , show . worstCost
            , show . prependCost
            , show . tailCost
            ]


instance Storable CostMatrix2d where

    sizeOf _  = (#size struct cost_matrices_2d_t)

    alignment = sizeOf -- alignment (undefined :: StablePtr CostMatrix2d)

    peek ptr  = do
        aSizeVal               <- (#peek struct cost_matrices_2d_t, alphSize           ) ptr
        costMatrixDimensionVal <- (#peek struct cost_matrices_2d_t, costMatrixDimension) ptr
        gapcharVal             <- (#peek struct cost_matrices_2d_t, gap_char           ) ptr
        costModelVal           <- (#peek struct cost_matrices_2d_t, cost_model_type    ) ptr
        combosVal              <- (#peek struct cost_matrices_2d_t, include_ambiguities) ptr
        gapOpenVal             <- (#peek struct cost_matrices_2d_t, gap_open_cost      ) ptr
        metricVal              <- (#peek struct cost_matrices_2d_t, is_metric          ) ptr
        elemsVal               <- (#peek struct cost_matrices_2d_t, num_elements       ) ptr
        bestVal                <- (#peek struct cost_matrices_2d_t, cost               ) ptr
        medsVal                <- (#peek struct cost_matrices_2d_t, median             ) ptr
        worstVal               <- (#peek struct cost_matrices_2d_t, worst              ) ptr
        prependVal             <- (#peek struct cost_matrices_2d_t, prepend_cost       ) ptr
        tailVal                <- (#peek struct cost_matrices_2d_t, tail_cost          ) ptr
        pure CostMatrix2d
            { alphSize            = aSizeVal
            , costMatrixDimension = costMatrixDimensionVal
            , gapChar             = gapcharVal
            , costModelType       = costModelVal
            , include_ambiguities = combosVal
            , gapOpenCost         = gapOpenVal
            , isMetric            = metricVal
            , allElems            = elemsVal
            , bestCost            = bestVal
            , medians             = medsVal
            , worstCost           = worstVal
            , prependCost         = prependVal
            , tailCost            = tailVal
            }

    poke ptr (CostMatrix2d
                  alphSizeVal
                  costMatrixDimensionVal
                  gapCharVal
                  costModelTypeVal
                  include_ambiguitiesVal
                  gapOpenVal
                  isMetricVal
                  elemsVal
                  bestCostVal
                  mediansVal
                  worstCostVal
                  prependCostVal
                  tailCostVal
              ) = do -- to modify values in the C app
        (#poke struct cost_matrices_2d_t, alphSize           ) ptr alphSizeVal
        (#poke struct cost_matrices_2d_t, costMatrixDimension) ptr costMatrixDimensionVal
        (#poke struct cost_matrices_2d_t, gap_char           ) ptr gapCharVal
        (#poke struct cost_matrices_2d_t, cost_model_type    ) ptr costModelTypeVal
        (#poke struct cost_matrices_2d_t, include_ambiguities) ptr include_ambiguitiesVal
        (#poke struct cost_matrices_2d_t, gap_open_cost      ) ptr gapOpenVal
        (#poke struct cost_matrices_2d_t, is_metric          ) ptr isMetricVal
        (#poke struct cost_matrices_2d_t, num_elements       ) ptr elemsVal
        (#poke struct cost_matrices_2d_t, cost               ) ptr bestCostVal
        (#poke struct cost_matrices_2d_t, median             ) ptr mediansVal
        (#poke struct cost_matrices_2d_t, worst              ) ptr worstCostVal
        (#poke struct cost_matrices_2d_t, prepend_cost       ) ptr prependCostVal
        (#poke struct cost_matrices_2d_t, tail_cost          ) ptr tailCostVal


instance Show CostMatrix3d where

    show = unlines . (fieldRendering <*>)  . pure
      where
        fieldRendering =
            [ show . alphSize3D
            , show . costMatrixDimension3D
            , show . gapChar3D
            , show . costModelType3D
            , show . include_ambiguities3D
            , show . gapOpenCost3D
            , show . allElems3D
            , show . bestCost3D
            , show . medians3D
            ]


instance Storable CostMatrix3d where

    sizeOf _  = (#size struct cost_matrices_2d_t)

    alignment = sizeOf -- alignment (undefined :: StablePtr CostMatrix2d)

    peek ptr  = do
        aSizeVal               <- (#peek struct cost_matrices_3d_t, alphSize           ) ptr
        costMatrixDimensionVal <- (#peek struct cost_matrices_3d_t, costMatrixDimension) ptr
        gapcharVal             <- (#peek struct cost_matrices_3d_t, gap_char           ) ptr
        costModelVal           <- (#peek struct cost_matrices_3d_t, cost_model_type    ) ptr
        combosVal              <- (#peek struct cost_matrices_3d_t, include_ambiguities) ptr
        gapOpenVal             <- (#peek struct cost_matrices_3d_t, gap_open_cost      ) ptr
        elemsVal               <- (#peek struct cost_matrices_3d_t, num_elements       ) ptr
        bestVal                <- (#peek struct cost_matrices_3d_t, cost               ) ptr
        medsVal                <- (#peek struct cost_matrices_3d_t, median             ) ptr
        pure CostMatrix3d
            { alphSize3D            = aSizeVal
            , costMatrixDimension3D = costMatrixDimensionVal
            , gapChar3D             = gapcharVal
            , costModelType3D       = costModelVal
            , include_ambiguities3D = combosVal
            , gapOpenCost3D         = gapOpenVal
            , allElems3D            = elemsVal
            , bestCost3D            = bestVal
            , medians3D             = medsVal
            }

    poke ptr (CostMatrix3d
                  alphSizeVal
                  costMatrixDimensionVal
                  gapCharVal
                  costModelTypeVal
                  include_ambiguitiesVal
                  gapOpenVal
                  elemsVal
                  bestCostVal
                  mediansVal
              ) = do -- to modify values in the C app
        (#poke struct cost_matrices_3d_t, alphSize           ) ptr alphSizeVal
        (#poke struct cost_matrices_3d_t, costMatrixDimension) ptr costMatrixDimensionVal
        (#poke struct cost_matrices_3d_t, gap_char           ) ptr gapCharVal
        (#poke struct cost_matrices_3d_t, cost_model_type    ) ptr costModelTypeVal
        (#poke struct cost_matrices_3d_t, include_ambiguities) ptr include_ambiguitiesVal
        (#poke struct cost_matrices_3d_t, gap_open_cost      ) ptr gapOpenVal
        (#poke struct cost_matrices_3d_t, num_elements       ) ptr elemsVal
        (#poke struct cost_matrices_3d_t, cost               ) ptr bestCostVal
        (#poke struct cost_matrices_3d_t, median             ) ptr mediansVal


instance Enum AlignmentStrategy where

    fromEnum Linear = 0
    fromEnum Affine = 3
    fromEnum Other  = -1

    toEnum 0 = Linear
    toEnum 3 = Affine
    toEnum _ = Other


-- |
-- Create and allocate cost matrix first argument, TCM, is only for non-ambiguous
-- nucleotides, and it used to generate the entire cost matrix, which includes ambiguous elements.
-- TCM is row-major, with each row being the left character element.
-- It is therefore indexed not by powers of two, but by cardinal integer.
foreign import ccall unsafe "c_code_alloc_setup.h setUp2dCostMtx"

    setUpCostMatrix2dFn_c :: Ptr CostMatrix2d
                          -> Ptr CUInt         -- ^ tcm
                          -> CSize             -- ^ alphSize
                          -> CInt              -- ^ gap_open_cost
                          -> IO ()


foreign import ccall unsafe "c_code_alloc_setup.h setUp3dCostMtx"

    setUpCostMatrix3dFn_c :: Ptr CostMatrix3d
                          -> Ptr CUInt         -- ^ tcm
                          -> CSize             -- ^ alphSize
                          -> CInt              -- ^ gap_open_cost
                          -> IO ()


-- TODO: Collapse this definition and defer branching to the C side of the FFI call.
-- |
-- /O(a^5)/ where /a/ is the size of the character alphabet
--
-- Generate the 2D and 3D dense TCM matricies used for FFI calls to
-- 'foreignPairwiseDO' and 'foreignThreeWayDO'.
--
-- Lookup pairwise and threeway medians and cost with calls to
-- 'lookupPairwise' and 'lookupThreeway'.
generateDenseTransitionCostMatrix
  :: Word                   -- ^ The gap open cost. A zero value indicates non-affine alignment context
  -> Word                   -- ^ The character alphabet size
  -> (Word -> Word -> Word) -- ^ The function defining the cost to transition between two symbols
  -> DenseTransitionCostMatrix
generateDenseTransitionCostMatrix affineCost alphabetSize costFunction =
    case affineCost of
      0 -> getCostMatrix2dNonAffine            alphabetSize costFunction
      _ -> getCostMatrix2dAffine    affineCost alphabetSize costFunction


-- |
-- /O(1)/
--
-- Lookup the cost and median of /two/ elements.
--
-- _NOTE: /Only considers the first 8 bits of the elements!/_
lookupPairwise
  :: Bits b
  => DenseTransitionCostMatrix
  -> b
  -> b
  -> (b, Word)
lookupPairwise m e1 e2 = unsafePerformIO $ do
    cm2d <- peek $ costMatrix2D m
    let dim = 1 `shiftL` (fromEnum (alphSize cm2d))
    let off = toByteValue e1 * dim + toByteValue e2
    cost <- peek $ advancePtr (bestCost cm2d) off
    med  <- peek $ advancePtr (medians  cm2d) off
    let val = fromByteValue e1 $ fromEnum med
    pure (val, toEnum $ fromEnum cost)


-- |
-- /O(1)/
--
-- Lookup the cost and median of /three/ elements.
--
-- _NOTE: /Only considers the first 8 bits of the elements!/_
lookupThreeway
  :: Bits b
  => DenseTransitionCostMatrix
  -> b
  -> b
  -> b
  -> (b, Word)
lookupThreeway dtcm e1 e2 e3 = unsafePerformIO $ do
    cm3d <- peek $ costMatrix3D dtcm
    let dim = 1 `shiftL` (fromEnum (alphSize3D cm3d))
    let off = toByteValue e1 * dim * dim + toByteValue e2 * dim + toByteValue e3
    cost <- peek $ advancePtr (bestCost3D cm3d) off
    med  <- peek $ advancePtr ( medians3D cm3d) off
    let val = fromByteValue e1 $ fromEnum med
    pure (val, toEnum $ fromEnum cost)


-- |
-- /O(1)/
--
-- Determine the alignment strategy encoded for the matrix.
getAlignmentStrategy :: CostMatrix2d -> AlignmentStrategy
getAlignmentStrategy = toEnum . fromEnum . costModelType


-- |
-- /O(1)/
--
-- Retreive the first 8 bits of the value.
--
-- Performs 8 individual bit checks.
toByteValue :: Bits b => b -> Int
toByteValue e = f 7
  where
    f !n
      | n >= 0    = v + f (n-1)
      | otherwise = 0
      where
        !v | e `testBit` n = 1 `shiftL` n
           | otherwise     = 0


-- |
-- /O(1)/
--
-- Set the first 8 bits of the value.
--
-- Performs 8 individual bit sets.
fromByteValue :: Bits b => b -> Int -> b
fromByteValue x i = f 7 x
  where
    f !n b
      | n < 0         = b
      | i `testBit` n = f (n-1) $ b   `setBit` n 
      | otherwise     = f (n-1) $ b `clearBit` n
  

-- |
-- Set up and return a non-affine cost matrix
--
-- The cost matrix is allocated strictly.
getCostMatrix2dNonAffine :: Word -> (Word -> Word -> Word) -> DenseTransitionCostMatrix
getCostMatrix2dNonAffine = performMatrixAllocation 0


-- |
-- Set up and return a non-affine cost matrix
--
-- The cost matrix is allocated strictly.
getCostMatrix2dAffine :: Word -> Word -> (Word -> Word -> Word) -> DenseTransitionCostMatrix
getCostMatrix2dAffine = performMatrixAllocation


-- |
-- Perform the allocation of the dense TCM
performMatrixAllocation :: Word -> Word -> (Word -> Word -> Word) -> DenseTransitionCostMatrix
performMatrixAllocation openningCost alphabetSize costFn = unsafePerformIO . withArray rowMajorList $ \allocedTCM -> do
        !ptr2D <- malloc :: IO (Ptr CostMatrix2d)
        !ptr3D <- malloc :: IO (Ptr CostMatrix3d)
        !_ <- setUpCostMatrix2dFn_c ptr2D allocedTCM matrixDimension gapOpen
        !_ <- setUpCostMatrix3dFn_c ptr3D allocedTCM matrixDimension gapOpen
        pure DenseTransitionCostMatrix
             { costMatrix2D = ptr2D
             , costMatrix3D = ptr3D
             }
    where
        matrixDimension = coerceEnum alphabetSize
        gapOpen         = coerceEnum openningCost
        range           = [0 .. alphabetSize - 1]
        -- This *should* be in row major order due to the manner in which list comprehensions are performed.
        rowMajorList    = [ coerceEnum $ costFn i j | i <- range,  j <- range ]


-- |
-- Coercing one 'Enum' to another through their corresponding 'Int' values.
coerceEnum :: (Enum a, Enum b) => a -> b
coerceEnum = toEnum . fromEnum


