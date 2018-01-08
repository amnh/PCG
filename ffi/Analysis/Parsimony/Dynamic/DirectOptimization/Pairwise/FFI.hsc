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

module Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.FFI
  ( CostMatrix2d
  , DenseTransitionCostMatrix
  , foreignPairwiseDO
  , foreignThreeWayDO
  , generateDenseTransitionCostMatrix
  ) where

import Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.Internal (filterGaps, handleMissingCharacter, handleMissingCharacterThreeway)
import Bio.Character.Encodable
import Bio.Character.Exportable.Class
import Control.DeepSeq
import Control.Lens
--import Data.Foldable
--import Data.List        (intercalate)
--import Data.MonoTraversable
import Data.Semigroup
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
-- Input/output data type for C alignment code to avoid having to write the
-- whole seq type.
data Align_io
   = Align_io
   { character :: Ptr CUInt
   , charLen   :: CSize     -- Total length of the character stored
   , arrCap    :: CSize     -- Total capacity of allocated array
   }


data AlignmentStrategy = Linear | Affine | Other deriving (Eq, Show)


-- |
-- Holds single cost matrix, which contains costs and medians for all possible
-- character elements. It is completely filled using a TCM.
--
-- See note below at 'setupCostMatrixFn_c'.
data CostMatrix2d
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
   } deriving (Eq, Generic)


data CostMatrix3d
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
   } deriving (Eq, Generic)


-- TODO: StablePtr here maybe?
-- |
-- Exposed wrapper for C allocated cost matrix structs.
data DenseTransitionCostMatrix
   = DenseTransitionCostMatrix
   { costMatrix2D :: Ptr CostMatrix2d
   , costMatrix3D :: Ptr CostMatrix3d
   } deriving (Generic)


data MedianContext = ComputeMedians | DoNotComputeMedians


data UnionContext  = ComputeUnions  | DoNotComputeUnions


-- | Because we're using a struct we need to make a Storable instance
instance Storable Align_io where

    sizeOf    _  = (#size struct alignIO_t)

    alignment _  = alignment (undefined :: CSize)

    peek ptr     = do
        arr  <- (#peek struct alignIO_t, character) ptr
        len  <- (#peek struct alignIO_t, length)    ptr
        cap  <- (#peek struct alignIO_t, capacity)  ptr

        pure Align_io
            { character = arr
            , charLen   = len
            , arrCap    = cap
            }

    poke ptr (Align_io arr len cap) = do
        (#poke struct alignIO_t, character) ptr arr
        (#poke struct alignIO_t, length)    ptr len
        (#poke struct alignIO_t, capacity)  ptr cap


instance NFData CostMatrix2d


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


instance NFData CostMatrix3d


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


instance NFData DenseTransitionCostMatrix


instance Enum AlignmentStrategy where

    fromEnum Linear = 0
    fromEnum Affine = 3
    fromEnum Other  = -1

    toEnum 0 = Linear
    toEnum 3 = Affine
    toEnum _ = Other


instance Enum MedianContext where

    fromEnum      ComputeMedians = 1
    fromEnum DoNotComputeMedians = 0

    toEnum 0 = DoNotComputeMedians
    toEnum _ =      ComputeMedians


instance Enum UnionContext where

    fromEnum      ComputeUnions = 1
    fromEnum DoNotComputeUnions = 0

    toEnum 0 = DoNotComputeUnions
    toEnum _ =      ComputeUnions


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


foreign import ccall unsafe "c_alignment_interface.h align2d"

    align2dFn_c :: Ptr Align_io -- ^ character1, input & output
                -> Ptr Align_io -- ^ character2, input & output
                -> Ptr Align_io -- ^ gapped median output
                -> Ptr Align_io -- ^ ungapped median output
                -> Ptr CostMatrix2d
                -> CInt        -- ^ compute ungapped & not   gapped medians
                -> CInt        -- ^ compute   gapped & not ungapped medians
                -> CInt        -- ^ compute union
                -> CInt        -- ^ cost


foreign import ccall unsafe "c_alignment_interface.h align2dAffine"

    align2dAffineFn_c :: Ptr Align_io -- ^ character1, input & output
                      -> Ptr Align_io -- ^ character2, input & output
                      -> Ptr Align_io -- ^ gapped median output
                      -> Ptr Align_io -- ^ ungapped median output
                      -> Ptr CostMatrix2d
                      -> CInt        -- ^ compute medians
                      -> CInt        -- ^ cost


-- | Create and allocate cost matrix
-- first argument, TCM, is only for non-ambiguous nucleotides, and it used to generate
-- the entire cost matrix, which includes ambiguous elements.
-- TCM is row-major, with each row being the left character element.
-- It is therefore indexed not by powers of two, but by cardinal integer.
foreign import ccall unsafe "c_alignment_interface.h align3d"

    align3dFn_c :: Ptr Align_io -- ^ character1, input
                -> Ptr Align_io -- ^ character2, input
                -> Ptr Align_io -- ^ character3, input
                -> Ptr Align_io -- ^ character1, output
                -> Ptr Align_io -- ^ character2, output
                -> Ptr Align_io -- ^ character3, output
                -> Ptr Align_io -- ^ gapped median output
                -> Ptr Align_io -- ^ ungapped median output
                -> Ptr CostMatrix3d
                -> CInt        -- ^ substitution cost
                -> CInt        -- ^ gap open cost
                -> CInt        -- ^ indel cost
                -> CInt        -- ^ alignment cost


{- Exported Functions -}


-- TODO: Collapse this definition and defer branching to the C side of the FFI call.

-- |
-- Generate the 2D and 3D dense TCM matricies used for FFI calls to
-- 'foreignPairwiseDO' and 'foreignThreeWayDO'.
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
-- Align two dynamic characters using an FFI call for more efficient computation
-- on small alphabet sizes.
--
-- Requires a pre-generated 'DenseTransitionCostMatrix' from a call to
-- 'generateDenseTransitionCostMatrix' defining the alphabet and transition costs.
foreignPairwiseDO :: ( EncodableDynamicCharacter s
                     , Exportable s
--                     , Show s
                     )
                  => s                         -- ^ First  dynamic character
                  -> s                         -- ^ Second dynamic character
                  -> DenseTransitionCostMatrix -- ^ Structure defining the transition costs between character states
                  -> (Word, s, s, s, s)        -- ^ The /ungapped/ character derived from the the input characters' N-W-esque matrix traceback
foreignPairwiseDO lhs rhs costMatrix = algn2d lhs rhs costMatrix DoNotComputeUnions ComputeMedians


-- |
-- Align three dynamic characters using an FFI call for more efficient computation
-- on small (or smallish) alphabet sizes.
--
-- Requires a pre-generated 'DenseTransitionCostMatrix' from a call to
-- 'generateDenseTransitionCostMatrix' defining the alphabet and transition costs.
foreignThreeWayDO :: ( EncodableDynamicCharacter s
                     , Exportable s
--                     , Show s
                     )
                  => s                         -- ^ First  dynamic character
                  -> s                         -- ^ Second dynamic character
                  -> s                         -- ^ Third  dynamic character
                  -> Int                       -- ^ Mismatch cost
                  -> Int                       -- ^ Gap open cost
                  -> Int                       -- ^ Indel cost
                  -> DenseTransitionCostMatrix -- ^ Structure defining the transition costs between character states
                  -> (Word, s, s, s, s, s)     -- ^ The /ungapped/ character derived from the the input characters' N-W-esque matrix traceback
foreignThreeWayDO char1 char2 char3 costMatrix = algn3d char1 char2 char3 costMatrix


{- Matrix allocation functionality -}


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


{- Alignment functionality -}


-- |
-- Performs a naive direct optimization
-- Takes in two characters to run DO on and a metadata object
-- Returns an assignment character, the cost of that assignment, the assignment character with gaps included,
-- the aligned version of the first input character, and the aligned version of the second input character
-- The process for this algorithm is to generate a traversal matrix then perform a traceback.
algn2d :: ( EncodableDynamicCharacter s
          , Exportable s
--          , Show s
          )
       => s                         -- ^ First  dynamic character
       -> s                         -- ^ Second dynamic character
       -> DenseTransitionCostMatrix -- ^ Structure defining the transition costs between character states
       -> UnionContext
       -> MedianContext
       -> (Word, s, s, s, s)        -- ^ The cost of the alignment
                                    --
                                    --   The /ungapped/ character derived from the the input characters' N-W-esque matrix traceback
                                    --
                                    --   The /gapped/ character derived from the the input characters' N-W-esque matrix traceback
                                    --
                                    --   The gapped alignment of the /first/ input character when aligned with the second character
                                    --
                                    --   The gapped alignment of the /second/ input character when aligned with the first character
                                    --
algn2d char1 char2 denseTCMs computeUnion computeMedians = handleMissingCharacter char1 char2 $
    case (toExportableElements char1, toExportableElements char2) of
      (Just x, Just y) -> f x y
      (     _,      _) -> error "2DO: There's a dynamic character missing!"
    where
        f exportedChar1 exportedChar2 = unsafePerformIO $
            do
--                !_ <- trace ("char 1: " <> show char1) $ pure ()
--                !_ <- trace ("char 2: " <> show char2) $ pure ()
                char1ToSend <- allocInitAlign_io maxAllocLen exportedChar1Len . fmap coerceEnum $ exportedCharacterElements exportedChar1
                char2ToSend <- allocInitAlign_io maxAllocLen exportedChar2Len . fmap coerceEnum $ exportedCharacterElements exportedChar2
                retGapped   <- allocInitAlign_io maxAllocLen 0 []
                retUngapped <- allocInitAlign_io maxAllocLen 0 []
                -- retUnion    <- allocInitALignIO 0 []

{--
                Align_io char1Ptr char1Len buffer1Len <- peek char1ToSend
                Align_io char2Ptr char2Len buffer2Len <- peek char2ToSend
                input1CharArr <- peekArray (fromEnum buffer1Len) char1Ptr
                input2CharArr <- peekArray (fromEnum buffer2Len) char2Ptr
                !_ <- trace (mconcat [" Input LHS : { ", show char1Len, " / ", show buffer1Len, " } ", renderBuffer input1CharArr]) $ pure ()
                !_ <- trace (mconcat [" Input RHS : { ", show char2Len, " / ", show buffer2Len, " } ", renderBuffer input2CharArr]) $ pure ()
--}

                strategy <- getAlignmentStrategy <$> peek costStruct
                let !cost = case strategy of
                              Affine -> align2dAffineFn_c char1ToSend char2ToSend retGapped retUngapped costStruct                        (coerceEnum computeMedians)
                              _      -> align2dFn_c       char1ToSend char2ToSend retGapped retUngapped costStruct neverComputeOnlyGapped (coerceEnum computeMedians) (coerceEnum computeUnion)

{-
                Align_io ungappedCharArr ungappedLen _ <- peek retUngapped
                Align_io gappedCharArr   gappedLen   _ <- peek retGapped
                Align_io retChar1CharArr char1Len    _ <- peek char1ToSend
                Align_io retChar2CharArr char2Len    _ <- peek char2ToSend
                -- Align_io unionCharArr    unionLen    _ <- peek retUnion

                -- A sanity check to ensure that the sequences were aligned
                _ <- if gappedLen == char1Len && gappedLen == char2Len
                     then pure ()
                     else error $ unlines
                         [ "Sequences returned from POY C code were not actually \"aligned.\""
                         , "gappedLen = " <> show gappedLen
                         , " char1Len = " <> show char1Len
                         , " char2Len = " <> show char2Len
                         ]
--                ungappedChar <- peekArray (fromEnum ungappedLen) ungappedCharArr
                gappedChar   <- reverse <$> peekArray (fromEnum gappedArrLen)   gappedCharArr
                char1Aligned <- reverse <$> peekArray (fromEnum  char1ArrLen) retChar1CharArr
                char2Aligned <- reverse <$> peekArray (fromEnum  char2ArrLen) retChar2CharArr
                -- unionChar    <- peekArray (fromEnum unionLen)    unionCharArr

--                !_ <- trace (" Gapped Char : " <> renderBuffer   gappedChar) $ pure ()
--                !_ <- trace (" Aligned LHS : " <> renderBuffer char1Aligned) $ pure ()
--                !_ <- trace (" Aligned RHS : " <> renderBuffer char2Aligned) $ pure ()
-}

{-
                Align_io char1Ptr' char1Len' buffer1Len' <- peek char1ToSend
                Align_io char2Ptr' char2Len' buffer2Len' <- peek char2ToSend
                output1Buffer <- peekArray (fromEnum buffer1Len') char1Ptr'
                output2Buffer <- peekArray (fromEnum buffer2Len') char2Ptr'
                !_ <- trace (mconcat [" Output LHS : { ", show char1Len', " / ", show buffer1Len', " } ", renderBuffer output1Buffer]) $ pure ()
                !_ <- trace (mconcat [" Output RHS : { ", show char2Len', " / ", show buffer2Len', " } ", renderBuffer output2Buffer]) $ pure ()
-}

                resultingAlignedChar1 <- extractFromAlign_io elemWidth char1ToSend
                resultingAlignedChar2 <- extractFromAlign_io elemWidth char2ToSend
                resultingGapped       <- extractFromAlign_io elemWidth retGapped
                let resultingUngapped = filterGaps resultingGapped

{--
                !_ <- trace ("Ungapped Char: " <> show     resultingUngapped) $ pure ()
                !_ <- trace ("  Gapped Char: " <> show       resultingGapped) $ pure ()
                !_ <- trace (" Aligned LHS : " <> show resultingAlignedChar1) $ pure ()
                !_ <- trace (" Aligned RHS : " <> show resultingAlignedChar2) $ pure ()
--}
--                !_ <- trace  " > Done with FFI Alignment\n" $ pure ()

                -- NOTE: We swapped resultingAlignedChar1 & resultingAlignedChar2
                -- because the C code returns the values in the wrong order!
                pure (fromIntegral cost, resultingUngapped, resultingGapped, resultingAlignedChar2, resultingAlignedChar1)

            where
                costStruct = costMatrix2D denseTCMs
                neverComputeOnlyGapped = 0

                elemWidth        = exportedChar1 ^. exportedElementWidth

                exportedChar1Len = coerceEnum $ exportedChar1 ^. exportedElementCount
                exportedChar2Len = coerceEnum $ exportedChar2 ^. exportedElementCount
                -- Add two because the C code needs stupid gap prepended to each character.
                -- Forgetting to do this will eventually corrupt the heap memory
                maxAllocLen      = exportedChar1Len + exportedChar2Len + 2

                -- allocInitAlign_io :: CSize -> [CUInt] -> IO (Ptr Align_io)
                -- allocInitAlign_io elemCount elemArr  =
                --     do
                --         output   <- malloc :: IO (Ptr Align_io)
                --         outArray <- newArray paddedArr
                --         poke output $ Align_io outArray elemCount maxAllocLen
                --         pure output
                --     where
                --         paddedArr = replicate (max 0 (fromEnum (maxAllocLen - elemCount))) 0 <> elemArr

                -- Used for debugging
{-
                renderBuffer buf = "[" <> intercalate "," (fmap pad shownElems) <> "]"
                  where
                    maxElemChars = maximum $ fmap length shownElems
                    shownElems   = fmap show buf
                    pad e        = replicate (maxElemChars - length e) ' ' <> e
-}

-- |
-- Performs a naive direct optimization
-- Takes in three characters to run DO on and a metadata object.
-- Returns an assignment character, the cost of that assignment, the assignment character with gaps included,
-- the aligned versions of the three input characters.
-- The process for this algorithm is to generate a traversal matrix, then perform a traceback.
algn3d :: ( EncodableDynamicCharacter s
          , Exportable s
          )
       => s                         -- ^ First  dynamic character
       -> s                         -- ^ Second dynamic character
       -> s                         -- ^ Third  dynamic character
       -> Int                       -- ^ Mismatch cost
       -> Int                       -- ^ Gap open cost
       -> Int                       -- ^ Indel cost
       -> DenseTransitionCostMatrix -- ^ Structure defining the transition costs between character states
       -> (Word, s, s, s, s, s)     -- ^ The cost of the alignment
                                    --
                                    --   The /ungapped/ character derived from the the input characters' N-W-esque matrix traceback
                                    --
                                    --   The /gapped/ character derived from the the input characters' N-W-esque matrix traceback
                                    --
                                    --   The gapped alignment of the /first/ input character when aligned with the second & third character
                                    --
                                    --   The gapped alignment of the /second/ input character when aligned with the first & third character
                                    --
                                    --   The gapped alignment of the /third/ input character when aligned with the first & second character
                                    --
algn3d char1 char2 char3 mismatchCost openningGapCost indelCost denseTCMs = handleMissingCharacterThreeway someFun char1 char2 char3 $
    case (toExportableElements char1, toExportableElements char2, toExportableElements char3) of
      (Just x, Just y, Just z) -> f x y z
      (     _,      _,      _) -> error "3DO: There's a dynamic character missing!"
    where
        someFun = undefined
        f exportedChar1 exportedChar2 exportedChar3 = unsafePerformIO $
            do
--                !_ <- trace ("char 1: " <> show char1) $ pure ()
--                !_ <- trace ("char 2: " <> show char2) $ pure ()
                char1ToSend <- allocInitAlign_io maxAllocLen exportedChar1Len . fmap coerceEnum $ exportedCharacterElements exportedChar1
                char2ToSend <- allocInitAlign_io maxAllocLen exportedChar2Len . fmap coerceEnum $ exportedCharacterElements exportedChar2
                char3ToSend <- allocInitAlign_io maxAllocLen exportedChar3Len . fmap coerceEnum $ exportedCharacterElements exportedChar3
                char1Return <- allocInitAlign_io maxAllocLen 0 []    -- Note that the next six can be empty as their C-side
                char2Return <- allocInitAlign_io maxAllocLen 0 []    -- internal arrays are alloc'ed
                char3Return <- allocInitAlign_io maxAllocLen 0 []
                retGapped   <- allocInitAlign_io maxAllocLen 0 []
                retUngapped <- allocInitAlign_io maxAllocLen 0 []
                -- retUnion    <- allocInitALignIO 0 []

                let !cost = align3dFn_c char1ToSend char2ToSend char3ToSend
                                        char1Return char2Return char3Return
                                        retGapped   retUngapped
                                        costStruct
                                        (coerceEnum mismatchCost)
                                        (coerceEnum openningGapCost)
                                        (coerceEnum indelCost)

                resultingAlignedChar1 <- extractFromAlign_io elemWidth char1Return
                resultingAlignedChar2 <- extractFromAlign_io elemWidth char2Return
                resultingAlignedChar3 <- extractFromAlign_io elemWidth char3Return
                resultingGapped       <- extractFromAlign_io elemWidth retGapped
                resultingUngapped     <- extractFromAlign_io elemWidth retUngapped

                pure ( fromIntegral cost
                     , resultingUngapped
                     , resultingGapped
                     , resultingAlignedChar1
                     , resultingAlignedChar2
                     , resultingAlignedChar3
                     )

            where
                costStruct       = costMatrix3D denseTCMs -- TODO: get memoized matrix wedged in here

                elemWidth        = exportedChar1 ^. exportedElementWidth

                exportedChar1Len = coerceEnum $ exportedChar1 ^. exportedElementCount
                exportedChar2Len = coerceEnum $ exportedChar2 ^. exportedElementCount
                exportedChar3Len = coerceEnum $ exportedChar3 ^. exportedElementCount

                maxAllocLen      = exportedChar1Len + exportedChar2Len + exportedChar3Len


-- |
-- Allocates space for an align_io struct to be sent to C.
allocInitAlign_io :: CSize -> CSize -> [CUInt] -> IO (Ptr Align_io)
allocInitAlign_io maxAllocLen elemCount elemArr  =
    do
        output   <- malloc :: IO (Ptr Align_io)
        outArray <- newArray paddedArr
        poke output $ Align_io outArray elemCount maxAllocLen
        pure output
    where
        paddedArr = replicate (max 0 (fromEnum (maxAllocLen - elemCount))) 0 <> elemArr

{-
-- | A C binding that computes only the cost of a 2d alignment
align2dCostOnly
  :: ( EncodableDynamicCharacter s
     , Exportable s
     , Show s
     )
  => s
  -> s
  -> DenseTransitionCostMatrix
  -> (Word, s, s, s, s)
align2dCostOnly c1 c2 cm = algn2d c1 c2 cm DoNotComputeUnions DoNotComputeMedians


-- | A C binding that aligns two DO characters and returns the cost and the ungapped median sequence
align2dGetUngapped
  :: ( EncodableDynamicCharacter s
     , Exportable s
     , Show s
     )
  => s
  -> s
  -> DenseTransitionCostMatrix
  -> (Word, s, s, s, s)
align2dGetUngapped c1 c2 cm = algn2d c1 c2 cm DoNotComputeUnions ComputeMedians


-- | A C binding that aligns two DO characters and returns the cost and the union median
align2dGetUnion
  :: ( EncodableDynamicCharacter s
     , Exportable s
     , Show s
     )
  => s
  -> s
  -> DenseTransitionCostMatrix
  -> (Word, s, s, s, s)
align2dGetUnion c1 c2 cm = algn2d c1 c2 cm ComputeUnions DoNotComputeMedians


-- | A C binding that aligns two DO characters and returns the cost and the gapped and ungapped median sequences
align2dGappedUngapped
  :: ( EncodableDynamicCharacter s
     , Exportable s
     , Show s
     )
  => s
  -> s
  -> DenseTransitionCostMatrix
  -> (Word, s, s, s, s)
align2dGappedUngapped c1 c2 cm = algn2d c1 c2 cm ComputeUnions ComputeMedians
-}


{- Generic helper functions -}


-- |
-- Coercing one 'Enum' to another through their corresponding 'Int' values.
coerceEnum :: (Enum a, Enum b) => a -> b
coerceEnum = toEnum . fromEnum


-- |
-- Converts the data behind an 'Align_io' pointer to an 'Exportable' type.
extractFromAlign_io :: Exportable s => Word -> Ptr Align_io -> IO s
extractFromAlign_io elemWidth ptr = do
    Align_io bufferPtr charLenC bufferLenC <- peek ptr
    let    charLength = fromEnum   charLenC
    let  bufferLength = fromEnum bufferLenC
    buffer <- peekArray bufferLength bufferPtr
    let !charElems = drop (bufferLength - charLength) buffer
    let  exportVal = ExportableCharacterElements (toEnum charLength) elemWidth charElems
    _ <- free bufferPtr
    _ <- free ptr
    pure $ fromExportableElements exportVal


-- |
-- Determine the alignment strategy encoded for the matrix.
getAlignmentStrategy :: CostMatrix2d -> AlignmentStrategy
getAlignmentStrategy = toEnum . fromEnum . costModelType
