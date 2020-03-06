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
{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.FFI
  ( DenseTransitionCostMatrix
  , foreignPairwiseDO
--  , foreignThreeWayDO
  ) where

import Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.Internal
import Bio.Character.Encodable
import Bio.Character.Exportable
import Control.Lens
--import Data.List.NonEmpty   (NonEmpty, fromList)
--import Data.MonoTraversable (Element)
import Data.Semigroup
import Data.TCM.Dense
import Foreign
--import Foreign.Ptr
--import Foreign.C.String
import Foreign.C.Types
--import Foreign.ForeignPtr
--import Foreign.Marshal.Array
--import Foreign.StablePtr
import Prelude   hiding (sequence, tail)
import System.IO.Unsafe (unsafePerformIO)

import Debug.Trace


#include "c_alignment_interface.h"
#include "c_code_alloc_setup.h"
#include "costMatrix.h"
#include "alignmentMatrices.h"


-- |
-- Input/output data type for C alignment code to avoid having to write the
-- whole seq type.
data Align_io
   = Align_io
   { character :: Ptr CUInt
   , charLen   :: CSize     -- Total length of the character stored
   , arrCap    :: CSize     -- Total capacity of allocated array
   }


-- |
-- Specify whether or not to compute median state values
data MedianContext = ComputeMedians | DoNotComputeMedians


-- |
-- Specify whether or not to compute union state values
data UnionContext  = ComputeUnions  | DoNotComputeUnions


-- |
-- Because we're using a struct we need to make a Storable instance
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


{-
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
-}


-- |
-- Align two dynamic characters using an FFI call for more efficient computation
-- on small alphabet sizes.
--
-- Requires a pre-generated 'DenseTransitionCostMatrix' from a call to
-- 'generateDenseTransitionCostMatrix' defining the alphabet and transition costs.
{-# INLINE foreignPairwiseDO #-}
{-# SPECIALISE foreignPairwiseDO :: DynamicCharacter -> DynamicCharacter -> DenseTransitionCostMatrix -> (Word, DynamicCharacter) #-}
foreignPairwiseDO
  :: ( EncodableDynamicCharacter s
     , ExportableElements s
--     , Show s
     )
  => s                         -- ^ First  dynamic character
  -> s                         -- ^ Second dynamic character
  -> DenseTransitionCostMatrix -- ^ Structure defining the transition costs between character states
  -> (Word, s)        -- ^ The /ungapped/ character derived from the the input characters' N-W-esque matrix traceback
foreignPairwiseDO lhs rhs costMatrix = algn2d lhs rhs costMatrix DoNotComputeUnions ComputeMedians


{-
-- |
-- Align three dynamic characters using an FFI call for more efficient computation
-- on small (or smallish) alphabet sizes.
--
-- Requires a pre-generated 'DenseTransitionCostMatrix' from a call to
-- 'generateDenseTransitionCostMatrix' defining the alphabet and transition costs.
foreignThreeWayDO :: ( EncodableDynamicCharacter s
                     , ExportableElements s
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
-}

-- |
-- Performs a naive direct optimization
-- Takes in two characters to run DO on and a metadata object
-- Returns an assignment character, the cost of that assignment, the assignment character with gaps included,
-- the aligned version of the first input character, and the aligned version of the second input character
-- The process for this algorithm is to generate a traversal matrix then perform a traceback.
{-# INLINE algn2d #-}
{-# SPECIALISE algn2d :: DynamicCharacter -> DynamicCharacter -> DenseTransitionCostMatrix -> UnionContext -> MedianContext -> (Word, DynamicCharacter) #-}
algn2d :: ( EncodableDynamicCharacter s
          , ExportableElements s
--          , Show s
          )
       => s                         -- ^ First  dynamic character
       -> s                         -- ^ Second dynamic character
       -> DenseTransitionCostMatrix -- ^ Structure defining the transition costs between character states
       -> UnionContext
       -> MedianContext
       -> (Word, s)        -- ^ The cost of the alignment
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
    case (toExportableElements t char1, toExportableElements t char2) of
      (Just x, Just y) -> f x y
      (     _,      _) -> error "2DO: There's a dynamic character missing!"
  where
    t x y = fst $ lookupPairwise denseTCMs x y
    f exportedChar1 exportedChar2 = unsafePerformIO $ do
--        !_ <- trace ("char 1: " <> show char1) $ pure ()
--        !_ <- trace ("char 2: " <> show char2) $ pure ()
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
--        ungappedChar <- peekArray (fromEnum ungappedLen) ungappedCharArr
        gappedChar   <- reverse <$> peekArray (fromEnum gappedArrLen)   gappedCharArr
        char1Aligned <- reverse <$> peekArray (fromEnum  char1ArrLen) retChar1CharArr
        char2Aligned <- reverse <$> peekArray (fromEnum  char2ArrLen) retChar2CharArr
        -- unionChar    <- peekArray (fromEnum unionLen)    unionCharArr

--        !_ <- trace (" Gapped Char : " <> renderBuffer   gappedChar) $ pure ()
--        !_ <- trace (" Aligned LHS : " <> renderBuffer char1Aligned) $ pure ()
--        !_ <- trace (" Aligned RHS : " <> renderBuffer char2Aligned) $ pure ()
-}

{-
        Align_io char1Ptr' char1Len' buffer1Len' <- peek char1ToSend
        Align_io char2Ptr' char2Len' buffer2Len' <- peek char2ToSend
        output1Buffer <- peekArray (fromEnum buffer1Len') char1Ptr'
        output2Buffer <- peekArray (fromEnum buffer2Len') char2Ptr'
        !_ <- trace (mconcat [" Output LHS : { ", show char1Len', " / ", show buffer1Len', " } ", renderBuffer output1Buffer]) $ pure ()
        !_ <- trace (mconcat [" Output RHS : { ", show char2Len', " / ", show buffer2Len', " } ", renderBuffer output2Buffer]) $ pure ()
-}

        resultingAlignedChar1 <- extractFromElems_io char1ToSend
        resultingAlignedChar2 <- extractFromElems_io char2ToSend
        resultingGapped       <- extractFromElems_io retGapped

{--
        !_ <- trace ("Ungapped Char: " <> show     resultingUngapped) $ pure ()
--}
{-
        !_ <- trace ("\n Len: " <> show (length resultingAlignedChar1)) $ pure ()
        !_ <- trace ("  Gapped Char: " <> show       resultingGapped) $ pure ()
        !_ <- trace (" Aligned LHS : " <> show resultingAlignedChar1) $ pure ()
        !_ <- trace (" Aligned RHS : " <> show resultingAlignedChar2) $ pure ()
-}
--        !_ <- trace  " > Done with FFI Alignment\n" $ pure ()

        -- NOTE: We swapped resultingAlignedChar1 & resultingAlignedChar2
        -- because the C code returns the values in the wrong order!
        let pairedElems    = zip3 resultingGapped resultingAlignedChar2 resultingAlignedChar1
        
        let reimportResult = ReImportableCharacterElements
                             { reimportableElementCountElements = toEnum $ length pairedElems
                             , reimportableElementWidthElements = elemWidth
                             , reimportableCharacterElements    = pairedElems
                             }

        pure (fromIntegral cost, fromExportableElements reimportResult)

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
        -- allocInitAlign_io elemCount elemArr = do
        --       output   <- malloc :: IO (Ptr Align_io)
        --       outArray <- newArray paddedArr
        --       poke output $ Align_io outArray elemCount maxAllocLen
        --       pure output
        --     where
        --       paddedArr = replicate (max 0 (fromEnum (maxAllocLen - elemCount))) 0 <> elemArr
        
        -- Used for debugging
{-
      renderBuffer buf = "[" <> intercalate "," (fmap pad shownElems) <> "]"
        where
          maxElemChars = maximum $ fmap length shownElems
          shownElems   = fmap show buf
          pad e        = replicate (maxElemChars - length e) ' ' <> e
-}


{-
-- |
-- Performs a naive direct optimization
-- Takes in three characters to run DO on and a metadata object.
-- Returns an assignment character, the cost of that assignment, the assignment character with gaps included,
-- the aligned versions of the three input characters.
-- The process for this algorithm is to generate a traversal matrix, then perform a traceback.
algn3d
  :: ( EncodableDynamicCharacter s
     , ExportableElements s
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
    f exportedChar1 exportedChar2 exportedChar3 = unsafePerformIO $ do
--        !_ <- trace ("char 1: " <> show char1) $ pure ()
--        !_ <- trace ("char 2: " <> show char2) $ pure ()
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
-}


{- Generic helper functions -}


-- |
-- Allocates space for an align_io struct to be sent to C.
allocInitAlign_io :: CSize -> CSize -> [CUInt] -> IO (Ptr Align_io)
allocInitAlign_io maxAllocLen elemCount elemArr  = do
    output   <- malloc :: IO (Ptr Align_io)
    outArray <- newArray paddedArr
    poke output $ Align_io outArray elemCount maxAllocLen
    pure output
  where
    paddedArr = replicate (max 0 (fromEnum (maxAllocLen - elemCount))) 0 <> elemArr


{-
-- |
-- Converts the data behind an 'Align_io' pointer to an 'Exportable' type.
{-# INLINE extractFromAlign_io #-}
{-# SPECIALISE extractFromAlign_io :: Word -> Ptr Align_io -> IO DynamicCharacter #-}
extractFromAlign_io :: ExportableElements s => Word -> Ptr Align_io -> IO s
extractFromAlign_io elemWidth ptr = do
    Align_io bufferPtr charLenC bufferLenC <- peek ptr
    let    charLength = fromEnum   charLenC
    let  bufferLength = fromEnum bufferLenC
    buffer <- peekArray bufferLength bufferPtr
    let !charElems = drop (bufferLength - charLength) buffer
    let  exportVal = ReImportableCharacterElements (toEnum charLength) elemWidth charElems
    _ <- free bufferPtr
    _ <- free ptr
    pure $ fromExportableElements exportVal
-}


-- |
-- Converts the data behind an 'Align_io' pointer to an 'Exportable' type.
extractFromElems_io :: Ptr Align_io -> IO [CUInt]
extractFromElems_io ptr = do
    Align_io bufferPtr charLenC bufferLenC <- peek ptr
    let    charLength = fromEnum   charLenC
    let  bufferLength = fromEnum bufferLenC
    buffer <- peekArray bufferLength bufferPtr
    let !charElems = drop (bufferLength - charLength) buffer
    _ <- free bufferPtr
    _ <- free ptr
    pure charElems


-- |
-- Coercing one 'Enum' to another through their corresponding 'Int' values.
{-# INLINE coerceEnum #-}
{-# SPECIALISE coerceEnum :: Word  -> Int   #-}
{-# SPECIALISE coerceEnum :: Int   -> Word  #-}
{-# SPECIALISE coerceEnum :: Int   -> CUInt #-}
{-# SPECIALISE coerceEnum :: CUInt -> Int   #-}
{-# SPECIALISE coerceEnum :: Word  -> CUInt #-}
{-# SPECIALISE coerceEnum :: CUInt -> Word  #-}
coerceEnum :: (Enum a, Enum b) => a -> b
coerceEnum = toEnum . fromEnum

