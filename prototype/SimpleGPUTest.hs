module SimpleGPUTest where

import qualified Data.Array.Accelerate as A
-- to run on CUDA GPUs, replace Data.Array.Accelerate.Interpreter with Data.Array.Accelerate.CUDA
import qualified Data.Array.Accelerate.Interpreter as AI
import qualified Data.Vector as V
import Data.Word
import Data.Bits

type SamplePacked = V.Vector Word16

simpleTest :: SamplePacked -> SamplePacked -> SamplePacked
simpleTest bit1 bit2 = 
    let
        shape = A.Z A.:. (V.length bit1)
        array1 = A.fromList shape (V.toList bit1) :: A.Vector A.Word16 
        array2 = A.fromList shape (V.toList bit2) :: A.Vector A.Word16
        a     | (V.length bit1) /= (V.length bit2) = error "Attempt to take and of bits of different lengths"
            | otherwise = AI.run $ A.zipWith (\b1 b2 ->b1 .&. b2) (A.use array1) (A.use array2)
    in V.fromList $ A.toList a

-- test1 should return [2, 0, 4, 2, 2] (possibly, not sure)
test1 = simpleTest (V.fromList [2, 3, 4, 2, 6]) (V.fromList [2, 4, 6, 2, 3])