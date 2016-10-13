module SimpleGPUTest where

import           Data.Array.Accelerate      as A
-- To run on CUDA GPUs, replace Data.Array.Accelerate.Interpreter with Data.Array.Accelerate.CUDA
-- import qualified Data.Array.Accelerate.Interpreter as AI
import qualified Data.Array.Accelerate.CUDA as AI
-- To perform Bitwise operations, use accelerate's custom Bits type-class which parody's the core type-class
import           Data.Array.Accelerate.Data.Bits
import qualified Data.Vector                as V
import           Data.Word

type SamplePacked = V.Vector Word16

simpleTest :: SamplePacked -> SamplePacked -> SamplePacked
simpleTest bit1 bit2 = V.fromList $ A.toList result
  where
    shape  = A.Z :. V.length bit1
    array1 = A.fromList shape $ V.toList bit1 :: A.Vector A.Word16 
    array2 = A.fromList shape $ V.toList bit2 :: A.Vector A.Word16
    result = AI.run $ A.zipWith xor (A.use array1) (A.use array2)

arr = A.fromList (A.Z :. 3 :. 5) [1..] :: Array DIM2 Int

-- test1 should return [2, 0, 4, 2, 2] (possibly, not sure)
test1    = simpleTest (V.fromList [2, 3, 4, 2, 6]) (V.fromList [2, 4, 6, 2, 3])
test2    = AI.run $ A.map (+1) (A.use arr)
bookTest = AI.run $ A.map (+1) (use (fromList (Z:.3:.5) [1..] :: Array DIM2 Int))
