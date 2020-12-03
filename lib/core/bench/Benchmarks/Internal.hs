{-# LANGUAGE BlockArguments #-}
module Benchmarks.Internal where

import           Control.DeepSeq
import qualified Criterion.Main  as C (bench, bgroup, defaultMain, nf)
import           Data.Foldable
import qualified Weigh           as W

timeAndWeigh
  :: (NFData b)
  => String -- groupName
  -> [(String, a -> b)] -- name of benchmark and function
  -> a -- input
  -> IO ()
timeAndWeigh groupName functions testInput = do
    let benchInputs =
          [C.bench funName $ C.nf fun testInput | (funName, fun) <- functions]

    let weighInputs =
          traverse_
            (\(funName, fun) -> W.func funName fun testInput)
            functions
    C.defaultMain
       [ C.bgroup groupName benchInputs]
    W.mainWith
      do
        W.setColumns [W.Case, W.Allocated, W.GCs, W.Max]
        weighInputs
