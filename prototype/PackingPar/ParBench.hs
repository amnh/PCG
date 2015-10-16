{-# LANGUAGE BangPatterns #-}
module ParBench where

import Criterion.Main
import qualified Data.Vector as V
import Debug.Trace           (trace)
import Component
import CharacterData
import Packing.UnpackedBuild
import Packing.PackedTest
import qualified GPUNode as GN
import qualified CPUNode as CN
import Packing.PackedBuild
import Packing.PackedOptimize
import qualified PackedOptimize as POpt
import Packing.BitPackedNode

main :: IO()
main = do
    benches <- sequence [ benchmarkFitches "../data-sets/28S_trimal.fas" "../data-sets/sp_rand_28s.tre" "(Medium Fasta: )"]
    defaultMain [ bgroup "fitch opts" (concat benches) ]

benchmarkFitches :: String -> String -> String -> IO [Benchmark]
benchmarkFitches seqsFile treeFile prefix = do
	!seqs <- getSeqsFromFile seqsFile
    !tree <- getTreeFromFile treeFile
    let !names  = --trace ("names " ++ show (head $  filter (not.null) . fmap nodeName <$> fmap V.toList tree))
                    head $ filter (not.null) . fmap nodeName <$> fmap V.toList tree
    let !pack = performPack seqs names tree ("adaptive", "16")
    pure [bench (prefix++" parallelized Fitch")   $ nf (POpt.costForest tree (performPack seqs names tree ("adaptive","16"      ))) weight]