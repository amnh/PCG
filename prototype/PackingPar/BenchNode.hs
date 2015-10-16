{-# LANGUAGE BangPatterns #-}
module BenchNode where

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
import Packing.BitPackedNode

main :: IO()
main = do
    benches <- sequence [ benchmarkOps "../data-sets/28S_trimal.fas" "../data-sets/sp_rand_28s.tre" "(Medium Fasta: )"]
    defaultMain [ bgroup "fitch opts" (concat benches) ]

benchmarkOps :: String -> String -> String -> IO [Benchmark]
benchmarkOps seqsFile treeFile prefix = do
    !seqs <- getSeqsFromFile seqsFile
    !tree <- getTreeFromFile treeFile
    let !names  = --trace ("names " ++ show (head $  filter (not.null) . fmap nodeName <$> fmap V.toList tree))
                    head $ filter (not.null) . fmap nodeName <$> fmap V.toList tree
    let !(packed, info, mode) = performPack seqs names tree ("adaptive", "16")
    let !myTree = V.head packed
    let !pareTree = [x | x <- (V.toList myTree), x /= EmptyPackNode]
    pure [bench (prefix++" GPU and") $ nf (GN..&. (pareTree !! 0)) (pareTree !! 1)
    	  , bench (prefix++" CPU and") $ nf (CN..&. (pareTree !! 0)) (pareTree !! 1)
    	  , bench (prefix++" normal and") $ nf (.&. (pareTree !! 0)) (pareTree !! 1)
    	  , bench (prefix++" GPU or") $ nf (GN..|. (pareTree !! 0)) (pareTree !! 1)
    	  , bench (prefix++" CPU or") $ nf (CN..&. (pareTree !! 0)) (pareTree !! 1)
    	  , bench (prefix++" normal or") $ nf (.|. (pareTree !! 0)) (pareTree !! 1)]