{-# LANGUAGE BangPatterns #-}
module BenchNode where

import Criterion.Main
import qualified Data.Vector as V
import Debug.Trace           (trace)
import Component
import CharacterData
import Packing.UnpackedBuild
import Packing.PackedTest
import qualified PackingPar.GPUNode as GN
import qualified PackingPar.CPUNode as CN
import Packing.PackedBuild
import Packing.BitPackedNode
import qualified PackingPar.ParEnabledNode2 as PN
import qualified PackingPar.PackedBuild as PB
import Data.Word

main :: IO()
main = do
    benches <- sequence [ benchmarkOps "../data-sets/28S_trimal.fas" "../data-sets/sp_rand_28s.tre" "(Medium Fasta: )"
                          , benchmarkUnifiedOps "../data-sets/28S_trimal.fas" "../data-sets/sp_rand_28s.tre" "(Medium Fasta: )"]
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

benchmarkUnifiedOps :: String -> String -> String -> IO [Benchmark]
benchmarkUnifiedOps seqsFile treeFile prefix = do
    !seqs <- getSeqsFromFile seqsFile
    !tree <- getTreeFromFile treeFile
    let !names  = head $ filter (not.null) . fmap nodeName <$> fmap V.toList tree
    let !(packedG, info, mode) = PB.performPack (0 :: Word16) seqs names tree ("adaptive", "16") PN.GPU
    let !(packedC, info, mode) = PB.performPack (0 :: Word16) seqs names tree ("adaptive", "16") PN.CPU
    let !(packedN, info, mode) = PB.performPack (0 :: Word16) seqs names tree ("adaptive", "16") PN.Normal
    let !myTreeG = V.head packedG
    let !pareTreeG = [x | x <- (V.toList myTreeG), x /= PN.EmptyPackNode]
    let !myTreeC = V.head packedC
    let !pareTreeC = [x | x <- (V.toList myTreeC), x /= PN.EmptyPackNode]
    let !myTreeN = V.head packedN
    let !pareTreeN = [x | x <- (V.toList myTreeN), x /= PN.EmptyPackNode]
    pure [bench (prefix++" GPU and, ParNode") $ nf (PN..&. (pareTreeG !! 0)) (pareTreeG !! 1)
          , bench (prefix++" CPU and, ParNode") $ nf (PN..&. (pareTreeC !! 0)) (pareTreeC !! 1)
          , bench (prefix++" normal and, ParNode") $ nf (PN..&. (pareTreeN !! 0)) (pareTreeN !! 1)
          , bench (prefix++" GPU or, ParNode") $ nf (PN..|. (pareTreeG !! 0)) (pareTreeG !! 1)
          , bench (prefix++" CPU or, ParNode") $ nf (PN..&. (pareTreeC !! 0)) (pareTreeC !! 1)
          , bench (prefix++" normal or, ParNode") $ nf (PN..|. (pareTreeN !! 0)) (pareTreeN !! 1)]