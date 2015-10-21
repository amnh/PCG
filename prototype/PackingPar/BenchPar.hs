{-# LANGUAGE BangPatterns #-}
module BenchPar where

import Criterion.Main
import qualified Data.Vector as V
import Debug.Trace           (trace)
import Component
import CharacterData
import Packing.UnpackedBuild
import Packing.PackedTest
import Packing.PackedBuild
import Packing.BitPackedNode
import Packing.PackedOptimize
import qualified PackingPar.ParEnabledNode2 as PN
import qualified PackingPar.PackedBuild as PB
import Data.Word
import qualified PackingPar.PackedParOptimize as PO

main :: IO ()
main = do
    benches <- sequence 
               [ benchmarkFitchOptimization "data-sets/28S_trimal.fas" "data-sets/sp_rand_28s.tre" "(Medium Fasta: )"
                -- , benchmarkPackingOnly "data-sets/28S_trimal.fas" "data-sets/sp_rand_28s.tre" "(Medium Fasta: )"
                -- , benchmarkFitchOnly "data-sets/28S_trimal.fas" "data-sets/sp_rand_28s.tre" "(Medium Fasta: )"
               ]
    defaultMain [ bgroup "fitch opts" (concat benches) ]

benchmarkFitchOptimization :: FilePath -> FilePath -> [Char] -> IO [Benchmark]
benchmarkFitchOptimization seqsFile treeFile prefix = do
    !seqs <- getSeqsFromFile seqsFile
    !tree <- getTreeFromFile treeFile
    let !weight = 1
    let !names  = --trace ("names " ++ show (head $  filter (not.null) . fmap nodeName <$> fmap V.toList tree))
                    head $ filter (not.null) . fmap nodeName <$> fmap V.toList tree
    pure [ bench (prefix++" packing & optimizing S64")   $ nf (costForest tree (performPack seqs names tree ("static"  ,"64"      ))) weight
         , bench (prefix++" packing & optimizing Inf")   $ nf (costForest tree (performPack seqs names tree ("static"  ,"infinite"))) weight
         , bench (prefix++" packing & optimizing unpacked")   $ nf (costForest tree (performBuild seqs names tree)) weight
         , bench (prefix++" packing & optimizing GPU Nodes, S64") $ nf (PO.costForest tree (PB.performPack (0 :: Word64) seqs names tree ("static"  ,"64"      ) PN.GPU)) weight
         , bench (prefix++" packing & optimizing CPU Nodes, S64") $ nf (PO.costForest tree (PB.performPack (0 :: Word64) seqs names tree ("static"  ,"64"      ) PN.CPU)) weight
         , bench (prefix++" packing & optimizing Normal Par Nodes, S64") $ nf (PO.costForest tree (PB.performPack (0 :: Word64) seqs names tree ("static"  ,"64"      ) PN.Normal)) weight
         ]

--benchmarkFitchOnly :: FilePath -> FilePath -> [Char] -> IO [Benchmark]
--benchmarkFitchOnly seqsFile treeFile prefix = do
--  !seqs <- getSeqsFromFile seqsFile
--  !tree <- getTreeFromFile treeFile
--  let !weight = 1
--  let !names  = head $ filter (not.null) . fmap nodeName <$> fmap V.toList tree
--  let !packA16 = performPack seqs names tree ("adaptive","16"      )
--  let !packS16 = performPack seqs names tree ("static"  ,"16"      )
--  let !packA64 = performPack seqs names tree ("adaptive","64"      )
--  let !packS64 = performPack seqs names tree ("static"  ,"64"      )
--  let !packInf = performPack seqs names tree ("static"  ,"infinite")
--  let !unpacked = performBuild seqs names tree
--  let f = flip (costForest tree) weight
--  pure [ bench (prefix++" just optimizing A16")   $ nf f packA16
--         , bench (prefix++" just optimizing S16")   $ nf f packS16
--         , bench (prefix++" just optimizing A64")   $ nf f packA64
--         , bench (prefix++" just optimizing S64")   $ nf f packS64
--         , bench (prefix++" just optimizing Inf")   $ nf f packInf
--         , bench (prefix++" just optimizing unpacked")   $ nf (costForest tree unpacked) weight
--        ]

--benchmarkPackingOnly :: FilePath -> FilePath -> [Char] -> IO [Benchmark]
--benchmarkPackingOnly seqsFile treeFile prefix = do
--  !seqs <- getSeqsFromFile seqsFile
--  !tree <- getTreeFromFile treeFile
--  let !names  = head $ filter (not.null) . fmap nodeName <$> fmap V.toList tree
--  let f = performPack seqs names tree
--  pure [ bench (prefix++" packing A16")   $ nf f ("adaptive","16"      )
--         , bench (prefix++" packing S16")   $ nf f ("static"  ,"16"      )
--         , bench (prefix++" packing A64")   $ nf f ("adaptive","64"      )
--         , bench (prefix++" packing S64")   $ nf f ("static"  ,"64"      )
--         , bench (prefix++" packing Inf")   $ nf f ("static"  ,"infinite")
--         , bench (prefix++" packing unpacked")   $ nf (performBuild seqs names) tree
--         ]