{-# LANGUAGE BangPatterns #-}
module PackingPar.ParBench where

import Criterion.Main
import qualified Data.Vector as V
import Debug.Trace           (trace)
import Component
import CharacterData
import Packing.UnpackedBuild
import Packing.PackedTest (getSeqsFromFile, getTreeFromFile)
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
               [ --benchmarkFitchOptimization "data-sets/28S_trimal.fas" "data-sets/sp_rand_28s.tre" "(Medium Fasta: )"
               -- , benchmarkPackingOnly "data-sets/28S_trimal.fas" "data-sets/sp_rand_28s.tre" "(Medium Fasta: )"
               -- , benchmarkFitchOnly "data-sets/28S_trimal.fas" "data-sets/sp_rand_28s.tre" "(Medium Fasta: )"
                benchmarkFitchOptimization "data-sets/IASallToFasta.fas" "data-sets/sall_o0g1t1_satf27-newick.tre" "(Large Fasta: )"
                , benchmarkPackingOnly "data-sets/IASallToFasta.fas" "data-sets/sall_o0g1t1_satf27-newick.tre" "(Large Fasta: )"
                , benchmarkFitchOnly "data-sets/IASallToFasta.fas" "data-sets/sall_o0g1t1_satf27-newick.tre" "(Large Fasta: )"
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
         --, bench (prefix++" packing & optimizing unpacked")   $ nf (costForest tree (performBuild seqs names tree)) weight
         --, bench (prefix++" packing & optimizing GPU Nodes, S64") $ nf (PO.costForest tree (PB.performPack (0 :: Word64) seqs names tree ("static"  ,"64"      ) PN.GPU) weight) 0
         , bench (prefix++" packing & optimizing CPU Nodes, S64") $ nf (PO.costForest tree (PB.performPack (0 :: Word64) seqs names tree ("static"  ,"64"      ) PN.CPU) weight) 0
         , bench (prefix++" packing & optimizing Normal Par Nodes, S64") $ nf (PO.costForest tree (PB.performPack (0 :: Word64) seqs names tree ("static"  ,"64"      ) PN.Normal) weight) 0
         , bench (prefix++" packing & parallel optimizing Normal Par Nodes, S64") $ nf (PO.costForest tree (PB.performPack (0 :: Word64) seqs names tree ("static"  ,"64"      ) PN.Normal) weight) 1
         , bench (prefix++" packing & full parallel optimizing Normal Par Nodes, S64") $ nf (PO.costForest tree (PB.performPack (0 :: Word64) seqs names tree ("static"  ,"64"      ) PN.Normal) weight) 2
         , bench (prefix++" packing & parallel optimizing CPU Nodes, S64") $ nf (PO.costForest tree (PB.performPack (0 :: Word64) seqs names tree ("static"  ,"64"      ) PN.CPU) weight) 1
         , bench (prefix++" packing & full parallel optimizing CPU Nodes, S64") $ nf (PO.costForest tree (PB.performPack (0 :: Word64) seqs names tree ("static"  ,"64"      ) PN.Normal) weight) 2
         ]

benchmarkFitchOnly :: FilePath -> FilePath -> [Char] -> IO [Benchmark]
benchmarkFitchOnly seqsFile treeFile prefix = do
  !seqs <- getSeqsFromFile seqsFile
  !tree <- getTreeFromFile treeFile
  let !weight = 1
  let !names  = head $ filter (not.null) . fmap nodeName <$> fmap V.toList tree
  let !packS64 = performPack seqs names tree ("static"  ,"64"      )
  let !packInf = performPack seqs names tree ("static"  ,"infinite")
  let !packGPU = PB.performPack (0 :: Word64) seqs names tree ("static"  ,"64"      ) PN.GPU
  let !packCPU = PB.performPack (0 :: Word64) seqs names tree ("static"  ,"64"      ) PN.CPU
  let !packParNormal = PB.performPack (0 :: Word64) seqs names tree ("static"  ,"64"      ) PN.Normal
  let f = flip (costForest tree) weight
  pure [ bench (prefix++" just optimizing S64")   $ nf f packS64
         , bench (prefix++" just optimizing Inf")   $ nf f packInf
         --, bench (prefix++" just sequential optimizing GPU")   $ nf (PO.costForest tree packGPU weight) 0
         , bench (prefix++" just sequential optimizing CPU")   $ nf (PO.costForest tree packCPU weight) 0
         , bench (prefix++" just sequential optimizing Par Normal")   $ nf (PO.costForest tree packParNormal weight) 0
         --, bench (prefix++" just parallel optimizing GPU")   $ nf (PO.costForest tree packGPU weight) 1
         , bench (prefix++" just parallel optimizing CPU")   $ nf (PO.costForest tree packCPU weight) 1
         , bench (prefix++" just parallel optimizing Par Normal")   $ nf (PO.costForest tree packParNormal weight) 1
         , bench (prefix++" just full parallel optimizing CPU")   $ nf (PO.costForest tree packCPU weight) 2
         , bench (prefix++" just parallel optimizing Par Normal")   $ nf (PO.costForest tree packCPU weight) 2
        ]

benchmarkPackingOnly :: FilePath -> FilePath -> [Char] -> IO [Benchmark]
benchmarkPackingOnly seqsFile treeFile prefix = do
  !seqs <- getSeqsFromFile seqsFile
  !tree <- getTreeFromFile treeFile
  let !names  = head $ filter (not.null) . fmap nodeName <$> fmap V.toList tree
  let f = performPack seqs names tree
  pure [ bench (prefix++" packing S64")   $ nf f ("static"  ,"64"      )
         , bench (prefix++" packing Inf")   $ nf f ("static"  ,"infinite")
         --, bench (prefix++" packing unpacked")   $ nf (performBuild seqs names) tree
         --, bench (prefix++" packing GPU Nodes, S64") $ nf (PB.performPack (0 :: Word64) seqs names tree ("static"  ,"64"      )) PN.GPU
         , bench (prefix++" packing CPU Nodes, S64") $ nf (PB.performPack (0 :: Word64) seqs names tree ("static"  ,"64"      )) PN.CPU
         , bench (prefix++" packing Normal Par Nodes, S64") $ nf (PB.performPack (0 :: Word64) seqs names tree ("static"  ,"64"      )) PN.Normal
         ]