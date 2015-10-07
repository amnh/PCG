{-# LANGUAGE BangPatterns #-}
module Packing.PackedTest where

import Control.Arrow         ((&&&),first)
import Criterion.Main
import Data.Functor.Identity (runIdentity)
import Data.List             (partition)
import Data.Map              (Map,elems,empty,insert,insertWith,lookup)
import Data.Maybe
import qualified Data.Vector as V
import Debug.Trace           (trace)
import File.Format.Fastc.Parser
import File.Format.Newick
import CharacterData
import ReadFiles
import Component
import Packing.PackedBuild
import Packing.PackedOptimize
import Prelude hiding (lookup)

main :: IO ()
main = do
    benches <- sequence 
               [ benchmarkFitchOptimization "data-sets/artmor.fastc"    "data-sets/artmor.tree"    "(Small ):"
               --, benchmarkFitchOptimization "data-sets/spider-10.fastc" "data-sets/spider-10.tree" "(Medium):"
               --, benchmarkFitchOptimization "data-sets/spider.fastc"    "data-sets/spider.tree"    "(Large ):"
               ]
    defaultMain [ bgroup "fitch opts" (concat benches) ]

getSeqsFromFile :: FilePath -> IO RawData
getSeqsFromFile file = fastcToCharData . fromRight . runIdentity  . parseFastcStream  <$> readFile file
getTreeFromFile :: FilePath -> IO PhyloForest
getTreeFromFile file = head . fmap newickToPhylo . fromRight . runIdentity  . parseNewickStream <$> readFile file

benchmarkFitchOptimization :: FilePath -> FilePath -> [Char] -> IO [Benchmark]
benchmarkFitchOptimization seqsFile treeFile prefix = do
    !seqs <- getSeqsFromFile seqsFile
    --putStrLn(show seqs)
    -- !preTree <- fromRight . runIdentity  . parseNewickStream <$> readFile treeFile
    --putStrLn(show preTree)
    !tree <- getTreeFromFile treeFile
    --putStrLn(show tree)
    let !weight = 1
    let !names  = --trace ("names " ++ show (head $  filter (not.null) . fmap nodeName <$> fmap V.toList tree))
                    head $ filter (not.null) . fmap nodeName <$> fmap V.toList tree
    pure [ bench (prefix++" packing & optimizing A16")   $ nf (optimizeForest tree (performPack seqs names tree ("adaptive","16"      ))) weight
         , bench (prefix++" packing & optimizing S16")   $ nf (optimizeForest tree (performPack seqs names tree ("static"  ,"16"      ))) weight
         , bench (prefix++" packing & optimizing A64")   $ nf (optimizeForest tree (performPack seqs names tree ("adaptive","64"      ))) weight
         , bench (prefix++" packing & optimizing S64")   $ nf (optimizeForest tree (performPack seqs names tree ("static"  ,"64"      ))) weight
         --, bench (prefix++" packing & optimizing Inf")   $ whnf (optimizeForest tree (performPack seqs names tree ("static"  ,"infinite"))) weight
         ]
fromRight :: Show a => Either a b -> b
fromRight (Right x) = x
fromRight (Left  x) = error $ show x

fastcToCharData :: [FastcData] -> RawData
fastcToCharData = (map fastcToTermData &&& map fastcToCharInfo)
  where
    fastcToTermData :: FastcData -> TermData
    fastcToTermData = fastcLabel &&& fastcSymbols
    fastcToCharInfo :: FastcData -> CharInfo
    fastcToCharInfo (FastcData _ syms) = CharInfo NucSeq False 1.0 [] "Input" (length syms) [] 0.0


newickToPhylo :: NewickForest -> PhyloForest
newickToPhylo = fmap treeToComponent
  where

    treeToComponent :: NewickNode -> PhyloComponent--V.Vector PhyloNode
    treeToComponent newickTree = V.fromList $ toPhyloNodes newickTree
      where
        toPhyloNodes = fmap toPhyloNode
                     . referenceEdges
                     . zipWith setIndex' [0..] 
                     . uncurry (++) . first collate'
                     . partition hasLabel . setRoot' . toProtoNodes []
        hasLabel :: (NewickNode, [NewickNode], NodeCode, Bool) -> Bool
        hasLabel (x,_,_,_) = isJust $ newickLabel x

        toProtoNode :: [NewickNode] -> NewickNode -> (NewickNode, [NewickNode], NodeCode, Bool)
        toProtoNode  parent node = --trace ("grab node")
                                    (node, parent, 0, False)

        toProtoNodes :: [NewickNode] -> NewickNode -> [(NewickNode, [NewickNode], NodeCode, Bool)]
        toProtoNodes parent node = --trace ("toProtoNodes with parent " ++ show parent ++ "and node " ++ show node)
                                    --(toProtoNode parent node :) $ toProtoNode [node] <$> descendants node
                                    let
                                        curNode = toProtoNode parent node
                                        restNodes = foldr (\child acc -> acc ++ (toProtoNodes [node] child)) [] (descendants node)
                                    in curNode : restNodes

        setRoot'             []  = []
        setRoot' ((n,p,c,_):xs)  = (n,p,c,True):xs

        referenceEdges :: [(NewickNode, [NewickNode], NodeCode, Bool)] -> [(NewickNode, [NodeCode], [NodeCode], NodeCode, Bool)]
        referenceEdges xs = updateEdges <$> xs
         where
            mapping :: Map NewickNode NodeCode
            mapping     = foldr h empty xs
              where 
                h :: (NewickNode, [NewickNode], NodeCode, Bool) -> Map NewickNode NodeCode -> Map NewickNode NodeCode
                h = (\(n,_,i,_) m -> insert n i m)
            updateEdges :: (NewickNode, [NewickNode], NodeCode, Bool) -> (NewickNode, [NodeCode], [NodeCode], NodeCode, Bool)
--            updateEdges (n,p,i,r) = (n,(mapping!)<$>(descendants n),<$>p,i,r)
            updateEdges (n,p,i,r) = 
                (n
                ,h<$>(descendants n)
                ,h<$>p
                ,i
                ,r)
                where
                  h x = case lookup x mapping of
                         Just y  -> y
                         Nothing -> 0

        collate' :: [(NewickNode, [NewickNode], NodeCode, Bool)] -> [(NewickNode, [NewickNode], NodeCode, Bool)]
        collate' = elems . foldr (\(x,p,i,r) m -> insertWith g (newickLabel x) (x,p,i,r) m) empty
        g :: (NewickNode, [NewickNode], NodeCode, Bool) -> (NewickNode, [NewickNode], NodeCode, Bool) -> (NewickNode, [NewickNode], NodeCode, Bool)
        g (n,p,c,r) (_,q,_,s) = (n,p++q,c,r||s)
        setIndex' :: Int -> (NewickNode, [NewickNode], NodeCode, Bool) -> (NewickNode, [NewickNode], NodeCode, Bool)
        setIndex' i (n,p,_,r) = (n,p,i,r)
        toPhyloNode :: (NewickNode, [NodeCode], [NodeCode], NodeCode, Bool) -> PhyloNode
        toPhyloNode (n,c,p,i,r) = PhyloNode i nodeName' isTerminal' r isTreeNode' children' p preliminaryStates' localCost' totalCost'
          where
            nodeName'          = maybe "" id $ newickLabel n
            isTerminal'        = null children'
--            isRoot'            = False
            isTreeNode'        = not $ null children'
            children'          = c
            preliminaryStates' = [] :: CharacterSetList
            localCost'         = V.fromList [] :: (V.Vector Float)
            totalCost'         = V.fromList [] :: (V.Vector Float)