{-# LANGUAGE BangPatterns #-}
module Packing.PackedTest where

import Control.Arrow         ((&&&),first)
import Criterion.Main
import Data.Functor.Identity (runIdentity)
import Data.List             (partition, nub)
import Data.Map              (Map,elems,empty,insert,insertWith,lookup, toList)
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
import           Text.Parsec
import File.Format.Fasta
import Component
import CharacterData
import Packing.UnpackedBuild

type TaxonSequenceMap  = Map String (V.Vector [String])

main :: IO ()
main = do
    benches <- sequence 
               [ --benchmarkFitchOptimization "data-sets/artmor.fastc"    "data-sets/artmor.tree"    "(Small ):"
                --benchmarkFitchOptimization "data-sets/spider-10.fastc" "data-sets/spider-10.tree" "(Medium):"
               --, benchmarkFitchOptimization "data-sets/spider.fastc"    "data-sets/spider.tree"    "(Large ):"
                  benchmarkFitchOptimization "data-sets/28S_trimal.fas" "data-sets/sp_rand_28s.tre" "(Medium Fasta: )"
               ]
    defaultMain [ bgroup "fitch opts" (concat benches) ]

getSeqsFromFile :: FilePath -> IO RawData
getSeqsFromFile file = newToOld . fromRight . parse (fastaStreamConverter DNA =<< fastaStreamParser) file <$> readFile file
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
    pure [ bench (prefix++" packing & optimizing A16")   $ nf (costForest tree (performPack seqs names tree ("adaptive","16"      ))) weight
         , bench (prefix++" packing & optimizing S16")   $ nf (costForest tree (performPack seqs names tree ("static"  ,"16"      ))) weight
         , bench (prefix++" packing & optimizing A64")   $ nf (costForest tree (performPack seqs names tree ("adaptive","64"      ))) weight
         , bench (prefix++" packing & optimizing S64")   $ nf (costForest tree (performPack seqs names tree ("static"  ,"64"      ))) weight
         , bench (prefix++" packing & optimizing Inf")   $ nf (optimizeForest tree (performPack seqs names tree ("static"  ,"infinite"))) weight
         , bench (prefix++" packing & optimizing old pcg")   $ nf (costForest tree (performBuild seqs names tree)) weight
         ]

--benchmarkOldOptimization :: FilePath -> FilePath -> [Char] -> IO [Benchmark]
--benchmarkOldOptimization seqsFile treeFile prefix = do
--    !seqs <- getSeqsFromFile seqsFile
--    !tree <- getTreeFromFile treeFile
--    let !names  = head $ filter (not.null) . fmap nodeName <$> fmap V.toList tree
--    let !weight = 1
--    pure [ bench (prefix++" packing & optimizing old pcg")   $ nf (runOrigPCG seqs tree) names]

fromRight :: Show a => Either a b -> b
fromRight (Right x) = x
fromRight (Left  x) = error $ show x

newToOld :: TaxonSequenceMap -> RawData
newToOld taxseq = 
  let 
    listForm = toList taxseq
    charToList = map (\(name, vec) -> (name, V.toList $ V.map (\pos -> head pos) vec)) listForm
    alphabet = nub $ foldr (\(name, charList) acc -> charList ++ acc) [] charToList
    info = trace ("set alph " ++ show alphabet)
            CharInfo NucSeq False 1.0 [] "Input" (length $ snd $ head listForm) alphabet 0.0
    infoList = replicate (length charToList) info
  in (charToList, infoList)

--runOrigPCG :: RawData -> PhyloForest -> [String] -> [Float]
----runOrigPCG inputData forest names | trace ("run old with input data " ++ show (head $ fst inputData) ++ "forest" ++ show (head forest) ++ " names " ++ show names) False = undefined
--runOrigPCG inputData forest names = 
--  let 
--    rootIndices = map (\tree -> V.foldr (\i acc -> if isRoot (tree V.! i) then i else acc) 0 (V.fromList [0..length tree -1])) forest
--    reIndexForest = zipWith (\tree rootPos -> V.map (\i -> if i < rootPos then modifyNodeCode (tree V.! i) (i - 1) else tree V.! i) (V.fromList [0..V.length tree -1])) forest rootIndices
--    reorderForest = --trace ("rootIndices " ++ show rootIndices)
--                      zipWith (\tree rootPos -> (V.take (rootPos - 1) tree) V.++ (V.drop rootPos tree) V.++ (V.singleton (modifyNodeCode (tree V.! rootPos) (V.length tree - 1)))) reIndexForest rootIndices
--    phyloData = trace ("phyloData " ++ show reorderForest)
--                createBaseData inputData
--    newCharInfo = trace ("update char ifo " ++ show phyloData)
--                    redoRootCosts phyloData (snd inputData) 0
--    inCost = trace ("get cost " ++ show newCharInfo)
--              getForestCostList phyloData [reorderForest] newCharInfo names
--  in inCost

modifyNodeCode :: PhyloNode -> Int -> PhyloNode
modifyNodeCode node newCode = node {code = newCode}

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
