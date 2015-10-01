{-# LANGUAGE BangPatterns #-}
module Packing.PackedTest where

import Control.Arrow         ((&&&),first)
import Criterion.Main
import Criterion.Types
import Data.Functor.Identity (runIdentity)
import Data.List             (partition)
import Data.Map              (Map,(!),elems,empty,insert,insertWith,lookup)
import Data.Maybe
import Data.Vector           (Vector,fromList,toList)
import File.Format.Fastc.Parser
import File.Format.Newick
--import qualified Data.Set as Set
import CharacterData
import ReadFiles
--import ReadGraphs
import Component
import Packing.PackedBuild
import Packing.PackedOptimize
import Prelude hiding (lookup)

import Debug.Trace (trace)

{-
-- | 'pcg' Main Function to run PCG
main :: IO ()
main = 
    do
        --get input command filename
        args <- getArgs
        scriptFileHandle <- checkScriptInfo args
        commandDataString <- hGetContents scriptFileHandle
        let commandList = reverse $ parseCommandList (T.pack commandDataString)
        hPutStrLn stderr ("Command list " ++ show commandList)
        let (readDataList, readGraphList, reportList, exitList, analysisList) = parseCommands commandDataString 
        let pairDataList = getReadContents readDataList
        let inputGraphList = getGraphContents readGraphList
        if not $ null inputGraphList then printGraphVizDot (head inputGraphList) "tempFile.dot"
        else hPutStrLn stderr "No input graph to report"
        let finalInput  = flattenCharList pairDataList  
        let termNameList = getNameList $ fst finalInput
        --Check that leaves on graph and input data agree
        let graphAndDataJibe = checkGraphAndData (Set.fromList termNameList) inputGraphList
        if graphAndDataJibe then hPutStrLn stderr "Input graphs and data jibe"
        else error "Input graphs and data terminals are inconsistent"

        -- | Beginning of section to pack and fitch optimize
        let weight = 1
        --let cInfo = head $ snd finalInput
--        let curForestList = --trace ("forest "++ show inputGraphList) 
--                            baseDataToLeafNodes inputGraphList
        let pack@(allPacked, packInfo, pMode) = --trace ("pack "++ show curForestList) 
                           performPack finalInput termNameList (head curForestList) ("adaptive","16")
        --putStrLn("packed " ++ show (V.head $ V.head $! allPacked))
        let optimized = --trace ("optimize with pack info " ++ show packInfo) 
                            optimizeForest (head curForestList) pack weight
        let costs = map (\dat -> getRootCost $ fst dat) optimized
        hPutStrLn stderr ("Tree cost done " ++ show costs)
-}

main = do
    benches <- sequence 
               [ benchmarkFitchOptimization "data-sets/artmor.fastc"    "data-sets/artmor.tree"    "(Small ):"
               , benchmarkFitchOptimization "data-sets/spider-10.fastc" "data-sets/spider-10.tree" "(Medium):"
               , benchmarkFitchOptimization "data-sets/spider.fastc"    "data-sets/spider.tree"    "(Large ):"
               ]
    defaultMain [ bgroup "fitch opts" (concat benches) ]

getSeqsFromFile file =           fastcToCharData . fromRight . runIdentity  . parseFastcStream  <$> readFile file
getTreeFromFile file = head . fmap newickToPhylo . fromRight . runIdentity  . parseNewickStream <$> readFile file

benchmarkFitchOptimization seqsFile treeFile prefix = do
    !seqs <- getSeqsFromFile seqsFile
    !tree <- getTreeFromFile treeFile
    let !weight = 1
    let !names  = concat . filter (not.null) . fmap nodeName <$> fmap toList tree
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
fastcToCharData = (pure . fastcToTermData &&& pure . fastcToCharInfo) . head
  where
    fastcToTermData :: FastcData -> TermData
    fastcToTermData = fastcLabel &&& pure . fastcLabel
    fastcToCharInfo :: FastcData -> CharInfo
    fastcToCharInfo (FastcData name syms) = CharInfo NucSeq False 1.0 [] "Input" (length syms) syms 0.0


newickToPhylo :: NewickForest -> PhyloForest
newickToPhylo = fmap treeToComponent
  where

    treeToComponent :: NewickNode -> PhyloComponent --V.Vector PhyloNode
    treeToComponent = fromList . toPhyloNodes
      where
        toPhyloNodes = fmap toPhyloNode
                     . referenceEdges
                     . zipWith setIndex' [0..] 
                     . uncurry (++) . first collate'
                     . partition hasLabel . setRoot' . toProtoNodes []
        hasLabel :: (NewickNode, [NewickNode], NodeCode, Bool) -> Bool
        hasLabel (x,_,_,_) = isJust $ newickLabel x
        toProtoNode :: [NewickNode] -> NewickNode -> (NewickNode, [NewickNode], NodeCode, Bool)
        toProtoNode  parent node = (node, parent, 0, False)
        toProtoNodes parent node = (toProtoNode parent node :) $ toProtoNode [node] <$> descendants node
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
                         Nothing -> trace (show x) 0
        setRoot' ((n,p,c,_):xs)  = (n,p,c,True):xs
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
            isRoot'            = False
            isTreeNode'        = not $ null children'
            children'          = c
            preliminaryStates' = [] :: CharacterSetList
            localCost'         = fromList [] :: (Vector Float)
            totalCost'         = fromList [] :: (Vector Float)