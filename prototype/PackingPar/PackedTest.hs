module PackedTest where

import System.IO
import System.Environment
import qualified Data.Text as T
import qualified Data.Set as Set
import CharacterData
import ReadGraphs
import ProcessCommands
import ExecuteCommands
import Component
import PackedBuild
import PackedOptimize

-- | 'pcg' Main Function to run PCG
main :: IO()
main = 
    do
        --get input command filename
        args <- getArgs
        scriptFileHandle <- checkScriptInfo args
        commandDataString <- hGetContents scriptFileHandle
        let commandList = reverse $ parseCommandList (T.pack commandDataString)
        hPutStrLn stderr ("Command list " ++ show commandList)
        let (readDataList, readGraphList, _, _, _) = parseCommands commandDataString 
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
        let inWeight = 0.5
        let curForestList = --trace ("forest "++ show inputGraphList) 
                            baseDataToLeafNodes inputGraphList
        let pack = --trace ("pack "++ show curForestList) 
                           performPack finalInput termNameList (head curForestList) ("adaptive","16")
        --putStrLn("packed " ++ show (V.head $ V.head $! allPacked))
        let optimized = --trace ("optimize with pack info " ++ show packInfo) 
                            optimizeForest (head curForestList) pack inWeight
        let costs = map (\dat -> getRootCost $ fst dat) optimized
        hPutStrLn stderr ("Tree cost done " ++ show costs)


        ---- | PCG comparison code
        --let inputData  = flattenCharList pairDataList  
        --let phyloData = createBaseData inputData
        --let newCharInfo = redoRootCosts phyloData (snd inputData) 0
        --let curForestList = baseDataToLeafNodes inputGraphList
        --hPutStrLn stderr ("Data recoded " ++ show (V.length phyloData) ++ " leaves in " 
        --    ++ show (length $ head curForestList) ++ " components" ++ " with root cost " 
        --    ++ show (getRootCosts newCharInfo))
        ----hPrint stderr curForestList
        --let inCost = getForestCostList phyloData curForestList newCharInfo termNameList
        --hPutStrLn stderr ("At cost " ++ show (head inCost))
